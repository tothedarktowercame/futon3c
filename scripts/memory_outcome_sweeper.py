#!/usr/bin/env python3
"""Append outcome-half memory-use receipts from the durable invoke-job ledger.

The sweeper is deliberately offline from the serving JVM.  Idempotency is
keyed by Agency job id: existing outcome receipts are discovered by bounded
receipt read, and this writer additionally uses a deterministic evidence id.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import subprocess
import sys
import urllib.error
import urllib.parse
import urllib.request
from collections import Counter, defaultdict
from collections.abc import Mapping, Sequence
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

from edn_format import Keyword, dumps, loads

sys.path.insert(0, str(Path(__file__).resolve().parent))
import runner_gate  # noqa: E402


K = Keyword
LEDGER_PATH = Path(os.environ.get("FUTON3C_INVOKE_JOBS_FILE", "/tmp/futon3c-invoke-jobs.edn"))
SUBSTRATE_URL = os.environ.get("FUTON_SUBSTRATE_URL", "http://127.0.0.1:7073").rstrip("/")
ROLLOUT_AT = datetime.fromisoformat("2026-08-01T15:10:00+00:00")
ATTRIBUTION_GATE_ROLLOUT_AT = datetime.fromisoformat(
    os.environ.get("RUNNER_GATE_ROLLOUT_AT", "2026-08-02T13:56:00+00:00")
)
TERMINAL_STATES = {"done", "failed", "timeout", "cancelled", "deduped"}
NO_MEMORY_OUTCOMES = {"completed-empty", "timeout", "store-unavailable", "recall-error"}
MEMORY_ID_RE = re.compile(r"\be-[A-Za-z0-9][A-Za-z0-9_-]{5,}\b")
OUTCOME_RE = re.compile(r"\[dispatch-recall-outcome=([a-z-]+)\]")
MEMORY_USAGE_HEADING_RE = re.compile(
    r"(?im)^\s*(?:#{1,6}\s*)?memory usage\s*:?[ \t]*$"
)
PROBLEM_RE = re.compile(r"\b(?:a|bpm-)[0-9A-Za-z.-]*[AJ][0-9]{2}\b", re.IGNORECASE)
NEGATIVE_MARKERS = (
    "ignored", "irrelevant", "not used", "unused", "did not use",
    "not applicable", "no role", "unrelated",
)
POSITIVE_MARKERS = (
    "used", "useful", "applied", "guided", "carried", "influenced",
    "prevented", "prompted", "directly", "conceptually", "relied",
)


def now_iso() -> str:
    return datetime.now(timezone.utc).isoformat().replace("+00:00", "Z")


def value(mapping: Any, name: str, default: Any = None) -> Any:
    if not isinstance(mapping, Mapping):
        return default
    return mapping.get(K(name), mapping.get(name, default))


def parse_time(raw: Any) -> datetime | None:
    if not isinstance(raw, str):
        return None
    try:
        parsed = datetime.fromisoformat(raw.replace("Z", "+00:00"))
    except ValueError:
        return None
    if parsed.tzinfo is None:
        parsed = parsed.replace(tzinfo=timezone.utc)
    return parsed.astimezone(timezone.utc)


def deterministic_evidence_id(job_id: str) -> str:
    digest = hashlib.sha256(job_id.encode("utf-8")).hexdigest()[:24]
    return f"e-memory-outcome-sweeper-{digest}"


def deterministic_gate_evidence_id(job_id: str) -> str:
    digest = hashlib.sha256(job_id.encode("utf-8")).hexdigest()[:24]
    return f"e-use-attribution-gate-{digest}"


def dispatch_evidence_entries(job_id: str, base: str = SUBSTRATE_URL) -> list[Any]:
    """Read evidence explicitly keyed to one Agency dispatch."""
    query = urllib.parse.urlencode({"session-id": job_id, "limit": 500, "sort": "desc"})
    request = urllib.request.Request(
        f"{base}/api/alpha/evidence?{query}", headers={"Accept": "application/edn"}
    )
    with urllib.request.urlopen(request, timeout=30) as response:
        payload = loads(response.read().decode("utf-8"))
    return list(value(payload, "entries", []))


def pull_surfaced_ids(job_id: str, base: str = SUBSTRATE_URL) -> list[str]:
    """Union per-call pull offers for exactly one dispatch."""
    offered: set[str] = set()
    for entry in dispatch_evidence_entries(job_id, base):
        body = value(entry, "evidence/body", {})
        if (value(body, "event") == K("memory-pull-offer")
                and value(body, "dispatch-id") == job_id):
            offered.update(str(item) for item in value(body, "pull-surfaced-ids", []))
    return sorted(offered)


def offered_surfaced_ids(job_id: str, base: str = SUBSTRATE_URL) -> list[str]:
    """Return the complete push-union-pull offered set for one dispatch."""
    offered: set[str] = set()
    found = False
    for entry in dispatch_evidence_entries(job_id, base):
        body = value(entry, "evidence/body", {})
        if value(body, "phase") == K("offered") and value(body, "job-id") == job_id:
            receipt = value(body, "memory-use", {})
            offered.update(str(item) for item in value(receipt, "memory-use/surfaced-ids", []))
            found = True
        elif (value(body, "event") == K("memory-pull-offer")
              and value(body, "dispatch-id") == job_id):
            offered.update(str(item) for item in value(body, "pull-surfaced-ids", []))
            found = True
    if not found:
        raise LookupError(f"offered receipt not found for job {job_id}")
    return sorted(offered)


def deposit_correction(violation: runner_gate.Violation, message: str,
                       base: str = SUBSTRATE_URL) -> dict[str, Any]:
    """Deposit and review through memory-write + memory-lifecycle, not raw rows."""
    payload = {
        K("agent"): violation.agent,
        K("job-id"): violation.run_id,
        K("missing-ids"): violation.missing_ids,
        K("feedback"): message,
        K("substrate-url"): base,
    }
    process = subprocess.run(
        ["clojure", "-M", "scripts/deposit_runner_gate_memory.clj"],
        cwd=Path(__file__).resolve().parent.parent,
        input=dumps(payload), text=True, capture_output=True, timeout=180, check=False,
        env={**os.environ, "FUTON_SUBSTRATE_URL": base},
    )
    if process.returncode:
        raise RuntimeError(process.stderr[-1000:] or process.stdout[-1000:])
    result = loads(process.stdout.strip().splitlines()[-1])
    return {
        "memory_id": str(value(result, "memory-id")),
        "attachment_status": str(value(result, "attachment-status")),
        "review_evidence_id": str(value(result, "review-evidence-id")),
    }


def gate_evidence_entry(job_id: str, agent: str, adjudication: dict[str, Any],
                        swept_at: str) -> dict[Any, Any]:
    evidence_id = deterministic_gate_evidence_id(job_id)
    return {
        K("evidence/id"): evidence_id,
        K("evidence/subject"): {K("ref/type"): K("agency-job"), K("ref/id"): job_id},
        K("evidence/type"): K("pattern-outcome"),
        K("evidence/claim-type"): K("observation"),
        K("evidence/at"): swept_at,
        K("evidence/author"): "runner-use-attribution-gate",
        K("evidence/session-id"): job_id,
        K("evidence/body"): {
            K("event"): K("memory-use"), K("phase"): K("outcome"),
            K("job-id"): job_id, K("agent"): agent,
            K("writer"): K("runner-use-attribution-gate"),
            K("memory-use/status"): K(adjudication["run_status"]),
            K("counts-toward-endpoints"): False,
            K("missing-ids"): adjudication.get("missing_ids", []),
            K("verdict"): K(adjudication["verdict"]),
            K("adjudication"): json.dumps(adjudication, sort_keys=True),
        },
        K("evidence/tags"): [K("memory-use"), K("attribution-incomplete"), K("runner-gate")],
    }


def classify_memory_lines(result: str) -> tuple[set[str], set[str], set[str]]:
    used: set[str] = set()
    ignored: set[str] = set()
    unknown: set[str] = set()
    for line in result.splitlines():
        ids = set(MEMORY_ID_RE.findall(line))
        if not ids:
            continue
        lowered = line.lower()
        if any(marker in lowered for marker in NEGATIVE_MARKERS):
            ignored.update(ids)
        elif any(marker in lowered for marker in POSITIVE_MARKERS):
            used.update(ids)
        else:
            unknown.update(ids)
    unknown.difference_update(used | ignored)
    used.difference_update(ignored)
    return used, ignored, unknown


def memory_usage_section(result: str) -> str | None:
    matches = list(MEMORY_USAGE_HEADING_RE.finditer(result))
    if not matches:
        return None
    return result[matches[-1].end():]


def extract_outcome(job_id: str, job: Any) -> dict[str, Any]:
    result = value(job, "result")
    state = str(value(job, "state", ""))
    if not isinstance(result, str) or not result.strip():
        return {"recoverable": False, "reason": "not-recall-dispatch"}
    match = OUTCOME_RE.search(result)
    usage_section = memory_usage_section(result)
    if not match and usage_section is None:
        return {"recoverable": False, "reason": "not-recall-dispatch"}
    if state not in TERMINAL_STATES:
        return {"recoverable": False, "reason": "nonterminal-recall-dispatch"}
    recall_outcome = match.group(1) if match else "legacy-unknown"
    used, ignored, unknown = classify_memory_lines(usage_section or result)
    all_ids = used | ignored | unknown
    explicit_none = bool(re.search(
        r"(?i)(?:no\s+(?:dispatch-time\s+)?memories?|none).{0,40}"
        r"(?:used|supplied|surfaced)",
        usage_section or result,
    ))
    if recall_outcome in NO_MEMORY_OUTCOMES:
        method = "dispatch-recall-outcome-no-memories"
        used = set()
        unknown = set()
    elif unknown:
        return {
            "recoverable": False,
            "reason": "unclassified-memory-lines",
            "unknown_ids": sorted(unknown),
        }
    elif used or ignored or explicit_none:
        method = (
            "legacy-final-memory-usage-line-classification"
            if not match
            else "final-memory-usage-line-classification"
        )
    else:
        return {"recoverable": False, "reason": "no-memory-attribution"}
    problem_match = PROBLEM_RE.search(result)
    return {
        "recoverable": True,
        "job_id": job_id,
        "used_ids": sorted(used),
        "surfaced_ids": sorted(all_ids),
        "ignored_ids": sorted(ignored),
        "recall_outcome": recall_outcome,
        "extraction_method": method,
        "problem": problem_match.group(0) if problem_match else None,
        "session_id": str(value(job, "session-id", job_id) or job_id),
        "finished_at": value(job, "finished-at"),
    }


def load_jobs(path: Path = LEDGER_PATH) -> list[tuple[str, Any]]:
    ledger = loads(path.read_text(encoding="utf-8"))
    jobs = value(ledger, "jobs", {})
    order = [str(job_id) for job_id in value(ledger, "job-order", [])]
    return [(job_id, jobs.get(job_id, jobs.get(K(job_id), {}))) for job_id in order]


def fetch_existing_outcome_jobs(
    job_ids: Sequence[str], base: str = SUBSTRATE_URL
) -> set[str]:
    """Find sweeper-authored rows with bounded, deterministic by-id reads."""
    def present(job_id: str) -> str | None:
        evidence_id = deterministic_evidence_id(job_id)
        request = urllib.request.Request(
            f"{base}/api/alpha/evidence/{evidence_id}",
            headers={"Accept": "application/edn"},
        )
        try:
            with urllib.request.urlopen(request, timeout=10) as response:
                entry = loads(response.read().decode("utf-8"))
        except urllib.error.HTTPError as error:
            if error.code == 404:
                return None
            raise
        body = value(entry, "evidence/body", {})
        if (
            value(body, "writer") == K("outcome-sweeper")
            and value(body, "job-id") == job_id
        ):
            return job_id
        return None

    with ThreadPoolExecutor(max_workers=4) as pool:
        return {job_id for job_id in pool.map(present, job_ids) if job_id}


def evidence_entry(outcome: dict[str, Any], swept_at: str) -> dict[Any, Any]:
    job_id = outcome["job_id"]
    evidence_id = deterministic_evidence_id(job_id)
    finished = parse_time(outcome.get("finished_at"))
    backfill = finished is None or finished < ROLLOUT_AT
    surfaced = outcome["surfaced_ids"]
    used = outcome["used_ids"]
    unused = [memory_id for memory_id in surfaced if memory_id not in set(used)]
    problem = outcome.get("problem")
    subject = {
        K("ref/type"): K("apm-problem") if problem else K("agency-job"),
        K("ref/id"): problem or job_id,
    }
    tags = [K("memory"), K("memory-use"), K("memory-outcome"), K("outcome-sweeper")]
    if backfill:
        tags.append(K("backfill"))
    receipt = {
        K("memory-use/signal"): K("agent-attribution"),
        K("memory-use/decision-id"): job_id,
        K("memory-use/session-id"): outcome["session_id"],
        K("memory-use/domain"): K("mathematics"),
        K("memory-use/surfaced-ids"): surfaced,
        K("memory-use/used-ids"): used,
        K("memory-use/rejected-ids"): [],
        K("memory-use/unused-ids"): unused,
        K("memory-use/inclusion-reasons"): [
            {K("memory-id"): memory_id,
             K("reason"): "memory named in runner's final Memory usage section"}
            for memory_id in surfaced
        ],
        K("memory-use/rejection-reasons"): [],
        K("memory-use/status"): K("outcome-attached"),
        K("memory-use/cascade-id"): job_id,
        K("memory-use/outcome-id"): evidence_id,
        K("memory-use/recorded-at"): swept_at,
    }
    return {
        K("evidence/id"): evidence_id,
        K("evidence/subject"): subject,
        K("evidence/type"): K("pattern-outcome"),
        K("evidence/claim-type"): K("observation"),
        K("evidence/at"): swept_at,
        K("evidence/author"): "outcome-sweeper",
        K("evidence/session-id"): outcome["session_id"],
        K("evidence/body"): {
            K("event"): K("memory-use"),
            K("phase"): K("outcome"),
            K("recall-system"): K("v1.2-receipt-instrumented"),
            K("job-id"): job_id,
            K("problem"): problem,
            K("writer"): K("outcome-sweeper"),
            K("backfill"): backfill,
            K("recall-outcome"): K(outcome["recall_outcome"]),
            K("memory-use"): receipt,
            K("provenance"): {
                K("job-id"): job_id,
                K("ledger-path"): str(LEDGER_PATH),
                K("extraction-method"): K(outcome["extraction_method"]),
                K("swept-at"): swept_at,
            },
        },
        K("evidence/tags"): tags,
    }


def post_entry(entry: dict[Any, Any], base: str = SUBSTRATE_URL) -> None:
    request = urllib.request.Request(
        f"{base}/api/alpha/evidence",
        data=dumps(entry).encode("utf-8"),
        method="POST",
        headers={
            "Content-Type": "application/edn",
            "Accept": "application/edn",
            "x-penholder": "api",
        },
    )
    with urllib.request.urlopen(request, timeout=60):
        pass


def entry_exists(evidence_id: str, base: str = SUBSTRATE_URL) -> bool:
    request = urllib.request.Request(
        f"{base}/api/alpha/evidence/{evidence_id}", headers={"Accept": "application/edn"}
    )
    try:
        with urllib.request.urlopen(request, timeout=10):
            return True
    except urllib.error.HTTPError as error:
        if error.code == 404:
            return False
        raise


def adjudicate_job(job_id: str, job: Any, *, base: str = SUBSTRATE_URL,
                   deposit_fn=deposit_correction) -> dict[str, Any] | None:
    """Return None when the run is outside the Type-B gate's applicability."""
    result = value(job, "result")
    state = str(value(job, "state", ""))
    if state not in TERMINAL_STATES or not isinstance(result, str):
        return None
    marker = OUTCOME_RE.search(result)
    if marker is None or marker.group(1) != "completed-with-memories":
        return None
    finished = parse_time(value(job, "finished-at"))
    if finished is not None and finished < ATTRIBUTION_GATE_ROLLOUT_AT:
        return None
    agent = str(value(job, "agent-id", "unknown-agent"))
    try:
        surfaced = offered_surfaced_ids(job_id, base)
    except Exception as error:
        class OfferedReceiptGate:
            norm = "use-attribution"

            def check(self, _run):
                raise RuntimeError(f"offered-receipt-resolution failed: {error}")

        return runner_gate.adjudicate(
            runner_gate.Run(agent, job_id, result, []), [OfferedReceiptGate()]
        )
    return runner_gate.adjudicate(
        runner_gate.Run(agent, job_id, result, surfaced),
        [runner_gate.UseAttributionGate()],
        deposit=lambda violation, message: deposit_fn(violation, message, base),
    )


def sweep(*, dry_run: bool = False, base: str = SUBSTRATE_URL,
          ledger_path: Path = LEDGER_PATH, gate: bool = True) -> dict[str, Any]:
    extracted: list[dict[str, Any]] = []
    unrecoverable: list[dict[str, Any]] = []
    gate_results: list[dict[str, Any]] = []
    reasons: Counter[str] = Counter()
    for job_id, job in load_jobs(ledger_path):
        adjudication = adjudicate_job(job_id, job, base=base) if gate and not dry_run else None
        if adjudication is not None and not adjudication["counts_toward_endpoints"]:
            gate_results.append(adjudication)
            reason = adjudication["run_status"]
            reasons[reason] += 1
            unrecoverable.append({
                "job_id": job_id,
                "reason": reason,
                "finished_at": value(job, "finished-at"),
                "missing_ids": adjudication.get("missing_ids", []),
            })
            gate_id = deterministic_gate_evidence_id(job_id)
            if not dry_run and not entry_exists(gate_id, base):
                post_entry(
                    gate_evidence_entry(job_id, adjudication["agent"], adjudication, now_iso()),
                    base,
                )
            continue
        outcome = extract_outcome(job_id, job)
        if outcome.get("recoverable"):
            extracted.append(outcome)
        elif outcome.get("reason") != "not-recall-dispatch":
            reason = str(outcome.get("reason"))
            reasons[reason] += 1
            unrecoverable.append({
                "job_id": job_id,
                "reason": reason,
                "finished_at": value(job, "finished-at"),
                "unknown_ids": outcome.get("unknown_ids", []),
            })
    existing = fetch_existing_outcome_jobs(
        [outcome["job_id"] for outcome in extracted], base
    )
    existing_before = len(existing)
    swept_at = now_iso()
    written: list[dict[str, Any]] = []
    skipped_existing = 0
    per_day: dict[str, Counter[str]] = defaultdict(Counter)
    for failure in unrecoverable:
        day = str(failure.get("finished_at") or "unknown")[:10]
        per_day[day]["unrecoverable"] += 1
    for outcome in extracted:
        day = str(outcome.get("finished_at") or "unknown")[:10]
        per_day[day]["recoverable"] += 1
        if outcome["job_id"] in existing:
            skipped_existing += 1
            per_day[day]["existing"] += 1
            continue
        entry = evidence_entry(outcome, swept_at)
        if not dry_run:
            post_entry(entry, base)
        written.append({
            "evidence_id": value(entry, "evidence/id"),
            "job_id": outcome["job_id"],
            "used_ids": outcome["used_ids"],
            "extraction_method": outcome["extraction_method"],
        })
        per_day[day]["would_write" if dry_run else "written"] += 1
        existing.add(outcome["job_id"])
    return {
        "swept_at": swept_at,
        "ledger_path": str(ledger_path),
        "existing_outcome_jobs_before": existing_before,
        "existing_outcome_jobs_after": (
            existing_before if dry_run else len(existing)
        ),
        "recoverable": len(extracted),
        "unrecoverable": len(unrecoverable),
        "unrecoverable_reasons": dict(sorted(reasons.items())),
        "unrecoverable_samples": unrecoverable[:3],
        "skipped_existing": skipped_existing,
        "would_write": len(written) if dry_run else 0,
        "written": 0 if dry_run else len(written),
        "per_day": {day: dict(counts) for day, counts in sorted(per_day.items())},
        "samples": written[:3],
        "attribution_gate": {
            "checked": len(gate_results),
            "incomplete": sum(
                result["run_status"] == "attribution-incomplete" for result in gate_results
            ),
            "review_required": sum(
                result["run_status"] == "attribution-gate-error" for result in gate_results
            ),
            "samples": gate_results[:3],
        },
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument("--quiet", action="store_true")
    parser.add_argument("--summary-json", type=Path)
    args = parser.parse_args()
    summary = sweep(dry_run=args.dry_run)
    rendered = json.dumps(summary, indent=2, sort_keys=True)
    if args.summary_json:
        args.summary_json.parent.mkdir(parents=True, exist_ok=True)
        args.summary_json.write_text(rendered + "\n", encoding="utf-8")
    if not args.quiet:
        print(rendered)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
