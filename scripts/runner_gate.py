#!/usr/bin/env python3
"""Production learning-loop gates for completed runner reports.

The Type-B use-attribution gate checks the final report against the surfaced
ids from the *offered* receipt.  State is durable, per-agent, and idempotent by
job id.  A gate failure never turns into a clean outcome: unexpected errors
are recorded as review-required and remain excluded from endpoints.

Operator commands:
  python3 scripts/runner_gate.py --demo
  python3 scripts/runner_gate.py --status AGENT
  python3 scripts/runner_gate.py --clear-stop AGENT --operator NAME
"""
from __future__ import annotations

import argparse
from collections import Counter
import hashlib
import json
import os
import re
import sys
import tempfile
from dataclasses import asdict, dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import Callable

from edn_format import Keyword, loads

DEFAULT_STATE_DIR = Path(__file__).resolve().parent.parent / ".state/runner-gate"
STATE_DIR = Path(os.environ.get("RUNNER_GATE_STATE_DIR", DEFAULT_STATE_DIR))
STOP_THE_LINE_THRESHOLD = int(os.environ.get("RUNNER_GATE_STOP_THRESHOLD", "3"))


def now_iso() -> str:
    return datetime.now(timezone.utc).isoformat().replace("+00:00", "Z")


def _safe(value: str) -> str:
    return re.sub(r"[^A-Za-z0-9_.-]", "_", value)


def _job_key(job_id: str) -> str:
    return hashlib.sha256(job_id.encode("utf-8")).hexdigest()[:24]


def _atomic_json(path: Path, value: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    temporary.replace(path)


def _append_jsonl(path: Path, value: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as stream:
        stream.write(json.dumps(value, sort_keys=True) + "\n")


@dataclass
class Violation:
    agent: str
    norm: str
    run_id: str
    detail: str
    missing_ids: list[str] = field(default_factory=list)


@dataclass
class Run:
    agent: str
    run_id: str
    report: str
    surfaced_ids: list[str] = field(default_factory=list)
    artifact_path: Path | None = None


class UseAttributionGate:
    """Reconcile offered-memory verdicts with citations in the work artifact.

    Attribution coverage remains mandatory.  Artifact inconsistencies are
    reported but do not refuse a run unless ``strict=True`` is requested.
    """

    norm = "use-attribution"
    teaches = (
        "Emit `USED <id>: <mechanism>` or `IGNORED <id>: <reason>` for every "
        "surfaced memory id. A missing verdict destroys the retrieval-to-use witness."
    )
        # Backtick tolerance must cover the KEYWORD too: ams-codex-1 wraps whole
    # attribution lines in backticks, and the old pattern silently failed
    # every such line, writing false compliance rows against the seat
    # (claude-3, batch-2; silence-catalogue instance 9).
    _verdict = re.compile(r"^\s*(?:[-*]\s*)?`?(USED|IGNORED)\s+`?(e-[A-Za-z0-9_-]+)`?\s*:", re.MULTILINE)

    def __init__(self, *, strict: bool = False):
        self.strict = strict

    def _reported_verdicts(self, report: str) -> list[tuple[str, str]]:
        return [(match.group(2), match.group(1).lower())
                for match in self._verdict.finditer(report)]

    def reconcile(self, run: Run, artifact_path: Path | None = None) -> dict:
        """Return the report/artifact consistency verdict without refusing."""
        path = artifact_path or run.artifact_path
        if path is None:
            return {
                "status": "not-checked",
                "reason": "artifact-path-not-provided",
                "strict": self.strict,
                "entries": [],
                "findings": [],
            }
        path = Path(path)
        try:
            artifact = path.read_text(encoding="utf-8")
        except OSError as error:
            return {
                "status": "not-checked",
                "reason": "artifact-unreadable",
                "error": f"{type(error).__name__}: {error}",
                "artifact": str(path),
                "strict": self.strict,
                "entries": [],
                "findings": [],
            }

        entries = []
        for memory_id, reported in self._reported_verdicts(run.report):
            exact = f"-- (Memory: {memory_id})"
            exact_count = artifact.count(exact)
            bare_count = len(re.findall(
                rf"(?<![A-Za-z0-9_-]){re.escape(memory_id)}(?![A-Za-z0-9_-])",
                artifact,
            ))
            cited = bare_count > 0
            form = "exact" if exact_count else ("free" if cited else None)
            classification = (
                "used-and-cited" if reported == "used" and cited else
                "used-but-uncited" if reported == "used" else
                "cited-but-ignored" if cited else
                "ignored-and-uncited"
            )
            entries.append({
                "memory_id": memory_id,
                "reported": reported,
                "classification": classification,
                "cited": cited,
                "form": form,
                "citation_count": bare_count,
                "exact_citation_count": exact_count,
            })
        findings = [entry for entry in entries if entry["classification"] in {
            "used-but-uncited", "cited-but-ignored",
        }]
        return {
            "status": "findings" if findings else "consistent",
            "artifact": str(path),
            "strict": self.strict,
            "entries": entries,
            "findings": findings,
            "finding_counts": dict(Counter(
                entry["classification"] for entry in findings
            )),
        }

    def check(self, run: Run) -> list[Violation]:
        counts = Counter(memory_id for memory_id, _ in self._reported_verdicts(run.report))
        invalid = sorted(memory_id for memory_id in set(run.surfaced_ids)
                         if counts[memory_id] != 1)
        violations = []
        if invalid:
            violations.append(Violation(
                run.agent,
                self.norm,
                run.run_id,
                f"{len(invalid)} surfaced id(s) without exactly one verdict: {', '.join(invalid)}",
                invalid,
            ))
        if self.strict and run.artifact_path is not None:
            reconciliation = self.reconcile(run)
            findings = reconciliation["findings"]
            if findings:
                finding_ids = sorted({entry["memory_id"] for entry in findings})
                classes = ", ".join(sorted({entry["classification"]
                                             for entry in findings}))
                violations.append(Violation(
                    run.agent,
                    self.norm,
                    run.run_id,
                    f"artifact citation inconsistency ({classes}): "
                    f"{', '.join(finding_ids)}",
                    finding_ids,
                ))
        return violations


class AxiomReverifyGate:
    """Type-A interface reserved for a later increment."""

    norm = "axiom-cleanliness"

    def check(self, run: Run) -> list[Violation]:  # pragma: no cover
        raise NotImplementedError("Type-A axiom re-verification is not wired")


def feedback(violation: Violation, gate: UseAttributionGate) -> str:
    return (
        f"RUN REJECTED ({violation.norm}). {violation.detail}\n"
        f"WHY IT MATTERS / WHAT TO LEARN: {gate.teaches}\n"
        "The correction is reviewed and will be supplied on your next dispatch."
    )


def _paths(state_dir: Path, agent: str, run_id: str) -> dict[str, Path]:
    agent_dir = state_dir / "agents" / _safe(agent)
    return {
        "agent_dir": agent_dir,
        "ledger": agent_dir / "violations.jsonl",
        "corrections": agent_dir / "corrections.jsonl",
        "adjudication": state_dir / "adjudications" / f"{_job_key(run_id)}.json",
        "review": state_dir / "review-required" / f"{_job_key(run_id)}.json",
        "stop": state_dir / "stop-the-line" / f"{_safe(agent)}.json",
        "meta": state_dir / "meta-learning.jsonl",
    }


def prior_count(agent: str, norm: str, *, state_dir: Path = STATE_DIR) -> int:
    ledger = _paths(state_dir, agent, "unused")["ledger"]
    if not ledger.exists():
        return 0
    count = 0
    for line in ledger.read_text(encoding="utf-8").splitlines():
        try:
            record = json.loads(line)
        except ValueError:
            continue
        if record.get("norm") == norm:
            count += 1
    return count


def is_stopped(agent: str, *, state_dir: Path = STATE_DIR) -> bool:
    return _paths(state_dir, agent, "unused")["stop"].exists()


def stop_record(agent: str, *, state_dir: Path = STATE_DIR) -> dict | None:
    path = _paths(state_dir, agent, "unused")["stop"]
    return json.loads(path.read_text(encoding="utf-8")) if path.exists() else None


def clear_stop(agent: str, operator: str, *, state_dir: Path = STATE_DIR) -> bool:
    paths = _paths(state_dir, agent, "operator-clear")
    existed = paths["stop"].exists()
    paths["stop"].unlink(missing_ok=True)
    _append_jsonl(paths["meta"], {
        "at": now_iso(), "event": "stop-the-line-cleared", "agent": agent,
        "operator": operator, "flag_existed": existed,
    })
    return existed


def pending_corrections(agent: str, *, state_dir: Path = STATE_DIR) -> list[dict]:
    path = _paths(state_dir, agent, "unused")["corrections"]
    if not path.exists():
        return []
    latest: dict[str, dict] = {}
    for line in path.read_text(encoding="utf-8").splitlines():
        record = json.loads(line)
        job_id = record.get("job_id")
        if job_id:
            latest[job_id] = record
    return [
        record for record in latest.values()
        if not record.get("delivered_at")
        and record.get("attachment_status") in {"reviewed", ":reviewed"}
    ]


def correction_packet(agent: str, *, state_dir: Path = STATE_DIR) -> str:
    pending = pending_corrections(agent, state_dir=state_dir)
    if not pending:
        return ""
    blocks = ["LEARNING-LOOP CORRECTIONS (reviewed; act on these in this run)"]
    for record in pending:
        blocks.append(
            f"[correction-memory={record.get('memory_id', 'deposit-pending')}]\n"
            f"{record['feedback']}"
        )
    return "\n\n".join(blocks) + "\n\n"


def mark_corrections_delivered(agent: str, dispatch_job_id: str,
                               *, state_dir: Path = STATE_DIR) -> None:
    path = _paths(state_dir, agent, "unused")["corrections"]
    for record in pending_corrections(agent, state_dir=state_dir):
        delivered = dict(record)
        delivered.update({"delivered_at": now_iso(), "delivered_on_job": dispatch_job_id})
        _append_jsonl(path, delivered)


def audit_receipts_export(path: Path) -> dict:
    """Audit the frozen receipt rows without pretending they contain reports."""
    payload = loads(path.read_text(encoding="utf-8"))
    entries = payload.get(Keyword("entries"), [])
    applicable = no_recorded_use = strict_incomplete = 0
    for entry in entries:
        body = entry.get(Keyword("evidence/body"), {})
        receipt = body.get(Keyword("memory-use"), {})
        surfaced = set(map(str, receipt.get(Keyword("memory-use/surfaced-ids"), [])))
        if not surfaced:
            continue
        applicable += 1
        used = set(map(str, receipt.get(Keyword("memory-use/used-ids"), [])))
        ignored = set(map(str, receipt.get(Keyword("memory-use/rejected-ids"), [])))
        if not used:
            no_recorded_use += 1
        if not surfaced.issubset(used | ignored):
            strict_incomplete += 1
    return {
        "artifact": str(path),
        "applicable_receipt_rows": applicable,
        "rows_with_no_recorded_use": no_recorded_use,
        "rows_incomplete_under_used_or_rejected_coverage": strict_incomplete,
        "caveat": "The export contains receipt fields, not raw runner reports; exact USED/IGNORED syntax cannot be reconstructed.",
    }


Deposit = Callable[[Violation, str], dict]


def adjudicate(run: Run, gates, *, state_dir: Path = STATE_DIR,
               deposit: Deposit | None = None) -> dict:
    """Adjudicate exactly once per run id; all non-accept paths fail safe."""
    paths = _paths(state_dir, run.agent, run.run_id)
    if paths["adjudication"].exists():
        result = json.loads(paths["adjudication"].read_text(encoding="utf-8"))
        result["idempotent_replay"] = True
        return result
    reconciliations = []
    try:
        if run.artifact_path is not None:
            reconciliations = [gate.reconcile(run) for gate in gates
                               if hasattr(gate, "reconcile")]
        violations = [violation for gate in gates for violation in gate.check(run)]
    except Exception as error:  # fail safe: exclude and flag, never crash/accept
        result = {
            "verdict": "review-required",
            "run_status": "attribution-gate-error",
            "counts_toward_endpoints": False,
            "agent": run.agent,
            "run_id": run.run_id,
            "error": f"{type(error).__name__}: {error}",
            "adjudicated_at": now_iso(),
        }
        if reconciliations:
            result["artifact_reconciliations"] = reconciliations
        _atomic_json(paths["review"], result)
        _atomic_json(paths["adjudication"], result)
        return result
    if not violations:
        result = {
            "verdict": "accept", "run_status": "attribution-complete",
            "counts_toward_endpoints": True, "agent": run.agent,
            "run_id": run.run_id, "missing_ids": [], "adjudicated_at": now_iso(),
        }
        if reconciliations:
            result["artifact_reconciliations"] = reconciliations
        _atomic_json(paths["adjudication"], result)
        return result

    # Claim the job before any append or external deposit. If the process is
    # killed mid-flight, replay remains excluded and cannot double-count; the
    # operator can inspect the durable processing marker and review-required
    # record rather than receiving a second violation.
    invalid_ids = sorted({item for violation in violations for item in violation.missing_ids})
    processing = {
        "verdict": "processing", "run_status": "attribution-incomplete",
        "counts_toward_endpoints": False, "agent": run.agent,
        "run_id": run.run_id, "missing_ids": invalid_ids,
        "adjudicated_at": now_iso(),
    }
    if reconciliations:
        processing["artifact_reconciliations"] = reconciliations
    _atomic_json(paths["adjudication"], processing)
    messages: list[str] = []
    deposit_results: list[dict] = []
    stop = False
    meta = None
    for violation in violations:
        gate = next(item for item in gates if item.norm == violation.norm)
        message = feedback(violation, gate)
        messages.append(message)
        record = asdict(violation) | {"at": now_iso()}
        _append_jsonl(paths["ledger"], record)
        correction = {"at": now_iso(), "job_id": run.run_id, "norm": violation.norm,
                      "feedback": message, "missing_ids": violation.missing_ids}
        if deposit is not None:
            try:
                deposited = deposit(violation, message)
                correction.update(deposited)
                deposit_results.append(deposited)
            except Exception as error:
                correction["deposit_error"] = f"{type(error).__name__}: {error}"
                _atomic_json(paths["review"], correction)
        _append_jsonl(paths["corrections"], correction)
        count = prior_count(run.agent, violation.norm, state_dir=state_dir)
        if count >= STOP_THE_LINE_THRESHOLD:
            stop = True
            meta = {
                "at": now_iso(), "event": "repeated-norm-violation",
                "agent": run.agent, "norm": violation.norm, "count": count,
                "run_id": run.run_id,
                "detail": "Correction memory is not changing behaviour; operator review required.",
            }
            _atomic_json(paths["stop"], meta)
            _append_jsonl(paths["meta"], meta)
    result = {
        "verdict": "stop-the-line" if stop else "reject-push-back",
        "run_status": "attribution-incomplete",
        "counts_toward_endpoints": False,
        "agent": run.agent, "run_id": run.run_id, "missing_ids": invalid_ids,
        "feedback": messages, "deposits": deposit_results,
        "meta_learning": meta, "adjudicated_at": now_iso(),
    }
    if reconciliations:
        result["artifact_reconciliations"] = reconciliations
    _atomic_json(paths["adjudication"], result)
    return result


def _demo() -> int:
    with tempfile.TemporaryDirectory(prefix="runner-gate-demo-") as tmp:
        state = Path(tmp)
        gates = [UseAttributionGate()]
        surfaced = ["e-alpha00", "e-beta000", "e-gamma00"]
        ok = Run("codex-9", "run-1", "USED e-alpha00: route\nIGNORED e-beta000: mismatch\nUSED e-gamma00: API\n", surfaced)
        print(json.dumps(adjudicate(ok, gates, state_dir=state), indent=2))
        bad = Run("codex-7", "run-2", "Memory usage: none attributed\n", surfaced)
        print(json.dumps(adjudicate(bad, gates, state_dir=state), indent=2))
        for index in range(3, STOP_THE_LINE_THRESHOLD + 2):
            result = adjudicate(Run("codex-7", f"run-{index}", "n/a", surfaced), gates, state_dir=state)
        print(json.dumps(result, indent=2))
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--demo", action="store_true")
    parser.add_argument("--status")
    parser.add_argument("--clear-stop")
    parser.add_argument("--operator")
    parser.add_argument("--audit-export", type=Path)
    args = parser.parse_args()
    if args.demo:
        return _demo()
    if args.status:
        print(json.dumps({"agent": args.status, "stop": stop_record(args.status),
                          "pending_corrections": pending_corrections(args.status)}, indent=2))
        return 0
    if args.clear_stop:
        if not args.operator:
            parser.error("--clear-stop requires --operator")
        print(json.dumps({"cleared": clear_stop(args.clear_stop, args.operator)}))
        return 0
    if args.audit_export:
        print(json.dumps(audit_receipts_export(args.audit_export), indent=2))
        return 0
    parser.print_help()
    return 0


if __name__ == "__main__":
    sys.exit(main())
