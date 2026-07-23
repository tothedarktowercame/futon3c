#!/usr/bin/env python3
"""Dispatch one untouched APM formalization to a Zai agent through Agency.

The cron entry runs this every 15 minutes.  A dispatch is allowed only when
every reported Z.AI token quota has strictly more than the configured amount
remaining and Agency reports no more than the configured number of other Zai
agents invoking.  Every lock-owning run records formal-proof progress before
evaluating those gates.  Usage or Agency failures are fail-closed.
"""

from __future__ import annotations

import argparse
import csv
import fcntl
import json
import os
import re
import sys
import urllib.error
import urllib.request
from datetime import datetime, timezone
from pathlib import Path
from typing import Any


FUTON3C_DIR = Path(os.environ.get("FUTON3C_DIR", "/home/joe/code/futon3c"))
APM_LEAN_DIR = Path(os.environ.get("APM_LEAN_DIR", "/home/joe/code/apm-lean"))
INFORMAL_DIR = Path(
    os.environ.get("APM_FORMAL_INFORMAL_DIR", FUTON3C_DIR / "data/apm-informal-proofs")
)
OVERRIDES_PATH = Path(
    os.environ.get("APM_FORMAL_OVERRIDES", FUTON3C_DIR / "data/apm-problem-overrides.tsv")
)
STATE_DIR = Path(
    os.environ.get("APM_FORMAL_ZAI_STATE_DIR", FUTON3C_DIR / ".state/apm-formal-zai")
)
PROGRESS_PATH = Path(
    os.environ.get("APM_FORMAL_ZAI_PROGRESS", STATE_DIR / "formal-progress.jsonl")
)
LOG_PATH = Path(
    os.environ.get(
        "APM_FORMAL_ZAI_LOG", "/home/joe/code/futon2/logs/apm-formal-zai.log"
    )
)
LOCK_PATH = Path(
    os.environ.get("APM_FORMAL_ZAI_LOCK", "/tmp/apm-formal-zai-cron.lock")
)
AGENCY_BASE = os.environ.get("APM_FORMAL_AGENCY_BASE", "http://localhost:7070").rstrip("/")
QUOTA_URL = os.environ.get(
    "APM_FORMAL_ZAI_QUOTA_URL", "https://api.z.ai/api/monitor/usage/quota/limit"
)
MIN_AVAILABLE_PERCENT = float(os.environ.get("APM_FORMAL_ZAI_MIN_AVAILABLE", "50"))
MAX_OTHER_INVOKING = int(os.environ.get("APM_FORMAL_ZAI_MAX_OTHER_INVOKING", "1"))
HTTP_TIMEOUT = float(os.environ.get("APM_FORMAL_ZAI_HTTP_TIMEOUT", "15"))

PROBLEM_ID_RE = re.compile(r"^[a-z][A-Za-z0-9]+$")


class GateClosed(RuntimeError):
    """A required scheduling signal is unavailable or outside its threshold."""


def iso_now() -> str:
    return datetime.now(timezone.utc).isoformat()


def log(message: str) -> None:
    line = f"{iso_now()} {message}"
    print(line, flush=True)
    LOG_PATH.parent.mkdir(parents=True, exist_ok=True)
    with LOG_PATH.open("a", encoding="utf-8") as stream:
        stream.write(line + "\n")


def api_key() -> str:
    key = os.environ.get("ZAI_API_KEY", "").strip()
    if key:
        return key
    for path in (Path.home() / ".zaikey", Path.home() / ".zai-key"):
        try:
            key = path.read_text(encoding="utf-8").strip()
        except OSError:
            continue
        if key:
            return key
    raise GateClosed("usage-unavailable missing ZAI_API_KEY/~/.zaikey/~/.zai-key")


def get_json(url: str, headers: dict[str, str] | None = None) -> dict[str, Any]:
    request = urllib.request.Request(url, headers=headers or {})
    try:
        with urllib.request.urlopen(request, timeout=HTTP_TIMEOUT) as response:
            return json.load(response)
    except (OSError, ValueError, urllib.error.URLError) as exc:
        raise GateClosed(f"request-failed url={url} error={exc}") from exc


def post_json(url: str, body: dict[str, Any]) -> dict[str, Any]:
    request = urllib.request.Request(
        url,
        data=json.dumps(body).encode("utf-8"),
        headers={"Content-Type": "application/json"},
        method="POST",
    )
    try:
        with urllib.request.urlopen(request, timeout=HTTP_TIMEOUT) as response:
            return json.load(response)
    except (OSError, ValueError, urllib.error.URLError) as exc:
        raise GateClosed(f"dispatch-failed url={url} error={exc}") from exc


def quota_snapshot(obj: dict[str, Any]) -> list[dict[str, float | int]]:
    """Return normalized token limits or reject an incomplete response."""
    if obj.get("success") is not True:
        raise GateClosed(f"usage-unavailable unsuccessful-response code={obj.get('code')}")
    limits = (obj.get("data") or {}).get("limits") or []
    token_limits = []
    for item in limits:
        if item.get("type") != "TOKENS_LIMIT":
            continue
        percentage = item.get("percentage")
        if not isinstance(percentage, (int, float)):
            raise GateClosed("usage-unavailable token-limit-without-percentage")
        token_limits.append(
            {
                "unit": int(item.get("unit", -1)),
                "number": int(item.get("number", -1)),
                "used": float(percentage),
                "available": 100.0 - float(percentage),
                "next_reset_ms": int(item.get("nextResetTime", 0)),
            }
        )
    if not token_limits:
        raise GateClosed("usage-unavailable no-token-limits")
    return token_limits


def enforce_quota(token_limits: list[dict[str, float | int]]) -> None:
    blocked = [
        limit
        for limit in token_limits
        if float(limit["available"]) <= MIN_AVAILABLE_PERCENT
    ]
    summary = ",".join(
        f"unit={x['unit']}/number={x['number']}/used={x['used']:g}/available={x['available']:g}"
        for x in token_limits
    )
    if blocked:
        raise GateClosed(
            f"usage-gate-closed min-available={MIN_AVAILABLE_PERCENT:g} limits={summary}"
        )
    log(f"usage-gate-open min-available={MIN_AVAILABLE_PERCENT:g} limits={summary}")


def fetch_and_enforce_quota() -> None:
    obj = get_json(
        QUOTA_URL,
        {
            # Z.AI's own usage plugin sends the coding-plan token verbatim.
            "Authorization": api_key(),
            "Accept-Language": "en-US,en",
            "Content-Type": "application/json",
        },
    )
    enforce_quota(quota_snapshot(obj))


def zai_agents(obj: dict[str, Any]) -> dict[str, dict[str, Any]]:
    agents = obj.get("agents")
    if obj.get("ok") is not True or not isinstance(agents, dict):
        raise GateClosed("agency-unavailable malformed-agent-roster")
    return {
        agent_id: agent
        for agent_id, agent in agents.items()
        if agent.get("type") == "zai" or agent_id.startswith("zai-")
    }


def numeric_agent_key(agent_id: str) -> tuple[int, str]:
    match = re.search(r"(\d+)$", agent_id)
    return (int(match.group(1)) if match else sys.maxsize, agent_id)


def choose_agent(agents: dict[str, dict[str, Any]]) -> tuple[str, int]:
    invoking = [aid for aid, agent in agents.items() if agent.get("status") == "invoking"]
    if len(invoking) > MAX_OTHER_INVOKING:
        raise GateClosed(
            f"concurrency-gate-closed invoking={len(invoking)} "
            f"max-other={MAX_OTHER_INVOKING} agents={','.join(sorted(invoking))}"
        )
    candidates = [
        aid
        for aid, agent in agents.items()
        if agent.get("status") != "invoking"
        and agent.get("invoke-ready?") is True
        and agent.get("invoke-route") == "local"
        and not (agent.get("metadata") or {}).get("proxy?")
    ]
    if not candidates:
        raise GateClosed("concurrency-gate-closed no-local-invoke-ready-zai")
    return sorted(candidates, key=numeric_agent_key)[0], len(invoking)


def unavailable_problem_ids() -> set[str]:
    unavailable: set[str] = set()
    if not OVERRIDES_PATH.exists():
        return unavailable
    with OVERRIDES_PATH.open(newline="", encoding="utf-8") as stream:
        for row in csv.DictReader(stream, delimiter="\t"):
            if (row.get("machine_processing_state") or "").strip():
                unavailable.add((row.get("id") or "").strip())
    return unavailable


def formal_artifact_exists(problem_id: str) -> bool:
    bundle = APM_LEAN_DIR / "problems" / problem_id
    legacy = APM_LEAN_DIR / "lean-proofs" / problem_id
    pipeline = APM_LEAN_DIR / "pipeline/lean-proofs" / problem_id
    canary = APM_LEAN_DIR / "ApmCanaries/Frames" / problem_id.upper()
    return (
        (bundle / "lean/Main.lean").exists()
        or (bundle.exists() and any((bundle / "candidates").glob("*/lean/Main.lean")))
        or (legacy / "Main.lean").exists()
        or (legacy / "Statement.lean").exists()
        or (pipeline / "Statement.lean").exists()
        or (canary.exists() and any(canary.rglob("Main.lean")))
    )


APM_PROGRESS_SCHEMA = "apm-formal-progress.v2"
APM_PROGRESS_METRIC_VERSION = "current-lean-code.v1"


def count_lean_sorries(text: str) -> int:
    """Count executable lowercase ``sorry`` tokens in Lean source text."""
    index = 0
    count = 0
    block_depth = 0
    state = "code"

    def ident_char(char: str) -> bool:
        return char.isalnum() or char in "_'"

    while index < len(text):
        if state == "line-comment":
            if text[index] == "\n":
                state = "code"
            index += 1
        elif state == "block-comment":
            if text.startswith("/-", index):
                block_depth += 1
                index += 2
            elif text.startswith("-/", index):
                block_depth -= 1
                index += 2
                if block_depth == 0:
                    state = "code"
            else:
                index += 1
        elif state == "string":
            if text[index] == "\\":
                index = min(len(text), index + 2)
            elif text[index] == '"':
                state = "code"
                index += 1
            else:
                index += 1
        elif state == "quoted-ident":
            if text[index] == "»":
                state = "code"
            index += 1
        elif text.startswith("--", index):
            state = "line-comment"
            index += 2
        elif text.startswith("/-", index):
            state = "block-comment"
            block_depth = 1
            index += 2
        elif text[index] == '"':
            state = "string"
            index += 1
        elif text[index] == "«":
            state = "quoted-ident"
            index += 1
        elif (
            text.startswith("sorry", index)
            and (index == 0 or not ident_char(text[index - 1]))
            and (
                index + 5 == len(text)
                or not ident_char(text[index + 5])
            )
        ):
            count += 1
            index += 5
        else:
            index += 1
    return count


def proof_progress_snapshot() -> dict[str, Any]:
    """Scan the same canonical problem universe used by the Stack HUD."""
    problem_ids = sorted(path.stem for path in (APM_LEAN_DIR / "apm").glob("*.tex"))
    snapshot: dict[str, Any] = {
        "schema": APM_PROGRESS_SCHEMA,
        "metric_version": APM_PROGRESS_METRIC_VERSION,
        "timestamp": iso_now(),
        "total": len(problem_ids),
        "informal": 0,
        "lean_total": 0,
        "lean_with_sorry": 0,
        "lean_clean": 0,
        "sorries": 0,
    }
    for problem_id in problem_ids:
        bundle = APM_LEAN_DIR / "problems" / problem_id
        informal = bundle / "informal-solution.md"
        try:
            if informal.stat().st_size >= 100:
                snapshot["informal"] += 1
        except OSError:
            pass
        lean_dir = bundle / "lean"
        lean_files = list(lean_dir.rglob("*.lean")) if lean_dir.is_dir() else []
        if not lean_files:
            continue
        snapshot["lean_total"] += 1
        sorry_count = 0
        for path in lean_files:
            text = path.read_text(encoding="utf-8")
            sorry_count += count_lean_sorries(text)
        snapshot["sorries"] += sorry_count
        if sorry_count:
            snapshot["lean_with_sorry"] += 1
        else:
            snapshot["lean_clean"] += 1
    snapshot["remaining"] = snapshot["total"] - snapshot["lean_clean"]
    return snapshot


def append_progress_snapshot() -> dict[str, Any]:
    """Append one durable progress observation for the burndown chart."""
    snapshot = proof_progress_snapshot()
    PROGRESS_PATH.parent.mkdir(parents=True, exist_ok=True)
    with PROGRESS_PATH.open("a", encoding="utf-8") as stream:
        stream.write(json.dumps(snapshot, separators=(",", ":")) + "\n")
    return snapshot


def canonical_bundle_is_informal_only(problem_id: str) -> bool:
    """Require the canonical handoff bundle, not only the transient harvest."""
    bundle = APM_LEAN_DIR / "problems" / problem_id
    status_path = bundle / "status.json"
    required = [
        bundle / "problem.tex",
        bundle / "problem.md",
        bundle / "informal-solution.md",
        bundle / "proof-outline.md",
        status_path,
    ]
    if not all(path.exists() for path in required):
        return False
    try:
        status = json.loads(status_path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return False
    lean = status.get("lean") or {}
    return status.get("classification") == "informal-only" and not lean.get("main")


def claim_path(problem_id: str) -> Path:
    return STATE_DIR / "claims" / f"{problem_id}.json"


def candidate_problem_ids() -> list[str]:
    unavailable = unavailable_problem_ids()
    candidates = []
    for informal in sorted(INFORMAL_DIR.glob("apm-*.md")):
        problem_id = informal.stem.removeprefix("apm-")
        if not PROBLEM_ID_RE.fullmatch(problem_id):
            continue
        try:
            useful = informal.stat().st_size >= 100
        except OSError:
            useful = False
        if (
            useful
            and problem_id not in unavailable
            and canonical_bundle_is_informal_only(problem_id)
            and not formal_artifact_exists(problem_id)
            and not claim_path(problem_id).exists()
        ):
            candidates.append(problem_id)
    return candidates


def make_prompt(problem_id: str) -> str:
    informal = INFORMAL_DIR / f"apm-{problem_id}.md"
    tex = APM_LEAN_DIR / "apm" / f"{problem_id}.tex"
    bundle = APM_LEAN_DIR / "problems" / problem_id
    target = bundle / "lean/Main.lean"
    return f"""UNATTENDED APM LEAN FORMALIZATION — ONE PROBLEM

Problem: apm-{problem_id}
Informal proof: {informal}
Original TeX: {tex}
Required Lean artifact: {target}
Canonical bundle: {bundle}

You are running through Agency because its Zai custom harness is required. Work on
this one problem only. Read /home/joe/code/AGENTS.md first and obey its invariant:
no workarounds, invariant bypasses, silent theorem weakening, or fake closure.

1. Read the informal proof and TeX. Sanity-check that the proposed Lean statement
   really expresses the mathematical problem; repair an incorrect generated
   statement structurally and explain the repair in comments.
2. Create {target} early, then make a genuine Lean 4 + Mathlib proof attempt.
   Aim for a complete zero-sorry proof. Local helper lemmas are encouraged when
   they expose the actual proof structure.
3. Run `cd {APM_LEAN_DIR} && lake env lean {target}` repeatedly while developing
   and once at the end. Never replace a blocked mathematical obligation with an
   axiom, `True`, a weakened theorem, or another escape hatch.
4. If a genuine Mathlib/theory boundary remains after real attempts, leave the
   smallest explicit `sorry` at that boundary with a precise comment recording
   what was tried. A checked partial is valid evidence; disguised failure is not.
5. Update the bundle's `proof-outline.md` and `status.json` to record the checked
   Lean state, exact sorry count, and `complete` or `partial` classification. Keep
   the `apm-problem-bundle.v1` shape. Do not write to the legacy `lean-proofs/` or
   `ApmCanaries/Frames/` trees: `problems/{problem_id}/` is canonical.
6. Commit only {bundle} in the apm-lean repo with a problem-specific message. Use
   a path-limited commit so unrelated/shared-worktree changes are not captured.

Your final response must name the target, validation command/result, sorry count,
commit SHA if committed, and exact blocker if partial. Do not dispatch follow-on
work and do not bell a Claude agent; the Agency invoke-job ledger is the overnight
review surface.
"""


def reserve(problem_id: str, agent_id: str, dry_run: bool) -> Path | None:
    if dry_run:
        return None
    path = claim_path(problem_id)
    path.parent.mkdir(parents=True, exist_ok=True)
    record = {
        "problem_id": problem_id,
        "agent_id": agent_id,
        "claimed_at": iso_now(),
        "state": "dispatching",
    }
    try:
        fd = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o664)
    except FileExistsError as exc:
        raise GateClosed(f"candidate-already-claimed problem={problem_id}") from exc
    with os.fdopen(fd, "w", encoding="utf-8") as stream:
        json.dump(record, stream, indent=2)
        stream.write("\n")
    return path


def finish_claim(path: Path, job_id: str) -> None:
    record = json.loads(path.read_text(encoding="utf-8"))
    record.update({"state": "dispatched", "job_id": job_id, "dispatched_at": iso_now()})
    temporary = path.with_suffix(".tmp")
    temporary.write_text(json.dumps(record, indent=2) + "\n", encoding="utf-8")
    temporary.replace(path)


def run(dry_run: bool) -> int:
    LOCK_PATH.parent.mkdir(parents=True, exist_ok=True)
    with LOCK_PATH.open("a+") as lock:
        try:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            log(f"already-running lock={LOCK_PATH}")
            return 0

        progress = append_progress_snapshot()
        log(
            f"progress lean-clean={progress['lean_clean']} "
            f"remaining={progress['remaining']} sorries={progress['sorries']}"
        )
        fetch_and_enforce_quota()
        roster = get_json(f"{AGENCY_BASE}/api/alpha/agents")
        agent_id, invoking_count = choose_agent(zai_agents(roster))
        candidates = candidate_problem_ids()
        if not candidates:
            log("complete no-unattempted-informal-problems")
            return 0

        problem_id = candidates[0]
        if dry_run:
            log(
                f"dry-run problem={problem_id} agent={agent_id} "
                f"other-invoking={invoking_count} remaining-candidates={len(candidates)}"
            )
            return 0

        claim = reserve(problem_id, agent_id, dry_run=False)
        assert claim is not None
        try:
            response = post_json(
                f"{AGENCY_BASE}/api/alpha/bell",
                {"agent-id": agent_id, "prompt": make_prompt(problem_id)},
            )
            job_id = response.get("job-id") or response.get("job_id")
            if not job_id:
                raise GateClosed(f"dispatch-failed missing-job-id response={response}")
            finish_claim(claim, str(job_id))
        except Exception:
            claim.unlink(missing_ok=True)
            raise
        log(
            f"dispatched problem={problem_id} agent={agent_id} job={job_id} "
            f"other-invoking={invoking_count} remaining-candidates={len(candidates) - 1}"
        )
        return 0


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args()
    try:
        return run(args.dry_run)
    except GateClosed as exc:
        log(str(exc))
        return 0
    except Exception as exc:  # cron must log unexpected defects loudly
        log(f"unexpected-error type={type(exc).__name__} error={exc}")
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
