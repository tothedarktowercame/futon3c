#!/usr/bin/env python3
"""Dispatch at most one verified-queue sorry task to an idle Codex runner.

All scheduling signals fail closed. The owner installs the cron only after
review; this module never edits crontab.
"""

from __future__ import annotations

import argparse
import fcntl
import json
import os
import re
import subprocess
import sys
import urllib.error
import urllib.request
from collections.abc import Sequence
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Any, Iterator

from edn_format import Keyword, dumps, loads


FUTON3C_DIR = Path(os.environ.get("FUTON3C_DIR", "/home/joe/code/futon3c"))
QUEUE_PATH = Path(
    os.environ.get("CODEX_SORRY_QUEUE", FUTON3C_DIR / "data/codex-sorry-queue.edn")
)
TEMPLATE_PATH = Path(
    os.environ.get(
        "CODEX_SORRY_TEMPLATE", FUTON3C_DIR / "data/codex-sorry-packet-template.txt"
    )
)
SESSIONS_DIR = Path(
    os.environ.get("CODEX_SESSIONS_DIR", Path.home() / ".codex/sessions")
)
STATE_DIR = Path(
    os.environ.get("CODEX_SORRY_STATE_DIR", FUTON3C_DIR / ".state/codex-sorry")
)
PROGRESS_PATH = Path(
    os.environ.get("CODEX_SORRY_PROGRESS", STATE_DIR / "progress.jsonl")
)
LOG_PATH = Path(
    os.environ.get("CODEX_SORRY_LOG", "/home/joe/code/futon2/logs/codex-sorry.log")
)
LOCK_PATH = Path(
    os.environ.get("CODEX_SORRY_LOCK", "/tmp/codex-sorry-cron.lock")
)
AGENCY_BASE = os.environ.get("CODEX_SORRY_AGENCY_BASE", "http://localhost:7070").rstrip(
    "/"
)
# The live futon1b store. Must be passed explicitly to the dispatch subprocess:
# see the comment in dispatch(). :7071 (the substrate client's built-in default)
# is the retired futon1a store and is dead.
SUBSTRATE_URL = os.environ.get("FUTON_SUBSTRATE_URL", "http://127.0.0.1:7073").rstrip("/")
MAX_USED_PERCENT = float(os.environ.get("CODEX_SORRY_MIN_HEADROOM_USED", "50"))
MAX_OTHER_INVOKING = int(os.environ.get("CODEX_SORRY_MAX_OTHER_INVOKING", "1"))
HTTP_TIMEOUT = float(os.environ.get("CODEX_SORRY_HTTP_TIMEOUT", "15"))
USAGE_FILE_HORIZON = timedelta(hours=48)
USAGE_SIGNAL_MAX_AGE = timedelta(hours=24)
EXCLUDED_RUNNERS = {"codex-4", "codex-5"}
PREFERRED_RUNNERS = ("codex-6", "codex-7", "codex-8")

K = Keyword


class GateClosed(RuntimeError):
    """A required scheduling signal is absent, stale, or outside its bound."""


def now_utc() -> datetime:
    return datetime.now(timezone.utc)


def iso_now() -> str:
    return now_utc().isoformat()


def emit(message: str, dry_run: bool = False) -> None:
    line = f"{iso_now()} {message}"
    print(line, flush=True)
    if dry_run:
        return
    LOG_PATH.parent.mkdir(parents=True, exist_ok=True)
    with LOG_PATH.open("a", encoding="utf-8") as stream:
        stream.write(line + "\n")


def get_json(url: str) -> dict[str, Any]:
    try:
        with urllib.request.urlopen(url, timeout=HTTP_TIMEOUT) as response:
            return json.load(response)
    except (OSError, ValueError, urllib.error.URLError) as exc:
        raise GateClosed(f"request-failed url={url} error={exc}") from exc


def parse_timestamp(value: str) -> datetime:
    try:
        parsed = datetime.fromisoformat(value.replace("Z", "+00:00"))
    except (TypeError, ValueError) as exc:
        raise GateClosed(f"usage-unavailable invalid-timestamp={value!r}") from exc
    if parsed.tzinfo is None:
        parsed = parsed.replace(tzinfo=timezone.utc)
    return parsed.astimezone(timezone.utc)


def reverse_lines(path: Path, block_size: int = 64 * 1024) -> Iterator[str]:
    """Yield a potentially large JSONL file newest-first without loading it."""
    with path.open("rb") as stream:
        stream.seek(0, os.SEEK_END)
        position = stream.tell()
        remainder = b""
        while position:
            size = min(block_size, position)
            position -= size
            stream.seek(position)
            chunk = stream.read(size) + remainder
            parts = chunk.split(b"\n")
            remainder = parts[0]
            for raw in reversed(parts[1:]):
                if raw:
                    yield raw.decode("utf-8")
        if remainder:
            yield remainder.decode("utf-8")


def newest_rate_limit(
    sessions_dir: Path = SESSIONS_DIR, reference: datetime | None = None
) -> dict[str, Any]:
    """Find the newest local token_count.rate_limits payload, fail closed."""
    reference = reference or now_utc()
    candidates: list[tuple[datetime, Path]] = []
    if not sessions_dir.exists():
        raise GateClosed(f"usage-unavailable sessions-dir-missing path={sessions_dir}")
    for path in sessions_dir.rglob("*.jsonl"):
        try:
            modified = datetime.fromtimestamp(path.stat().st_mtime, timezone.utc)
        except OSError:
            continue
        if reference - modified <= USAGE_FILE_HORIZON:
            candidates.append((modified, path))
    if not candidates:
        raise GateClosed("usage-unavailable no-recent-session-files")

    newest: dict[str, Any] | None = None
    for _, path in sorted(candidates, reverse=True):
        for line in reverse_lines(path):
            try:
                event = json.loads(line)
            except (UnicodeDecodeError, ValueError):
                continue
            payload = event.get("payload") or {}
            if (
                event.get("type") == "event_msg"
                and payload.get("type") == "token_count"
                and isinstance(payload.get("rate_limits"), dict)
            ):
                timestamp = parse_timestamp(event.get("timestamp"))
                if newest is None or timestamp > newest["timestamp"]:
                    newest = {
                        "timestamp": timestamp,
                        "path": str(path),
                        "rate_limits": payload["rate_limits"],
                    }
                break
    if newest is None:
        raise GateClosed("usage-unavailable no-token-count-rate-limits")
    age = reference - newest["timestamp"]
    if age < timedelta(0) or age > USAGE_SIGNAL_MAX_AGE:
        raise GateClosed(
            f"usage-unavailable stale-rate-limit age-hours={age.total_seconds()/3600:.2f}"
        )
    primary = newest["rate_limits"].get("primary") or {}
    used = primary.get("used_percent")
    if not isinstance(used, (int, float)):
        raise GateClosed("usage-unavailable primary-used-percent-missing")
    newest["used_percent"] = float(used)
    newest["age_seconds"] = age.total_seconds()
    return newest


def enforce_usage(snapshot: dict[str, Any]) -> None:
    used = snapshot["used_percent"]
    if used >= MAX_USED_PERCENT:
        raise GateClosed(
            f"usage-gate-closed used={used:g} threshold-exclusive={MAX_USED_PERCENT:g}"
        )


def codex_agents(roster: dict[str, Any]) -> dict[str, dict[str, Any]]:
    agents = roster.get("agents")
    if roster.get("ok") is not True or not isinstance(agents, dict):
        raise GateClosed("agency-unavailable malformed-agent-roster")
    return {
        agent_id: agent
        for agent_id, agent in agents.items()
        if agent.get("type") == "codex" or agent_id.startswith("codex-")
    }


def numeric_agent_key(agent_id: str) -> tuple[int, str]:
    match = re.search(r"(\d+)$", agent_id)
    return (int(match.group(1)) if match else sys.maxsize, agent_id)


def choose_agent(agents: dict[str, dict[str, Any]]) -> tuple[str, int]:
    invoking = [
        agent_id
        for agent_id, agent in agents.items()
        if agent.get("status") == "invoking"
    ]
    if len(invoking) > MAX_OTHER_INVOKING:
        raise GateClosed(
            f"concurrency-gate-closed invoking={len(invoking)} "
            f"max-other={MAX_OTHER_INVOKING} agents={','.join(sorted(invoking))}"
        )
    candidates = [
        agent_id
        for agent_id, agent in agents.items()
        if agent_id not in EXCLUDED_RUNNERS
        and agent.get("status") != "invoking"
        and agent.get("invoke-ready?") is True
        and agent.get("invoke-route") == "local"
        and not (agent.get("metadata") or {}).get("proxy?")
    ]
    if not candidates:
        raise GateClosed("concurrency-gate-closed no-idle-codex-runner")
    preference = {agent: index for index, agent in enumerate(PREFERRED_RUNNERS)}
    candidates.sort(
        key=lambda agent: (
            0 if agent in preference else 1,
            preference.get(agent, sys.maxsize),
            numeric_agent_key(agent),
        )
    )
    return candidates[0], len(invoking)


def load_queue(path: Path = QUEUE_PATH) -> list[dict[Any, Any]]:
    try:
        queue = loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError) as exc:
        raise GateClosed(f"queue-unavailable path={path} error={exc}") from exc
    if not isinstance(queue, Sequence) or isinstance(queue, (str, bytes)):
        raise GateClosed("queue-unavailable expected-vector")
    return list(queue)


def status_name(row: dict[Any, Any]) -> str:
    value = row.get(K("status"))
    return str(value).removeprefix(":")


def enforce_backpressure(queue: list[dict[Any, Any]]) -> None:
    pending = [str(row.get(K("id"))) for row in queue if status_name(row) == "dispatched"]
    if pending:
        raise GateClosed(
            "verification-backpressure pending=" + ",".join(pending)
        )


def zai_busy_problem_ids(roster: dict[str, Any]) -> set[str]:
    busy: set[str] = set()
    agents = roster.get("agents") or {}
    for agent_id, agent in agents.items():
        if (
            (agent.get("type") == "zai" or agent_id.startswith("zai-"))
            and agent.get("status") == "invoking"
        ):
            searchable = json.dumps(agent, sort_keys=True)
            busy.update(re.findall(r"\b[a-z]\d{2}[A-Z]\d{2}\b", searchable))
    return busy


def row_problem_id(row: dict[Any, Any]) -> str | None:
    match = re.search(r"(?:^|/)problems/([^/]+)/", str(row.get(K("file"), "")))
    return match.group(1) if match else None


def choose_row(
    queue: list[dict[Any, Any]], busy_problem_ids: set[str]
) -> tuple[int, dict[Any, Any]]:
    # Rows carrying :priority go first (lower value = sooner), then queue
    # order. Used for rows whose supporting lemma is already proved and
    # importable — those are the highest-probability wins, and dispatching
    # them promptly is what converts ConstructionTargets work into a
    # movement in the problems/ sorry count. Ties and unprioritised rows
    # keep their original census order (sort is stable).
    eligible = [
        (index, row)
        for index, row in enumerate(queue)
        if status_name(row) == "untouched"
        and not (
            (problem_id := row_problem_id(row)) and problem_id in busy_problem_ids
        )
    ]
    if not eligible:
        raise GateClosed("queue-complete no-eligible-untouched-row")
    eligible.sort(key=lambda pair: row_value(pair[1], "priority", 1_000_000))
    return eligible[0]


def row_value(row: dict[Any, Any], name: str, default: Any = "") -> Any:
    return row.get(K(name), default)


def instantiate_packet(row: dict[Any, Any], template: str) -> str:
    unblocks = row_value(row, "unblocks", [])
    replacements = {
        "@@ID@@": str(row_value(row, "id")),
        "@@KIND@@": str(row_value(row, "kind")).removeprefix(":"),
        "@@FILE@@": str(row_value(row, "file")),
        "@@LINE@@": str(row_value(row, "line")),
        "@@STATEMENT@@": str(row_value(row, "statement-hint")),
        "@@UNBLOCKS@@": ", ".join(map(str, unblocks)) if unblocks else "[none recorded]",
        # Proved, importable lemmas that exist specifically for this row.
        # Before 2026-07-30 ConstructionTargets was not on the module path at
        # all, so runners could not have used it even if told; now they can,
        # and the packet has to say so or the work stays unused.
        "@@AVAILABLE@@": str(row_value(row, "available-support", "[none recorded]")),
        # An inherited plan, when one exists — e.g. a previous session that
        # named a precise remaining obstruction before running out of time.
        # Deliberately OPTIONAL rather than a boolean "route known?": a closer
        # inheriting a route should not be the typical case, since deferring
        # work that could have been closed directly is lazy for a closer.
        "@@ROUTE@@": str(row_value(row, "suggested-route", "[none — find your own route]")),
    }
    for marker, value in replacements.items():
        template = template.replace(marker, value)
    return template


SUBJECT_STOPWORDS = {
    # generic prose that carries no recall signal (observed polluting the
    # rouche row's offered half, 2026-07-29: one giant unsplit phrase term)
    "build", "reusable", "theorem", "under", "strict", "with", "that",
    "this", "from", "into", "over", "the", "and", "for", "case",
    "prove", "proof", "statement", "exact", "target", "final", "found",
    "only", "probe", "work", "anchor", "file", "lemma", "main",
}


def subjects_for(row: dict[Any, Any]) -> list[str]:
    """Tokenized mathematical vocabulary from the statement hint.

    Recall matches pattern descriptions lexically; an unsplit sentence
    matches nothing (rouche row, receipt e-99ba9b71: :recall-empty while
    'holomorphic'/'disk' sat unmatched inside one phrase term).
    """
    hint = str(row_value(row, "statement-hint"))
    words = re.findall(r"[A-Za-z][A-Za-z'-]{3,}", hint)
    terms: list[str] = []
    for word in words:
        lowered = word.lower()
        if lowered in SUBJECT_STOPWORDS or lowered in terms:
            continue
        terms.append(lowered)
    return terms[:12] or [str(row_value(row, "id"))]


def dispatch(row: dict[Any, Any], runner: str, packet: str) -> str:
    command = [
        "clojure",
        "-M",
        "scripts/dispatch_with_recall.clj",
        "--problem",
        str(row_value(row, "id")),
        "--to",
        runner,
        "--from",
        # Ground-control seat for this loop. Was claude-6 (Fable) until the
        # 2026-07-29 succession; the nick is a routing address (nick ->
        # /tmp/futon-session-id-<nick> -> resumed pouch), so completion bells
        # follow whichever seat is named here.
        "claude-9",
        "--mission",
        "M-codex-sorry-loop",
    ]
    for subject in subjects_for(row):
        command.extend(["--subject", subject])
    # Cron runs with a minimal environment (no shell profile), so
    # FUTON_SUBSTRATE_URL is unset here even though it is set in an interactive
    # shell. substrate.client/configured-url then falls back to :7071 — the
    # RETIRED futon1a store — every recall throws, and safe-recall records the
    # result as {:recall-reason :store-unavailable :recall-status :recall-empty}.
    # That is indistinguishable from a genuine empty result in the receipts,
    # which is how it went unnoticed: every cron-lane "recall empty" datum from
    # 2026-07-28 to 2026-07-29 was this bug, not a recall-semantics finding.
    # Receipt WRITES were unaffected because record-offered! defaults to :7073
    # explicitly, so offered halves kept landing while recall never ran.
    env = dict(os.environ)
    env.setdefault("FUTON_SUBSTRATE_URL", SUBSTRATE_URL)
    env.setdefault("FUTON1A_URL", SUBSTRATE_URL)
    result = subprocess.run(
        command,
        cwd=FUTON3C_DIR,
        input=packet,
        text=True,
        capture_output=True,
        timeout=180,
        check=False,
        env=env,
    )
    if result.returncode:
        raise GateClosed(
            f"dispatch-failed exit={result.returncode} stderr={result.stderr[-500:]}"
        )
    job_ids = re.findall(r"^invoke-[^\s]+$", result.stdout, re.MULTILINE)
    if not job_ids:
        raise GateClosed("dispatch-failed no-job-id")
    return job_ids[-1]


def save_queue(queue: list[dict[Any, Any]], path: Path = QUEUE_PATH) -> None:
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(dumps(queue) + "\n", encoding="utf-8")
    temporary.replace(path)


def append_progress(record: dict[str, Any]) -> None:
    PROGRESS_PATH.parent.mkdir(parents=True, exist_ok=True)
    with PROGRESS_PATH.open("a", encoding="utf-8") as stream:
        stream.write(json.dumps(record, separators=(",", ":")) + "\n")


def run(dry_run: bool) -> int:
    LOCK_PATH.parent.mkdir(parents=True, exist_ok=True)
    with LOCK_PATH.open("a+", encoding="utf-8") as lock:
        try:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            emit(f"already-running lock={LOCK_PATH}", dry_run=dry_run)
            return 0

        usage = newest_rate_limit()
        enforce_usage(usage)
        roster = get_json(f"{AGENCY_BASE}/api/alpha/agents")
        agent_id, invoking_count = choose_agent(codex_agents(roster))
        queue = load_queue()
        enforce_backpressure(queue)
        row_index, row = choose_row(queue, zai_busy_problem_ids(roster))
        template = TEMPLATE_PATH.read_text(encoding="utf-8")
        packet = instantiate_packet(row, template)
        gate_summary = (
            f"gates-open used={usage['used_percent']:g} "
            f"age-seconds={usage['age_seconds']:.0f} "
            f"other-codex-invoking={invoking_count}"
        )
        emit(gate_summary, dry_run=dry_run)

        if dry_run:
            print(
                f"DRY RUN row={row_value(row, 'id')} runner={agent_id} "
                f"file={row_value(row, 'file')}\n\n{packet}"
            )
            return 0

        job_id = dispatch(row, agent_id, packet)
        updated = dict(row)
        updated.update(
            {
                K("status"): K("dispatched"),
                K("job-id"): job_id,
                K("dispatched-at"): iso_now(),
            }
        )
        queue[row_index] = updated
        save_queue(queue)
        record = {
            "at": iso_now(),
            "event": "dispatched",
            "row_id": str(row_value(row, "id")),
            "runner": agent_id,
            "job_id": job_id,
            "used_percent": usage["used_percent"],
        }
        append_progress(record)
        emit(
            f"dispatched row={record['row_id']} runner={agent_id} job={job_id}",
            dry_run=False,
        )
        return 0


def _record_gate_streak(open_gate: bool) -> int:
    """Track consecutive gate-closed fires. Returns the current streak."""
    path = STATE_DIR / "gate-streak"
    try:
        STATE_DIR.mkdir(parents=True, exist_ok=True)
        if open_gate:
            path.write_text("0", encoding="utf-8")
            return 0
        try:
            previous = int(path.read_text(encoding="utf-8").strip())
        except (OSError, ValueError):
            previous = 0
        streak = previous + 1
        path.write_text(str(streak), encoding="utf-8")
        return streak
    except OSError:
        return -1  # never let bookkeeping break the gate logic


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args()
    try:
        result = run(args.dry_run)
        if not args.dry_run:
            _record_gate_streak(open_gate=True)
        return result
    except GateClosed as exc:
        # Backpressure fails SAFE but historically failed SILENT: on 2026-07-29
        # an unresolved row gate-closed four consecutive fires and the only
        # trace was a repeated log line nobody was reading at 23:30. Carrying
        # the consecutive count makes an unattended stall visible in the
        # artifact the operator already has.
        streak = _record_gate_streak(open_gate=False) if not args.dry_run else 1
        suffix = f" consecutive-gate-closed={streak}"
        if streak >= 3:
            suffix += " STALLED-CHECK-GROUND-CONTROL"
        emit(f"{exc}{suffix}", dry_run=args.dry_run)
        return 0
    except Exception as exc:
        emit(
            f"unexpected-error type={type(exc).__name__} error={exc}",
            dry_run=args.dry_run,
        )
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
