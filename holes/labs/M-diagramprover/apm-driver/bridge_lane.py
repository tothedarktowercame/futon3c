#!/usr/bin/env python3
"""Hop-4 bridge lane: keep Codex fed with hole-closing work, unattended.

Design point (Joe, 2026-08-08): my REVIEW is the gate on acceptance, not on
dispatch. So this runs on a timer and keeps the seats busy; nothing it produces
counts until reviewed. A missed wake then costs review latency, not throughput,
which is the right failure mode for an overnight run.

Backpressure rather than a fixed schedule: it counts jobs still in flight and
only tops up to IN_FLIGHT_MAX. Agency queues per seat, so firing a fixed batch
every tick would build an unbounded queue that cannot be cancelled if a packet
turns out to be wrong.

Idempotent by construction: every dispatch is recorded, and a problem already
recorded is never dispatched again. The lane stops on its own when the worklist
is exhausted -- there is no runaway.
"""
import fcntl
import json
import pathlib
import re
import subprocess
import sys

sys.path.insert(0, "/home/joe/code/futon3c/holes/labs/M-diagramprover/apm-driver")
import agency
import gates
import statement_campaign as sc

DRIVER = pathlib.Path("/home/joe/code/futon3c/holes/labs/M-diagramprover/apm-driver")
REPO = pathlib.Path("/home/joe/code/apm-lean")
ROWS = DRIVER / "mathlib-holes.jsonl"
JOBS = DRIVER / "bridge-pilot-jobs.jsonl"
LOG = DRIVER / "bridge-lane.log"

SEATS = ["ams-codex-1", "ams-codex-2", "ams-scribe-1"]
IN_FLIGHT_MAX = 6          # ~2 per seat; keeps the queue cancellable
TOP_UP_TO = 6

STRICT = re.compile(r"(no packaged|does not exist|Mathlib (?:has no|lacks|installs no)|"
                    r"no general|no lemma|no such|not found)", re.I)

sys.path.insert(0, str(DRIVER))
from bridge_packets import TIER_A, TIER_B, COMMON  # single source of packet truth


def job_state(job_id: str) -> str:
    out = subprocess.run(["curl", "-s", f"localhost:7070/api/alpha/invoke/jobs/{job_id}"],
                         capture_output=True, text=True).stdout
    try:
        data = json.loads(out)
        job = data.get("job", data)
        return str(job.get("state") or "unknown")
    except Exception:
        return "unreachable"


def say(message: str) -> None:
    line = f"{sc.now_iso()} {message}"
    print(line, flush=True)
    with LOG.open("a", encoding="utf-8") as fh:
        fh.write(line + "\n")


LOCK = DRIVER / ".bridge-lane.lock"


def main() -> int:
    """Serialise ticks, then dispatch.

    The module docstring's idempotence claim -- "a problem already recorded is
    never dispatched again" -- holds only if ticks are SERIALISED. `already` is
    read once, before any dispatch is appended, so two concurrent ticks both see
    the same todo list and dispatch the same problem. That is not hypothetical:
    a95J03 went to ams-codex-2 TWICE, 0.8s apart, as jobs
    invoke-1786270526931-3355 and invoke-1786270527687-3357 at
    2026-08-09T10:15:27 and :28, wasting a Codex job on work already in flight.
    (Found by claude-7 during bridge review, 2026-08-09; the worklist itself has
    no duplicate rows -- 167 rows, 167 unique problem ids -- so a duplicated
    entry was ruled out and a read-then-write race is the only remaining cause.)

    A non-blocking exclusive lock makes read-todo/dispatch/record atomic across
    processes. Non-blocking rather than blocking on purpose: if a tick is already
    running there is nothing useful for a second one to do, and queueing ticks
    behind each other would just fire a stale batch late.
    """
    with LOCK.open("w", encoding="utf-8") as lockfh:
        try:
            fcntl.flock(lockfh, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            say("another bridge-lane tick holds the lock; skipping this tick")
            return 0
        try:
            return _dispatch_tick()
        finally:
            fcntl.flock(lockfh, fcntl.LOCK_UN)


def _dispatch_tick() -> int:
    rows = [json.loads(l) for l in ROWS.read_text().splitlines() if l.strip()]
    done = [json.loads(l) for l in JOBS.read_text().splitlines() if l.strip()] if JOBS.exists() else []
    already = {r["problem-id"] for r in done}

    in_flight = [r for r in done if job_state(r["job-id"]) in ("running", "queued", "invoking", "pending")]
    if len(in_flight) >= IN_FLIGHT_MAX:
        say(f"in-flight {len(in_flight)} >= {IN_FLIGHT_MAX}; no dispatch this tick")
        return 0

    todo = [r for r in rows if r["problem-id"] not in already]
    if not todo:
        say(f"worklist EXHAUSTED: all {len(rows)} holes dispatched; nothing left to do")
        return 0

    # Most specific reports first: a longer hole sentence citing identifiers has
    # actually said something a prover can act on.
    todo.sort(key=lambda r: (-len(r["identifiers"]), -len(r["hole"])))
    want = min(TOP_UP_TO - len(in_flight), len(todo))
    say(f"in-flight {len(in_flight)}; {len(todo)} holes remain; dispatching {want}")

    sent = 0
    for i, row in enumerate(todo[:want]):
        pid = row["problem-id"]
        lean_rel = f"problems/{pid}/lean/Main.lean"
        path = REPO / lean_rel
        if not path.exists():
            say(f"  {pid}: no artifact, skipped")
            continue
        source = path.read_text(encoding="utf-8")
        if gates.count_sorries(source) == 0:
            say(f"  {pid}: already closed since harvest, skipped")
            continue
        try:
            main_name = gates.statement_hash(source, pid)[0]
        except Exception:
            main_name = f"apm_{pid.lower()}"
        tier = "A" if STRICT.search(row["hole"]) else "B"
        fields = {"pid": pid, "hole": row["hole"], "path": lean_rel,
                  "sorries": gates.count_sorries(source), "main": main_name}
        fields["common"] = COMMON.format(**fields)
        packet = (TIER_A if tier == "A" else TIER_B).format(**fields)
        seat = SEATS[(len(already) + i) % len(SEATS)]
        try:
            job = agency.dispatch_fn(seat, packet)["job-id"]
        except Exception as exc:
            say(f"  {pid}: DISPATCH FAILED ({exc})")
            continue
        with JOBS.open("a", encoding="utf-8") as fh:
            fh.write(json.dumps({"at": sc.now_iso(), "problem-id": pid, "tier": tier,
                                 "seat": seat, "job-id": job}) + "\n")
        say(f"  {tier} {pid} -> {seat} job {job}")
        sent += 1
    say(f"dispatched {sent}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
