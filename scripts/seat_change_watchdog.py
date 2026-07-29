#!/usr/bin/env python3
"""One-shot watchdog for the 2026-07-29 ground-control seat change.

The codex-sorry loop's dispatch seat moved claude-6 -> claude-9 (commit
006eaf50). The bell path for the new seat is UNTESTED, so this watches the
first fire and guarantees ground control gets woken even if the completion
bell fails to route: once the dispatched job reaches a terminal state it POSTs
a park awaiting that job-id, which releases immediately and injects the
continuation payload into the claude-9 buffer.

Runs as a child of the futon3c JVM via scripts/bg.py (durable across pouch
eviction). Single-purpose and disposable: delete once the first fire under the
new seat has been confirmed.
"""
import json
import re
import time
import urllib.request

AGENT = "claude-9"
SESSION = "4fc6f9c2-1807-40ac-8721-d38eb588cfe9"
LOG = "/home/joe/code/futon2/logs/codex-sorry.cron.log"
API = "http://localhost:7070/api/alpha"

# The fire we are watching for. Cron log timestamps are UTC, not BST.
FIRE_PREFIX = "2026-07-29T12:30"
GIVE_UP_AFTER = 3 * 60 * 60  # seconds; the fire is ~45 min out at launch


def get(url):
    with urllib.request.urlopen(url, timeout=30) as r:
        return json.load(r)


def post(url, body):
    req = urllib.request.Request(
        url,
        data=json.dumps(body).encode(),
        headers={"Content-Type": "application/json"},
    )
    with urllib.request.urlopen(req, timeout=30) as r:
        return json.load(r)


def find_fire():
    """Return (kind, job-id-or-None) for the watched fire, or None if not yet."""
    try:
        with open(LOG) as f:
            lines = [ln for ln in f if ln.startswith(FIRE_PREFIX)]
    except OSError:
        return None
    for ln in lines:
        m = re.search(r"job=(\S+)", ln)
        if m:
            return ("dispatched", m.group(1))
    if lines:
        # gate-closed (usage / concurrency / backpressure / zai-live)
        return ("gate-closed", lines[-1].strip())
    return None


def wake(payload):
    post(
        f"{API}/park",
        {
            "agent": AGENT,
            "session": SESSION,
            "surface": "emacs-repl",
            "awaiting": [],
            "deadline-ms": int(time.time() * 1000),
            "payload": payload,
        },
    )


def main():
    deadline = time.time() + GIVE_UP_AFTER
    job = None
    while time.time() < deadline:
        if job is None:
            fire = find_fire()
            if fire is None:
                time.sleep(60)
                continue
            kind, detail = fire
            if kind == "gate-closed":
                wake(
                    "SEAT-CHANGE WATCHDOG: the 12:30Z cron fire was GATE-CLOSED, "
                    "not dispatched, so the new --from claude-9 seat is still "
                    f"untested. Log line: {detail}. Check which gate closed "
                    "(usage <50% / concurrency / verification-backpressure / "
                    "zai-live) and whether that is expected; the next fire is "
                    "the next test."
                )
                return
            job = detail
        state = (get(f"{API}/invoke/jobs/{job}") or {}).get("state")
        if state in ("done", "error", "failed", "cancelled"):
            wake(
                "SEAT-CHANGE WATCHDOG (first fire under --from claude-9). Job "
                f"{job} reached state={state}. If you ALREADY handled this row "
                "via a normal completion bellback, the repoint routed correctly "
                "-- log a one-line win and stop. If this is your first sight of "
                "it, THE BELL DID NOT ROUTE: (1) verify the row yourself per "
                "M-codex-sorry-loop standing protocol (lake env lean + /tmp "
                "scratch #print axioms + statement-integrity diff + hygiene); "
                "(2) write the outcome-half receipt and resolve the queue row so "
                "verification backpressure reopens; (3) diagnose the bell path -- "
                "roster entry for claude-9 live? a second hardcoded from-agent in "
                "dispatch_with_recall.clj? -- and report honestly to Joe before "
                "the next fire. Delete scripts/seat_change_watchdog.py either way."
            )
            return
        time.sleep(60)
    wake(
        "SEAT-CHANGE WATCHDOG timed out without a terminal job state. Check the "
        "cron log and the roster by hand; the seat change may be untested still."
    )


if __name__ == "__main__":
    main()
