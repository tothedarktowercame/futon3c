#!/usr/bin/env python3
"""Operator bellback: notify Joe's emacs when watched agents' jobs finish.

Interim push channel (Joe's request, 2026-08-10): polls the Agency's
recent-jobs window and sends one emacsclient message per newly terminal
job whose caller is watched. The proper feature — a delivery hook inside
the Agency notifying the operator surface — is filed as follow-up; this
poller needs no serving-path change and can be retired when that lands.

Run durably as a child of the futon3c JVM:
  scripts/bg.py launch "python3 scripts/operator_bellback.py" \
      --agent ams-claude-1 --label operator-bellback
(Survives pouch teardown; dies with a JVM restart — relaunch after one.)
"""

import json
import os
import subprocess
import sys
import time
import urllib.request

BASE = os.environ.get("FUTON3C_BASE", "http://127.0.0.1:7070")
SOCKET = os.environ.get("FUTON3C_EMACS_SOCKET_NAME", "server")
WATCHED_CALLERS = set(
    os.environ.get("BELLBACK_CALLERS", "ams-claude-1,joe").split(",")
)
TERMINAL = {"done", "failed", "timeout", "cancelled", "deduped", "overrun"}
STATE_FILE = "/tmp/operator-bellback-seen.json"
POLL_S = 15


def load_seen():
    try:
        with open(STATE_FILE) as f:
            return set(json.load(f))
    except Exception:
        return set()


def save_seen(seen):
    # Keep the file bounded; the jobs window is only 20 deep anyway.
    with open(STATE_FILE, "w") as f:
        json.dump(sorted(seen)[-500:], f)


def notify(text):
    # message → minibuffer + *Messages*; single-line, elisp-escaped.
    safe = text.replace("\\", "\\\\").replace('"', '\\"')
    subprocess.run(
        ["emacsclient", "-s", SOCKET, "--eval",
         f'(message "%s" "{safe}")'],
        capture_output=True, timeout=10,
    )


def poll_once(seen, announce=True):
    with urllib.request.urlopen(f"{BASE}/api/alpha/invoke/jobs", timeout=10) as r:
        jobs = json.load(r).get("jobs", [])
    for j in jobs:
        jid = j.get("job-id")
        state = j.get("state") or (j.get("events") or [{}])[-1].get("type")
        caller = j.get("caller")
        if not jid or jid in seen or caller not in WATCHED_CALLERS:
            continue
        if state not in TERMINAL and not j.get("finished-at"):
            continue
        if announce:
            summary = (j.get("result-summary") or "").strip().replace("\n", " ")
            if len(summary) > 120:
                summary = summary[:117] + "..."
            notify(f"[bellback] {j.get('agent-id')} {state or 'finished'} "
                   f"{jid}{' — ' + summary if summary else ''}")
        seen.add(jid)
    return seen


def main():
    seen = load_seen()
    # On first run, silently mark everything currently terminal as seen so
    # the watcher only announces NEW completions, not history.
    if not seen:
        try:
            seen = poll_once(set(), announce=False)
        except Exception:
            seen = set()
        save_seen(seen)
    while True:
        try:
            before = set(seen)
            seen = poll_once(seen)
            if seen != before:
                save_seen(seen)
        except Exception as e:
            print(f"poll error: {e}", file=sys.stderr)
        time.sleep(POLL_S)


if __name__ == "__main__":
    main()
