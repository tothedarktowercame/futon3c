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

import fcntl
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
# Operator seats whose bellback-turn REPLIES would otherwise route to the
# void ("a bellback never bellbacks" — deliberate loop safety in the
# Agency). The watcher is the delivery net: their completions render in
# the REPL, and replies carrying an escalation marker are RELAYED as a
# real bell to the supervisor (2026-08-11: a claude-3 escalation sat
# unread three hours in exactly this gap).
WATCHED_AGENTS = set(
    os.environ.get("BELLBACK_AGENTS", "claude-3,claude-2").split(",")
)
ESCALATION_MARKERS = ("ESCALAT", "# HOLDING", "RULING", "needs your hand",
                      "NEEDS A RULING", "STOP BEFORE")
SUPERVISOR = os.environ.get("BELLBACK_SUPERVISOR", "ams-claude-1")
AGENCY_SEND = "/home/joe/code/futon3c/scripts/agency_send.py"
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
    # Land in the REPL conversation flow as a continuation (Joe,
    # 2026-08-10): insert a "bellback" message above the prompt in every
    # live claude-repl buffer via agent-chat-insert-message. Falls back
    # to a minibuffer message when no REPL buffer is live.
    safe = text.replace("\\", "\\\\").replace('"', '\\"')
    eval_form = (
        '(let ((hit 0))'
        ' (dolist (buf (buffer-list))'
        '  (with-current-buffer buf'
        '   (when (and (string-match-p "\\\\*claude-repl" (buffer-name))'
        '              (boundp (quote agent-chat--prompt-marker))'
        '              agent-chat--prompt-marker'
        '              (marker-position agent-chat--prompt-marker))'
        f'    (agent-chat-insert-message "bellback" "{safe}")'
        '    (setq hit (1+ hit)))))'
        f' (when (zerop hit) (message "%s" "{safe}"))'
        ' hit)'
    )
    subprocess.run(
        ["emacsclient", "-s", SOCKET, "--eval", eval_form],
        capture_output=True, timeout=10,
    )


def relay_escalation(j):
    """A watched agent's bellback-turn reply carries an escalation marker:
    convert the void reply into a delivered bell to the supervisor."""
    result = (j.get("result") or "")
    pointer = (f"ESCALATION RELAY (watcher): {j.get('agent-id')} wrote an "
               f"escalation into a bellback-turn reply (the void). Job "
               f"{j.get('job-id')}. Opening lines:\n\n" + result[:400] +
               "\n\nFetch the full result from the job endpoint and rule.")
    subprocess.run(
        ["python3", AGENCY_SEND, "--to", SUPERVISOR, "--from",
         "escalation-relay", "--kind", "bell", "--mode", "brief"],
        input=pointer, text=True, capture_output=True, timeout=30,
    )


def poll_once(seen, announce=True):
    with urllib.request.urlopen(f"{BASE}/api/alpha/invoke/jobs", timeout=10) as r:
        jobs = json.load(r).get("jobs", [])
    for j in jobs:
        jid = j.get("job-id")
        state = j.get("state") or (j.get("events") or [{}])[-1].get("type")
        caller = j.get("caller")
        agent = j.get("agent-id")
        watched_agent_reply = (caller == "auto-bellback"
                               and agent in WATCHED_AGENTS)
        if not jid or jid in seen or (caller not in WATCHED_CALLERS
                                      and not watched_agent_reply):
            continue
        if state not in TERMINAL and not j.get("finished-at"):
            continue
        if watched_agent_reply and announce:
            # The window listing has only result-summary; fetch the full
            # job for the marker scan.
            try:
                with urllib.request.urlopen(
                        f"{BASE}/api/alpha/invoke/jobs/{jid}", timeout=10) as rf:
                    full = json.load(rf).get("job", {})
            except Exception:
                full = j
            result = (full.get("result") or "")
            if any(m in result for m in ESCALATION_MARKERS):
                relay_escalation(full)
        if announce:
            summary = (j.get("result-summary") or "").strip().replace("\n", " ")
            if len(summary) > 120:
                summary = summary[:117] + "..."
            notify(f"[bellback] {j.get('agent-id')} {state or 'finished'} "
                   f"{jid}{' — ' + summary if summary else ''}")
        seen.add(jid)
    return seen


def main():
    # Single-instance guard: two watchers sharing the seen-state race
    # between load and save, and each relays the same escalation (two
    # duplicate relay bells 10s apart, 2026-08-11). The lock fd must
    # stay open for the process lifetime.
    lock = open("/tmp/operator-bellback.lock", "w")
    try:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except OSError:
        print("another operator_bellback instance holds the lock; exiting",
              file=sys.stderr)
        sys.exit(0)
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
