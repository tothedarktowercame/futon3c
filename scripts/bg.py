#!/usr/bin/env python3
"""bg.py — durable background processes for REPL-inhabiting agents.

Spawn long-running work as a child of the futon3c JVM (durable across pouch
teardown) instead of as a child of your ephemeral warm pouch (which is evicted
between turns and reaps even setsid-detached children). See
futon3c.agency.bg-process and futon3c/CLAUDE.md "Durable background work".

Usage:
  scripts/bg.py launch "<shell command>" [--agent <id>] [--label <l>] [--dir <d>]
  scripts/bg.py status <id>
  scripts/bg.py tail   <id> [n]
  scripts/bg.py list   [agent-id]
  scripts/bg.py kill   <id>
  scripts/bg.py forget <id>

Thin wrapper over the Drawbridge eval endpoint (the agent->JVM channel).
"""
import sys
import os
import json
import subprocess

DRAWBRIDGE = os.environ.get("FUTON3C_DRAWBRIDGE_URL", "http://127.0.0.1:6768/eval")
TOKEN_FILE = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
                          ".admintoken")


def _token():
    with open(TOKEN_FILE) as f:
        return f.read().strip()


def _cljstr(s):
    """Render a Python string as a Clojure string literal."""
    return '"' + str(s).replace("\\", "\\\\").replace('"', '\\"') + '"'


def _eval(form):
    out = subprocess.run(
        ["curl", "-s", "-X", "POST", DRAWBRIDGE,
         "-H", "x-admin-token: " + _token(),
         "-H", "Content-Type: text/plain",
         "--data-binary", form],
        capture_output=True, text=True).stdout
    return out


def main(argv):
    if not argv:
        print(__doc__)
        return 2
    cmd = argv[0]
    if cmd == "launch":
        if len(argv) < 2:
            print("launch needs a command string"); return 2
        shell_cmd = argv[1]
        opts = {}
        i = 2
        while i < len(argv) - 1:
            if argv[i] == "--agent": opts["agent-id"] = argv[i + 1]; i += 2
            elif argv[i] == "--label": opts["label"] = argv[i + 1]; i += 2
            elif argv[i] == "--dir": opts["dir"] = argv[i + 1]; i += 2
            else: i += 1
        parts = [":cmd " + _cljstr(shell_cmd)]
        for k, v in opts.items():
            parts.append(":" + k + " " + _cljstr(v))
        form = "(futon3c.agency.bg-process/launch! {" + " ".join(parts) + "})"
    elif cmd == "status":
        form = "(futon3c.agency.bg-process/status " + _cljstr(argv[1]) + ")"
    elif cmd == "tail":
        n = argv[2] if len(argv) > 2 else "40"
        form = "(futon3c.agency.bg-process/tail " + _cljstr(argv[1]) + " " + str(int(n)) + ")"
    elif cmd == "list":
        form = ("(futon3c.agency.bg-process/list-tasks " + _cljstr(argv[1]) + ")"
                if len(argv) > 1 else "(futon3c.agency.bg-process/list-tasks)")
    elif cmd == "kill":
        form = "(futon3c.agency.bg-process/kill! " + _cljstr(argv[1]) + ")"
    elif cmd == "forget":
        form = "(futon3c.agency.bg-process/forget! " + _cljstr(argv[1]) + ")"
    else:
        print("unknown command: " + cmd); print(__doc__); return 2
    print(_eval("(do (require 'futon3c.agency.bg-process) " + form + ")"))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
