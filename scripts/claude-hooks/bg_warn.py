#!/usr/bin/env python3
"""PreToolUse hook: warn Agency pouch agents off pouch-bound background work.

A claude-N pouch is torn down between turns, and its background shells die
with it (futon3c/CLAUDE.md, "Durable background work").  Durable work must go
through scripts/bg.py, which re-parents it to the futon3c JVM.

This hook only WARNS: it never denies, so short `&` use still runs.  It acts
only when FUTON_AGENT_ID is set (Agency's agent-env); terminal sessions are
left alone.  Wired from ~/.claude/settings.json (Bash|Monitor matcher).
Origin: futon0/holes/missions/M-landscape-positioning.md §5.
"""
import json
import os
import re
import sys

# A lone `&` ending a command segment; not `&&`, `2>&1`, `&>`, `|&`.
TRAILING_AMP = re.compile(r"(?<![&|<>])&(?![&>\d])\s*(?:$|;|\n|\))", re.M)
DETACH = re.compile(r"\b(nohup|setsid|disown)\b")


def reasons(tool, inp):
    if tool == "Monitor":
        return ["a Monitor watch ends when your pouch is torn down"]
    out = []
    if inp.get("run_in_background"):
        out.append("run_in_background is reaped when your pouch is torn down")
    cmd = inp.get("command", "") or ""
    if "bg.py" in cmd:
        return out
    if DETACH.search(cmd):
        out.append("nohup/setsid/disown children are reaped with the pouch too")
    if TRAILING_AMP.search(cmd):
        out.append("a trailing `&` job dies with the pouch")
    return out


def main():
    agent = os.environ.get("FUTON_AGENT_ID")
    if not agent:
        return
    try:
        payload = json.load(sys.stdin)
    except ValueError:
        return
    why = reasons(payload.get("tool_name", ""), payload.get("tool_input") or {})
    if not why:
        return
    msg = (
        f"[bg-warn] {agent}: " + "; ".join(why) + ". If this work must outlive "
        "the current turn, launch it with `python3 ~/code/futon3c/scripts/bg.py "
        f'launch "<cmd>" --agent {agent} --label <name>` and check it with '
        "`bg.py status|tail <id>`. If it finishes within this turn, ignore this."
    )
    print(json.dumps({"hookSpecificOutput": {
        "hookEventName": "PreToolUse", "additionalContext": msg}}))


if __name__ == "__main__":
    try:
        main()
    except Exception:  # a warning hook must never block a tool call
        pass
