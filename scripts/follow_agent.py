#!/usr/bin/env python3
"""Follow an Agency Claude seat's conversation as it happens, in a terminal.

Joe, 2026-11-10: bells to claude-cs land in its conversation, but the Claude
Code panel in code-server only shows them when the conversation is reopened
("claude-cs doesn't have the equivalent of agent-follow-mode").  Run this in a
code-server terminal beside the panel:

    python3 ~/code/futon3c/scripts/follow_agent.py claude-cs

It asks the roster (GET :7070/api/alpha/agents) for the seat's session id on
every poll, so a re-pointed seat is followed automatically, and prints each new
user or assistant turn from that session's transcript
(~/.claude/projects/*/<session>.jsonl).  Read-only: it writes nothing.
"""
import argparse
import glob
import json
import os
import sys
import time
import urllib.request

AGENTS_URL = "http://localhost:7070/api/alpha/agents"
DIM, BOLD, CYAN, GREEN, RESET = "\033[2m", "\033[1m", "\033[36m", "\033[32m", "\033[0m"


def session_id(agent):
    try:
        with urllib.request.urlopen(AGENTS_URL, timeout=5) as r:
            agents = json.load(r).get("agents", {})
        return (agents.get(agent) or {}).get("session-id")
    except Exception:
        return None


def transcript(sid):
    hits = glob.glob(os.path.expanduser(f"~/.claude/projects/*/{sid}.jsonl"))
    return max(hits, key=os.path.getmtime) if hits else None


def turn_text(entry):
    """(role, time, text) for a user/assistant turn with visible text, else None."""
    msg = entry.get("message") or {}
    role = msg.get("role")
    if role not in ("user", "assistant"):
        return None
    content = msg.get("content")
    if isinstance(content, str):
        parts = [content]
    else:
        parts = [c.get("text", "") for c in (content or [])
                 if isinstance(c, dict) and c.get("type") == "text"]
    text = "\n".join(p for p in parts if p and not p.lstrip().startswith("<system-reminder>")).strip()
    if not text:
        return None
    return role, (entry.get("timestamp") or "")[11:19], text


def show(role, at, text, width):
    colour = CYAN if role == "user" else GREEN
    label = "in " if role == "user" else "out"
    print(f"{DIM}{at}{RESET} {colour}{BOLD}{label}{RESET}")
    lines = text.splitlines()
    for line in lines[:width] if width else lines:
        print("  " + line)
    if width and len(lines) > width:
        print(f"  {DIM}… {len(lines) - width} more lines{RESET}")
    print()
    sys.stdout.flush()


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("agent", nargs="?", default="claude-cs")
    ap.add_argument("--last", type=int, default=3, help="turns to show at start (default 3)")
    ap.add_argument("--lines", type=int, default=40, help="max lines per turn, 0 = all (default 40)")
    ap.add_argument("--interval", type=float, default=2.0, help="poll seconds (default 2)")
    args = ap.parse_args()

    current, path, offset = None, None, 0
    while True:
        sid = session_id(args.agent) or current
        if sid and sid != current:
            path = transcript(sid)
            print(f"{BOLD}following {args.agent} · session {sid[:8]}{RESET}"
                  + ("" if path else f" {DIM}(no transcript yet){RESET}") + "\n")
            current, offset = sid, 0
            if path:
                with open(path, encoding="utf-8") as f:
                    turns = [t for t in (turn_text(json.loads(l)) for l in f if l.strip()) if t]
                    offset = f.tell()
                for t in turns[-args.last:] if args.last else []:
                    show(*t, args.lines)
        elif current and not path:
            path = transcript(current)
        if path:
            with open(path, encoding="utf-8") as f:
                f.seek(offset)
                chunk = f.read()
                # Only whole lines: a partly written last line is read next time.
                cut = chunk.rfind("\n") + 1
                offset += len(chunk[:cut].encode("utf-8"))
            for line in chunk[:cut].splitlines():
                try:
                    t = turn_text(json.loads(line))
                except ValueError:
                    continue
                if t:
                    show(*t, args.lines)
        time.sleep(args.interval)


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        pass
