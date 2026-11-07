#!/usr/bin/env python3
"""Stop hook: catch 🖖 (U+1F596) written where the 象 mark 🈖 (U+1F216) was meant.

The two differ in one UTF-8 byte (f0 9f 96 96 vs f0 9f 88 96), and Claude kept
writing the Vulcan salute at the start of explain paragraphs (Joe, 2026-11-07:
"it is starting to annoy me"). Only a 🖖 at the start of a line or paragraph is
flagged: that is where the slip happens, and it leaves prose that mentions the
character alone.

A Stop hook runs after the reply is shown, so it cannot hide the slip; it makes
Claude send a correction straight away. It blocks once per stop
(stop_hook_active), so a repeat slip cannot loop.
"""
import json
import re
import sys

SLIP = re.compile(r"(?m)^[ \t>*-]*\U0001F596")


def turn_text(path):
    """Assistant text written since the last genuine user message."""
    texts = []
    with open(path, encoding="utf-8") as f:
        rows = [json.loads(line) for line in f if line.strip()]
    for row in reversed(rows):
        msg = row.get("message") or {}
        content = msg.get("content")
        if row.get("type") == "user" and not row.get("isMeta"):
            if isinstance(content, str) or any(
                    isinstance(b, dict) and b.get("type") == "text" for b in content or []):
                break
        if row.get("type") == "assistant" and isinstance(content, list):
            texts.extend(b.get("text", "") for b in content
                         if isinstance(b, dict) and b.get("type") == "text")
    return "\n".join(reversed(texts))


def main():
    data = json.load(sys.stdin)
    if data.get("stop_hook_active"):
        return
    path = data.get("transcript_path")
    if not path:
        return
    if SLIP.search(turn_text(path)):
        print(json.dumps({
            "decision": "block",
            "reason": ("Your reply has a paragraph that starts with 🖖 (U+1F596, Vulcan salute). "
                       "That is the known slip for the 象 explain mark 🈖 (U+1F216). "
                       "Send a one-line correction naming which paragraph should have opened with 🈖. "
                       "Do not resend the whole reply."),
        }))


if __name__ == "__main__":
    main()
