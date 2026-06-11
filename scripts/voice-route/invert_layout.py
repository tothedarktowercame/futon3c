#!/usr/bin/env python3
"""Invert the voice-typing layout transform on intercepted text.

enhanced-voice-typing.py maps intended text through DVORAK_TO_QWERTY so that
qwerty keycodes typed by ydotool produce the right characters on a dvorak
system. The shim intercepts the MAPPED form; this recovers the original.
The keymap is parsed out of the voice script itself (ast, no import — its
deps live in a venv), so shim and script cannot drift.
"""
import ast
import sys
from pathlib import Path

VOICE_SCRIPT = Path("/home/joe/opt/voice-typing-linux/enhanced-voice-typing.py")


def qwerty_to_dvorak():
    tree = ast.parse(VOICE_SCRIPT.read_text())
    for node in ast.walk(tree):
        if (isinstance(node, ast.Assign)
                and any(getattr(t, "id", None) == "DVORAK_KEYMAP" for t in node.targets)):
            keymap = ast.literal_eval(node.value)
            inverse = {}
            for q_char, q_shift, d_char, d_shift in keymap:
                inverse[q_char] = d_char
                inverse[q_shift] = d_shift
            return inverse
    raise SystemExit("DVORAK_KEYMAP not found in voice script")


def main():
    inverse = qwerty_to_dvorak()
    text = sys.stdin.read()
    sys.stdout.write("".join(inverse.get(ch, ch) for ch in text))


if __name__ == "__main__":
    main()
