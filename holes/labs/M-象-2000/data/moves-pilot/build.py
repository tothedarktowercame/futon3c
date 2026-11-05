#!/usr/bin/env python3
"""Build .moves.json sidecars from authored acts; every quote is verified as a
normalized substring of its own text before writing (so validate.py's quote
check can only see valid labels). Usage: build.py <defs module>"""
import importlib.util
import json
import re
import sys
from pathlib import Path

HERE = Path(__file__).parent
MARKS = set("㊥㊭㊣🈚㊟🈖🈯㊢㊩㊬🈹🈳🈲🈸㊯㊝㊮🈕🈰🈝🈘㊫🈡")


def norm(t):
    return " ".join(str(t or "").split())


def marked_paragraphs(reply):
    out = []
    for para in re.split(r"\n\s*\n", reply):
        p = para.strip()
        if not p:
            continue
        # a mark that starts mid-text right after sentence punctuation opens
        # its own marked paragraph (the recorder sometimes joins them with
        # no whitespace: "…message.㊥ (…)")
        pieces = [p]
        while True:
            for piece in list(pieces):
                cut = None
                for m in re.finditer("[" + "".join(MARKS) + "]", piece):
                    if m.start() == 0:
                        continue
                    if piece[m.start() - 1] in ".!?:;\n":
                        cut = m.start()
                        break
                if cut:
                    i = pieces.index(piece)
                    pieces[i:i + 1] = [piece[:cut].strip(), piece[cut:].strip()]
                    break
            else:
                break
        for piece in pieces:
            if piece and piece[0] in MARKS:
                out.append(piece)
    return out


def build(record, acts, unlabelled=(), at="2026-10-11T20:30:00Z"):
    src = json.loads((HERE / "input" / f"{record}.input.json").read_text())
    texts = {}
    for a in src.get("acts", []) + src.get("fragments", []):
        texts[a["id"]] = a.get("text", "")
    if not src.get("acts"):  # derived reply paragraphs only when :acts absent
        paras = marked_paragraphs(src.get("reply_text", ""))
        turn = src["turn_id"]
        for i, p in enumerate(paras):
            texts[f"{turn}-r-{i}"] = p
    problems = []
    for act in acts:
        t = texts.get(act["act"])
        if t is None:
            problems.append(f"unknown act {act['act']}")
            continue
        if norm(act["quote"]) not in norm(t):
            problems.append(f"{act['act']}: quote not in own text: {act['quote']!r}")
        for ans in act.get("answers", []):
            if norm(ans["quote"]) not in norm(t):
                problems.append(f"{act['act']}: answer quote not in own text: {ans['quote']!r}")
    if problems:
        print(f"{record}: REFUSED"); [print(" ", p) for p in problems]
        return False
    sidecar = {"record": record, "labeller": "zai-1", "schema": "moves/1", "at": at,
               "acts": acts, "unlabelled": list(unlabelled)}
    (HERE / f"{record}.moves.json").write_text(
        json.dumps(sidecar, ensure_ascii=False, indent=1) + "\n", encoding="utf-8")
    print(f"{record}: wrote {len(acts)} acts")
    return True
