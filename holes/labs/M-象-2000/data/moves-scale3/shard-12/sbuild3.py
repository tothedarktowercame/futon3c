#!/usr/bin/env python3
"""Batch-3 shard builder: verify every quote against its act's own text, then
write.  Authority: fragments + reply_acts + acts (shortest text per id).
Answer/scope targets may also be open_ports ids or previous_reply_acts ids."""
import json
import sys
from pathlib import Path

HERE = Path(__file__).parent


def norm(t):
    return " ".join(str(t or "").split())


def load(record):
    src = json.loads((HERE / "input" / f"{record}.input.json").read_text())
    own = {}
    for a in src.get("reply_acts", []) + src.get("acts", []) + src.get("fragments", []):
        aid = a.get("id")
        if not aid:
            continue
        t = a.get("text", "")
        if aid not in own or len(t) < len(own[aid]):
            own[aid] = t
    prev = {a["id"] for a in src.get("previous_reply_acts", [])}
    ports = {p["act"] for p in src.get("open_ports", [])}
    frags = [f["id"] for f in src.get("fragments", [])] + [a["id"] for a in src.get("acts", [])]
    return src, own, prev | ports, frags, ports


def build(record, acts, unlabelled=(), at="2026-10-12T20:00:00Z"):
    src, own, targets, frags, ports = load(record)
    problems = []
    for act in acts:
        t = own.get(act["act"])
        if t is None:
            problems.append(f"unknown act {act['act']} (known: {len(own)})")
            continue
        if norm(act["quote"]) not in norm(t):
            problems.append(f"{act['act']}: quote not in own text: {act['quote']!r}")
        for ans in act.get("answers", []):
            if norm(ans["quote"]) not in norm(t):
                problems.append(f"{act['act']}: answer quote not in own text: {ans['quote']!r}")
            if ans["act"] not in frags and ans["act"] not in targets:
                problems.append(f"{act['act']}: bad answer target {ans['act']}")
        for k in ("within", "closes"):
            v = act["scope"].get(k)
            if v and v not in frags and v not in targets:
                problems.append(f"{act['act']}: bad scope.{k} target {v}")
    for u in unlabelled:
        if u not in own:
            problems.append(f"unlabelled unknown act {u}")
    if problems:
        print(f"{record}: REFUSED")
        for p in problems:
            print("  " + p)
        return False
    sidecar = {"record": record, "labeller": "zai-1", "schema": "moves/1", "at": at,
               "acts": acts, "unlabelled": list(unlabelled)}
    (HERE / f"{record}.moves.json").write_text(
        json.dumps(sidecar, ensure_ascii=False, indent=1) + "\n", encoding="utf-8")
    print(f"{record}: wrote {len(acts)} acts")
    return True


def A(act, kind, move, quote, answers=(), opens=None, closes=None, within=None, conf="high", note=None):
    a = {"act": act, "kind": kind, "move": move, "quote": quote,
         "answers": [dict(zip(("act", "quote", "effect"), x)) for x in answers],
         "scope": {"opens": opens, "closes": closes, "within": within},
         "confidence": conf}
    if note:
        a["note"] = note
    return a
