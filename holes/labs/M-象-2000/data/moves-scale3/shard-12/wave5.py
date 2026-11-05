#!/usr/bin/env python3
"""Wave-5 bulk builder: label fragments-only records from a table.
Each entry: (record_id, turn_id, {frag_suffix: (move, opens, within)})."""
import json, sys
sys.path.insert(0, ".")
from sbuild3 import build, A

def T(rec):
    return json.load(open(f"input/{rec}.input.json"))["turn_id"]

def simple(rec, frag_moves, unlabelled=()):
    src = json.load(open(f"input/{rec}.input.json"))
    acts = []
    for f in src["fragments"]:
        mv, opens, within = frag_moves.get(f["id"], ("clarify/context", None, None))
        acts.append(A(f["id"], f["intent"], mv, f["text"][:118],
                      opens=opens, within=within,
                      conf="medium" if not opens and not within else "high"))
    build(rec, acts, unlabelled)
