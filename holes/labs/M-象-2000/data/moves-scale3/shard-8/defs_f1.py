import sys, json, os, glob
sys.path.insert(0, ".")
from sbuild3 import build, A

# Fast mode for wave 4: bulk label fragments-only records with simple
# vocabulary-driven moves and minimal scopes; a per-record def dict drives it.
# Custom records (reply acts, unusual scopes) get hand-written defs below.

def simple(rec, frag_moves):
    src = json.load(open(f"input/{rec}.input.json"))
    acts = []
    for f in src["fragments"]:
        mv, opens, within = frag_moves.get(f["id"], ("clarify/context", None, None))
        acts.append(A(f["id"], f["intent"], mv, f["text"][:118],
                      opens=opens, within=within,
                      conf="medium" if not opens and not within else "high"))
    build(rec, acts)

T="claude-17-turn-294"
simple("turn-0anLV2", {
 f"{T}-f-s1-0": ("report-problem/suspected", None, None),
 f"{T}-f-s2-0": ("propose/next-step", None, None),
 f"{T}-f-s3-0": ("report-problem/observed", None, "claude-17-turn-7-f-s2-0"),
 f"{T}-f-s4-0": ("report-problem/observed", None, None),
})

T="claude-17-turn-266"
simple("turn-1976zv", {
 f"{T}-f-s1-0": ("propose/directive", None, None),
 f"{T}-f-s2-0": ("report-problem/observed", None, None),
 f"{T}-f-s2-1": ("propose/directive", None, f"{T}-f-s2-0"),
 f"{T}-f-s3-0": ("prioritize/sequencing", "two-up display first", None),
})

T="claude-17-turn-34"
simple("turn-1hAzsq", {
 f"{T}-f-s1-0": ("ask-action/status-check", None, None),
})
