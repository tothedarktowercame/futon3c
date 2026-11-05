import sys
sys.path.insert(0, ".")
from wave5 import simple, T

def rec_of(tid):
    import json, os
    for f in os.listdir("input"):
        d = json.load(open("input/"+f))
        if d["turn_id"] == tid:
            return f[:-11]
    return None

# batch 1: records from the dump above (in order encountered)
R = {}
def add(tid, moves):
    R[rec_of(tid)] = (tid, moves)

add("claude-2-turn-12", {"f-s1-0": ("report/status-update",None,None),
 "f-s2-0": ("ask-action/ask-decision",None,None),
 "f-s3-0": ("report-problem/observed",None,None)})
add("claude-3-turn-17", {"f-s1-0": ("approve/plain",None,None)})
add("claude-1-turn-21", {"f-s1-0": ("approve/plain",None,None),
 "f-s1-1": ("constrain/stand-rule",None,None)})
add("claude-17-turn-185", {"f-s1-0": ("ask-action/status-check",None,None),
 "f-s2-0": ("clarify/assertion",None,None),
 "f-s3-0": ("clarify/ask-decision",None,None)})
add("claude-3-turn-20", {"f-s1-0": ("propose/tentative",None,None),
 "f-s2-0": ("constrain/stand-rule",None,None),
 "f-s3-0": ("approve/plain",None,None),
 "f-s4-0": ("propose/directive",None,None),
 "f-s5-0": ("clarify/assertion",None,None),
 "f-s6-0": ("clarify/assertion",None,None)})
add("claude-3-turn-19", {"f-s1-0": ("clarify/context",None,None),
 "f-s2-0": ("qualify/caveat",None,None),
 "f-s3-0": ("defer/until",None,None),
 "f-s4-0": ("clarify/assertion",None,None),
 "f-s5-0": ("clarify/assertion",None,None),
 "f-s6-0": ("explain/analogy",None,None),
 "f-s7-0": ("propose/extend",None,None),
 "f-s8-0": ("approve/plain",None,None)})
add("claude-17-turn-215", {"f-s1-0": ("disagree/correction",None,None),
 "f-s1-1": ("report/context",None,None)})
add("claude-17-turn-252", {"f-s1-0": ("propose/directive",None,None),
 "f-s2-0": ("clarify/context",None,None),
 "f-s3-0": ("clarify/assertion",None,None)})
add("claude-2-turn-4", {"f-s1-0": ("report-problem/observed",None,None),
 "f-s2-0": ("clarify/assertion",None,None)})
add("claude-1-turn-61", {"f-s1-0": ("approve/plain",None,None)})
add("claude-3-turn-5", {"f-s1-0": ("clarify/context",None,None),
 "f-s2-0": ("approve/plain",None,None),
 "f-s3-0": ("propose/extend",None,None)})
add("claude-3-turn-22", {"f-s1-0": ("clarify/assertion",None,None),
 "f-s2-0": ("explain/interpretation",None,None)})
add("claude-3-turn-3", {"f-s1-0": ("disagree/correction",None,None),
 "f-s2-0": ("clarify/assertion",None,None)})
add("claude-1-turn-60", {"f-s1-0": ("report/status-update",None,None),
 "f-s1-1": ("ask-action/ask-decision",None,None)})
add("claude-17-turn-232", {"f-s1-0": ("ask-action/commission",None,None)})
add("claude-1-turn-87", {"f-s1-0": ("clarify/context",None,None),
 "f-s2-0": ("report-problem/observed",None,None),
 "f-s3-0": ("prioritize/benefit",None,None)})
add("claude-1-turn-51", {"f-s2-0": ("approve/plain",None,None),
 "f-s4-0": ("report-problem/observed",None,None),
 "f-s5-0": ("clarify/check-understand",None,None),
 "f-s6-0": ("report-problem/observed",None,None),
 "f-s8-0": ("approve/plain",None,None)})
add("claude-1-turn-59", {"f-s1-0": ("report/context",None,None),
 "f-s1-1": ("ask-action/commission",None,None)})
add("claude-3-turn-24", {"f-s1-0": ("constrain/stand-rule",None,None),
 "f-s2-0": ("propose/alternative",None,None),
 "f-s3-0": ("constrain/stand-rule",None,None)})
add("claude-1-turn-67", {"f-s1-0": ("report/context",None,None),
 "f-s1-1": ("delegate/to-agent",None,None),
 "f-s2-0": ("constrain/stand-rule",None,None)})
add("claude-3-turn-16", {"f-s1-0": ("propose/tentative",None,None),
 "f-s1-1": ("report/context",None,None),
 "f-s2-0": ("propose/extend",None,None)})
add("claude-1-turn-10", {"f-s1-0": ("approve/plain",None,None),
 "f-s2-0": ("constrain/stand-rule",None,None),
 "f-s3-0": ("report-problem/observed",None,None),
 "f-s3-1": ("collect/ask-option",None,None)})
add("claude-1-turn-88", {"f-s1-0": ("ask-action/commission",None,None)})
add("claude-1-turn-45", {"f-s1-0": ("clarify/check-understand",None,None),
 "f-s2-0": ("disagree/correction",None,None),
 "f-s3-0": ("challenge/other",None,None),
 "f-s4-0": ("challenge/other",None,None)})
add("claude-2-turn-9", {"f-s1-0": ("report/status-update",None,None),
 "f-s2-0": ("report/status-update",None,None)})
add("claude-1-turn-14", {"f-s1-0": ("delegate/to-agent",None,None)})
add("claude-1-turn-55", {"f-s1-0": ("approve/plain",None,None)})

for rec,(tid,moves) in R.items():
    if rec is None:
        print("SKIP (record not found):", tid); continue
    full = {f"{tid}-{k}": v for k,v in moves.items()}
    simple(rec, full)
