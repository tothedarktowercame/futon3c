import sys
sys.path.insert(0, ".")
from wave5 import simple

def rec_of(tid):
    import json, os
    for f in os.listdir("input"):
        if json.load(open("input/"+f))["turn_id"] == tid:
            return f[:-11]
    return None
R={}
def add(tid, moves): R[rec_of(tid)]=(tid,moves)

add("claude-3-turn-23",{"f-s2-0":("propose/tentative",None,None),"f-s3-0":("propose/redirect-scope",None,None),"f-s4-0":("propose/directive",None,None)})
add("claude-17-turn-231",{"f-s1-0":("constrain/for-now",None,None),"f-s1-1":("ask-action/status-check",None,None),"f-s1-2":("explain/justification",None,None)})
add("claude-3-turn-18",{"f-s1-0":("approve/plain",None,None),"f-s2-0":("propose/redirect-scope",None,None),"f-s3-0":("clarify/assertion",None,None),"f-s4-0":("propose/extend",None,None),"f-s5-0":("propose/extend",None,None)})
add("claude-17-turn-248",{"f-s1-0":("constrain/stand-rule",None,None),"f-s2-0":("propose/directive",None,None)})
add("claude-17-turn-242",{"f-s1-0":("ask-action/commission",None,None),"f-s2-0":("propose/tentative",None,None),"f-s3-0":("ask-action/commission",None,None),"f-s4-0":("constrain/stand-rule",None,None),"f-s5-0":("constrain/stand-rule",None,None),"f-s6-0":("constrain/stand-rule",None,None),"f-s7-0":("constrain/stand-rule",None,None),"f-s8-0":("constrain/stand-rule",None,None)})
add("claude-17-turn-212",{"f-s1-0":("propose/directive",None,None)})
add("claude-1-turn-64",{"f-s1-0":("ask-action/commission",None,None)})
add("claude-3-turn-7",{"f-s1-0":("disagree/correction",None,None),"f-s2-0":("clarify/assertion",None,None),"f-s3-0":("report/context",None,None),"f-s4-0":("report/context",None,None)})
add("claude-17-turn-230",{"f-s1-0":("report-problem/observed",None,None),"f-s2-0":("clarify/assertion",None,None),"f-s2-1":("qualify/caveat",None,None)})
add("claude-17-turn-249",{"f-s1-0":("propose/redirect-scope",None,None)})
add("claude-3-turn-6",{"f-s1-0":("qualify/caveat",None,None),"f-s2-0":("explain/interpretation",None,None),"f-s3-0":("explain/interpretation",None,None)})
add("claude-1-turn-50",{"f-s1-0":("ask-action/commission",None,None)})
add("claude-2-turn-6",{"f-s1-0":("report-problem/observed",None,None)})
add("claude-2-turn-3",{"f-s1-0":("challenge/other",None,None),"f-s2-0":("verify/ask-option",None,None)})
add("claude-3-turn-2",{"f-s1-0":("clarify/context",None,None)})
add("claude-2-turn-14",{"f-s1-0":("retract/correction",None,None),"f-s2-0":("approve/plain",None,None)})
add("claude-17-turn-241",{"f-s1-0":("approve/plain",None,None)})
add("claude-1-turn-53",{"f-s1-0":("approve/plain",None,None)})
add("claude-2-turn-11",{"f-s1-0":("report/context",None,None),"f-s1-1":("delegate/to-agent",None,None)})
add("claude-3-turn-15",{"f-s1-0":("ask-action/commission",None,None)})
add("claude-2-turn-13",{"f-s1-0":("disagree/correction",None,None),"f-s2-0":("propose/alternative",None,None)})
add("claude-17-turn-251",{"f-s1-0":("propose/redirect-scope",None,None),"f-s1-1":("clarify/context",None,None)})
add("claude-2-turn-2",{"f-s1-0":("report-problem/suspected",None,None),"f-s2-0":("propose/next-step",None,None)})

for rec,(tid,moves) in R.items():
    if rec is None: print("SKIP:",tid); continue
    simple(rec, {f"{tid}-{k}": v for k,v in moves.items()})
