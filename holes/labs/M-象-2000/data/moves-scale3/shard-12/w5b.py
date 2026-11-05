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

add("claude-17-turn-228",{"f-s1-0":("disagree/correction",None,None),"f-s1-1":("explain/interpretation",None,None)})
add("claude-1-turn-35",{"f-s1-0":("prioritize/benefit",None,None)})
add("claude-2-turn-5",{"f-s1-0":("report/status-update",None,None),"f-s1-1":("ask-action/ask-decision",None,None)})
add("claude-17-turn-167",{"f-s1-0":("ask-action/commission","open M-象-cascade",None),"f-s2-0":("explain/justification",None,"claude-17-turn-167-f-s1-0")})
add("claude-17-turn-245",{"f-s1-0":("approve/plain",None,None)})
add("claude-3-turn-13",{"f-s1-0":("clarify/analogy",None,None),"f-s1-1":("delegate/to-agent",None,None)})
add("claude-17-turn-164",{"f-s1-0":("report-problem/suspected",None,None),"f-s2-0":("collect/ask-option",None,None),"f-s3-0":("report/context",None,None)})
add("claude-3-turn-4",{"f-s1-0":("clarify/assertion",None,None),"f-s2-0":("delegate/interview",None,None),"f-s3-0":("clarify/context",None,None),"f-s4-0":("clarify/context",None,None),"f-s5-0":("collect/ask-option",None,None),"f-s6-0":("report/context",None,None),"f-s7-0":("report/context",None,None)})
add("claude-3-turn-14",{"f-s1-0":("clarify/context",None,None),"f-s2-0":("qualify/caveat",None,None)})
add("claude-3-turn-9",{"f-s1-0":("disagree/correction",None,None),"f-s2-0":("report/context",None,None)})
add("claude-3-turn-21",{"f-s1-0":("propose/directive","password check as first classical tool",None)})
add("claude-17-turn-246",{"f-s1-0":("approve/plain",None,None)})
add("claude-17-turn-247",{"f-s1-0":("approve/plain",None,None),"f-s2-0":("report/status-update",None,None),"f-s3-0":("propose/tentative",None,None)})
add("claude-1-turn-30",{"f-s1-0":("approve/plain",None,None),"f-s2-0":("explain/interpretation",None,None),"f-s3-0":("qualify/caveat",None,None),"f-s4-0":("explain/context",None,None),"f-s5-0":("report/context",None,None),"f-s6-0":("propose/directive","scope the BMR alternatives",None),"f-s7-0":("report-problem/suspected",None,"claude-1-turn-30-f-s6-0")})
add("claude-17-turn-217",{"f-s2-0":("qualify/caveat",None,None),"f-s2-1":("defer/until",None,None)})
add("claude-2-turn-8",{"f-s1-0":("explain/interpretation",None,None),"f-s2-0":("report/context",None,None),"f-s2-1":("propose/directive",None,None),"f-s2-2":("constrain/for-now",None,None)})
add("claude-2-turn-10",{"f-s1-0":("ask-action/commission",None,None)})
add("claude-2-turn-7",{"f-s1-0":("report/status-update",None,None)})
add("claude-17-turn-243",{"f-s1-0":("approve/plain",None,None),"f-s1-1":("clarify/assertion",None,None),"f-s2-0":("prioritize/benefit","demo first, held items later",None)})
add("claude-3-turn-10",{"f-s1-0":("qualify/caveat",None,None),"f-s2-0":("clarify/ask-decision",None,None),"f-s3-0":("report/context",None,None),"f-s4-0":("propose/extend",None,None),"f-s4-1":("propose/tentative",None,None)})
add("claude-1-turn-75",{"f-s1-0":("ask-action/commission",None,None)})
add("claude-17-turn-166",{"f-s3-0":("qualify/caveat",None,None),"f-s4-0":("propose/tentative",None,None)})
add("claude-17-turn-226",{"f-s1-0":("withdraw/correction",None,None),"f-s1-1":("ask-action/commission",None,None)})
add("claude-1-turn-56",{"f-s1-0":("delegate/to-agent",None,None)})
add("claude-1-turn-33",{"f-s1-0":("propose/directive","build (c) then (b)",None),"f-s2-0":("explain/analogy",None,None),"f-s3-0":("clarify/distinction",None,None)})
add("claude-17-turn-187",{"f-s1-0":("approve/plain",None,None),"f-s1-1":("constrain/stand-rule",None,None)})
add("claude-3-turn-11",{"f-s1-0":("clarify/context",None,None),"f-s2-0":("propose/redirect-scope","code, no software",None),"f-s3-0":("collect/idea",None,None),"f-s4-0":("clarify/assertion",None,None),"f-s5-0":("explain/context",None,None),"f-s6-0":("propose/tentative",None,None),"f-s7-0":("verify/assertion",None,None)})
add("claude-17-turn-250",{"f-s1-0":("propose/extend",None,None),"f-s2-0":("propose/tentative",None,None),"f-s3-0":("qualify/caveat",None,None)})
add("claude-17-turn-244",{"f-s1-0":("disagree/correction",None,None),"f-s2-0":("explain/context",None,None),"f-s3-0":("clarify/analogy",None,None)})
add("claude-17-turn-173",{"f-s1-0":("propose/tentative",None,None),"f-s3-0":("propose/extend",None,None)})
add("claude-3-turn-8",{"f-s1-0":("qualify/caveat",None,None),"f-s2-0":("propose/redirect-scope","code, no software",None),"f-s3-0":("qualify/caveat",None,None),"f-s4-0":("clarify/assertion",None,None),"f-s5-0":("clarify/assertion",None,None),"f-s6-0":("report/context",None,None),"f-s7-0":("report/context",None,None),"f-s8-0":("propose/extend",None,None),"f-s9-0":("clarify/assertion",None,"claude-3-turn-8-f-s2-0")})
add("claude-3-turn-12",{"f-s1-0":("delegate/to-agent",None,None)})

for rec,(tid,moves) in R.items():
    if rec is None: print("SKIP:",tid); continue
    simple(rec, {f"{tid}-{k}": v for k,v in moves.items()})
