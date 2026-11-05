import sys
sys.path.insert(0, ".")
from defs_f1 import simple

T="claude-17-turn-339"; simple("turn-BKhLfg", {
 f"{T}-f-s1-0": ("clarify/context",None,None),
 f"{T}-f-s2-0": ("collect/ask-option",None,None)})
T="claude-1-turn-167"; simple("turn-BNEg24", {
 f"{T}-f-s1-0": ("clarify/assertion",None,None),
 f"{T}-f-s2-0": ("report-problem/observed",None,f"{T}-f-s1-0")})
T="claude-17-turn-289"; simple("turn-BVIor6", {
 f"{T}-f-s1-0": ("report-problem/observed",None,None),
 f"{T}-f-s2-0": ("report-problem/observed",None,f"{T}-f-s1-0")})
T="claude-1-turn-97"; simple("turn-BbSpLU", {
 f"{T}-f-s1-0": ("propose/directive","supersede with PROOF-2b",None),
 f"{T}-f-s2-0": ("clarify/context",None,f"{T}-f-s1-0"),
 f"{T}-f-s2-1": ("explain/justification",None,f"{T}-f-s1-0")})
T="claude-17-turn-290"; simple("turn-BbVD3M", {
 f"{T}-f-s1-0": ("report-problem/observed",None,None),
 f"{T}-f-s2-0": ("report-problem/observed",None,f"{T}-f-s1-0")})
T="claude-17-turn-305"; simple("turn-BcyWaL", {f"{T}-f-s1-0": ("ask-action/commission",None,None)})
T="codex-10-turn-6@turn-BoYXrQ"; simple("turn-BoYXrQ", {
 f"{T}-f-s1-0": ("prioritize/benefit",None,None),
 f"{T}-f-s2-0": ("approve/plain",None,None),
 f"{T}-f-s3-0": ("clarify/ask-decision",None,None),
 f"{T}-f-s4-0": ("clarify/assertion",None,f"{T}-f-s3-0"),
 f"{T}-f-s5-0": ("clarify/context",None,None),
 f"{T}-f-s6-0": ("delegate/to-agent","evaluation strategy for the click",None),
 f"{T}-f-s7-0": ("constrain/for-now",None,f"{T}-f-s6-0")})
