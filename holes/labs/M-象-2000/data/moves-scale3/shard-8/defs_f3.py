import sys
sys.path.insert(0, ".")
from defs_f1 import simple

T="claude-17-turn-310"
simple("turn-4cK2SO", {f"{T}-f-s1-0": ("approve/plain",None,None), f"{T}-f-s2-0": ("delegate/to-agent",None,None)})
T="claude-14-turn-53"
simple("turn-4lBxvU", {f"{T}-f-s1-0": ("approve/plain",None,None)})
T="claude-17-turn-334"
simple("turn-5XGjvV", {f"{T}-f-s1-0": ("delegate/to-agent",None,None)})
T="claude-1-turn-181"
simple("turn-5emFRr", {
 f"{T}-f-s1-0": ("delegate/to-agent","formal requirements in Lean",None),
 f"{T}-f-s1-1": ("explain/justification",None,f"{T}-f-s1-0"),
})
T="claude-17-turn-285"
simple("turn-6VOQjn", {
 f"{T}-f-s1-0": ("clarify/condition",None,None),
 f"{T}-f-s1-1": ("propose/directive",None,f"{T}-f-s1-0"),
})
T="claude-17-turn-342"
simple("turn-7qJycS", {
 f"{T}-f-s1-0": ("qualify/caveat",None,None),
 f"{T}-f-s2-0": ("disagree/correction",None,None),
 f"{T}-f-s3-0": ("clarify/ask-decision",None,f"{T}-f-s2-0"),
 f"{T}-f-s4-0": ("propose/alternative","claude-1 session case study",None),
 f"{T}-f-s5-0": ("clarify/context",None,f"{T}-f-s4-0"),
 f"{T}-f-s6-0": ("report-problem/observed",None,f"{T}-f-s4-0"),
 f"{T}-f-s7-0": ("propose/redirect-scope",None,f"{T}-f-s2-0"),
 f"{T}-f-s8-0": ("qualify/caveat",None,f"{T}-f-s6-0"),
})
T="claude-17-turn-313"
simple("turn-7uXKF3", {
 f"{T}-f-s1-0": ("report/status-update",None,None),
 f"{T}-f-s1-1": ("propose/next-step",None,None),
})
