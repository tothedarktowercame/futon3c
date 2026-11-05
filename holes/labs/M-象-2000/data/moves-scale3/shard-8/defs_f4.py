import sys
sys.path.insert(0, ".")
from defs_f1 import simple

T="claude-17-turn-307"; simple("turn-876OU4", {f"{T}-f-s1-0": ("propose/tentative",None,None)})
T="claude-17-turn-296"; simple("turn-8YHKJm", {
 f"{T}-f-s1-0": ("clarify/context",None,None),
 f"{T}-f-s2-0": ("propose/tentative",None,None),
 f"{T}-f-s2-1": ("propose/next-step",None,f"{T}-f-s2-0"),
})
T="claude-1-turn-180"; simple("turn-9FVDwo", {f"{T}-f-s1-0": ("report-problem/observed",None,None)})
T="claude-14-turn-47"; simple("turn-9WIXyW", {
 f"{T}-f-s1-0": ("report-problem/observed",None,None),
 f"{T}-f-s2-0": ("constrain/for-now",None,f"{T}-f-s1-0"),
})
T="claude-17-turn-304"; simple("turn-9e36YJ", {
 f"{T}-f-s1-0": ("propose/directive",None,None),
 f"{T}-f-s2-0": ("ask-action/commission",None,None),
 f"{T}-f-s2-1": ("ask-action/commission",None,f"{T}-f-s2-0"),
})
T="codex-10-turn-5@turn-ANJJCk"; simple("turn-ANJJCk", {f"{T}-f-s1-0": ("ask-action/commission",None,None)})
T="claude-17-turn-33"; simple("turn-BAy58C", {
 f"{T}-f-s1-0": ("clarify/context",None,None),
 f"{T}-f-s2-0": ("clarify/assertion",None,None),
})
T="claude-17-turn-311"; simple("turn-BI6luN", {
 f"{T}-f-s1-0": ("propose/extend",None,None),
 f"{T}-f-s1-1": ("clarify/context",None,f"{T}-f-s1-0"),
})
