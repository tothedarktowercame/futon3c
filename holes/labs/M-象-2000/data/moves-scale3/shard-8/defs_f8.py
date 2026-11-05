import sys
sys.path.insert(0, ".")
from defs_f1 import simple

T="claude-17-turn-300"; simple("turn-JrJL97", {
 f"{T}-f-s1-0": ("clarify/assertion",None,None),
 f"{T}-f-s2-0": ("defer/until",None,None),
 f"{T}-f-s3-0": ("propose/redirect-scope",None,None)})
T="claude-1-turn-198"; simple("turn-KdEYHP", {
 f"{T}-f-s1-0": ("clarify/ask-decision",None,None),
 f"{T}-f-s1-1": ("ask-action/status-check",None,None)})
T="claude-14-turn-50"; simple("turn-Kh7zgD", {f"{T}-f-s1-0": ("ask-action/status-check",None,None)})
T="claude-1-turn-150"; simple("turn-KlXnrB", {
 f"{T}-f-s1-0": ("prioritize/benefit","run end to end, no side quests",None),
 f"{T}-f-s2-0": ("constrain/for-now",None,f"{T}-f-s1-0"),
 f"{T}-f-s2-1": ("disagree/correction",None,f"{T}-f-s1-0"),
 f"{T}-f-s3-0": ("report-problem/observed",None,f"{T}-f-s1-0")})
T="claude-17-turn-265"; simple("turn-KuONgH", {
 f"{T}-f-s1-0": ("approve/plain",None,None),
 f"{T}-f-s1-1": ("propose/extend",None,f"{T}-f-s1-0"),
 f"{T}-f-s2-0": ("clarify/assertion",None,f"{T}-f-s1-1"),
 f"{T}-f-s3-0": ("propose/directive",None,None),
 f"{T}-f-s3-1": ("report-problem/suspected",None,f"{T}-f-s3-0"),
 f"{T}-f-s4-0": ("propose/directive","coalesce duplicate intent labels",None),
 f"{T}-f-s5-0": ("propose/extend",None,f"{T}-f-s4-0"),
 f"{T}-f-s6-0": ("propose/directive",None,None)})
