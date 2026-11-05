import sys
sys.path.insert(0, ".")
from defs_f1 import simple

T="claude-1-turn-149"; simple("turn-2oR20D", {f"{T}-f-s1-0": ("clarify/ask-decision",None,None)})
T="claude-1-turn-98"; simple("turn-DN3nq3", {
 f"{T}-f-s1-0": ("disagree/correction",None,None),
 f"{T}-f-s2-0": ("report-problem/observed",None,None),
 f"{T}-f-s3-0": ("delegate/to-agent",None,f"{T}-f-s2-0")})
T="claude-1-turn-109"; simple("turn-Dg128r", {
 f"{T}-f-s1-0": ("collect/idea",None,None),
 f"{T}-f-s2-0": ("qualify/caveat",None,None),
 f"{T}-f-s2-1": ("propose/tentative",None,f"{T}-f-s2-0")})
T="claude-1-turn-96"; simple("turn-G0DwY4", {
 f"{T}-f-s1-0": ("approve/plain",None,None),
 f"{T}-f-s2-0": ("propose/directive","standing curiosity",None),
 f"{T}-f-s3-0": ("collect/idea",None,None),
 f"{T}-f-s4-0": ("propose/extend",None,f"{T}-f-s2-0")})
T="claude-17-turn-326"; simple("turn-JGONrX", {f"{T}-f-s1-0": ("ask-action/status-check",None,None)})
T="claude-17-turn-324"; simple("turn-JnVEsw", {
 f"{T}-f-s1-0": ("qualify/caveat",None,None),
 f"{T}-f-s1-1": ("propose/directive","coalesce names into core set",None),
 f"{T}-f-s1-2": ("propose/extend",None,f"{T}-f-s1-1")})
T="claude-1-turn-233"; simple("turn-LoYBR8", {f"{T}-f-s1-0": ("report-problem/observed",None,None)})
