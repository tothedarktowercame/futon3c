import sys
sys.path.insert(0, ".")
from defs_f1 import simple

T="claude-1-turn-193"; simple("turn-GbsFoA", {
 f"{T}-f-s1-0": ("disagree/correction",None,None),
 f"{T}-f-s1-1": ("propose/redirect-scope",None,None)})
T="codex-10-turn-10@turn-Gs8LFo"; simple("turn-Gs8LFo", {f"{T}-f-s1-0": ("collect/ask-option",None,None)})
T="claude-17-turn-272"; simple("turn-IMDlDO", {
 f"{T}-f-s1-0": ("verify/confirmed",None,None),
 f"{T}-f-s1-1": ("approve/plain",None,None),
 f"{T}-f-s2-0": ("report-problem/observed",None,None)})
T="claude-17-turn-350"; simple("turn-IPhcHY", {
 f"{T}-f-s1-0": ("report/context",None,None),
 f"{T}-f-s2-0": ("qualify/caveat",None,f"{T}-f-s1-0"),
 f"{T}-f-s3-0": ("report/status-update",None,f"{T}-f-s1-0"),
 f"{T}-f-s4-0": ("report-problem/suspected",None,f"{T}-f-s1-0"),
 f"{T}-f-s4-1": ("propose/tentative",None,f"{T}-f-s4-0")})
T="claude-17-turn-355"; simple("turn-Izl0Vd", {
 f"{T}-f-s1-0": ("propose/tentative",None,None),
 f"{T}-f-s2-0": ("clarify/assertion",None,None),
 f"{T}-f-s3-0": ("propose/extend",None,f"{T}-f-s1-0")})
T="claude-14-turn-24"; simple("turn-J8jbAD", {f"{T}-f-s1-0": ("approve/plain",None,None)})
T="claude-1-turn-136"; simple("turn-JA6VCn", {
 f"{T}-f-s1-0": ("disagree/correction",None,None),
 f"{T}-f-s2-0": ("propose/directive","make the work progressive",None)})
