import sys
sys.path.insert(0, ".")
from defs_f1 import simple

T="claude-17-turn-277"; simple("turn-LXuYgW", {f"{T}-f-s1-0": ("report-problem/observed",None,None)})

T="claude-17-turn-344"; simple("turn-MBEjk1", {
 f"{T}-f-s1-0": ("clarify/assertion",None,None),
 f"{T}-f-s2-0": ("explain/interpretation",None,f"{T}-f-s1-0"),
 f"{T}-f-s3-0": ("explain/interpretation",None,None),
 f"{T}-f-s4-0": ("clarify/check-understanding",None,f"{T}-f-s3-0"),
 f"{T}-f-s5-0": ("clarify/distinction",None,f"{T}-f-s3-0")})
