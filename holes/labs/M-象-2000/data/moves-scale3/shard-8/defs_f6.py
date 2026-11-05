import sys
sys.path.insert(0, ".")
from sbuild3 import build, A
from defs_f1 import simple

T="claude-16-turn-80"
build("turn-Dinwic", [
    A(f"{T}-f-s1-0", "propose", "propose/directive",
      "we have a 5 hour window back we could continue with thd zai mining"),
    A(f"{T}-f-s2-0", "extend", "propose/extend",
      "it would also be useful to work more on tge frontend features",
      conf="medium"),
    A(f"{T}-r-0", "reply-mark", "report/design",
      '("zai mining of Futon related scopes") I read this as the scope relation',
      answers=[(f"{T}-f-s1-0", '("zai mining of Futon related scopes")', "closes")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/design",
      "(batch 3) Each input now carries `mission_id`",
      answers=[(f"{T}-f-s1-0", "(batch 3)", "closes")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/status",
      "(second wave) The monthly meter was 73% used",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "report/evidence",
      '("inspired by this paper") [KnowTeX]',
      answers=[(f"{T}-f-s2-0", '("inspired by this paper")', "keeps")],
      conf="medium"),
    A(f"{T}-r-4", "reply-mark", "report/in-flight",
      "(front-end packets) codex-57 builds",
      conf="medium"),
    A(f"{T}-r-5", "reply-mark", "report-problem/observed",
      "(one slip, fixed) My first batch-3 commit",
      conf="medium"),
])

T="claude-17-turn-263"; simple("turn-Dl9rjZ", {
 f"{T}-f-s1-0": ("collect/idea","good 象 demo",None),
 f"{T}-f-s2-0": ("report-problem/observed",None,None)})
T="claude-1-turn-151"; simple("turn-DonEjS", {
 f"{T}-f-s1-0": ("propose/alternative","forward direction only",None),
 f"{T}-f-s2-0": ("report-problem/observed",None,f"{T}-f-s1-0")})
T="claude-1-turn-159"; simple("turn-ET4KU4", {
 f"{T}-f-s1-0": ("propose/directive","codex team by component",None),
 f"{T}-f-s2-0": ("clarify/context",None,f"{T}-f-s1-0"),
 f"{T}-f-s3-0": ("propose/extend",None,f"{T}-f-s1-0"),
 f"{T}-f-s4-0": ("propose/extend",None,f"{T}-f-s1-0")})
T="claude-14-turn-57"; simple("turn-F9hR1X", {f"{T}-f-s1-0": ("report-problem/observed",None,None)})
T="claude-15-turn-44"; simple("turn-FDa8gc", {f"{T}-f-s1-0": ("clarify/ask-decision",None,None)})
T="claude-1-turn-170"; simple("turn-FHE2sG", {f"{T}-f-s1-0": ("report-problem/observed",None,None)})
T="claude-1-turn-157"; simple("turn-Fnjx7F", {
 f"{T}-f-s1-0": ("withdraw/correction",None,None),
 f"{T}-f-s2-0": ("clarify/assertion","failures as critical incidents",None),
 f"{T}-f-s3-0": ("clarify/assertion",None,f"{T}-f-s2-0"),
 f"{T}-f-s4-0": ("propose/redirect-scope","debugger, not thin path",None),
 f"{T}-f-s5-0": ("explain/justification",None,f"{T}-f-s4-0")})
