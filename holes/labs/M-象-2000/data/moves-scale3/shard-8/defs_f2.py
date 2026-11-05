import sys
sys.path.insert(0, ".")
from sbuild3 import build, A
from defs_f1 import simple

T="claude-1-turn-107"
simple("turn-2tGOUJ", {
 f"{T}-f-s1-0": ("disagree/correction", None, None),
 f"{T}-f-s2-0": ("clarify/ask-decision", None, None),
})

T="claude-17-turn-309"
simple("turn-2z2EPI", {
 f"{T}-f-s1-0": ("clarify/context", None, None),
 f"{T}-f-s2-0": ("defer/until", None, None),
 f"{T}-f-s3-0": ("propose/directive", "review the McCarthy paper", None),
 f"{T}-f-s4-0": ("report-problem/observed", None, f"{T}-f-s3-0"),
 f"{T}-f-s5-0": ("ask-action/status-check", None, f"{T}-f-s3-0"),
 f"{T}-f-s6-0": ("qualify/caveat", None, f"{T}-f-s5-0"),
 f"{T}-f-s7-0": ("propose/directive", "interactional compliance spec", None),
 f"{T}-f-s7-1": ("delegate/to-agent", None, f"{T}-f-s7-0"),
 f"{T}-f-s8-0": ("prioritize/sequencing", "interactive features first", f"{T}-f-s7-0"),
})

T="codex-10-turn-3@turn-32eHlH"
simple("turn-32eHlH", {
 f"{T}-f-s1-0": ("delegate/to-agent", None, None),
})

T="claude-17-turn-314"
simple("turn-3BnHz3", {
 f"{T}-f-s1-0": ("report-problem/observed", None, None),
 f"{T}-f-s2-0": ("report-problem/observed", None, f"{T}-f-s1-0"),
 f"{T}-f-s2-1": ("constrain/stand-rule", "stepper per-REPL only", None),
 f"{T}-f-s2-2": ("clarify/assertion", None, f"{T}-f-s2-1"),
})

T="claude-16-turn-92"
build("turn-3mROV9", [
    A(f"{T}-f-s1-0", "propose", "collect/idea",
      "https://claude.ai/share/c917810a-c440-4e79-ad37-5420ee47ae7d may be of interest",
      conf="medium"),
    A(f"{T}-f-s1-1", "report-problem", "report-problem/observed",
      "https://zone.hyperreal.enterprises/wip/turns/feed.html is showing non-operator turns"),
    A(f"{T}-r-0", "reply-mark", "report/diagnosis",
      '("may be of interest if you can read it") The share page sits behind',
      answers=[(f"{T}-f-s1-0", 'The share page sits behind', "keeps")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/evidence",
      '("showing non-operator turns") Of the 120 entries',
      answers=[(f"{T}-f-s1-1", '("showing non-operator turns")', "closes")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "explain/mechanism",
      "(how the feed gets refreshed) Nothing schedules the generator",
      answers=[(f"{T}-f-s1-1", "(how the feed gets refreshed)", "keeps")],
      conf="medium"),
])

T="claude-17-turn-295"
simple("turn-4apM38", {
 f"{T}-f-s1-0": ("ask-action/commission", None, None),
 f"{T}-f-s1-1": ("propose/tentative", None, None),
 f"{T}-f-s2-0": ("report/status-update", None, None),
})
