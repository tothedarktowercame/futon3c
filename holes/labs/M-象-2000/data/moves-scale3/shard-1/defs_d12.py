import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-18-turn-128"
build("turn-89O9ZV", [
    A(f"{T}-f-s1-0", "continue", "ask-action/commission",
      "Let's start the topology loop again"),
    A(f"{T}-f-s1-1", "constrain", "constrain/standing-rule",
      "with the same topology loop agents from before",
      within=f"{T}-f-s1-0"),
    A(f"{T}-f-s1-2", "constrain", "constrain/standing-rule",
      "picking up where they left off",
      within=f"{T}-f-s1-0"),
])

T = "claude-11-turn-25"
build("turn-8AU38e", [
    A(f"{T}-f-s1-0", "extend", "propose/next-step",
      "lets try it on atEnd"),
    A(f"{T}-r-0", "reply-mark", "report/design",
      "(how the lane works)",
      answers=[(f"{T}-f-s1-0", "(how the lane works)", "keeps")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/diagnosis",
      "(the likely outcome) The remaining difference",
      answers=[(f"{T}-f-s1-0", "(the likely outcome)", "keeps")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "explain/mechanism",
      "(mechanics) A cold run takes about 20 minutes",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "report/in-flight",
      "(also pending) The two isNone probes",
      conf="medium"),
])

T = "claude-11-turn-83"
build("turn-8N1Ph6", [
    A(f"{T}-f-s1-0", "prioritize", "propose/directive",
      "let's work on the parser-only mode first",
      opens="parser-only mode first"),
])
