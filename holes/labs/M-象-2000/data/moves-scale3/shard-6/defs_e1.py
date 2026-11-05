import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-17-turn-38"
build("turn-04zPYc", [
    A(f"{T}-f-s1-0", "report", "report/status-update",
      "The deep research results are back in E-the-dark-tower-3.md"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "(the corrections) Applied in futon3c `aeaa47d3`",
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/evidence",
      "(what the report changes in the strategy) Three findings matter",
      answers=[(f"{T}-f-s1-0", "(what the report changes in the strategy)", "keeps")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "propose/next-step",
      "(order of work)",
      answers=[(f"{T}-f-s1-0", "(order of work)", "keeps")],
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "report/evidence",
      "(Answer to §1) The report's headline answer",
      answers=[(f"{T}-f-s1-0", "(Answer to §1) The report's headline answer", "closes")],
      conf="medium"),
])

T = "claude-14-turn-63"
build("turn-1g6VRa", [
    A(f"{T}-f-s1-0", "redirect", "propose/redirect-scope",
      "going back to the questions about speed, the issue is not how they are written in textbooks",
      opens="efficiency-first formalisation",
      note="reframes the whole program around cheapest-route formalisation"),
    A(f"{T}-r-0", "reply-mark", "approve/plain",
      '("how they would be efficiently formalised in lean") Agreed',
      answers=[(f"{T}-f-s1-0", '("how they would be efficiently formalised in lean") Agreed', "closes")],
      within=f"{T}-f-s1-0"),
    A(f"{T}-r-1", "reply-mark", "report/evidence",
      "(what Mathlib already has)",
      answers=[(f"{T}-f-s1-0", "(what Mathlib already has)", "keeps")],
      within=f"{T}-f-s1-0"),
    A(f"{T}-r-2", "reply-mark", "report/diagnosis",
      "(the covering-subgroup program I just started)",
      answers=[(f"{T}-f-s1-0", "(the covering-subgroup program I just started)", "keeps")],
      within=f"{T}-f-s1-0"),
    A(f"{T}-r-3", "reply-mark", "report/with-options",
      "(the other two cases)",
      answers=[(f"{T}-f-s1-0", "(the other two cases)", "keeps")],
      within=f"{T}-f-s1-0"),
    A(f"{T}-r-4", "reply-mark", "propose/next-step",
      "(redesign before more quota goes in) Pause the two new programs",
      answers=[(f"{T}-f-s1-0", "(redesign before more quota goes in)", "keeps")],
      within=f"{T}-f-s1-0"),
    A(f"{T}-r-5", "reply-mark", "ask-action/one-decision",
      "(pause and redesign) Shall I pause the two programs",
      answers=[(f"{T}-f-s1-0", "(pause and redesign)", "keeps")],
      within=f"{T}-f-s1-0"),
])
