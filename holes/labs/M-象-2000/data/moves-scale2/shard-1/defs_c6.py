import sys
sys.path.insert(0, ".")
from sbuild import build, A

T = "claude-17-turn-429"
build("turn-CoAnc5", [
    A(f"{T}-f-s1-0", "propose", "propose/tentative",
      'I think "figure" would be nice b/c we could basically repeat the same style'),
    A(f"{T}-r-0", "reply-mark", "report/dispatched",
      "Gist: zai-2, who built the current Minard page, is now building the family figure.",
      answers=[(f"{T}-f-s1-0", "zai-2, who built the current Minard page, is now building the family figure", "closes")],
      opens="family figure"),
    A(f"{T}-r-1", "reply-mark", "report/dispatched",
      "(job and park) Job `invoke-1790892198239-29892-7955b4be`",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/design",
      "(the design)",
      answers=[(f"{T}-f-s1-0", "(the design)", "keeps")],
      within="family figure",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "propose/acceptance-plan",
      "(what will make it trustworthy) One test checks",
      answers=[(f"{T}-f-s1-0", "(what will make it trustworthy)", "keeps")],
      within="family figure",
      conf="medium"),
])

T = "claude-17-turn-486"
build("turn-D1BUn9", [
    A(f"{T}-f-s1-0", "report-problem", "report-problem/observed",
      'I am seeing a highlighting collision at the end of *codex-repl:codex-10*'),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "(the rule now) An intent underline and a red term never share a character",
      answers=[(f"{T}-f-s1-0", "(the rule now) An intent underline and a red term never share a character", "closes")]),
    A(f"{T}-r-1", "reply-mark", "verify/checked-live",
      "(what I checked)",
      answers=[(f"{T}-f-s1-0", "(what I checked)", "closes")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/context",
      "(decision log) The previous fix",
      conf="medium"),
])

T = "claude-17-turn-387"
build("turn-DwSflT", [
    A(f"{T}-f-s1-0", "approve", "approve/plain", "Yes please"),
    A(f"{T}-f-s1-1", "clarify", "clarify/scope",
      "both to the fast-mode + the pilot offer"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "(fast mode, futon3c commit 38ec87aa)",
      answers=[(f"{T}-f-s1-0", "(fast mode, futon3c commit 38ec87aa)", "closes")],
      opens="fast mode and pilot offer"),
    A(f"{T}-r-1", "reply-mark", "qualify/caveat",
      "(confidence) 小象's probabilities are badly overconfident",
      answers=[(f"{T}-f-s1-0", "(confidence)", "keeps")],
      within="fast mode and pilot offer"),
    A(f"{T}-r-2", "reply-mark", "verify/checked-live",
      "(what I checked)",
      answers=[(f"{T}-f-s1-0", "(what I checked)", "keeps")],
      within="fast mode and pilot offer",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "report/done",
      "(pilot offer, `futon7/holes/E-xiaoxiang-pilot-offer.md`, futon7 commit b7357fa)",
      answers=[(f"{T}-f-s1-1", "(pilot offer, `futon7/holes/E-xiaoxiang-pilot-offer.md`", "closes")],
      within="fast mode and pilot offer",
      conf="medium"),
    A(f"{T}-r-4", "reply-mark", "report/gap",
      "(gaps the draft states)",
      answers=[(f"{T}-f-s1-1", "(gaps the draft states)", "keeps")],
      within="fast mode and pilot offer",
      conf="medium"),
])
