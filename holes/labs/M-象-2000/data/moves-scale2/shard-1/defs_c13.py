import sys
sys.path.insert(0, ".")
from sbuild import build, A

T = "claude-17-turn-498"
build("turn-STbLRn", [
    A(f"{T}-f-s1-0", "report-problem", "clarify/ask-decision",
      "I'm not sure what the refusals are saying."),
    A(f"{T}-f-s2-0", "disagree", "disagree/correction",
      'You claimed "my 🈸 posted no offer"'),
    A(f"{T}-f-s3-0", "explain", "explain/counter",
      "🈸:yes does have an obvious interpretation"),
    A(f"{T}-r-0", "reply-mark", "explain/mechanism",
      "(what the refusal says) The refusal reports a gap in the record",
      answers=[(f"{T}-f-s1-0", "(what the refusal says) The refusal reports a gap in the record", "closes")]),
    A(f"{T}-r-1", "reply-mark", "explain/mechanism",
      "(why it's built that way) Offers were designed as explicit records",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "propose/next-step",
      "(fix on the reading side) When no stored offer is visible",
      answers=[(f"{T}-f-s2-0", "(fix on the reading side)", "closes")],
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "ask-action/one-decision",
      "(one decision) Shall I make that change",
      conf="medium"),
])

T = "claude-17-turn-541"
build("turn-SuuilW", [
    A(f"{T}-f-s1-0", "verify", "verify/test-turn",
      "This is a test turn, I have *象 stepper* open"),
    A(f"{T}-r-0", "reply-mark", "report/diagnosis",
      "(your test turn) This turn is testing the old file-based path",
      answers=[(f"{T}-f-s1-0", "(your test turn) This turn is testing the old file-based path", "closes")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/done",
      "(what I did) I loaded the current `session-turn-analysis.el`",
      answers=[(f"{T}-f-s1-0", "(what I did)", "keeps")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "propose/next-step",
      "(watching the JVM test) For the turn after you switch",
      answers=[(f"{T}-f-s1-0", "(watching the JVM test)", "keeps")],
      conf="medium"),
])

T = "claude-17-turn-408"
build("turn-TQBr0s", [
    A(f"{T}-f-s1-0", "explore", "explore/open-question", "Hm..."),
    A(f"{T}-f-s2-0", "propose", "propose/tentative",
      "If there is too much evidence in the Evidence Landscape, I'm not sure what to do"),
    A(f"{T}-f-s3-0", "redirect", "propose/redirect-scope",
      "If the \"coordination traffic\" is actually automated material"),
    A(f"{T}-f-s4-0", "disagree", "disagree/correction",
      "I also find it somewhat implausible that I personally sent around 200 turns"),
    A(f"{T}-r-0", "reply-mark", "retract/correction",
      "(my earlier count) I made the 843 sound like your turns, and they weren't.",
      answers=[(f"{T}-f-s4-0", "(my earlier count) I made the 843 sound like your turns", "closes")]),
    A(f"{T}-r-1", "reply-mark", "report/status",
      "(the pipes) In this sample the labelling already does what you asked",
      answers=[(f"{T}-f-s3-0", "(the pipes) In this sample the labelling already does what you asked", "closes")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/design",
      "(the Minard method) It's in `futon0/analysis/audits/`",
      answers=[(f"{T}-f-s2-0", "(the Minard method)", "keeps")],
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "ask-action/one-decision",
      "(one decision) Shall I send the 21 September to today extension",
      answers=[(f"{T}-f-s2-0", "(one decision)", "keeps")],
      conf="medium"),
])
