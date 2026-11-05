import sys
sys.path.insert(0, ".")
from sbuild import build, A

T = "claude-17-turn-479"
build("turn-hFxHzc", [
    A(f"{T}-f-s1-0", "approve", "approve/plain",
      "this is super interesting because it puts 象 in the position of being a tiny AIF agent itself,"),
    A(f"{T}-f-s1-1", "qualify", "qualify/caveat",
      "we don't need the same degree of processing that the War Machine has;"),
    A(f"{T}-f-s1-2", "propose", "propose/directive",
      "it should *itself* have policies for tagging texts etc."),
    A(f"{T}-r-0", "reply-mark", "approve/plain",
      "(象 as a small AIF agent) Yes.",
      answers=[(f"{T}-f-s1-0", "(象 as a small AIF agent) Yes.", "closes")]),
    A(f"{T}-r-1", "reply-mark", "approve/with-amendment",
      "(keeping it small) This needs no Lean, cascades or War Machine machinery.",
      answers=[(f"{T}-f-s1-1", "(keeping it small) This needs no Lean", "closes")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "propose/next-step",
      "(order of work)",
      answers=[(f"{T}-f-s1-2", "(order of work)", "keeps")],
      conf="medium"),
])

T = "claude-17-turn-41@turn-hTMlRg"
build("turn-hTMlRg", [
    A(f"{T}-f-s1-0", "ask-action", "ask-action/commission",
      "Taking your briefing for codex-18 as the basis of an excursion"),
    A(f"{T}-f-s1-1", "constrain", "constrain/standing-rule",
      "we can develop a robust and *repeatable* analysis strategy"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "(where it is) The file is `futon3c/holes/excursions/E-象-evaluation-strategy.md`",
      answers=[(f"{T}-f-s1-0", "(where it is) The file is", "closes")]),
    A(f"{T}-r-1", "reply-mark", "report/design",
      "(how it uses 象's logic)",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/design",
      "(making it repeatable) The fixed parts are the split into acts",
      answers=[(f"{T}-f-s1-1", "(making it repeatable)", "closes")],
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "report/with-options",
      "(things the first run must test)",
      conf="medium"),
])

T = "claude-17-turn-536"
build("turn-jlorAL", [
    A(f"{T}-f-s1-0", "qualify", "qualify/caveat",
      "This is more of an Excursion than a missions"),
    A(f"{T}-f-s2-0", "ask-action", "ask-action/commission",
      "Let's make it E-agency-work-orders.md as an excursion attached to M-象-2000"),
    A(f"{T}-r-0", "reply-mark", "report/design",
      "(contents) The excursion has:",
      answers=[(f"{T}-f-s2-0", "(contents) The excursion has:", "closes")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/gap",
      "(the codex-19 case) I couldn't find it yet.",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/in-flight",
      "(step 1) kimi-1 is still working on step 1",
      conf="medium"),
])
