import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-14-turn-54"
build("turn-627KUC", [
    A(f"{T}-f-s1-0", "report-problem", "report-problem/observed",
      "i think done meaning stopped was noted before"),
    A(f"{T}-f-s1-1", "ask-action", "ask-action/commission",
      "please look onto it",
      within=f"{T}-f-s1-0"),
    A(f"{T}-r-0", "reply-mark", "report/diagnosis",
      "(my 10-11 fix) You had flagged this before",
      answers=[(f"{T}-f-s1-0", "(my 10-11 fix) You had flagged this before", "keeps")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/evidence",
      "(how much it affected)",
      answers=[(f"{T}-f-s1-0", "(how much it affected)", "keeps")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/done",
      "(the fix)",
      answers=[(f"{T}-f-s1-1", "(the fix)", "closes")],
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "report/done",
      "(migration)",
      answers=[(f"{T}-f-s1-1", "(migration)", "closes")],
      conf="medium"),
    A(f"{T}-r-4", "reply-mark", "report/in-flight",
      "(the t93J03 round now running)",
      conf="medium"),
])

T = "claude-17-turn-467"
build("turn-76NiTi", [
    A(f"{T}-f-s1-0", "prioritize", "prioritize/benefit",
      "any places where there *isn't* good alignment"),
    A(f"{T}-f-s2-0", "report", "report/context",
      "There's a narrative from line 2979",
      within=f"{T}-f-s1-0"),
    A(f"{T}-r-0", "reply-mark", "report/evidence",
      "(where the narrative and the tree disagree)",
      answers=[(f"{T}-f-s1-0", "(where the narrative and the tree disagree)", "closes")],
      within=f"{T}-f-s1-0"),
    A(f"{T}-r-1", "reply-mark", "report/evidence",
      "(the six misaligned nodes)",
      answers=[(f"{T}-f-s1-0", "(the six misaligned nodes)", "closes")],
      within=f"{T}-f-s1-0",
      conf="low",
      note="heading-only paragraph over a table"),
    A(f"{T}-r-2", "reply-mark", "qualify/caveat",
      "(caveats) The run was defective, as you say.",
      answers=[(f"{T}-f-s2-0", "(caveats) The run was defective, as you say.", "closes")],
      within=f"{T}-f-s1-0"),
    A(f"{T}-r-3", "reply-mark", "propose/next-step",
      "(next step) I'd add a third cue set",
      answers=[(f"{T}-f-s1-0", "(next step)", "keeps")],
      within=f"{T}-f-s1-0"),
])
