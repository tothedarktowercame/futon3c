import sys
sys.path.insert(0, ".")
from sbuild import build, A

T = "claude-17-turn-484"
build("turn-reya1l", [
    A(f"{T}-f-s1-0", "approve", "approve/plain", "OK, got it."),
    A(f"{T}-f-s2-0", "report", "report/context",
      "It's kind of fun to discover the red words!"),
    A(f"{T}-f-s3-0", "report", "report/uncertainty",
      "I don't know enough about them to decide which are good ones"),
    A(f"{T}-f-s4-0", "continue", "defer/until",
      "We can keep going for a while under these circumstances and then audit it later."),
    A(f"{T}-f-s5-0", "ask-action", "ask-action/one-decision",
      "Shall we look at M-aif-over-the-operator"),
    A(f"{T}-f-s6-0", "qualify", "qualify/caveat",
      "any overlap with what's in the mission will be pretty incidental."),
    A(f"{T}-r-0", "reply-mark", "report/evidence",
      "(where the overlap is)",
      answers=[(f"{T}-f-s5-0", "(where the overlap is)", "keeps")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report-problem/observed",
      "(the decision log) Eight of the R-node/象 tests",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "propose/next-step",
      '(writing it into the mission) I suggest adding a short "Side C',
      answers=[(f"{T}-f-s5-0", "(writing it into the mission)", "keeps")],
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "ask-action/one-decision",
      "(one decision) Shall I add that section?",
      conf="medium"),
])

T = "claude-17-turn-411"
build("turn-tyrtVs", [
    A(f"{T}-f-s1-0", "approve", "approve/plain", "yes"),
    A(f"{T}-f-s1-1", "delegate", "delegate/to-agent",
      "(a Codex or Zai job)"),
    A(f"{T}-r-0", "reply-mark", "report/dispatched",
      "(the job) You can poll it yourself:",
      answers=[(f"{T}-f-s1-1", "(the job) You can poll it yourself:", "closes")]),
    A(f"{T}-r-1", "reply-mark", "report/in-flight",
      "(backfill) The backfill is running. Coverage is now 4,856 of",
      answers=[(f"{T}-f-s1-0", "(backfill) The backfill is running.", "keeps")],
      conf="medium"),
])

T = "claude-17-turn-2@turn-vX22Ib"
build("turn-vX22Ib", [
    A(f"{T}-f-s1-0", "report-problem", "report-problem/observed",
      "M-x turn-stepper isn't available"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "(M-x turn-stepper) `M-x turn-stepper` works again.",
      answers=[(f"{T}-f-s1-0", "(M-x turn-stepper) `M-x turn-stepper` works again.", "closes")]),
    A(f"{T}-r-1", "reply-mark", "report/done",
      "(what changed) Two lines:",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/gap",
      "(other files not loaded) `futon-config.el` doesn't load two other 象 files",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "report/gap",
      "(still open) After the restart the recorder is back to `files`",
      conf="medium"),
])
