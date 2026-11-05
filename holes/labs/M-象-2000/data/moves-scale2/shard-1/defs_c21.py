import sys
sys.path.insert(0, ".")
from sbuild import build, A

T = "claude-17-turn-452"
build("turn-pZuUeq", [
    A(f"{T}-f-s1-0", "verify", "verify/provenance-question",
      "I don't know where the 52 @holds-at markers came from"),
    A(f"{T}-f-s2-0", "qualify", "qualify/caveat",
      "we'll have several different ways of forming associations now."),
    A(f"{T}-f-s3-0", "approve", "approve/plain", "You can ㊩:please rerun"),
    A(f"{T}-r-0", "reply-mark", "report/evidence",
      "(provenance of the 52 markers) Taking each file's first `@holds-at` commit",
      answers=[(f"{T}-f-s1-0", "(provenance of the 52 markers)", "closes")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/done",
      "(not saved yet) The regenerated graph is written over the old file",
      answers=[(f"{T}-f-s3-0", "(not saved yet)", "closes")],
      conf="medium"),
])

T = "claude-17-turn-475"
build("turn-qCKjmS", [
    A(f"{T}-f-s2-0", "disagree", "disagree/correction",
      "I question whether AIF terms can semantically overlap intent queues."),
    A(f"{T}-f-s3-0", "report-problem", "report-problem/observed",
      "the previous turn has been massively over-underlined"),
    A(f"{T}-f-s5-0", "report", "report/impact",
      "It is confusing."),
    A(f"{T}-r-0", "reply-mark", "ask-action/two-decisions",
      "(the purple fragments) Want me to change how they're shown? Two options:",
      answers=[(f"{T}-f-s3-0", "(the purple fragments) Want me to change how they're shown?", "keeps"),
               (f"{T}-f-s5-0", "(the purple fragments)", "keeps")],
      conf="medium"),
])

T = "claude-17-turn-44"
build("turn-qR3UaA", [
    A(f"{T}-f-s1-0", "verify", "verify/status-request",
      'are we in a position to show some "real" examples of the 象 family system working?'),
    A(f"{T}-r-0", "reply-mark", "report/partial",
      "Partly. One complete real example already works end to end",
      answers=[(f"{T}-f-s1-0", "Partly. One complete real example already works end to end", "closes")]),
    A(f"{T}-r-1", "reply-mark", "report/status",
      "(what is real today)",
      answers=[(f"{T}-f-s1-0", "(what is real today)", "keeps")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/gap",
      "(what is not yet shown on real data)",
      answers=[(f"{T}-f-s1-0", "(what is not yet shown on real data)", "keeps")],
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "ask-action/one-decision",
      "(one decision) Shall I check the reminder machine's log",
      answers=[(f"{T}-f-s1-0", "(one decision)", "keeps")],
      conf="medium"),
])
