import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-14-turn-4"
build("turn-AM8Lbn", [
    A(f"{T}-f-s1-0", "explain", "ask-action/status-check",
      "why were they stopping"),
    A(f"{T}-r-0", "reply-mark", "retract/correction",
      "(my earlier explanation) I said the authors were stopping on an unwritten rule",
      answers=[(f"{T}-f-s1-0", "(my earlier explanation)", "keeps")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "explain/mechanism",
      "(why rows come out that size)",
      answers=[(f"{T}-f-s1-0", "(why rows come out that size)", "closes")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/status",
      "(since the reopen)",
      answers=[(f"{T}-f-s1-0", "(since the reopen)", "keeps")],
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "propose/design",
      "(what would help) A STOP that names a missing prerequisite",
      conf="medium"),
])

T = "claude-15-turn-50"
build("turn-Aw7XPl", [
    A(f"{T}-f-s1-0", "approve", "approve/plain",
      "happy to go with your direction on this one"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "(what happened, live) I acted on its first proposal",
      answers=[(f"{T}-f-s1-0", "(what happened, live)", "closes")]),
    A(f"{T}-r-1", "reply-mark", "qualify/caveat",
      "(how far to trust it) The model's verdict is advice, not fact",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "qualify/scope",
      "(scope you should know about) The ingest hook is global",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "propose/next-step",
      "(next) The row of the table still missing is **answers**",
      conf="medium"),
])
