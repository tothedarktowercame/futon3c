import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-17-turn-415"
build("turn-GC5j2q", [
    A(f"{T}-f-s1-0", "collect", "ask-action/status-check",
      "How much more backfill is missing?"),
    A(f"{T}-r-0", "reply-mark", "report/evidence",
      "Gist: For 22 August to today, 21 turns are left",
      answers=[(f"{T}-f-s1-0", "For 22 August to today, 21 turns are left", "closes")]),
    A(f"{T}-r-1", "reply-mark", "report/evidence",
      "(target window, from `xiang_coverage.py` just now)",
      answers=[(f"{T}-f-s1-0", "(target window", "keeps")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/evidence",
      "(older periods)",
      answers=[(f"{T}-f-s1-0", "(older periods)", "keeps")],
      conf="low",
      note="heading-only paragraph"),
    A(f"{T}-r-3", "reply-mark", "report/evidence",
      "(pace) Since the restart at 20:34",
      answers=[(f"{T}-f-s1-0", "(pace)", "keeps")],
      conf="medium"),
])

T = "claude-17-turn-468"
build("turn-GKJ9v7", [
    A(f"{T}-f-s1-0", "approve", "approve/plain", "㊭:yes"),
    A(f"{T}-r-0", "reply-mark", "report/evidence",
      "(what the new cues catch in your 1,237 live turns)",
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "qualify/caveat",
      "(what limits the test)",
      conf="medium"),
])
