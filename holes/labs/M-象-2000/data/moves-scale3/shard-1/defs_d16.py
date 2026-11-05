import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-12-turn-88"
build("turn-AoeVH4", [
    A(f"{T}-f-s2-0", "constrain", "constrain/standing-rule",
      "I would rather tune parameters based on experience rather than fiat"),
    A(f"{T}-f-s2-1", "report-problem", "report-problem/observed",
      "right now we don't even have a runnable or valid machine.",
      within=f"{T}-f-s2-0"),
    A(f"{T}-f-s4-0", "approve", "approve/plain", "ok."),
    A(f"{T}-f-s6-0", "approve", "approve/plain", "ok."),
    A(f"{T}-f-s7-0", "report-problem", "report-problem/observed",
      "4 sure, but tge way you ran it before was intransparent."),
    A(f"{T}-f-s8-0", "constrain", "constrain/standing-rule",
      "you need to use thd drbugger and watch it not just chill while it runs",
      opens="debugger-watching required",
      within=f"{T}-f-s7-0"),
])

T = "claude-12-turn-149"
build("turn-AwjMCR", [
    A(f"{T}-f-s1-0", "qualify", "clarify/context",
      'Your memories might have been compacted but "replay offline at production scale" ...'),
    A(f"{T}-f-s2-0", "challenge", "challenge/other",
      "what does that even mean?",
      within=f"{T}-f-s1-0"),
    A(f"{T}-f-s3-0", "disagree", "disagree/correction",
      "It is a complete fabrication, not a compression of anything I requested.",
      within=f"{T}-f-s1-0"),
])

T = "claude-11-turn-98"
build("turn-BWV759", [
    A(f"{T}-f-s1-0", "report-problem", "report-problem/retrospective",
      "my main comment is that if we look back through work on mfuton",
      opens="mfuton false-start retrospective",
      note="opens a look-back over the wasted mfuton work"),
    A(f"{T}-f-s1-1", "qualify", "qualify/caveat",
      'it may have been "needed" in a way to come up with the discovery',
      within=f"{T}-f-s1-0"),
    A(f"{T}-f-s2-0", "explain", "clarify/context",
      'That era started with a map of "causality"',
      within=f"{T}-f-s1-0"),
    A(f"{T}-f-s3-0", "ask-action", "ask-action/commission",
      "Can we find that work and think about how that got us unto a false start?",
      within=f"{T}-f-s1-0"),
])
