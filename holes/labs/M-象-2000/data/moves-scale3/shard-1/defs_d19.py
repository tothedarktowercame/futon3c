import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-12-turn-144"
build("turn-DryWNa", [
    A(f"{T}-f-s1-0", "report-problem", "report-problem/observed",
      "You've created OOMs, you've fabricated an inner loop"),
    A(f"{T}-f-s1-1", "challenge", "challenge/other",
      "you've rerun the machine under conditions that are known to fail",
      note="cites the standing debugger-watching rule from turn 88, which this record's open_ports no longer lists"),
    A(f"{T}-f-s2-0", "challenge", "challenge/other",
      "I don't know if you've paid any attention to Lean."),
])

T = "claude-18-turn-105"
build("turn-DtnrMF", [
    A(f"{T}-f-s1-0", "withdraw", "constrain/for-now",
      "Let's take the 3 loops down."),
    A(f"{T}-f-s2-0", "approve", "qualify/caveat",
      "they can run effectively but I think they may still need supervision"),
    A(f"{T}-f-s2-1", "constrain", "constrain/for-now",
      "I think they may still need supervision via watching the logs",
      within=f"{T}-f-s1-0"),
    A(f"{T}-f-s3-0", "defer", "defer/until",
      "I can run them tomorrow.",
      within=f"{T}-f-s1-0"),
    A(f"{T}-f-s4-0", "report", "report/status-update",
      "We're nearly into the last hundred problems."),
    A(f"{T}-f-s5-0", "report-problem", "report-problem/observed",
      "I used 40% of my codex subscription in one day"),
    A(f"{T}-f-s5-1", "defer", "defer/until",
      "I used 40% of my codex subscription in one day",
      within=f"{T}-f-s5-0",
      conf="low",
      note="duplicate of f-s5-0 under a second intent"),
])

T = "claude-18-turn-76"
build("turn-EvJZPG", [
    A(f"{T}-f-s1-0", "report", "report/status-update",
      "OK I did the reset,"),
    A(f"{T}-f-s1-1", "delegate", "delegate/to-agent",
      "let's bring up 3x loops: One nontopology solving loop",
      within=f"{T}-f-s1-0"),
    A(f"{T}-f-s2-0", "constrain", "constrain/standing-rule",
      "All on Codex sol fresh sessions.",
      within=f"{T}-f-s1-1"),
])
