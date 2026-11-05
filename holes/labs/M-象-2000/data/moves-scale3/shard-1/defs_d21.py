import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-12-turn-4"
build("turn-FgowBB", [
    A(f"{T}-f-s1-0", "delegate", "delegate/to-agent",
      "please run it and report back.",
      opens="run and report",
      note="the standing delegation most later runs in this session sit inside"),
    A(f"{T}-f-s2-0", "approve", "constrain/standing-rule",
      "you can use zai agents as the staff",
      within=f"{T}-f-s1-0"),
])

T = "claude-12-turn-12"
build("turn-GK97ER", [
    A(f"{T}-f-s1-0", "redirect", "constrain/for-now",
      "if one run already completed you should summarizr that one",
      within="claude-12-turn-4-f-s1-0"),
])

T = "claude-12-turn-86"
build("turn-Glz20I", [
    A(f"{T}-f-s1-0", "clarify", "ask-action/status-check",
      "what is the hairsplitting detail about 0.05 vs 0.06 ?"),
])
