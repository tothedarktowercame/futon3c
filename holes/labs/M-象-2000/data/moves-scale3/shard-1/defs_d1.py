import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-11-turn-70"
build("turn-0I6y3G", [
    A(f"{T}-f-s1-0", "prioritize", "prioritize/sequencing",
      "let's aim for M3 before we push for Rob.",
      within="claude-11-turn-2-f-s1-0",
      note="orders milestones inside the health-scan work the session opened"),
    A(f"{T}-f-s2-0", "explore", "collect/idea",
      "The other thing he was talking about was somehow expressing math as Python types",
      conf="medium"),
])

T = "claude-11-turn-27"
build("turn-0l6bap", [
    A(f"{T}-f-s1-0", "verify", "ask-action/status-check",
      "there are many probe jobs listed on voxterm is thst expectecd?",
      within="claude-11-turn-2-f-s1-0",
      conf="medium",
      note="questioning observed output of the probe lane the scan opened"),
])
