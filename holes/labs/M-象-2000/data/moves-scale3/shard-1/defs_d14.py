import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-11-turn-40"
build("turn-9DcnO8", [
    A(f"{T}-f-s1-0", "verify", "ask-action/status-check",
      "what is the crash about?"),
])

T = "claude-11-turn-41"
build("turn-9VxB75", [
    A(f"{T}-f-s1-0", "constrain", "constrain/standing-rule",
      "ok, remember to be efficient like i said before"),
])

T = "claude-12-turn-129"
build("turn-9bbs6t", [
    A(f"{T}-f-s1-0", "explain", "ask-action/status-check",
      "why is there such a huge checkpoint?",
      within="claude-12-turn-13-f-s1-0",
      conf="low",
      note="questioning observed output inside the debugging lane"),
])
