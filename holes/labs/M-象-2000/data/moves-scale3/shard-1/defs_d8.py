import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-12-turn-159"
build("turn-6hFDIW", [
    A(f"{T}-f-s1-0", "propose", "propose/reframe",
      "Maybe we should view these claude-run sessions as an inadvertent red-teaming."),
    A(f"{T}-f-s2-0", "report", "report/status-update",
      "we have found lots of ways in which the machine is defective.",
      within=f"{T}-f-s1-0"),
])

T = "claude-13-turn-39"
build("turn-6mpGKG", [
    A(f"{T}-f-s1-0", "delegate", "delegate/to-agent",
      "bell the jobs to claude-16 since it is working on -40"),
])

T = "claude-11-turn-58"
build("turn-6vi8MM", [
    A(f"{T}-f-s1-0", "explore", "clarify/ask-decision",
      "so, what would be the next order of business.",
      within="claude-11-turn-2-f-s1-0"),
    A(f"{T}-f-s2-0", "propose", "propose/tentative",
      "maybe parsing hello world?",
      within=f"{T}-f-s1-0"),
])
