import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-12-turn-6"
build("turn-JTuY1v", [
    A(f"{T}-f-s1-0", "challenge", "ask-action/status-check",
      "did you use parks?",
      within="claude-12-turn-4-f-s1-0"),
    A(f"{T}-f-s2-0", "challenge", "clarify/ask-decision",
      "how will you report back if not?",
      within=f"{T}-f-s1-0"),
])

T = "claude-12-turn-127"
build("turn-K1IY3h", [
    A(f"{T}-f-s1-0", "verify", "ask-action/commission",
      "ok, please check it.",
      within="claude-12-turn-4-f-s1-0"),
    A(f"{T}-f-s2-0", "ask-action", "ask-action/commission",
      "if it looks goid, run 1 click wiyh yhe debugger engaged",
      within="claude-12-turn-4-f-s1-0",
      note="continues the debugger-engaged variant of the standing run rule"),
])

T = "claude-18-turn-101"
build("turn-K2Po4x", [
    A(f"{T}-f-s1-0", "report", "report/observed-output",
      'I see "02:24 solve   solve loop finished"'),
])

T = "claude-18-turn-86"
build("turn-KHmEpN", [
    A(f"{T}-f-s1-0", "approve", "approve/plain",
      "Yeah, that's a good point."),
    A(f"{T}-f-s2-0", "propose", "propose/tentative",
      "maybe we could find a use for t96J01 after all:"),
    A(f"{T}-f-s2-1", "propose", "propose/directive",
      "can we separate the AIF-relevant problems from the AIF-irrelevant problems",
      within=f"{T}-f-s2-0"),
])
