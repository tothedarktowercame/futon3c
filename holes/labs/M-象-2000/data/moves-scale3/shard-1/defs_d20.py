import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-18-turn-95"
build("turn-FOzAWz", [
    A(f"{T}-f-s2-0", "report-problem", "report-problem/observed",
      "stopped for 2 tries without progress isn't great."),
    A(f"{T}-f-s3-0", "explain", "clarify/context",
      "it's also a bit strange b/c Codex (GPT) is supposed to be the best",
      within=f"{T}-f-s2-0"),
    A(f"{T}-f-s3-1", "delegate", "delegate/to-agent",
      "we'd presumably want to ask a Codex agent to find the new route!",
      within=f"{T}-f-s3-0"),
])

T = "claude-18-turn-126"
build("turn-Fan3yE", [
    A(f"{T}-f-s1-0", "report", "report-problem/suspected",
      "m96J03 had a potential defect according to earlier comments"),
    A(f"{T}-f-s1-1", "verify", "ask-action/commission",
      "that should be investigated",
      within=f"{T}-f-s1-0"),
])

T = "claude-11-turn-74"
build("turn-FeAW0m", [
    A(f"{T}-f-s1-0", "approve", "approve/plain",
      "Yes, let's extend the warm environment with the key functions"),
    A(f"{T}-f-s2-0", "verify", "clarify/check-understanding",
      "I assume we are still on the way to the Mathlib Pythagorean theorem demo!"),
])
