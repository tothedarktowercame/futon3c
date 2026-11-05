import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-18-turn-114"
build("turn-19mnmt", [
    A(f"{T}-f-s1-0", "report-problem", "report-problem/observed",
      "if the agent makes no LSP calls certainly won't give us info on whether LSP is useful or not!",
      within="claude-18-turn-7-f-s2-2",
      note="objection inside the LSP-training-set proposal"),
])

T = "claude-18-turn-117"
build("turn-1BQH4X", [
    A(f"{T}-f-s1-0", "verify", "ask-action/commission",
      "let's try it on m02J02.",
      within="claude-18-turn-7-f-s2-2"),
    A(f"{T}-f-s2-0", "explain", "clarify/context",
      "I'm interested that it doesn't seem to help in most cases",
      within="claude-18-turn-7-f-s2-2",
      conf="medium"),
])

T = "claude-12-turn-138"
build("turn-1QmK3t", [
    A(f"{T}-f-s1-0", "ask-action", "ask-action/commission",
      "ok, please run 1 click",
      within="claude-12-turn-4-f-s1-0",
      note="a concrete run inside the standing run-and-report delegation"),
])
