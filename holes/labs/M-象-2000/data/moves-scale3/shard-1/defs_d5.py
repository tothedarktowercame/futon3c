import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-11-turn-97"
build("turn-37gfPf", [
    A(f"{T}-f-s1-0", "approve", "approve/plain", "OK please do 1 and 2"),
])

T = "claude-18-turn-110"
build("turn-3DA6UJ", [
    A(f"{T}-f-s1-0", "constrain", "constrain/stand-rule",
      "If solve-loop isn't checked in"),
    A(f"{T}-f-s1-1", "ask-action", "ask-action/commission",
      "please check it in and  push",
      within=f"{T}-f-s1-0"),
])

T = "claude-18-turn-70"
build("turn-3ecbrN", [
    A(f"{T}-f-s1-0", "approve", "approve/plain", "OK, all of that sounds good."),
    A(f"{T}-f-s2-0", "ask-action", "ask-action/commission",
      "Please make the changes and improvements."),
    A(f"{T}-f-s3-0", "approve", "approve/plain",
      "That may bring us close to using up the remaining 4% of Codex (which is fine)."),
    A(f"{T}-f-s4-0", "verify", "ask-action/status-check",
      "What's less clear right now is review of the DAG that is supposed to guide their work",
      conf="medium"),
])
