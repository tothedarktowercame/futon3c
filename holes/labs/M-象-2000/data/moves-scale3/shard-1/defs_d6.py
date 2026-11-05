import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-18-turn-109"
build("turn-3snPYU", [
    A(f"{T}-f-s1-0", "approve", "approve/plain", "Yes"),
    A(f"{T}-f-s1-1", "report-problem", "report-problem/observed",
      "we can't run lean with these settings in the same cgroup"),
])

T = "claude-12-turn-141"
build("turn-4VqGUm", [
    A(f"{T}-f-s1-0", "prioritize", "ask-action/commission",
      "why dont you look into the http 403 now",
      within="claude-12-turn-13-f-s1-0",
      conf="low",
      note="re-prioritisation inside the session's standing debugging lane"),
])

T = "claude-16-turn-78"
build("turn-57qRKT", [
    A(f"{T}-f-s1-0", "report-problem", "report-problem/observed",
      "I ran that in the fresg claude-19 buffer and got list: Wrong type argument: stringp, :null"),
    A(f"{T}-r-0", "reply-mark", "explain/mechanism",
      "(what the error shows about seats) claude-19 is a new seat with no clock decision yet",
      answers=[(f"{T}-f-s1-0", "(what the error shows about seats)", "closes")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "ask-action/one-decision",
      '("I ran that") Try again in the same buffer: `M-x xiang-resume-brief`',
      answers=[(f"{T}-f-s1-0", 'Try again in the same buffer', "keeps")],
      conf="medium"),
])
