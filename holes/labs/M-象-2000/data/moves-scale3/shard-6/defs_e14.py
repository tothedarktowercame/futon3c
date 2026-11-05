import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-14-turn-16"
build("turn-KEDpiQ", [
    A(f"{T}-f-s1-0", "approve", "approve/plain", "yes"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      '("yes") voxterm has restarted',
      answers=[(f"{T}-f-s1-0", '("yes") voxterm has restarted', "closes")]),
    A(f"{T}-r-1", "reply-mark", "verify/checked-live",
      "(checked) I queried the live `/usage` after the restart",
      conf="medium"),
])

T = "claude-17-turn-433"
build("turn-KlWJtd", [
    A(f"{T}-f-s1-0", "constrain", "constrain/for-now",
      "they should be foreground not background faces"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "(foreground, not background) The background tint is gone",
      answers=[(f"{T}-f-s1-0", "(foreground, not background) The background tint is gone", "closes")]),
    A(f"{T}-r-1", "reply-mark", "report/done",
      "(where it stands) Committed as futon3c `f6fc84d3`",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "ask-action/two-decisions",
      "(the emoji marks) If you want the emoji marks coloured by stage too",
      conf="medium"),
])
