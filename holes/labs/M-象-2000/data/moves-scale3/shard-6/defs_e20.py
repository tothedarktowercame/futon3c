import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-2-turn-52"
build("turn-QUKnek", [
    A(f"{T}-f-s1-0", "explain", "clarify/assertion",
      "Just to state one more really obvious thing,"),
    A(f"{T}-f-s1-1", "constrain", "constrain/standing-rule",
      "because it's written down right in the paper",
      opens="patterns as plain-language AI",
      note="standing reading rule for the pattern library"),
    A(f"{T}-r-0", "reply-mark", "approve/plain",
      '("if they don\'t do that, then they also need to be revised") Agreed',
      answers=[(f"{T}-f-s1-1", 'Agreed', "closes")],
      within=f"{T}-f-s1-1",
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "propose/next-step",
      "(how I'll carry that) For each node I'll keep three things together",
      answers=[(f"{T}-f-s1-1", "(how I'll carry that)", "keeps")],
      within=f"{T}-f-s1-1"),
    A(f"{T}-r-2", "reply-mark", "qualify/caveat",
      "(where a pattern may have nothing to say)",
      within=f"{T}-f-s1-1"),
    A(f"{T}-r-3", "reply-mark", "report/in-flight",
      "(status) codex-10 is still on the habit-prior module",
      conf="medium"),
])

T = "claude-2-turn-7"
build("turn-QUbt77", [
    A(f"{T}-f-s1-0", "explain", "clarify/context",
      "Regarding the mission and the dilemma between most of the elaborator"),
    A(f"{T}-r-0", "reply-mark", "approve/with-amendment",
      "(the blank file and the elaborator) I agree with most of that",
      answers=[(f"{T}-f-s1-0", "(the blank file and the elaborator) I agree with most of that", "keeps")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "explain/mechanism",
      "(what a blank file needs versus what the port needs)",
      answers=[(f"{T}-f-s1-0", "(what a blank file needs versus what the port needs)", "closes")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "explain/mechanism",
      "(how Lean itself handles this) Lean has the same dependency",
      answers=[(f"{T}-f-s1-0", "(how Lean itself handles this)", "keeps")],
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "report/evidence",
      "(what follows for the plan)",
      conf="medium"),
    A(f"{T}-r-4", "reply-mark", "propose/next-step",
      "(measure the subset instead of discovering it)",
      conf="medium"),
    A(f"{T}-r-5", "reply-mark", "ask-action/one-decision",
      "(one decision)",
      conf="medium"),
])
