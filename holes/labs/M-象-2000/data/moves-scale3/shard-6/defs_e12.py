import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-17-turn-393"
build("turn-I7n9AZ", [
    A(f"{T}-f-s1-0", "approve", "approve/plain", "1"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      'Gist: Done. "🈸:yes" now counts as yes',
      answers=[(f"{T}-f-s1-0", 'Done. "🈸:yes" now counts as yes', "closes")]),
    A(f"{T}-r-1", "reply-mark", "report/done",
      "(change, commit d9d30ead)",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "verify/checked-live",
      "(what I checked)",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "report/gap",
      "(what still connects the marks to Elephant-2000)",
      conf="medium"),
])

T = "claude-17-turn-494"
build("turn-IKLUvq", [
    A(f"{T}-f-s1-0", "disagree", "disagree/correction",
      "agent turns DO carry intents, that's denoted by the characters and the Legend"),
    A(f"{T}-r-0", "reply-mark", "retract/correction",
      "(my claim that agent turns have no readings) You're right, and I take it back",
      answers=[(f"{T}-f-s1-0", "(my claim that agent turns have no readings)", "closes")]),
    A(f"{T}-r-1", "reply-mark", "report-problem/observed",
      "(two things I found while checking the marks)",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "explain/mechanism",
      "(what this gives the flow)",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "ask-action/one-decision",
      "(one decision) Shall I build that joined model of the six turns",
      answers=[(f"{T}-f-s1-0", "(one decision)", "keeps")],
      conf="medium"),
])
