import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-12-turn-118"
build("turn-2aojFu", [
    A(f"{T}-f-s1-0", "report", "report/done",
      "I created a Warrant system; if we use that, then we won't have to rerun tests"),
    A(f"{T}-f-s2-0", "clarify", "clarify/distinction",
      'That is a separate issue from "slow tests".'),
    A(f"{T}-f-s3-0", "explore", "collect/idea",
      'what you said above about "The performance cliff is isolated" ...',
      conf="medium"),
    A(f"{T}-f-s4-0", "explore", "collect/idea",
      "I wonder what is going on with the one policy that takes so long to process?"),
    A(f"{T}-f-s5-0", "prioritize", "constrain/for-now",
      "Can you explain that before we proceed with anything else?",
      opens="explain the slow policy first",
      note="blocks further work until the explanation lands"),
])

T = "claude-18-turn-87"
build("turn-2jeD5k", [
    A(f"{T}-f-s1-0", "propose", "propose/tentative",
      "here's an interesting thought: since we have a writeup of AIF *in Lean*",
      opens="lean-derived curriculum",
      note="starts the formal-AIF-as-curriculum idea that later turns elaborate"),
    A(f"{T}-f-s2-0", "propose", "propose/next-step",
      'we could map back to the informal problems',
      within=f"{T}-f-s1-0"),
])

T = "claude-13-turn-37"
build("turn-345Cv3", [
    A(f"{T}-f-s1-0", "qualify", "clarify/context",
      "it is unsurprising that mastra won as of now, given that minus40 is still being developed!"),
    A(f"{T}-f-s2-0", "propose", "propose/directive",
      "my goal is to learn from the best, and if we can use ideas from these projects to improve FUTON we should do so.",
      opens="learn from the best"),
])
