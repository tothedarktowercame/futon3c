import sys
sys.path.insert(0, ".")
from build import build
from defs_34pFgV import A

T = "claude-6-turn-4"
build("turn-q0fjvy", [
    A(f"{T}-f-s1-0", "verify", "verify/get-story-straight",
      "these agents are wasting lots of time to satisfy a rule when a known-better solution exists?"),
    A(f"{T}-r-0", "reply-mark", "report/answer-with-correction",
      "Mostly yes, with one correction.",
      answers=[(f"{T}-f-s1-0", "But the better solution is likely, not known: nobody has tested it.", "closes")],
      note="confirms the time-waste reading, corrects 'known' to 'likely'"),
    A(f"{T}-r-1", "reply-mark", "report/evidence",
      "As of claude-3's census, 17 of 110 rows matched native Lean.",
      answers=[(f"{T}-f-s1-0", "Each step costs several probes of 30–60 minutes. Lean elaborates all 110 rows in 24 seconds", "keeps")]),
    A(f"{T}-r-2", "reply-mark", "qualify/risk-assessment",
      "Whether translating that output gives working Python.",
      answers=[(f"{T}-f-s1-0", "Those risks look smaller than rebuilding the remaining elaborator steps, but that's my judgement, not a measurement.", "keeps")]),
    A(f"{T}-r-3", "reply-mark", "report/attribution",
      "The agents followed the rules as written.",
      answers=[(f"{T}-f-s1-0", "The agents followed the rules as written.", "keeps")],
      conf="medium"),
    A(f"{T}-r-4", "reply-mark", "ask-action/one-decision",
      "The two-or-three-row test would turn \"likely\" into known or ruled out, for about one packet. Shall I dispatch it?",
      answers=[(f"{T}-f-s1-0", "The two-or-three-row test would turn \"likely\" into known or ruled out", "keeps")],
      note="repeats the previous turn's test offer now that the story is straight"),
])

T = "claude-16-turn-8"
build("turn-rcPsFk", [
    A(f"{T}-f-s1-0", "ask-action", "ask-action/status-check",
      "🈸: Can you show me what's in the offing?"),
    A(f"{T}-r-1", "reply-mark", "report/inventory",
      "(the inbox as it stands) Open ports at the tails of the 86 sessions with ports since 10-01, before the recompute:",
      answers=[(f"{T}-f-s1-0", "Open ports at the tails of the 86 sessions with ports since 10-01", "closes")]),
    A(f"{T}-r-2", "reply-mark", "propose/ordered-plan",
      "(next on my path, in order)",
      answers=[(f"{T}-f-s1-0", "1. Receipt of the recompute: totals before and after, the four closes in claude-15's sessio…", "keeps")],
      opens="inbox-zero path order",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "ask-action/two-decisions",
      "(waiting on you) Two decisions, both about your prior.",
      answers=[(f"{T}-f-s1-0", "First, `constrain`: remove it from the adjacency openers so it s…", "keeps")],
      conf="medium"),
    A(f"{T}-r-4", "reply-mark", "qualify/out-of-scope",
      "(not in the offing) The adjacency table otherwise, a futon3c restart, the switch of 40ºBelowDB onto the live store.",
      conf="medium"),
], unlabelled=[f"{T}-c-4a13ba7f"])
