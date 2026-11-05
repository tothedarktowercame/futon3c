import sys
sys.path.insert(0, ".")
from build import build
from defs_34pFgV import A

T = "claude-14-turn-30"
build("turn-4THvxs", [
    A(f"{T}-f-s1-0", "propose", "propose/alternative",
      "so, I think blueprints could follow the audits, when those exist, as this builds increasing clarity",
      note="endorses the blueprint idea, sequenced after audits"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "Set up that way in `3e72c0903`.",
      answers=[(f"{T}-f-s1-0", "The order is now: route audit, then a reviewed blueprint of the foundation the audit names, then formalisation one blueprint node at a time.", "closes"),
               ("claude-14-turn-28-r-5", "Both foundation programs already have audits, so their next jobs write blueprints.", "closes")],
      note="implements the go? offer and the operator's sequencing in one commit"),
    A(f"{T}-r-1", "reply-mark", "report/done",
      "**`BLUEPRINTS.md`** in the lab sets out the order and the format",
      answers=[(f"{T}-f-s1-0", "one node per future Lean declaration, each with `uses:` edges, the intended Lean type, and the Lean representation wherever there's a choice", "closes"),
               ("claude-14-turn-28-r-3", "A node needing more than about a page gets split, and every hypothesis of the final theorem has to be used by some node.", "closes")],
      note="the format addresses the limits raised earlier"),
    A(f"{T}-r-2", "reply-mark", "propose/next-step",
      "When all 10 batches are in, I'll group the FOUNDATION verdicts by the foundation they need and rank them by how many problems each serves.",
      answers=[(f"{T}-f-s1-0", "I'll elect a blueprint for each of the top few, and those then become programs as the two current ones finish.", "keeps")],
      opens="audit-driven blueprint ladder",
      note="future programs chosen by audit coverage"),
])
