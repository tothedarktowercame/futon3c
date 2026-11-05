import sys
sys.path.insert(0, ".")
from build import build
from defs_34pFgV import A

T = "claude-6-turn-2"
build("turn-RQwzVb", [
    A(f"{T}-f-s1-0", "explain", "explain/premise-history",
      "claude-2 also said that the route of using the Lean elaborator would alter things"),
    A(f"{T}-f-s2-0", "report-problem", "report-problem/observed",
      "agents have been getting lost in the details since then it seems to me."),
    A(f"{T}-r-0", "reply-mark", "report/diagnosis",
      "You're right, and the route was never actually taken.",
      answers=[(f"{T}-f-s1-0", "What got adopted was a weaker form: Lean is used as an answer key (preregistered expected output per row) and as a curriculum", "closes"),
               (f"{T}-f-s2-0", "that rebuilding is the detail the agents are lost in.", "closes")],
      opens="lean-elaborator route question"),
    A(f"{T}-r-1", "reply-mark", "explain/constraint",
      "The mission (`M-lean-subset-python-compiler-for-generated-parser-combinators.md`) rules it out in three places.",
      answers=[(f"{T}-f-s1-0", "A compiler that takes Lean's elaborated term as input would replace the port's front end, so the queue took the form those rules allow.", "closes")],
      within="lean-elaborator route question"),
    A(f"{T}-r-2", "reply-mark", "report/evidence",
      "The census shows how much elaborator is left to rebuild:",
      answers=[(f"{T}-f-s2-0", "coercion is needed by 44 failing rows and no correct row demonstrates it yet;", "keeps")],
      within="lean-elaborator route question"),
    A(f"{T}-r-3", "reply-mark", "propose/alternative",
      "a translator from it to Python would be the \"relatively small generic compiler\" the mission's own Operator Direction asks for.",
      answers=[(f"{T}-f-s1-0", "So the strong route fits the mission's stated aim and breaks its gates.", "keeps")],
      within="lean-elaborator route question"),
    A(f"{T}-r-4", "reply-mark", "ask-action/one-decision",
      "🈸 (one decision) S",
      conf="low",
      within="lean-elaborator route question",
      note="record truncated mid-sentence; the ask is the cheap-test decision"),
])

T = "claude-2-turn-123"
build("turn-Wcr0h9", [
    A(f"{T}-f-s2-0", "explain", "explain/interpretation",
      "I do like reading things based on HEAD but keeping in mind HEAD is like from jazz or free jazz..."),
    A(f"{T}-f-s3-0", "report-problem", "report-problem/concern",
      "it won't tell us whether the mission has spun off into abstractions like M-wyrd-modernism"),
    A(f"{T}-f-s4-0", "propose", "propose/cheat-sheet",
      "we might want something a little bit like a chord \"cheat sheet\", like a very quick distillation of the scopes and keyw"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "Added as B1d (futon2 `ff22a2876`), queued for codex-33 right after B1c",
      answers=[(f"{T}-f-s4-0", "a one-line cheat sheet per item, computed from the whole document and its history rather than HEAD.", "closes"),
               (f"{T}-f-s3-0", "concrete vs abstract, and whether the mission is heading out or coming back — because that is the feasibility signal B2 could not get from commit history", "closes")],
      opens="B1d chord sheet",
      note="the register/direction remark becomes the sheet's key field"),
    A(f"{T}-r-1", "reply-mark", "report/design",
      "Five fields, all classical counts so the sheet is the same on every run:",
      answers=[(f"{T}-f-s4-0", "*register* — per section, the share of lines carrying a path, sha, number, checkbox or date against prose lines", "closes")],
      within="B1d chord sheet"),
    A(f"{T}-r-2", "reply-mark", "explain/integration",
      "The `observe` square today emits an item as id, kind, source pin and one locator.",
      answers=[(f"{T}-f-s4-0", "The sheet is what it should emit instead — the item as a chord chart, not a bar", "keeps")],
      within="B1d chord sheet",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "report/in-flight",
      "B1c (revealed attention and co-work) running on codex-33, job `invoke-1791243893154-33311-1ff8ab13`",
      within="B1d chord sheet",
      answers=[(f"{T}-f-s4-0", "B1d follows; B3 then has B1–B2 and the sheet to draw on.", "keeps")]),
], unlabelled=[f"{T}-f-s1-0"])
