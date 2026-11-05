import sys
sys.path.insert(0, ".")
from build import build
from defs_34pFgV import A

T = "claude-11-turn-16"
build("turn-7QZ8sL", [
    A(f"{T}-f-s1-0", "retract", "retract/correction",
      "no, i forgot about 7.",
      answers=[("claude-11-turn-15-r-3", "no, i forgot about 7.", "closes")],
      note="retracts the premise of the previous question"),
    A(f"{T}-f-s2-0", "propose", "propose/best-of-review",
      "we should review both 6 and 7 to find a best-of"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "Both preambles are recovered, and I've drafted a best-of for you to review",
      answers=[(f"{T}-f-s2-0", "The draft is `holes/method/preamble-best-of-DRAFT.md`, and every section is tagged [6], [7] or [6+7].", "closes"),
               ("claude-11-turn-15-r-3", "`holes/method/recovered-2026-10-06/preamble6.md` is exact.", "closes")],
      opens="preamble best-of draft",
      note="recovery done from packet copies plus transcript appends"),
    A(f"{T}-r-1", "reply-mark", "explain/comparison",
      "Preamble 7 is a rewrite of 6 based on what went wrong on 10-06, and it's stronger on method.",
      answers=[(f"{T}-f-s2-0", "Preamble 7 is a rewrite of 6 based on what went wrong on 10-06, and it's stronger on method.", "closes")],
      within="preamble best-of draft"),
    A(f"{T}-r-2", "reply-mark", "propose/defaults",
      "two places I chose, which you may want to overrule",
      answers=[(f"{T}-f-s2-0", "I took 7's resolution: the cold comparator is the test, and unit tests are only for tools.", "keeps")],
      within="preamble best-of draft",
      conf="medium",
      note="resolution choices flagged for operator overrule"),
    A(f"{T}-r-3", "reply-mark", "report/gap",
      "Several things the preambles refer to were in `/tmp`, so the draft marks them as needing new homes",
      within="preamble best-of draft",
      note="resume prerequisites lost with /tmp"),
    A(f"{T}-r-4", "reply-mark", "ask-action/one-decision",
      "Read the draft when you're ready. When you've settled it, I'll make it the new preamble",
      answers=[(f"{T}-f-s2-0", "When you've settled it, I'll make it the new preamble and deal with the RESUME items.", "keeps")],
      within="preamble best-of draft",
      conf="medium"),
])
