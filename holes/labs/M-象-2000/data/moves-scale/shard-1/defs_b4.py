import sys
sys.path.insert(0, ".")
from sbuild import build, A

T = "claude-17-turn-399"
build("turn-7Tht2c", [
    A(f"{T}-f-s1-0", "approve", "approve/plain", "The one-line gray message is better, thanks."),
    A(f"{T}-f-s2-0", "propose", "delegate/to-agent",
      "we should put our heads together with codex-10"),
    A(f"{T}-f-s3-0", "report", "report/status-update",
      'we\'d increasingly be in a position to work on the "close AIF over the operator" material'),
    A(f"{T}-f-s4-0", "propose", "propose/zip-two-sides",
      'we could try to "zip" the two sides to form a mission'),
    A(f"{T}-f-s5-0", "extend", "propose/two-sided-work",
      "we could even work on the mission from both sides"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "I've started a mission draft, `futon3c/holes/missions/M-aif-over-the-operator.md`",
      answers=[(f"{T}-f-s4-0", "I've started a mission draft", "closes"),
               (f"{T}-f-s2-0", "asked codex-10 to fill in the War Machine half", "closes")],
      opens="M-aif-over-the-operator zip"),
    A(f"{T}-r-1", "reply-mark", "report/design",
      "**Origin:** your words today, plus the problem as `sec-operator.tex` states it.",
      answers=[(f"{T}-f-s4-0", "**Origin:** your words today", "keeps")],
      within="M-aif-over-the-operator zip"),
    A(f"{T}-r-2", "reply-mark", "report/dispatched",
      "**Job:** `invoke-1790873058224-29744-663ef2c1`, in work mode.",
      within="M-aif-over-the-operator zip",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "report/gap",
      "I don't know where 象's readings of your turns are stored durably.",
      within="M-aif-over-the-operator zip"),
])

T = "claude-17-turn-504"
build("turn-8a0Q2x", [
    A(f"{T}-f-s1-0", "propose", "propose/hydra-as-tools",
      "what if all the verbs that I have available via the C-c ; hydra became a set of tools"),
    A(f"{T}-f-s2-0", "explain", "explain/motive",
      "That would make the AIF harness more concrete."),
    A(f"{T}-f-s3-0", "propose", "propose/aligned-ledgers",
      'using our "aligned ledgers" between PBASE and R-numbers and intents'),
    A(f"{T}-f-s4-0", "explain", "explain/example",
      "I hav three verbs classed as PERCEIVE ㊩ 🈖 ㊢ and the War Machine has three nodes in that layer"),
    A(f"{T}-r-0", "reply-mark", "explain/mechanism",
      "R19 reads a mission's preferences (C) from the mission's own text",
      answers=[(f"{T}-f-s4-0", "R19 reads a mission's preferences (C) from the mission's own text", "keeps")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/evidence",
      "`futon2/holes/labs/wm-contract/wm-click-acts.edn` tags each WM click act",
      answers=[(f"{T}-f-s3-0", "`futon2/holes/labs/wm-contract/wm-click-acts.edn` tags each WM click act", "closes")]),
    A(f"{T}-r-2", "reply-mark", "report-problem/observed",
      "That same file tags request-interpretation as `:PERCEIVE :R7`",
      answers=[(f"{T}-f-s3-0", "That same file tags request-interpretation as `:PERCEIVE :R7`", "keeps")],
      note="a data inconsistency between two ledgers"),
    A(f"{T}-r-3", "reply-mark", "ask-action/one-decision",
      "Shall I write the 23-mark × R-node table, with a confidence for each row",
      answers=[(f"{T}-f-s3-0", "Shall I write the 23-mark × R-node table", "keeps")],
      conf="medium"),
])

T = "claude-17-turn-477"
build("turn-8aFNoU", [
    A(f"{T}-f-s1-0", "report", "report/status-update",
      'I can see underlining, but not anything but the standard underlining "for now".'),
    A(f"{T}-f-s2-0", "constrain", "constrain/for-now", "I also don't like visual noise."),
    A(f"{T}-f-s3-0", "approve", "approve/plain",
      "The red text for R-nodes plus underlining for other things is good enough."),
    A(f"{T}-f-s4-0", "propose", "explore/historical-count",
      "What will be interesting is to look historically and see how many red texts appear."),
    A(f"{T}-f-s5-0", "clarify", "clarify/check-understanding",
      "I still don't know what texts *should* turn red (R-number node but not an intent)."),
    A(f"{T}-r-0", "reply-mark", "report/evidence",
      "Across the 1,249 operator turns in the live store, from 22 September to now, 286 turns would show red",
      answers=[(f"{T}-f-s4-0", "286 turns would show red: 23%", "closes")]),
    A(f"{T}-r-1", "reply-mark", "qualify/caveat",
      "That's the intention, not a guarantee.",
      answers=[(f"{T}-f-s5-0", "That's the intention, not a guarantee.", "keeps")],
      conf="medium"),
])
