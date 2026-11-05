import sys
sys.path.insert(0, ".")
from build import build
from defs_34pFgV import A

T = "claude-11-turn-43"
build("turn-IJhnCI", [
    A(f"{T}-f-s1-0", "propose", "propose/direct-route",
      "so, why not port the core faithfully?",
      note="reply has no marked paragraphs; only fragment acts"),
    A(f"{T}-f-s1-1", "challenge", "challenge/why-not",
      "why not port the core faithfully?"),
    A(f"{T}-f-s2-0", "clarify", "clarify/remove-constraint",
      "i can't think of anything in the port itself that is sacrosanct."),
    A(f"{T}-f-s3-0", "clarify", "clarify/confirm-goal",
      "isn't the point to replucate Lean in Python?"),
    A(f"{T}-f-s4-0", "challenge", "challenge/hedging",
      "why pussyfoit around if so?"),
])

T = "claude-4-turn-299"
build("turn-NLfNvL", [
    A(f"{T}-f-s1-0", "verify", "verify/status-request",
      "can you comment on progress?"),
    A(f"{T}-r-0", "reply-mark", "report/partial",
      "No new row has become fully correct yet. What has changed is that rows reach later stops.",
      answers=[(f"{T}-f-s1-0", "Since about 23:00, six compiler fixes have been merged into cand-1, now at `e251cb68`", "closes")],
      note="direct status answer: fixes merged, no clean row yet"),
    A(f"{T}-r-1", "reply-mark", "report/detailed-movement",
      "**Null receiver** (codex-4): `nodeFn` and `atomicFn` now get past the `null.stackSize`/`null.pos` stop.",
      answers=[(f"{T}-f-s1-0", "On the chart, that group fell from 11 to 3", "keeps")]),
    A(f"{T}-r-2", "reply-mark", "explain/chart-lag",
      "The chart will lag until C lands and those rows are re-probed.",
      answers=[(f"{T}-f-s1-0", "That's because the null-receiver rows moved onto the term[_] stop, and most term[_] rows haven't been re-probed since A+B merged.", "keeps")],
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "report/problem-post-mortem",
      "codex-18 failed five times in a row the same way, so I paused it.",
      answers=[(f"{T}-f-s1-0", "claude-3 then found the real cause: the port has no type for `Ne`", "keeps"),
               ("claude-4-turn-246-r-4", "codex-18's first fix passed its own test while the defect was still there.", "closes")],
      note="also closes the earlier why-nothing-merged question with concrete causes"),
    A(f"{T}-r-4", "reply-mark", "report/in-flight",
      "**Cold acceptance running:** host parsers (codex-17), numerals (codex-19), nomatch (codex-25).",
      answers=[("claude-4-turn-246-r-5", "codex-4 merges cand-1 into the Rob merge branch next, then runs the 20-row cold acceptance.", "keeps")],
      conf="medium"),
    A(f"{T}-r-5", "reply-mark", "qualify/caveat",
      "The machine has been running at a load average around 24.",
      answers=[(f"{T}-f-s1-0", "so acceptance is now the slowest part of the cycle", "keeps")],
      conf="low",
      note="reply is truncated mid-sentence in the record"),
])
