import sys
sys.path.insert(0, ".")
from build import build

N = None  # no scope

def A(act, kind, move, quote, answers=(), opens=None, closes=None, within=None, conf="high", note=None):
    a = {"act": act, "kind": kind, "move": move, "quote": quote,
         "answers": [dict(zip(("act", "quote", "effect"), x)) for x in answers],
         "scope": {"opens": opens, "closes": closes, "within": within},
         "confidence": conf}
    if note:
        a["note"] = note
    return a

T = "claude-17-turn-518"

build("turn-34pFgV", [
    A(f"{T}-f-s1-0", "report", "report/status-update",
      "So I have started a big integration of 象 features with Element + Matrix.",
      note="opening context, no ask"),
    A(f"{T}-f-s2-0", "report", "report/context",
      "That's fun and I've been working with Codex in a fork of web-element."),
    A(f"{T}-f-s3-0", "report-problem", "report-problem/observed",
      "the problem is that it is noticeably VERY slow"),
    A(f"{T}-f-s4-0", "report-problem", "report-problem/observed",
      "象 annotations are being added to agent turns, which didn't match how I thought"),
    A(f"{T}-f-s5-0", "clarify", "clarify/check-understanding",
      "What I thought was that the xiaoxiang-insert-mark tokens would be used to support FAST inference",
      conf="medium"),
    A(f"{T}-f-s6-0", "delegate", "delegate/to-agent",
      "Can you read the element code and tell me where we'd make changes to support these fast mode features?"),
    A(f"{T}-r-0", "reply-mark", "explain/mechanism",
      "The fast reading you had in mind already runs on the server, but Element never shows it.",
      answers=[(f"{T}-f-s6-0", "There are four places to change. The first two are in futon3c and the other two are in the Element fork.", "closes"),
               (f"{T}-f-s5-0", "Each turn gets a 小象 naive-Bayes draft when it is recorded, in about 0.06 s, but the Element code only underlines the LLM's analysis.", "closes")],
      note="answers the where-question and corrects the fast-inference premise"),
    A(f"{T}-r-1", "reply-mark", "explain/mechanism",
      "The server side is fast:",
      answers=[(f"{T}-f-s3-0", "the turn list answers in about 45 ms, Matrix membership check included", "keeps")],
      note="measured decomposition of the slowness"),
    A(f"{T}-r-2", "reply-mark", "report/with-options",
      "**Show the draft first** (Element, `FutonIntentMarks.ts:79` `validatedIntentMarks`).",
      answers=[(f"{T}-f-s6-0", "1. **Show the draft first** (Element, `FutonIntentMarks.ts:79`", "keeps"),
               (f"{T}-f-s5-0", "With this, an operator turn gets its marks within a second of being recorded.", "keeps")],
      opens="element-fast-mode plan",
      note="four numbered changes, two repos"),
    A(f"{T}-r-3", "reply-mark", "report/gap",
      "Fast annotation, meaning you correct a draft's mark in Element and 小象 trains on the correction, has no route yet.",
      answers=[(f"{T}-f-s5-0", "Fast annotation, meaning you correct a draft's mark in Element and 小象 trains on the correction, has no route yet.", "keeps")],
      within="element-fast-mode plan",
      note="the fast-annotation half of the expectation is unsupported"),
    A(f"{T}-r-4", "reply-mark", "ask-action/one-decision",
      "Shall I bell 2 and 3 to a Kimi or Zai seat (one handoff each) and send the Element items to your Codex as a written spec?",
      answers=[(f"{T}-f-s6-0", "Items 2 and 3 are small futon3c changes, and 1 and 4 belong in the Element fork you're working on with Codex.", "keeps")],
      within="element-fast-mode plan"),
])
