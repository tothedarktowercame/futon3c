import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-13-turn-21"
build("turn-5HLIOX", [
    A(f"{T}-f-s1-0", "defer", "defer/until",
      "fixing up metameso isn't a priority right now, I'd fit that under M-hardening.md ..."),
    A(f"{T}-f-s2-0", "explore", "collect/idea",
      "if I was running a virtual startup on Metameso and one on Lucy"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      '("I\'d fit that under M-hardening.md") I\'ve deferred the metameso recall gap.',
      answers=[(f"{T}-f-s1-0", '("I\'d fit that under M-hardening.md")', "closes")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "approve/plain",
      '("a virtual startup on Metameso and one on Lucy") I like that as a way to play',
      answers=[(f"{T}-f-s2-0", '("a virtual startup on Metameso and one on Lucy") I like that', "closes")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "clarify/ask-decision",
      "(M-hardening) Is `M-hardening` a new mission you have in mind",
      answers=[(f"{T}-f-s1-0", "(M-hardening) Is `M-hardening` a new mission", "keeps")],
      conf="medium"),
])

T = "claude-18-turn-135"
build("turn-5iFDkQ", [
    A(f"{T}-f-s1-0", "clarify", "clarify/ask-decision",
      'Are the other 10 problems considered "hard" or "easy"'),
    A(f"{T}-f-s1-1", "collect", "collect/ask-option",
      'Are the other 10 problems considered "hard" or "easy"',
      note="duplicate fragment carrying the same text under two intents"),
])

T = "claude-11-turn-88"
build("turn-6OlWEO", [
    A(f"{T}-f-s1-0", "collect", "report/context",
      "here's what Rob says he's after:"),
])
