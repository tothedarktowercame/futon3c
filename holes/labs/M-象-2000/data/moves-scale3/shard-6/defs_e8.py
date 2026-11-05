import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-2-turn-60"
build("turn-BPuysJ", [
    A(f"{T}-f-s1-0", "explain", "report/context",
      "I was starting to look, from the elephant side, Add building up models of collaboration"),
    A(f"{T}-f-s1-1", "constrain", "constrain/for-now",
      "we should keep the implementation simple so that we can get back to running experiments",
      opens="simple implementation first",
      note="governs how the R11 work is read: simplicity over generality"),
    A(f"{T}-r-0", "reply-mark", "approve/plain",
      '("the war machine has two agents inside of it") Understood',
      answers=[(f"{T}-f-s1-0", '("the war machine has two agents inside of it")', "keeps")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/evidence",
      '("the theory should generalize if possible") It does',
      answers=[(f"{T}-f-s1-0", '("the theory should generalize if possible")', "keeps")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/evidence",
      '("one of my instructions might have gotten dropped")',
      answers=[(f"{T}-f-s1-0", '("one of my instructions might have gotten dropped")', "keeps")],
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "approve/plain",
      '("keep the implementation simple") For the War Machine I will hold R11',
      answers=[(f"{T}-f-s1-1", '("keep the implementation simple")', "closes")],
      within=f"{T}-f-s1-1"),
    A(f"{T}-r-4", "reply-mark", "propose/next-step",
      "(order of work, revised)",
      answers=[(f"{T}-f-s1-1", "(order of work, revised)", "keeps")],
      within=f"{T}-f-s1-1"),
    A(f"{T}-r-5", "reply-mark", "report/in-flight",
      "(status)",
      within=f"{T}-f-s1-1",
      conf="medium"),
])

T = "claude-2-turn-91"
build("turn-Bcf06J", [
    A(f"{T}-f-s1-0", "disagree", "disagree/correction",
      'you said "chosen cascade" but the cascade should be constructed not chosen'),
    A(f"{T}-r-0", "reply-mark", "retract/correction",
      '("the cascade should be constructed not chosen") You are right',
      answers=[(f"{T}-f-s1-0", '("the cascade should be constructed not chosen") You are right', "closes")]),
    A(f"{T}-r-1", "reply-mark", "report/evidence",
      "(what the click does today) I read the construction code",
      answers=[(f"{T}-f-s1-0", "(what the click does today)", "closes")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "propose/next-step",
      "(where the giant cascade enters construction)",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "report/done",
      '(wording fixed) The view\'s code and one test name said "chosen"',
      answers=[(f"{T}-f-s1-0", "(wording fixed)", "closes")]),
    A(f"{T}-r-4", "reply-mark", "ask-action/two-decisions",
      "(two decisions)",
      conf="medium"),
])
