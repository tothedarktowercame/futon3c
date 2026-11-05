import sys
sys.path.insert(0, ".")
from sbuild import build, A

T = "claude-17-turn-47@turn-1sNkzc"
build("turn-1sNkzc", [
    A(f"{T}-f-s1-0", "explain", "explain/expectation",
      "it would be relatively easy to close the ports on my requests"),
    A(f"{T}-f-s2-0", "ask-action", "approve/plain",
      "yes please fix/improve it and let me know whet it's ready for me to test"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "(what changed)",
      answers=[(f"{T}-f-s2-0", "(what changed)", "closes")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/diagnosis",
      "Ports are still computed from 象's reading, not 小象's, for now.",
      answers=[(f"{T}-f-s1-0", "Ports are still computed from 象's reading, not 小象's, for now.", "keeps")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "verify/checked-live",
      "(what I checked) Each fix has a test built from the three demo records.",
      answers=[(f"{T}-f-s2-0", "(what I checked) Each fix has a test built from the three demo records.", "closes")]),
    A(f"{T}-r-3", "reply-mark", "propose/next-step",
      "(expected this time)",
      answers=[(f"{T}-f-s2-0", "(expected this time)", "keeps")],
      conf="medium"),
])

T = "claude-17-turn-391"
build("turn-455grB", [
    A(f"{T}-f-s1-0", "redirect", "propose/redirect-scope",
      'your "ledger of open items" is effectively what Elephant-2000 is supposed to maintain'),
    A(f"{T}-f-s2-0", "report-problem", "report-problem/observed",
      "C-c ; says it isn't defined for me"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "Gist: C-c ; works in your buffer now.",
      answers=[(f"{T}-f-s2-0", "C-c ; works in your buffer now.", "closes")]),
    A(f"{T}-r-1", "reply-mark", "report/diagnosis",
      "(why the key was missing) Every other Claude REPL buffer already had C-c ;.",
      answers=[(f"{T}-f-s2-0", "(why the key was missing)", "keeps")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "approve/plain",
      "(Elephant-2000, not a new ledger) Agreed.",
      answers=[(f"{T}-f-s1-0", "(Elephant-2000, not a new ledger) Agreed.", "closes")],
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "ask-action/one-decision",
      "(one decision) Shall I read how P11's offer and agreement records get written",
      answers=[(f"{T}-f-s1-0", "(one decision) Shall I read how P11's", "keeps")],
      conf="medium"),
])

T = "claude-17-turn-489"
build("turn-4Y6qFF", [
    A(f"{T}-f-s1-0", "ask-action", "ask-action/commission",
      "Let's bring it onto master and try it out"),
    A(f"{T}-f-s2-0", "report", "report/context",
      'I seem to recall that we made some over the last couple of days'),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "(what I did)",
      answers=[(f"{T}-f-s1-0", "Fast-forwarded futon3c master", "closes")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/evidence",
      "(findings, in more detail)",
      answers=[(f"{T}-f-s2-0", "(findings, in more detail)", "keeps")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "qualify/caveat",
      "(limits)",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "propose/next-step",
      "(next) The step from here to the War Machine",
      answers=[(f"{T}-f-s1-0", "(next) The step from here to the War Machine", "keeps")],
      conf="medium"),
])
