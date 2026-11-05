import sys
sys.path.insert(0, ".")
from sbuild import build, A

T = "claude-17-turn-48"
build("turn-07igHH", [
    A(f"{T}-f-s1-0", "ask-action", "ask-action/look-at-buffer",
      "OK before we do that: look at the bottom of the  claude-6 REPL."),
    A(f"{T}-f-s2-0", "explain", "explain/justification",
      "we need the features we are building, and not just for me"),
    A(f"{T}-r-0", "reply-mark", "approve/plain",
      "Yes. The claude-6 case is exactly what the ledger is for",
      answers=[(f"{T}-f-s2-0", "Yes. The claude-6 case is exactly what the ledger is for", "closes"),
               (f"{T}-f-s1-0", "The claude-6 case is exactly what the ledger is for", "keeps")],
      opens="bells as turns"),
    A(f"{T}-r-1", "reply-mark", "explain/mechanism",
      "(how the claude-6 case would look with that in place)",
      answers=[(f"{T}-f-s1-0", "(how the claude-6 case would look with that in place)", "keeps")],
      within="bells as turns",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "propose/next-step",
      "(order of work)",
      answers=[(f"{T}-f-s2-0", "(order of work)", "keeps")],
      within="bells as turns",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "ask-action/one-decision",
      "(one decision) Shall I start on step 1's discovery pass now",
      answers=[(f"{T}-f-s2-0", "(one decision) Shall I start on step 1's discovery pass now", "keeps")],
      within="bells as turns",
      conf="medium"),
])

T = "claude-17-turn-485"
build("turn-1XWJmJ", [
    A(f"{T}-f-s1-0", "propose", "propose/framing",
      "what we have done is a relatively arduous version of MAP."),
    A(f"{T}-f-s2-0", "report", "report/status-update",
      "The IDENTIFY gap was real and attested to already."),
    A(f"{T}-f-s3-0", "explain", "explain/interpretation",
      'What we\'ve done by developing this operator-facing language is try to show "what\'s there already".'),
    A(f"{T}-f-s4-0", "explain", "explain/metaphor",
      "We needed some hard-won red pixels to do that."),
    A(f"{T}-r-0", "reply-mark", "approve/plain",
      "(MAP framing) I agree, and I've written it into the mission that way",
      answers=[(f"{T}-f-s1-0", "(MAP framing) I agree", "closes"),
               (f"{T}-f-s3-0", "(MAP framing) I agree", "keeps")],
      opens="MAP section",
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/evidence",
      "(contents of the section)",
      answers=[(f"{T}-f-s1-0", "(contents of the section)", "keeps")],
      within="MAP section",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/status",
      "(where this leaves IDENTIFY's work items) None of the eight work items has been completed.",
      answers=[(f"{T}-f-s2-0", "(where this leaves IDENTIFY's work items)", "closes")],
      within="MAP section",
      conf="medium"),
])
