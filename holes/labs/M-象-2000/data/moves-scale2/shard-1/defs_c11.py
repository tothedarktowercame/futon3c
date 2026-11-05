import sys
sys.path.insert(0, ".")
from sbuild import build, A

T = "claude-17-turn-469"
build("turn-NURgPg", [
    A(f"{T}-f-s1-0", "clarify", "clarify/ask-decision",
      "Do we have enough words that we could start tagging my turns deterministically"),
    A(f"{T}-r-0", "reply-mark", "report/evidence",
      "(enough words to start?) Yes for a provisional first pass",
      answers=[(f"{T}-f-s1-0", "(enough words to start?) Yes for a provisional first pass", "closes")],
      opens="rnode deterministic tagging",
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/design",
      "(deterministic tagging) Tag with the specific cues only",
      answers=[(f"{T}-f-s1-0", "(deterministic tagging) Tag with the specific cues only", "closes")],
      within="rnode deterministic tagging",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "propose/next-step",
      "(order) I'd do this as two small handoffs",
      answers=[(f"{T}-f-s1-0", "(order)", "keeps")],
      within="rnode deterministic tagging",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "ask-action/one-decision",
      "(go-ahead) Shall I start with the deterministic layer?",
      answers=[(f"{T}-f-s1-0", "(go-ahead)", "keeps")],
      within="rnode deterministic tagging",
      conf="medium"),
])

T = "claude-17-turn-517"
build("turn-NodQbx", [
    A(f"{T}-f-s1-0", "ask-action", "ask-action/commission",
      "please make commits including any updates to M-象-2000 and push to github"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "(committed and pushed) I've updated M-象-2000",
      answers=[(f"{T}-f-s1-0", "(committed and pushed) I've updated M-象-2000", "closes")]),
    A(f"{T}-r-1", "reply-mark", "report/done",
      "(what the M-象-2000 update says) I added three entries",
      answers=[(f"{T}-f-s1-0", "(what the M-象-2000 update says)", "closes")],
      conf="medium"),
])

T = "claude-17-turn-473"
build("turn-O2SDeG", [
    A(f"{T}-f-s1-0", "ask-action", "ask-action/commission",
      "for the moment can you make all R-node terms in my turns red so I notice them?"),
    A(f"{T}-f-s2-0", "extend", "ask-action/example-request",
      "Give me an example as well for testing purposes."),
    A(f"{T}-f-s3-0", "constrain", "constrain/for-now",
      'It should work the same way as underlining'),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "(details) The 112 existing tags in this buffer have switched to red",
      answers=[(f"{T}-f-s1-0", "(details) The 112 existing tags", "closes")],
      conf="medium"),
])
