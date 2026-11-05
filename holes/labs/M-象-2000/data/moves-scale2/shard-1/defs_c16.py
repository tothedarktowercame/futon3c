import sys
sys.path.insert(0, ".")
from sbuild import build, A

T = "claude-17-turn-428"
build("turn-WpMWo2", [
    A(f"{T}-f-s1-0", "continue", "approve/plain",
      "OK, let's assume it will complete eventually!"),
    A(f"{T}-f-s2-0", "report", "report/status-update",
      "the extended/new Minard diagram is not that different nor contains many surprises."),
    A(f"{T}-f-s3-0", "explore", "explore/hypothesis",
      'Possibly an increased emphasis on "assurance"'),
    A(f"{T}-f-s4-0", "propose", "propose/directive",
      "what would be useful would be a further-detail analysis"),
    A(f"{T}-r-0", "reply-mark", "report/evidence",
      "Gist: Yes, your guess holds.",
      answers=[(f"{T}-f-s3-0", "Gist: Yes, your guess holds.", "closes")]),
    A(f"{T}-r-1", "reply-mark", "report/evidence",
      "(families, 22 Aug–21 Sep vs 21 Sep–1 Oct, share of each window's hits)",
      answers=[(f"{T}-f-s4-0", "(families, 22 Aug–21 Sep vs 21 Sep–1 Oct, share of each window's hits)", "closes")],
      conf="low",
      note="heading-only paragraph over a table"),
    A(f"{T}-r-2", "reply-mark", "qualify/caveat",
      "(how far this goes)",
      answers=[(f"{T}-f-s4-0", "(how far this goes)", "keeps")],
      conf="medium"),
])

T = "claude-17-turn-383"
build("turn-XwITxi", [
    A(f"{T}-f-s1-0", "explore", "explore/hypothesis",
      "I guess that's the risk of batching things."),
    A(f"{T}-f-s2-0", "propose", "propose/tentative",
      "We could give randomly assigned turns in the batches"),
    A(f"{T}-f-s3-0", "report", "report/uncertainty",
      "I don'tk now what to do about this."),
    A(f"{T}-r-0", "reply-mark", "report/diagnosis",
      "Gist: I don't think batching is the cause.",
      answers=[(f"{T}-f-s1-0", "Gist: I don't think batching is the cause.", "closes"),
               (f"{T}-f-s3-0", "My suggestion is to keep context and record the repetition", "closes")]),
    A(f"{T}-r-1", "reply-mark", "explain/mechanism",
      "(why not batching) The loop picks turns oldest first",
      answers=[(f"{T}-f-s1-0", "(why not batching)", "closes")]),
    A(f"{T}-r-2", "reply-mark", "report/diagnosis",
      "(what the repeats mean) When kimi-16 cites",
      answers=[(f"{T}-f-s3-0", "(what the repeats mean)", "keeps")],
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "propose/next-step",
      "(what I'd do)",
      conf="medium"),
    A(f"{T}-r-4", "reply-mark", "ask-action/one-decision",
      "(one decision) Shall I add `context_patterns`",
      conf="medium"),
])

T = "claude-17-turn-512"
build("turn-YrgOAz", [
    A(f"{T}-f-s1-0", "approve", "approve/plain", "Yes, please bell that out."),
    A(f"{T}-f-s2-0", "ask-action", "delegate/investigate",
      "please investigate line 2651 and surrounding context"),
    A(f"{T}-f-s3-0", "report", "report/context", "QUOTE"),
    A(f"{T}-r-0", "reply-mark", "report/case-study",
      "(timeline from 象's record `~/.emacs-graph/session-turn-analysis/turn-GYGJ2e.json`)",
      answers=[(f"{T}-f-s2-0", "(timeline from 象's record", "keeps")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/diagnosis",
      "(what's wrong with how it showed up)",
      answers=[(f"{T}-f-s2-0", "(what's wrong with how it showed up)", "keeps")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/dispatched",
      "(the bells)",
      answers=[(f"{T}-f-s1-0", "(the bells)", "closes")],
      conf="medium"),
])
