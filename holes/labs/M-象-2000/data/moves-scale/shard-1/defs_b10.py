import sys
sys.path.insert(0, ".")
from sbuild import build, A

T = "claude-17-turn-5"
build("turn-XE3XHH", [
    A(f"{T}-f-s1-0", "report-problem", "report-problem/observed",
      "象 is still much slower than you on your turns."),
    A(f"{T}-f-s2-0", "propose", "propose/alternative",
      "what if we develop a fully-classical path and run 象 in the background"),
    A(f"{T}-r-0", "reply-mark", "approve/with-plan",
      "Yes, and most of it already exists.",
      answers=[(f"{T}-f-s2-0", "Yes, and most of it already exists.", "closes"),
               (f"{T}-f-s1-0", "The classical 小象 draft takes about 0.1 s", "closes")],
      opens="classical path first"),
    A(f"{T}-r-1", "reply-mark", "report/evidence",
      "Your last three turns since you switched back to `jvm`:",
      answers=[(f"{T}-f-s1-0", "**Draft:** landed within 1 s of the send each time.", "closes")],
      within="classical path first"),
    A(f"{T}-r-2", "reply-mark", "report/design",
      "**Stepper shows drafts.**",
      answers=[(f"{T}-f-s2-0", "**Stepper shows drafts.**", "keeps")],
      within="classical path first"),
    A(f"{T}-r-3", "reply-mark", "ask-action/one-decision",
      "Is the two-entry evidence shape, a classical entry and then 象's update, all right with you?",
      answers=[(f"{T}-f-s2-0", "Is the two-entry evidence shape", "keeps")],
      within="classical path first",
      conf="medium"),
])

T = "claude-17-turn-437"
build("turn-ZKoe7v", [
    A(f"{T}-f-s1-0", "verify", "verify/confirmed", "OK, I see it on the clickthrough."),
    A(f"{T}-f-s2-0", "propose", "propose/consolidate-ui",
      "it would be much nicer if this was shown on ONE interactive infographic."),
    A(f"{T}-f-s3-0", "delegate", "delegate/to-agent-with-tool",
      "Do you think Kimi could sort that out using Playwright to validate?"),
    A(f"{T}-r-0", "reply-mark", "propose/acceptance-plan",
      "Kimi has to write a Playwright check that loads the page in headless Chromium",
      answers=[(f"{T}-f-s3-0", "Kimi has to write a Playwright check that loads the page in headless Chromium", "closes")],
      opens="interactive infographic page",
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/process-note",
      "My first send was refused because Kimi seats need a requisition line naming a mission.",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/dispatched",
      "(tracking) Job `invoke-1790893029289-29903-dcce93af`",
      within="interactive infographic page",
      conf="medium"),
])

T = "claude-17-turn-390"
build("turn-bSVU00", [
    A(f"{T}-f-s1-0", "explain", "explain/feasibility",
      "building the correspondence from 象 markup of my turns to the structured agent reply"),
    A(f"{T}-f-s2-0", "explain", "explain/example",
      '"🈸:yes" would be very easy to code.'),
    A(f"{T}-f-s3-0", "propose", "propose/tooling",
      "we could add Emacs bindings for those symbols, or better yet a Hydra"),
    A(f"{T}-f-s4-0", "redirect", "propose/redirect-scope",
      "the computational challenge seems to be threading all of this"),
    A(f"{T}-f-s5-0", "propose", "propose/cross-pollinate",
      "we could get some inspiration from the War Machine work and maybe also contribute something back."),
    A(f"{T}-f-s7-0", "explore", "explore/open-question",
      "how does the War Machine make sense of incremental progress?"),
    A(f"{T}-f-s8-0", "explain", "explain/hypothesis",
      "the War Machine has a *little* room for emergent learning"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "It's loaded in your running Emacs now.",
      answers=[(f"{T}-f-s3-0", "It's loaded in your running Emacs now.", "closes"),
               (f"{T}-f-s2-0", "(the menu, commit 73bb40f7) It's loaded in your running Emacs now.", "closes")],
      opens="mark hydra and session ledger"),
    A(f"{T}-r-1", "reply-mark", "report/design",
      "A reply to the turn before it is only the simplest case.",
      answers=[(f"{T}-f-s4-0", "A reply to the turn before it is only the simplest case.", "closes")],
      within="mark hydra and session ledger"),
])
