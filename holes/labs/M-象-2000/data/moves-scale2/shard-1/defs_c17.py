import sys
sys.path.insert(0, ".")
from sbuild import build, A

T = "claude-17-turn-533"
build("turn-ZUDPj4", [
    A(f"{T}-f-s1-0", "report-problem", "report-problem/observed",
      "On turn-stepper: OK, I see it but it is currently far out of date."),
    A(f"{T}-f-s2-0", "report", "report/observed-output",
      "It says frame 182 / 182, with a date that's 2 days ago"),
    A(f"{T}-r-0", "reply-mark", "report/diagnosis",
      "(cause and fix) `M-x turn-stepper` opened from its cache",
      answers=[(f"{T}-f-s1-0", "(cause and fix)", "closes"),
               (f"{T}-f-s2-0", "(cause and fix)", "closes")]),
    A(f"{T}-r-1", "reply-mark", "explain/mechanism",
      "(why 281, not 286) The stepper leaves out your newest turns",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/gap",
      "(still not pushed) futon3c master is now 3 commits ahead",
      conf="medium"),
])

T = "claude-17-turn-414"
build("turn-ZXmbOe", [
    A(f"{T}-f-s1-0", "agree", "approve/plain",
      "I think we have now hardened futon1b a good amount"),
    A(f"{T}-f-s2-0", "ask-action", "ask-action/one-decision",
      "we're pretty much ready, in this case, to do the Minard analysis on the full window?"),
    A(f"{T}-r-0", "reply-mark", "approve/with-amendment",
      "Gist: Almost.",
      answers=[(f"{T}-f-s2-0", "Gist: Almost.", "keeps")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "qualify/caveat",
      '(on "without alarm bells") Today I measured single pages',
      answers=[(f"{T}-f-s1-0", '(on "without alarm bells")', "keeps")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/gap",
      "(what the extension needs besides the fetch)",
      answers=[(f"{T}-f-s2-0", "(what the extension needs besides the fetch)", "keeps")],
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "ask-action/one-decision",
      "(one decision) Shall I dispatch step 1?",
      answers=[(f"{T}-f-s2-0", "(one decision) Shall I dispatch step 1?", "keeps")],
      conf="medium"),
])

T = "claude-17-turn-53@turn-aFXxNu"
build("turn-aFXxNu", [
    A(f"{T}-f-s1-0", "ask-action", "ask-action/commission",
      "please do those 2 unresolved actions"),
    A(f"{T}-r-0", "reply-mark", "report/diagnosis",
      "(the second cause) Emacs only sent a turn's reply to the JVM",
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/done",
      "(backfill)",
      answers=[(f"{T}-f-s1-0", "(backfill)", "closes")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/dispatched",
      "(discovery) kimi-1 has the packet.",
      answers=[(f"{T}-f-s1-0", "(discovery) kimi-1 has the packet.", "closes")],
      conf="medium"),
])
