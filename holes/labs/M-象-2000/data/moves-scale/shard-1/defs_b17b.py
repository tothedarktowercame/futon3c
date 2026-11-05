import sys
sys.path.insert(0, ".")
from sbuild import build, A

T = "claude-17-turn-500"
build("turn-uy7gV9", [
    A(f"{T}-f-s1-0", "propose", "propose/applied-idea",
      "one applied idea where the small-causal diagram idea could be *practical*."),
    A(f"{T}-f-s2-0", "qualify", "qualify/caveat",
      "I know we are still trying to check whether it even works at all."),
    A(f"{T}-f-s3-0", "clarify", "clarify/scenario",
      'suppose we want a "policy for selecting the next M-E-T-A- task to work on"'),
    A(f"{T}-f-s4-0", "clarify", "clarify/status",
      "that's what codex-10 is currently writing."),
    A(f"{T}-f-s5-0", "propose", "propose/validate-early",
      "We could validate *that* with our Fong-inspired approach"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "(checking codex-10's META policy) This works, and it has already found a gap.",
      answers=[(f"{T}-f-s5-0", "(checking codex-10's META policy) This works", "closes"),
               (f"{T}-f-s2-0", "This works", "keeps"),
               (f"{T}-f-s3-0", "I read codex-10's draft", "keeps")],
      opens="meta policy causal check"),
    A(f"{T}-r-1", "reply-mark", "explain/mechanism",
      "(how) The models follow the draft's own `:arms` guards:",
      answers=[(f"{T}-f-s5-0", "(how) The models follow the draft's own `:arms` guards:", "keeps")],
      within="meta policy causal check"),
    A(f"{T}-r-2", "reply-mark", "explain/mechanism",
      "(why this is practical) The method checks a policy's structure before it is ever run:",
      answers=[(f"{T}-f-s1-0", "(why this is practical)", "keeps")],
      within="meta policy causal check"),
    A(f"{T}-r-3", "reply-mark", "qualify/caveat",
      "(limits) This checks only the structure of the selection",
      answers=[(f"{T}-f-s2-0", "(limits) This checks only the structure", "keeps")],
      within="meta policy causal check",
      conf="medium"),
    A(f"{T}-r-4", "reply-mark", "ask-action/one-decision",
      "(one decision) Shall I send codex-10 the missing-arm finding",
      answers=[(f"{T}-f-s5-0", "(one decision) Shall I send codex-10 the missing-arm finding", "keeps")],
      within="meta policy causal check"),
])

T = "claude-17-turn-406"
build("turn-x2RU5I", [
    A(f"{T}-f-s1-0", "report-problem", "report-problem/observed",
      "RETRACT still hasn't got an IBOL label in the legend."),
    A(f"{T}-f-s2-0", "propose", "propose/icon-choice",
      "There's an x with an up-to-the-right arrow that would work for RETRACT."),
    A(f"{T}-f-s3-0", "clarify", "clarify/locator",
      "In the actual Chipwits game it is the 4th icon on the 2nd row in the Operators panel."),
    A(f"{T}-r-0", "reply-mark", "report/diagnosis",
      "(where the icon came from) It's not on the 1984 sheets the other chips were cut from.",
      answers=[(f"{T}-f-s3-0", "(where the icon came from) It's not on the 1984 sheets", "keeps")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "verify/checked-live",
      "(what I checked) I rendered `feed.html` in headless Chromium",
      answers=[(f"{T}-f-s1-0", "(what I checked) I rendered `feed.html` in headless Chromium", "closes"),
               (f"{T}-f-s2-0", "the chip loads (HTTP 200)", "closes")]),
    A(f"{T}-r-2", "reply-mark", "qualify/caveat",
      '(labels still unchecked) As with the other three, "X AND ARROW" describes the picture',
      answers=[(f"{T}-f-s1-0", '(labels still unchecked) As with the other three, "X AND ARROW" describes the picture', "keeps")],
      conf="medium"),
])

T = "claude-17-turn-432"
build("turn-xOZ5oh", [
    A(f"{T}-f-s1-0", "verify", "verify/observed-output",
      "No faces shown on e.g."),
    A(f"{T}-f-s2-0", "report-problem", "report-problem/observed",
      "㊬  in *claude-repl:claude-17* at the moment"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "(what changed) Under turn-tags mode, marks are now coloured as they come into view",
      answers=[(f"{T}-f-s1-0", "(what changed) Under turn-tags mode, marks are now coloured", "closes"),
               (f"{T}-f-s2-0", "(what changed) Under turn-tags mode", "closes")]),
    A(f"{T}-r-1", "reply-mark", "verify/checked-live",
      "(test) A new test colours a sample line twice",
      answers=[(f"{T}-f-s1-0", "(test) A new test colours a sample line twice", "closes")],
      conf="medium"),
])
