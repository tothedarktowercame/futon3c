import sys
sys.path.insert(0, ".")
from sbuild import build, A

T = "claude-17-turn-3@turn-aihw20"
build("turn-aihw20", [
    A(f"{T}-f-s1-0", "extend", "propose/extend",
      "Probably the others should be added the same way"),
    A(f"{T}-f-s1-1", "propose", "propose/directive",
      "also to the futon-hot reloaded in case we work on them live during this session"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "(the other 象 files) Done. `futon-config.el` now requires `xiang-trace`",
      answers=[(f"{T}-f-s1-0", "(the other 象 files) Done.", "closes"),
               (f"{T}-f-s1-1", "All three files (including `turn-stepper`) are on the hot-reload list", "closes")]),
    A(f"{T}-r-1", "reply-mark", "verify/checked-live",
      "(checked)",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/gap",
      "(a likely false alarm)",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "report/gap",
      "(still open) The recorder is still `files` after the restart",
      conf="medium"),
])

T = "claude-17-turn-51"
build("turn-dMgY2U", [
    A(f"{T}-f-s1-0", "clarify", "clarify/check-understanding",
      "i would imagine the ask was marked up correctly in an earlier claude round..."),
    A(f"{T}-f-s2-0", "explain", "explain/interpretation",
      "still waiting is a reminder about an earlier ask."),
    A(f"{T}-r-0", "reply-mark", "approve/with-amendment",
      "I agree that's what 🈳 means here.",
      answers=[(f"{T}-f-s2-0", "I agree that's what 🈳 means here.", "closes")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/diagnosis",
      "(what that shows) Most of the failure is at the point where the ask is made.",
      answers=[(f"{T}-f-s1-0", "(what that shows) Most of the failure", "keeps")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "ask-action/one-decision",
      "(one decision) Shall I make both changes?",
      conf="medium"),
])

T = "claude-17-turn-478"
build("turn-es3xLj", [
    A(f"{T}-f-s1-0", "approve", "approve/plain", "OK, that's good."),
    A(f"{T}-f-s2-0", "constrain", "constrain/standing-rule",
      "it's better if I *don't* know what the keywords are so that I'm not biased towards them."),
    A(f"{T}-f-s3-0", "prioritize", "prioritize/benefit",
      'we need to do is start to develop the strategy for 象 to look for "more words like this".'),
    A(f"{T}-f-s4-0", "propose", "propose/tentative",
      "we could do is create a Wittgenstein-style definition for the R-nodes,"),
    A(f"{T}-f-s4-1", "qualify", "qualify/caveat",
      "we'd also want to be careful that we're capturing the actual Friston/AIF meaning"),
    A(f"{T}-f-s5-0", "explain", "report/context",
      "The old glossary.tex would have *technical* definitions for AIF keywords in it"),
    A(f"{T}-r-0", "reply-mark", "report/design",
      '(a strategy for "more words like this") I\'d give each R-node a two-part definition',
      answers=[(f"{T}-f-s3-0", '(a strategy for "more words like this")', "closes"),
               (f"{T}-f-s4-0", "I'd give each R-node a two-part definition", "closes")],
      opens="rnode definitions",
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "propose/next-step",
      "(next step) I'd draft `rnode-definitions.edn`",
      answers=[(f"{T}-f-s5-0", "the glossary paragraph it rests on", "closes")],
      within="rnode definitions",
      conf="medium"),
])
