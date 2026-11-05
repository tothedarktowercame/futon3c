import sys
sys.path.insert(0, ".")
from sbuild import build, A

T = "claude-17-turn-525"
build("turn-86oO5M", [
    A(f"{T}-f-s1-0", "ask-action", "ask-action/commission",
      "Can you push the futon3c and element-web-futon repositories?"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "(pushing both repos) I've pushed both repos.",
      answers=[(f"{T}-f-s1-0", "I've pushed both repos.", "closes")]),
    A(f"{T}-r-1", "reply-mark", "qualify/caveat",
      "(what wasn't pushed) futon3c has an untracked file",
      answers=[(f"{T}-f-s1-0", "(what wasn't pushed) futon3c has an untracked file", "keeps")],
      conf="medium"),
])

T = "claude-17-turn-506"
build("turn-8aP916", [
    A(f"{T}-f-s1-0", "propose", "propose/tentative",
      'I think those could still be carried out in "self-talk".'),
    A(f"{T}-f-s3-0", "explain", "explain/example",
      'I can "clarify" something based on investigating its properties.'),
    A(f"{T}-f-s4-0", "explore", "explore/analogy",
      "the operator turns here are just an elaborate self-talk"),
    A(f"{T}-r-0", "reply-mark", "approve/with-amendment",
      "(self-talk) Taking that seriously fills most of the gap.",
      answers=[(f"{T}-f-s1-0", "(self-talk) Taking that seriously fills most of the gap.", "closes")],
      opens="self-talk reading",
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/with-options",
      "(how the uncovered nodes might read as self-talk)",
      answers=[(f"{T}-f-s1-0", "(how the uncovered nodes might read as self-talk)", "keeps")],
      within="self-talk reading",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "explain/mechanism",
      "(your operator turns as overheard self-talk) This fits the D3 ruling",
      answers=[(f"{T}-f-s4-0", "(your operator turns as overheard self-talk)", "keeps")],
      within="self-talk reading",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "propose/design",
      "(what this changes in the table) The acts file already records who an act is addressed to",
      answers=[(f"{T}-f-s1-0", "(what this changes in the table)", "keeps")],
      within="self-talk reading",
      conf="medium"),
    A(f"{T}-r-4", "reply-mark", "ask-action/one-decision",
      "(one decision) Shall I add a self-talk section",
      answers=[(f"{T}-f-s1-0", "(one decision) Shall I add a self-talk section", "keeps")],
      within="self-talk reading",
      conf="medium"),
])

T = "claude-17-turn-507"
build("turn-9RyYna", [
    A(f"{T}-f-s1-0", "explain", "explain/analogy",
      'Regarding "🈸 is the exception", that\'s similar to the War Machine too'),
    A(f"{T}-f-s2-0", "clarify", "clarify/repeat-question",
      "Same question as your 🈸."),
    A(f"{T}-f-s3-0", "approve", "approve/plain",
      'As to your "one decision", yes I approve.'),
    A(f"{T}-r-0", "reply-mark", "report/diagnosis",
      "extend at R17 doesn't hold.",
      answers=[(f"{T}-f-s1-0", "extend at R17 doesn't hold.", "keeps")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "verify/checked-live",
      '(what "holds up" means) For each candidate I opened the function',
      answers=[(f"{T}-f-s1-0", '(what "holds up" means)', "keeps")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/evidence",
      "(the ones that didn't hold)",
      answers=[(f"{T}-f-s2-0", "(the ones that didn't hold)", "keeps")],
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "report/gap",
      "(still open) Last turn's question about correcting the R7 row",
      answers=[(f"{T}-f-s3-0", "(still open) Last turn's question", "keeps")],
      conf="medium"),
])
