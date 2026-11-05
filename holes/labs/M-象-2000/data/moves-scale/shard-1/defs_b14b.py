import sys
sys.path.insert(0, ".")
from sbuild import build, A

T = "claude-17-turn-6"
build("turn-oTGuOV", [
    A(f"{T}-f-s1-0", "report", "report/quoted-ask",
      '"Shall I send that to kimi-1?"'),
    A(f"{T}-f-s2-0", "approve", "approve/plain", "yes;"),
    A(f"{T}-f-s2-1", "propose", "propose/optimisation",
      "we could still orchestrate things so that 象 is as fast as possible."),
    A(f"{T}-f-s4-0", "propose", "propose/division-of-labour",
      "小象 could work in the .json and then 象 would be the one to persist the finalised annotation."),
    A(f"{T}-f-s5-0", "explain", "explain/justification",
      "Yes there are 2 packets here but it still makes sense."),
    A(f"{T}-r-0", "reply-mark", "report/dispatched",
      "(sent to kimi-1) It's with kimi-1, set up the way you describe.",
      answers=[(f"{T}-f-s2-0", "It's with kimi-1, set up the way you describe.", "closes")],
      opens="provisional draft pipeline"),
    A(f"{T}-r-1", "reply-mark", "report/design",
      "(what the packet asks for)",
      answers=[(f"{T}-f-s4-0", "(what the packet asks for)", "keeps")],
      within="provisional draft pipeline",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/status",
      "(象 is already working from the draft) The 象 brief already gives 象 小象's draft",
      answers=[(f"{T}-f-s4-0", "(象 is already working from the draft) The 象 brief already gives 象 小象's draft", "closes")],
      within="provisional draft pipeline"),
    A(f"{T}-r-3", "reply-mark", "propose/next-step",
      "(when it lands) I'll review it, load the stepper code into your Emacs",
      answers=[(f"{T}-f-s2-1", "Your newest turn should then show up within about a second of sending.", "keeps")],
      within="provisional draft pipeline"),
])

T = "claude-17-turn-460"
build("turn-pK5idM", [
    A(f"{T}-f-s1-0", "ask-action", "ask-action/generate",
      "let's *generate* a set of keywords that we put into a fully-disambiguated tree."),
    A(f"{T}-f-s2-0", "delegate", "delegate/prime-then-elaborate",
      'You can do that as "priming data" and then we can ask a few other agents to elaborate'),
    A(f"{T}-r-0", "reply-mark", "report/design",
      "(structure) Under System 4, the Understanding",
      answers=[(f"{T}-f-s1-0", "(structure) Under System 4", "keeps")],
      opens="keyword tree",
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/design",
      "(checker) `check_tree.py` verifies four things:",
      answers=[(f"{T}-f-s1-0", "(checker) `check_tree.py` verifies four things:", "keeps")],
      within="keyword tree"),
    A(f"{T}-r-2", "reply-mark", "ask-action/one-decision",
      "(go-ahead) Shall I dispatch that round?",
      answers=[(f"{T}-f-s2-0", "(go-ahead) Shall I dispatch that round?", "keeps")],
      within="keyword tree"),
])
