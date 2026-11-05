import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-18-turn-94"
build("turn-7ThP4r", [
    A(f"{T}-f-s1-0", "ask-action", "ask-action/commission",
      "OK, let's reset topo1 with some more problems"),
])

T = "claude-11-turn-17"
build("turn-7jqJgO", [
    A(f"{T}-f-s1-0", "explore", "collect/ask-option",
      "given the procedural nature of task i think possibly any agent could do it, what do you think?"),
    A(f"{T}-r-0", "reply-mark", "approve/with-amendment",
      '(gist) I take "the task" to mean resuming the mfuton row loop',
      answers=[(f"{T}-f-s1-0", '(gist) I take "the task" to mean', "keeps")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "explain/mechanism",
      "(why the lanes can go to any agent) The preamble now turns a lane into a fixed procedure",
      answers=[(f"{T}-f-s1-0", "(why the lanes can go to any agent)", "closes")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "propose/next-step",
      "(how to find out cheaply) Give one discovery packet under the draft preamble",
      answers=[(f"{T}-f-s1-0", "(how to find out cheaply)", "closes")],
      conf="medium"),
])

T = "claude-11-turn-71"
build("turn-7mgeed", [
    A(f"{T}-f-s1-0", "prioritize", "propose/directive",
      "Let's start with the first item."),
    A(f"{T}-f-s2-0", "propose", "propose/tentative",
      "I know our first target isn't Mathlib",
      conf="medium"),
])
