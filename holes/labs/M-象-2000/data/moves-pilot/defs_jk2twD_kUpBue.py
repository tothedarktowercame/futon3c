import sys
sys.path.insert(0, ".")
from build import build
from defs_34pFgV import A

T = "claude-11-turn-45"
build("turn-jk2twD", [
    A(f"{T}-f-s1-0", "report-problem", "report-problem/possible-misalignment",
      "you may be on a different page from me about the goal here.",
      note="reply has no marked paragraphs; only fragment acts"),
    A(f"{T}-f-s2-0", "clarify", "clarify/scope-correction",
      "we are focusing on the C++ code, which isn't \"some\" but all of the path to compiling a blank lean file."),
    A(f"{T}-f-s2-1", "disagree", "disagree/correction",
      "which isn't \"some\" but all of the path to compiling a blank lean file."),
])

T = "claude-1-turn-174"
build("turn-kUpBue", [
    A(f"{T}-f-s1-0", "defer", "defer/until",
      "We can restart once we have sorted out this business about critical parameters"),
    A(f"{T}-f-s2-0", "propose", "propose/model-grounding",
      "each variable in the Lean model needs an interpretation in terms of real countable things"),
    A(f"{T}-f-s2-1", "constrain", "constrain/standing-rule",
      "the requirement should be that all countable things need to be taken into account at some point in the model in a meaningful way, or we have a fa",
      note="reply has no marked paragraphs; only fragment acts"),
])
