import sys
sys.path.insert(0, ".")
from sbuild import build, A

T = "claude-17-turn-542"
build("turn-wSIRlv", [
    A(f"{T}-f-s1-0", "verify", "verify/test-turn",
      "session-mode-turn-recorder configured, testing again"),
    A(f"{T}-r-0", "reply-mark", "report/diagnosis",
      "(your second test) Your turn reached the JVM and became evidence",
      answers=[(f"{T}-f-s1-0", "(your second test) Your turn reached the JVM", "keeps")]),
    A(f"{T}-r-1", "reply-mark", "report/diagnosis",
      "(what went wrong) Two separate problems:",
      answers=[(f"{T}-f-s1-0", "(what went wrong) Two separate problems:", "closes")]),
    A(f"{T}-r-2", "reply-mark", "report/done",
      "(fix, `b6cd9da4`) A turn that isn't sent to 象 straight away is now drafted",
      answers=[(f"{T}-f-s1-0", "(fix, `b6cd9da4`)", "closes")]),
    A(f"{T}-r-3", "reply-mark", "verify/checked-live",
      "(your first test) The file-path turn worked end to end",
      conf="medium"),
    A(f"{T}-r-4", "reply-mark", "propose/next-step",
      "(what to watch now) When this reply ends",
      conf="medium"),
    A(f"{T}-r-5", "reply-mark", "report/gap",
      "(still open) Neither path records the operator turn's evidence id",
      conf="medium"),
])

T = "claude-17-turn-451"
build("turn-zSV0Hd", [
    A(f"{T}-f-s1-0", "explain", "report/context",
      "the R-numbers themselves are directly mapped to patterns in plop-2026"),
    A(f"{T}-f-s1-1", "propose", "propose/tentative",
      "we could come at them via yet another method if they are in the giant connected component"),
    A(f"{T}-r-0", "reply-mark", "report/status",
      "(what the graph has) The graph is `storage/operator-turns/mined-pattern-graph.json`",
      answers=[(f"{T}-f-s1-1", "(what the graph has)", "keeps")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/design",
      "(how the third route would work) For each sampled pattern",
      answers=[(f"{T}-f-s1-1", "(how the third route would work)", "closes")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "qualify/caveat",
      "(gaps the graph can't cover) R1 and R7 sit outside the giant component",
      answers=[(f"{T}-f-s1-1", "(gaps the graph can't cover)", "keeps")],
      conf="medium"),
])
