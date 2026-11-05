import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-18-turn-106"
build("turn-HBUcH2", [
    A(f"{T}-f-s1-0", "report", "report-problem/retrospective",
      "it is pretty remarkable that we didn't close *any* topology problems"),
    A(f"{T}-f-s1-1", "approve", "qualify/caveat",
      "we did discover some where the bookkeeping hadn't been done",
      within=f"{T}-f-s1-0"),
    A(f"{T}-f-s2-0", "propose", "propose/tentative",
      "if we do this again we'd get down to a residual of \"hard problems\"",
      within=f"{T}-f-s1-0"),
    A(f"{T}-f-s2-1", "propose", "propose/alternative",
      "particularly if we drop your 20-attempt rule to, say, 10.",
      within=f"{T}-f-s2-0"),
])

T = "claude-12-turn-156"
build("turn-Hvqkdf", [
    A(f"{T}-f-s1-0", "report-problem", "report-problem/observed",
      "So, this is a bit of a discrepancy."),
    A(f"{T}-f-s2-0", "challenge", "challenge/other",
      "You're able to do expansive thinking, but only when it suits you",
      within=f"{T}-f-s1-0"),
    A(f"{T}-f-s3-0", "explain", "explain/interpretation",
      "you're not actually combining expansive with critical thinking.",
      within=f"{T}-f-s2-0"),
    A(f"{T}-f-s4-0", "explore", "explore/hypothesis",
      "If I'm asleep, maybe there's no critical facility around here.",
      within=f"{T}-f-s3-0"),
    A(f"{T}-f-s5-0", "challenge", "challenge/other",
      'You\'re suggesting as much when you said "question for Joe".',
      within=f"{T}-f-s1-0"),
    A(f"{T}-f-s6-0", "challenge", "challenge/other",
      "But then you ran the machine anyway.",
      within=f"{T}-f-s1-0"),
])

T = "claude-18-turn-78"
build("turn-IMPmwr", [
    A(f"{T}-f-s1-0", "extend", "report-problem/retrospective",
      'the APM machine was worse than just weakening statements'),
    A(f"{T}-f-s1-1", "explain", "explain/interpretation",
      'it was built on a "red tape"; it kept failing, breaking down',
      within=f"{T}-f-s1-0"),
])
