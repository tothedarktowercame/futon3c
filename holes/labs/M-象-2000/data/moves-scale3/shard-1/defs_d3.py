import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-11-turn-99"
build("turn-1mZPHN", [
    A(f"{T}-f-s1-0", "propose", "propose/directive",
      "this is a lot of design patterns which would perhaps go a bit cleverly into library/parsing",
      within="claude-11-turn-2-f-s1-0"),
    A(f"{T}-f-s2-0", "explain", "explain/reflection",
      'What\'s interesting I think about the "four useful things"'),
    A(f"{T}-f-s3-0", "explain", "explain/reflection",
      'Once I had found the "comparator" I figured everything could be mechanical.'),
    A(f"{T}-f-s4-0", "qualify", "qualify/caveat",
      "that itself turned out to be  a precession of simulacra for the real solution.",
      conf="medium"),
])

T = "claude-11-turn-92"
build("turn-1relYm", [
    A(f"{T}-f-s1-0", "qualify", "clarify/condition",
      "if the formatter bug reproduces on an actual-current version of Lean that would be worth reporting",
      conf="medium"),
    A(f"{T}-f-s1-1", "challenge", "challenge/other",
      "but 4.29 is kind of old at the moment"),
])

T = "claude-11-turn-13"
build("turn-2Vu8GA", [
    A(f"{T}-f-s1-0", "redirect", "propose/redirect-scope",
      "the other thing to look at is the claude-4 instructions to itself.",
      within="claude-11-turn-2-f-s1-0"),
    A(f"{T}-f-s2-0", "qualify", "approve/plain",
      "if we got to 21 rows that is real progress and the new method works!"),
])
