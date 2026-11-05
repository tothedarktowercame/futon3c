import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-11-turn-38"
build("turn-8X7BbX", [
    A(f"{T}-f-s1-0", "verify", "clarify/ask-decision",
      "so are we doing every efficiency enhancement noted so far?"),
])

T = "claude-11-turn-93"
build("turn-8mX1Ks", [
    A(f"{T}-f-s1-0", "qualify", "clarify/context",
      "rc3 is still a pinned version"),
    A(f"{T}-f-s1-1", "propose", "propose/directive",
      "we should be able to check by reading the code whether it is fixed in their HEAD",
      within=f"{T}-f-s1-0"),
])

T = "claude-12-turn-131"
build("turn-968NVf", [
    A(f"{T}-f-s1-0", "challenge", "challenge/other",
      "i feel as though you are approaching this without much seriousness"),
])
