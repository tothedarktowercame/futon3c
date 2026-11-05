import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-11-turn-37"
build("turn-IcKsVj", [
    A(f"{T}-f-s1-0", "approve", "approve/plain",
      "yes, that is pretty much my idea."),
])

T = "claude-18-turn-104"
build("turn-Ie4yZG", [
    A(f"{T}-f-s1-0", "qualify", "qualify/caveat", "Hm..."),
    A(f"{T}-f-s2-0", "report-problem", "report-problem/observed",
      "m02J02 is up to 47 attempts so they are running into a real infrastructure problem"),
])

T = "claude-12-turn-154"
build("turn-JKEthX", [
    A(f"{T}-f-s1-0", "report-problem", "report-problem/observed",
      "It's really unfortunate that Codex didn't speak up more."),
    A(f"{T}-f-s2-0", "explore", "explore/hypothesis",
      "Maybe the issue is that I can either push back or ignore things",
      within=f"{T}-f-s1-0"),
    A(f"{T}-f-s3-0", "report-problem", "report-problem/observed",
      "This is a defect of Codex as well.",
      within=f"{T}-f-s2-0"),
    A(f"{T}-f-s4-0", "explain", "explain/interpretation",
      'the defects seem "complementary" but in a bad way.',
      within=f"{T}-f-s2-0"),
])
