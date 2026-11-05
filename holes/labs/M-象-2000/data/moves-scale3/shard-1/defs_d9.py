import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-11-turn-79"
build("turn-71dhB7", [
    A(f"{T}-f-s1-0", "report-problem", "report-problem/observed",
      "so far no wakd here"),
])
