import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-11-turn-60"
build("turn-76Ae9x", [
    A(f"{T}-f-s1-0", "report", "constrain/stand-rule",
      "Rob said hecprefers direct push to PR, and tothedarktowercame should be a contributor",
      opens="direct push, no PRs",
      note="records Rob's standing collaboration preference as a rule for this work"),
    A(f"{T}-f-s1-1", "verify", "clarify/ask-decision",
      "please clarify?",
      within=f"{T}-f-s1-0"),
])

T = "claude-11-turn-82"
build("turn-77E8E0", [
    A(f"{T}-f-s1-0", "qualify", "qualify/caveat",
      "this can show us what success looks like"),
    A(f"{T}-f-s2-0", "propose", "propose/directive",
      "most of Lean's parser is written in correct Lean, so we should be able to port it to Python",
      within=f"{T}-f-s1-0"),
])

T = "claude-18-turn-111"
build("turn-7LdVhV", [
    A(f"{T}-f-s1-0", "ask-action", "ask-action/commission",
      "Let's push the futon3c"),
])
