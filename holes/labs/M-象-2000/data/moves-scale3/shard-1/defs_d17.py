import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-18-turn-89"
build("turn-BaIo42", [
    A(f"{T}-f-s1-0", "redirect", "propose/redirect-scope",
      'the original "solver" role card for APM was getting good results'),
    A(f"{T}-f-s2-0", "disagree", "explain/decline",
      "I'm a bit dubious about asceding to the request to check the m02J05",
      conf="medium"),
])

T = "claude-11-turn-55"
build("turn-Bb7G6A", [
    A(f"{T}-f-s1-0", "approve", "approve/plain",
      "ok, now we are getting somewhere."),
    A(f"{T}-f-s2-0", "clarify", "clarify/ask-decision",
      "what generates the C?"),
])

T = "claude-18-turn-115"
build("turn-CKNskM", [
    A(f"{T}-f-s1-0", "collect", "ask-action/status-check",
      "Any news back?"),
])
