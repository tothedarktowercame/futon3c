import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-11-turn-7"
build("turn-9y7ov6", [
    A(f"{T}-f-s1-0", "approve", "approve/plain",
      "so I think we can delete the image."),
    A(f"{T}-r-0", "reply-mark", "report/diagnosis",
      '("https://zone.hyperreal.enterprises/voxterm/) Your message seems to stop',
      conf="medium"),
])

T = "claude-11-turn-54"
build("turn-ABYrM4", [
    A(f"{T}-f-s1-0", "challenge", "challenge/other",
      "what you are calling a parser written in Lean must be processed by something to bootstrap it"),
])

T = "claude-12-turn-147"
build("turn-AMDIbu", [
    A(f"{T}-f-s1-0", "challenge", "challenge/other",
      'the entire "claude" offering seems intent on wrecking this work'),
    A(f"{T}-f-s2-0", "retract", "retract/correction",
      "I thought it was OK because it was well specified but",
      within=f"{T}-f-s1-0"),
    A(f"{T}-f-s2-1", "constrain", "constrain/standing-rule",
      "for whatever reason only Codex can be trusted with it at this point in the build process.",
      opens="codex only for build work",
      within=f"{T}-f-s1-0"),
])
