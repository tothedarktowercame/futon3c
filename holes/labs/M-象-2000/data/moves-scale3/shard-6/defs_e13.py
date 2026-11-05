import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-17-turn-482"
build("turn-IY8vrs", [
    A(f"{T}-f-s1-0", "qualify", "constrain/standing-rule",
      "I am not sure I like 象 to ask mulitple seats per turn??",
      opens="one seat per turn",
      note="standing constraint on 象's routing behaviour"),
    A(f"{T}-r-0", "reply-mark", "report/diagnosis",
      "(one seat per turn) It doesn't, and step 3 doesn't change that",
      answers=[(f"{T}-f-s1-0", "(one seat per turn) It doesn't", "closes")],
      within=f"{T}-f-s1-0"),
    A(f"{T}-r-1", "reply-mark", "qualify/caveat",
      "(the one cost) Promotion is slower",
      answers=[(f"{T}-f-s1-0", "(the one cost)", "keeps")],
      within=f"{T}-f-s1-0"),
    A(f"{T}-r-2", "reply-mark", "report/in-flight",
      "(one fix in review)",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "verify/checked-live",
      "(loaded and timed)",
      conf="medium"),
])

T = "claude-2-turn-49"
build("turn-JHpGXv", [
    A(f"{T}-f-s1-0", "withdraw", "constrain/standing-rule",
      "i am not interested in campaign S at all",
      opens="campaign S dropped",
      note="standing withdrawal: campaign S never returns"),
    A(f"{T}-r-0", "reply-mark", "constrain/accept",
      '("not interested in campaign S at all") Dropped',
      answers=[(f"{T}-f-s1-0", '("not interested in campaign S at all") Dropped', "closes")]),
    A(f"{T}-r-1", "reply-mark", "report/in-flight",
      "(status) codex-10 is still on R15 part 1",
      conf="medium"),
])
