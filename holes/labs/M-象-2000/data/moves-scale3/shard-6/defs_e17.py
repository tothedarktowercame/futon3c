import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-14-turn-6"
build("turn-NqKscZ", [
    A(f"{T}-f-s1-0", "constrain", "ask-action/commission",
      "regarding the refutation, please check if the error is in tye original latex"),
    A(f"{T}-r-0", "reply-mark", "report/evidence",
      "(original statements)",
      answers=[(f"{T}-f-s1-0", "(original statements)", "closes")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/diagnosis",
      "(what was wrong) It was false in two ways",
      answers=[(f"{T}-f-s1-0", "(what was wrong)", "closes")]),
    A(f"{T}-r-2", "reply-mark", "report/done",
      "(repair)",
      conf="medium"),
])

T = "claude-15-turn-48"
build("turn-Nquml7", [
    A(f"{T}-f-s1-0", "propose", "propose/directive",
      "can we start to bring features online i  this session?"),
    A(f"{T}-f-s2-0", "approve", "approve/with-amendment",
      "the brief is a nice concept for new sessions,"),
    A(f"{T}-f-s2-1", "qualify", "qualify/caveat",
      "but noisy as you say, and maybe not the coolest of cool memory demos",
      within=f"{T}-f-s2-0"),
    A(f"{T}-r-0", "reply-mark", "report/design",
      '("bring features online") It has three questions',
      answers=[(f"{T}-f-s1-0", '("bring features online")', "closes")]),
    A(f"{T}-r-1", "reply-mark", "report/evidence",
      "(a real result) Its first real query caught me out",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "qualify/caveat",
      "(what to discount)",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "propose/next-step",
      "(next, unless you'd rather something else) Recall at write time",
      answers=[(f"{T}-f-s1-0", "(next, unless you'd rather something else)", "keeps")],
      conf="medium"),
    A(f"{T}-r-4", "reply-mark", "qualify/caveat",
      "(the brief) I can leave the brief running",
      conf="medium"),
])
