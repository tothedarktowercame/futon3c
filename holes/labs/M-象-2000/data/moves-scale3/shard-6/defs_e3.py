import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-17-turn-380"
build("turn-4r2lVL", [
    A(f"{T}-f-s1-0", "approve", "approve/plain",
      "yes please start the next hour with all of the improvements"),
    A(f"{T}-f-s2-0", "withdraw", "constrain/standing-rule",
      "you don't need to get bellbacks for each turn",
      opens="no bellbacks per turn",
      note="withdraws the per-turn bellback expectation as a standing rule"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "(what changed, commit 48296e5f)",
      answers=[(f"{T}-f-s1-0", "(what changed, commit 48296e5f)", "closes")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/gap",
      "(not in this run) The check of hour 1's citations",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/in-flight",
      "(check at the hour) The wake-up is park `park-92d9e4d5",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "report/in-flight",
      "(throughput, 23:25–00:25 UTC)",
      conf="medium"),
    A(f"{T}-r-4", "reply-mark", "report/evidence",
      "(three records, each citation checked against its pattern)",
      conf="medium"),
    A(f"{T}-r-5", "reply-mark", "report/evidence",
      "(小象 edition 9) 3,493 turns, accuracy 0.357",
      conf="medium"),
    A(f"{T}-r-6", "reply-mark", "propose/next-step",
      "(next)",
      answers=[(f"{T}-f-s1-0", "(next)", "keeps")],
      conf="medium"),
    A(f"{T}-r-7", "reply-mark", "ask-action/one-decision",
      "(one decision) Shall I start the citation check on kimi-3",
      answers=[(f"{T}-f-s1-0", "(one decision)", "keeps")],
      conf="medium"),
])

T = "claude-17-turn-493"
build("turn-5nQZCO", [
    A(f"{T}-f-s1-0", "approve", "approve/plain", "🈸:yes"),
    A(f"{T}-r-0", "reply-mark", "verify/checked-live",
      "(how I tested it)",
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "qualify/caveat",
      "(how much this shows)",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/done",
      "(where it's recorded) The turns, method and table are in",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "ask-action/one-decision",
      "(next step) Shall I have 象 read C1–C3",
      answers=[(f"{T}-f-s1-0", "(next step)", "keeps")],
      conf="medium"),
])
