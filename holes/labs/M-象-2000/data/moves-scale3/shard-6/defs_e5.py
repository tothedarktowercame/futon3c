import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-17-turn-487"
build("turn-7Ye630", [
    A(f"{T}-f-s1-0", "report", "report/context",
      "I got an external agent to comment on the relationship"),
    A(f"{T}-f-s2-0", "report", "report/status-update",
      "f0ce648 adds the flow-map kit, now rung 0 of the ladder."),
    A(f"{T}-f-s3-0", "report", "report/status-update",
      "b6feb2c adds the ladder of diagram types"),
    A(f"{T}-f-s4-0", "report", "report/status-update",
      "f7ba3a34 adds open causal theories"),
    A(f"{T}-r-0", "reply-mark", "report/evidence",
      "(what the skeleton computes)",
      answers=[(f"{T}-f-s4-0", "(what the skeleton computes)", "keeps")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "qualify/caveat",
      "(limits it states itself)",
      answers=[(f"{T}-f-s4-0", "(limits it states itself)", "keeps")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/evidence",
      "(relation to M-aif-over-the-operator and today's MAP)",
      answers=[(f"{T}-f-s1-0", "(relation to M-aif-over-the-operator", "keeps")],
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "ask-action/one-decision",
      "(next step) If you want it in the running system",
      answers=[(f"{T}-f-s2-0", "(next step)", "keeps")],
      conf="medium"),
])

T = "claude-17-turn-495"
build("turn-9jNlJK", [
    A(f"{T}-f-s1-0", "approve", "approve/plain", "🈸:yes"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "(what I built) The probe is `holes/labs/M-diagramprover/flow_23519_probe.clj`",
      answers=[(f"{T}-f-s1-0", "(what I built)", "closes")]),
    A(f"{T}-r-1", "reply-mark", "report/evidence",
      "(what it computes)",
      answers=[(f"{T}-f-s1-0", "(what it computes)", "closes")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/evidence",
      "(what this says about operator intent and agent follow-up)",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "qualify/caveat",
      "(limits)",
      conf="medium"),
])
