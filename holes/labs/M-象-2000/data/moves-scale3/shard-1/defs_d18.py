import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-11-turn-3"
build("turn-Cq0RT7", [
    A(f"{T}-f-s1-0", "prioritize", "ask-action/status-check",
      'are any of the failures "critical" at the moment?'),
    A(f"{T}-r-0", "reply-mark", "report/evidence",
      "(gist) Nothing is critical",
      answers=[(f"{T}-f-s1-0", "(gist) Nothing is critical", "closes")]),
    A(f"{T}-r-1", "reply-mark", "report/evidence",
      "(the rest, by how much damage each could do)",
      answers=[(f"{T}-f-s1-0", "(the rest, by how much damage each could do)", "keeps")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "ask-action/one-decision",
      "(next) If you agree, I'll start on the evidence-store nightly backup",
      answers=[(f"{T}-f-s1-0", "(next)", "keeps")],
      conf="medium"),
])

T = "claude-18-turn-138"
build("turn-CrdCfB", [
    A(f"{T}-f-s1-0", "report", "report/status-update",
      'according to my *APM report* we have 111 left as open problems'),
    A(f"{T}-f-s2-0", "disagree", "challenge/other",
      'Your writeup of "three places where the statement says less than it could',
      conf="medium"),
    A(f"{T}-f-s3-0", "clarify", "clarify/definition",
      "permitted weakening?",
      within=f"{T}-f-s2-0"),
    A(f"{T}-f-s4-0", "ask-action", "ask-action/commission",
      "That's concerning, please look into it",
      within=f"{T}-f-s2-0"),
])

T = "claude-16-turn-64"
build("turn-D1MH8K", [
    A(f"{T}-f-s1-0", "qualify", "clarify/assertion",
      "there is one systems, and a session or thread isan interesting efficiency",
      opens="one system, sessions as efficiency",
      note="frames the reading for the whole session/thread redesign"),
    A(f"{T}-f-s2-0", "extend", "propose/analogy",
      "it is like the WEFT in futon6 whereas we also need WARP and TAPESTRY",
      within=f"{T}-f-s1-0"),
    A(f"{T}-r-0", "reply-mark", "approve/plain",
      '("one system … WEFT, WARP, TAPESTRY") Agreed on the first part',
      answers=[(f"{T}-f-s1-0", '("one system … WEFT, WARP, TAPESTRY") Agreed', "closes"),
               (f"{T}-f-s2-0", 'Agreed on the first part', "keeps")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/evidence",
      "(the mapping) In futon6 the weft is the per-paper markup pass",
      answers=[(f"{T}-f-s2-0", "(the mapping)", "closes")],
      within=f"{T}-f-s1-0",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report/design",
      "(what changes in the lane) The session-end path stays as a recorded fact",
      within=f"{T}-f-s1-0",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "ask-action/one-decision",
      "(first two packets, each one file, ready to send on your word)",
      within=f"{T}-f-s1-0",
      conf="medium"),
])
