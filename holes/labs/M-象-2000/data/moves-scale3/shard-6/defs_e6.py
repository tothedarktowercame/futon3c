import sys
sys.path.insert(0, ".")
from sbuild3 import build, A

T = "claude-6-turn-9"
build("turn-AFcJRx", [
    A(f"{T}-f-s1-0", "constrain", "constrain/standing-rule",
      "Can you please summarise more succinctly what the change implies",
      opens="succinct summaries",
      note="a standing preference for brevity in reports"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "(what the change implies) Agents will stop guessing where the port goes wrong",
      answers=[(f"{T}-f-s1-0", "(what the change implies)", "closes")],
      within=f"{T}-f-s1-0"),
    A(f"{T}-r-1", "reply-mark", "qualify/caveat",
      "(limits) It only sees steps the port records",
      answers=[(f"{T}-f-s1-0", "(limits)", "keeps")],
      within=f"{T}-f-s1-0"),
])

T = "claude-17-turn-505"
build("turn-AK9Jj8", [
    A(f"{T}-f-s1-0", "explain", "clarify/context",
      "wm-click-acts.edn seems as close to an authoritative alignment"),
    A(f"{T}-f-s1-1", "constrain", "constrain/standing-rule",
      "I'd prefer to follow the wm-click-acts alignment initially",
      opens="wm-click-acts as alignment authority",
      note="names the authority for the alignment work"),
    A(f"{T}-f-s1-2", "qualify", "qualify/caveat",
      "it's also possible that they are missing some of our xiaoxiang-insert-mark verbs",
      within=f"{T}-f-s1-1"),
    A(f"{T}-f-s2-0", "approve", "approve/plain",
      "Yes please to the new table based on the available sources."),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "(the table) I've written the table, using `wm-click-acts.edn` as the authority",
      answers=[(f"{T}-f-s2-0", "(the table) I've written the table", "closes")],
      within=f"{T}-f-s1-1"),
    A(f"{T}-r-1", "reply-mark", "report/done",
      "(what's in the commit)",
      answers=[(f"{T}-f-s2-0", "(what's in the commit)", "closes")],
      within=f"{T}-f-s1-1"),
    A(f"{T}-r-2", "reply-mark", "retract/correction",
      "(my last turn) Two of last turn's claims don't hold against the table",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "report-problem/observed",
      "(a defect in the acts file) `request-interpretation` is tagged `:PERCEIVE :R7`",
      conf="medium"),
    A(f"{T}-r-4", "reply-mark", "report/evidence",
      "(the audit, briefly)",
      answers=[(f"{T}-f-s1-2", "(the audit, briefly)", "closes")],
      within=f"{T}-f-s1-2",
      conf="medium"),
    A(f"{T}-r-5", "reply-mark", "ask-action/one-decision",
      "(one decision) Shall I correct the R7 row",
      conf="medium"),
])
