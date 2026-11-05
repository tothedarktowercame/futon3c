import sys
sys.path.insert(0, ".")
from sbuild import build, A

T = "claude-17-turn-413"
build("turn-MEqvCK", [
    A(f"{T}-f-s1-0", "verify", "verify/feasibility-question",
      'whether tasks like "read and analyse all of Joe\'s circa-5000 turns" would hammer futon1b'),
    A(f"{T}-r-0", "reply-mark", "report/diagnosis",
      "Gist: Reading and analysing your roughly 5,000 turns doesn't load futon1b at all.",
      answers=[(f"{T}-f-s1-0", "Reading and analysing your roughly 5,000 turns doesn't load futon1b at all.", "closes")]),
    A(f"{T}-r-1", "reply-mark", "verify/checked-live",
      "(what I measured this turn)",
      answers=[(f"{T}-f-s1-0", "(what I measured this turn)", "keeps")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "report-problem/observed",
      "(what is still unsafe)",
      answers=[(f"{T}-f-s1-0", "(what is still unsafe)", "keeps")],
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "explain/mechanism",
      "(why the Minard extraction was slower)",
      answers=[(f"{T}-f-s1-0", "(why the Minard extraction was slower)", "keeps")],
      conf="medium"),
])

T = "claude-17-turn-52@turn-MW216j"
build("turn-MW216j", [
    A(f"{T}-f-s1-0", "disagree", "disagree/correction",
      'I think that overly constrains "unresolved".'),
    A(f"{T}-f-s2-0", "propose", "propose/alternative",
      "it is better for us to try to parse it classically based on standard punctuation marks."),
    A(f"{T}-r-0", "reply-mark", "report/design",
      '(what counts as a question) A "?" counts',
      answers=[(f"{T}-f-s2-0", '(what counts as a question) A "?" counts', "closes")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "verify/checked-live",
      "(what I checked)",
      answers=[(f"{T}-f-s2-0", "(what I checked)", "closes")],
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "qualify/caveat",
      "(a cost) A rhetorical question in an explanation now makes an offer too.",
      answers=[(f"{T}-f-s1-0", "(a cost)", "keeps")],
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "report/gap",
      "(still open) The two earlier questions are still waiting",
      conf="medium"),
])

T = "claude-17-turn-497"
build("turn-Mkg80S", [
    A(f"{T}-f-s1-0", "approve", "approve/plain", "That works for me."),
    A(f"{T}-f-s2-0", "extend", "propose/extend",
      "We could do this for any set of turns"),
    A(f"{T}-f-s3-0", "propose", "propose/tentative",
      "by sending from operator to  象  to agent"),
    A(f"{T}-f-s4-0", "defer", "defer/until",
      "maybe we won't do it live right away."),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "(what I did) The probe is `holes/labs/M-diagramprover/flow_23519_agency_probe.clj`",
      answers=[(f"{T}-f-s1-0", "(what I did)", "closes")],
      conf="medium"),
    A(f"{T}-r-1", "reply-mark", "report/gap",
      "(what the gap means)",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "explain/mechanism",
      "(the live version) Your pipeline, operator → 象 → agent",
      answers=[(f"{T}-f-s2-0", "(the live version)", "keeps"),
               (f"{T}-f-s3-0", "(the live version)", "keeps")],
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "ask-action/one-decision",
      '(one decision) Shall I add the "nothing before this point" flag',
      conf="medium"),
])
