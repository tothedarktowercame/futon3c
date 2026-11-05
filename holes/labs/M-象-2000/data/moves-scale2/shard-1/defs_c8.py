import sys
sys.path.insert(0, ".")
from sbuild import build, A

T = "claude-17-turn-37@turn-IOt3kq"
build("turn-IOt3kq", [
    A(f"{T}-f-s1-0", "report", "report/status-update",
      "I've passed Fable's deep research task to an external agent;"),
    A(f"{T}-f-s1-1", "ask-action", "delegate/two-asks",
      "can you please 🈰 implement their polish notes and 🈯 discuss their strategy with me?"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "(implement their polish notes) All are done",
      answers=[(f"{T}-f-s1-1", "(implement their polish notes) All are done", "closes")]),
    A(f"{T}-r-1", "reply-mark", "explain/mechanism",
      "(their strategy) The strategy has three parts:",
      answers=[(f"{T}-f-s1-1", "(their strategy) The strategy has three parts:", "keeps")],
      opens="external strategy review",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "propose/next-step",
      "(where the two join) Start with part 3.",
      answers=[(f"{T}-f-s1-1", "(where the two join) Start with part 3.", "keeps")],
      within="external strategy review",
      conf="medium"),
    A(f"{T}-r-3", "reply-mark", "propose/next-step",
      "(McCarthy side, order) I'd do R19 next.",
      answers=[(f"{T}-f-s1-1", "(McCarthy side, order)", "keeps")],
      within="external strategy review",
      conf="medium"),
    A(f"{T}-r-4", "reply-mark", "explain/decline",
      "(Fong side) I'd hold off building typed ports and kernels",
      answers=[(f"{T}-f-s1-1", "(Fong side) I'd hold off", "keeps")],
      within="external strategy review",
      conf="medium"),
    A(f"{T}-r-5", "reply-mark", "ask-action/one-decision",
      "(one decision) Shall I count last week's agent ㊭ paragraphs",
      answers=[(f"{T}-f-s1-1", "(one decision) Shall I count", "keeps")],
      within="external strategy review",
      conf="medium"),
])

T = "claude-17-turn-474"
build("turn-InfUCq", [
    A(f"{T}-f-s1-0", "report-problem", "report-problem/observed",
      "I still see that for now the red terms are underlined."),
    A(f"{T}-f-s2-0", "constrain", "constrain/for-now",
      "They don't need underlining if they are red!"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "(red, no underline) Red terms no longer have an underline.",
      answers=[(f"{T}-f-s2-0", "Red terms no longer have an underline.", "closes"),
               (f"{T}-f-s1-0", "Red terms no longer have an underline.", "closes")]),
    A(f"{T}-r-1", "reply-mark", "verify/checked-live",
      '(checked) "for now" at the end of your last turn now shows the red face',
      answers=[(f"{T}-f-s1-0", "(checked)", "closes")],
      conf="medium"),
])

T = "claude-17-turn-480"
build("turn-JVOrQG", [
    A(f"{T}-f-s1-0", "delegate", "delegate/end-to-end",
      "I'm happy for you to do 1-3 end to end, I don't need to preview the spec"),
    A(f"{T}-r-0", "reply-mark", "report/done",
      "(step 1: the definitions file) futon0 `9725f42`",
      answers=[(f"{T}-f-s1-0", "(step 1: the definitions file) futon0 `9725f42`", "closes")],
      opens="rnode definitions rollout"),
    A(f"{T}-r-1", "reply-mark", "report/design",
      "(step 2: what Codex is building)",
      answers=[(f"{T}-f-s1-0", "(step 2: what Codex is building)", "keeps")],
      within="rnode definitions rollout",
      conf="medium"),
    A(f"{T}-r-2", "reply-mark", "propose/next-step",
      "(step 3, next) The fixed rule above gets replaced by policies",
      answers=[(f"{T}-f-s1-0", "(step 3, next)", "keeps")],
      within="rnode definitions rollout",
      conf="medium"),
])
