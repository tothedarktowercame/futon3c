# E-question-log: Joe's questions logged as questions, and given attention

**Status:** OPEN — IDENTIFY. Spun from [M-repl-circus](../missions/M-repl-circus.md)
by claude-13 on Joe's word, 2026-11-10.

## HEAD (Joe, 2026-11-10)

"the existence of "claim-type: question" makes me think of ArSE... while we
have REPL Circus working on analysis of my turns, it could be interesting to
extend that so that questions are logged as such and given some attention.
But we can add this as an Excursion from M-repl-circus and continue wrapping
up M-daily-scan"

## What exists

- **The tag.** futon1b evidence already records a speech act on Joe's turns,
  `evidence/claim-type`, with `"question"` among its values (e.g. Joe's
  2026-11-10 turn asking how to break the apex into questions,
  `emacs-80dd7c03ac1066881d616780535f2093`, is `:question`). Nothing reads
  it to keep track of whether a question was answered.
- **ArSE** (Artificial Stack Exchange): futon3c
  `src/futon3c/agents/arse_work_queue.clj` feeds synthetic question-and-answer
  generation and review through Tickle, from `futon6/data/arse-queue/`, and
  records progress as evidence. It is the precedent for treating a question
  as an item with a life: asked, answered, reviewed. Typed bells already use
  it between agents: a bell with `type=query` opens an ArSE thread, and
  `type=answer --ref <ask-id>` answers it (futon3c `CLAUDE.md`, "Typed Bell
  Contract"). Joe's own questions have no such thread.
- **A first measurement.** futon7 `holes/argument-graph/questions-joe-report.md`
  (M-daily-scan stage 1.5, 2026-11-10): BM25 over 23,145 of Joe's turns
  found, for 54 apex questions, that Joe had asked 1 and answered 1, and it
  listed questions Joe asked that no record covers. Three of kimi-2's five
  matches were to a different question (claude-13's review, futon7
  `bdf111c`), so matching a question to its answer needs reading, not only
  retrieval.

## The question for this excursion

What would it take for a question Joe asks in a turn to be logged as an
open question, linked to the turn or record that answers it, and surfaced
again while it stays open, without Joe doing the bookkeeping? In Circus
terms: which animal hooks onto `claim-type: question`, what does it give
Joe, and where does its output live (M-repl-circus, "Decided: where derived
output goes").

## Not yet decided

- Whether a question log is a derived store of its own or records in the
  GCC (E-gcc-schema), and how an answer is attested (Joe's word, an agent
  reply he approves, a commit).
- How many of Joe's tagged questions are rhetorical or answered in the same
  turn; a count is the first step.
