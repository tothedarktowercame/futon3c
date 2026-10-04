# Excursion: E-agency-work-orders — an order stays open until it is done

**Date:** 2026-10-03
**Status:** IDENTIFY (requirements clear; not built).
**Attached to:** M-象-2000. The open/closed judgement is P24's
`futon3c.logic.xiang` (`obligationo`, `openo`, the adjacency table), the
McCarthy semantics applied to agent-to-agent work.
**Repo:** futon3c — `transport/http.clj` (handle-bell, auto-bellback, the job
ledger), `agency/parked_on.clj`, `agency/promise_*.clj`,
`agency/obligations.clj`, `logic/xiang.clj`.

## HEAD (one line)
A bell job is `done` when the recipient's turn ends, whatever it delivered.
A work order should be `open` until the recipient reports it done against its
acceptance bar or the requester releases it, and an agent whose turn ends with
an order open should be reminded of it.

## The case (Joe, 2026-10-03)
claude-4 asked codex-19 for work; codex-19 did the first half and stopped.
Nothing in Agency noticed: the job was `done`, the bellback went out, and Joe
had to intervene by hand. Not yet located in the ledger: the last 200
codex-19 jobs show 27 bells codex-19 → claude-4 and none claude-4 → codex-19,
so the order may have travelled inside a reply. Finding it is the first task
(below).

## Second case: the handoff is a step, not a stop (codex-18, 2026-10-03)
`*codex-repl:codex-18*` around line 6077, M-diagramprover. Joe asked codex-18
to continue the validation harness; the row load stopped at missing `inspect`
support. Joe: "I am confused who you are assigning this to." codex-18 belled
codex-19 (`invoke-1791070189597-31303-13ddbb3b`, 23:29) asking it to *check*
whether its latest revision covers `inspect`. codex-19 answered (23:38):
"This is my transpiler/runtime blocker … No fix revision exists yet." The
auto-bellback reached codex-18, which wrote "㊟ Lisp `atEnd` validation is
blocked awaiting that fix. I own the independent retest afterward" and ended
its turn. Not parked; no order on codex-19 to make the fix.

The chain at that point: Joe → codex-18 (harness, open) → codex-19 (check,
closed) → [fix: owned by codex-19 in words, ordered by no one] → codex-18
(retest, waiting on nothing). Every job is `done`; the work is not. Two joints
failed:
- The order sent was the wrong one: "check" where the work needed "fix". An
  ownership claim ("my blocker") is a promise without a deadline; nothing
  records it as one.
- The agent that handed off stopped at the tee instead of parking on the
  branch it opened.

The marks were there (codex-18 and codex-19 both mark their paragraphs);
what is missing is the track they run on.

## The marble run (Joe's image)
The marks are the marbles; the adjacency table in `logic/xiang.clj`
(`:delegate → :promise → :fulfil`, `:offer → :accept`, …) is the track. A
handoff is a tee in the track (structure/cook-ting: a joint already in the
structure). At a tee the marble must go down a branch and the run must know
where the branches rejoin; a run that lets the marble rest at the tee is the
codex-18 case. Strong words in the system prompt do not build track; this
needs code at the joint.

## What exists, and on which side
All of it serves the requester; none of it holds the recipient to the order.
- Parking (`parked_on.clj`): the requester waits on a job id and wakes when
  the job ends, or at its deadline.
- Promise records and outcomes (`promise_record.clj`, `promise_outcome.clj`):
  P5 criteria evaluated on a 30-second sweep.
- `obligations.clj`: the P9 projection of who owes whom.
- `logic/xiang.clj` (P24): `obligationo debtor creditor source t s`, `openo`.
  Its first live run (claude-17, 2026-10-03, this session written up as acts)
  showed the matching gap in the logic: an accepted offer creates an
  obligation, and a commit that carries it out does not close it; only an
  explicit `:fulfil`, `:release` or `:lapse` does.

## Requirements
1. **An order is an act.** A bell with `--mode work` (and any 🈸 offer that is
   accepted) creates a work order: requester, recipient, the text, the
   acceptance bar where the packet states one, the job id. Recorded as
   evidence, so the kernel can read it.
2. **Open until closed by a named act.** Closed by the recipient's report that
   cites the order and says done (with shas where the bar asks for them), by
   the requester's release, or by a lapse at a deadline. A job ending is not a
   close.
3. **Reminder at turn end.** When the recipient's job ends with an order open,
   Agency bells the recipient once, naming the order and what is still owed,
   and tells the requester. Repeats are bounded (no reminder storm); the
   second unanswered reminder goes to the requester as a problem report.
4. **One judge.** Whether an order is open is answered by
   `futon3c.logic.xiang`, not by a separate reader, so the Emacs stepper,
   the Element client and Agency agree.
5. **Multi-part orders close only when every part is delivered.** A report
   that delivers part of an order keeps it open (`:keeps` in the adjacency
   table), and the recipient carries on rather than waiting to be asked
   again.
6. **A handoff is a child order.** When fulfilling an order means asking
   another agent, the bell records a child order under the parent. The
   parent cannot close while a child is open. An ownership claim in a reply
   ("this is mine", "my blocker") is recorded as a promise, i.e. a child order
   on the claimant.
7. **No resting at the tee, enforced in code.** When an agent's job ends with
   its order open and an open child, Agency parks it on the child instead of
   letting it go idle, and resumes it when the child closes, with the parent
   order restated. When it ends with its order open and no child, requirement
   3's reminder applies.
8. **Visible.** `GET /api/alpha/work-orders?agent=` lists open orders, owed
   by and owed to; the REPL lighter can show a count.

## Who does the reminding: Tickle, re-scoped (Joe, 2026-10-03)
Joe, after unjamming codex-18 and codex-19 by hand: in this session he does
knowledge work; relative to codex-18 he is "just going around unjamming the
machine". That is the job the Tickle persona was made for
(`src/futon3c/agents/tickle.clj`, M-tickle-overnight), unused for months
because "as implemented Tickle was perpetually a pain".

As built, Tickle pages any agent with no evidence for 300 s
(`detect-stalls`, `:threshold-seconds`) and escalates to a restart. Silence is
the wrong signal: an idle agent that owes nothing is not stalled, and an agent
that is busy elsewhere while it owes something looks alive. With work orders
the trigger becomes a fact the kernel can state: an order is open, its
debtor has no running job and no park, and no reminder is in flight. Tickle
then performs requirements 3 and 7 (the reminder; parking the agent on its
open child), and escalates to Joe only after the bounded reminders are
spent. Joe hears about orders that cannot move, not about agents that are
quiet.

## Token design for the live case (claude-17, 2026-10-04)
Joe, 2026-10-04: codex-18/19/20 keep reaching mutual stasis and he does not
want to tickle them; build the working system, not a one-off watcher.
The case: codex-19 orders codex-20 (implement) and codex-18 (replay); each
result comes back to codex-19 by auto-bellback; codex-19 replies ("handoff
closed", "codex-18 will replay") and its turn ends. If that turn dispatched
nothing, all three are idle and nobody owes anything visibly.

A token is a work order. It is always held by exactly one agent, and the
machine checks the holder at the one moment stasis can begin: the end of a
job. No timer.

Order shape: {:id :requester :debtor :parent :job-id :text :opened-at
:state (:open | :delivered | :closed) :closed-by :nudges [...]}.

Events:
- E1 open. A work bell (mode work) from a registered agent opens an order,
  debtor = recipient. Its parent is the order the caller is working on (the
  order whose job the caller is running). A work bell from an agent holding
  no order also opens a chain root: debtor = the caller, requester = the
  operator. The root is what codex-19 is carrying for Joe.
- E2 deliver. The debtor's job ends: the order is :delivered and the token
  returns to the requester, by the existing auto-bellback. When that bellback
  job ends, the order closes (:fulfil) and the requester holds the parent.
- E3 check. On every job end, for the agent whose job ended: if it is the
  debtor of an open order with no open child, and it has no running or
  queued job and no park, it is holding a token still. Nudge it once, by a
  work bell naming the order and the choice it owes: dispatch the next step,
  or close the order (`POST /api/alpha/work-orders/:id/close` with a reason).
  If its next job ends the same way, the order goes to its requester as a
  problem; for a root, to Joe's HUD and `GET /api/alpha/work-orders`.
- Close: the debtor or requester closes with a reason; Joe can close any.

Every transition is also written as a P24 act (:promise creates, :fulfil /
:release / :lapse close), so the kernel and the HUD read the same orders.

Packets: W1 the ledger and E1/E2 with the list route (no nudges);
W2 the pure E3 decision; W3 wiring E3 to job end, the close route, and
`agency_send.py --close-order`.

## 大象: the classical reader of agent turns (Joe, 2026-10-04)
Joe: 象 (an LLM) must not run over agent text, but the HUD and the token
system need a full account of what happened and what did not. So a
classical reader, 大象, parses each agent turn at its end, in well under a
second, and only then does the kernel (`futon3c.logic.xiang`: McCarthy's
speech acts, as in Elephant 2000; Fong's open ports) judge what closed and
what is open. Order per turn: 大象 produces acts, then the kernel queries.

大象's inputs, all available at turn end without an LLM:
- the reply's proforma marks with their bracketed targets
  (`turn-record/reply-marks`): 🈸 offer, 🈳 unresolved, ㊭ propose,
  🈡 withdraw, 🈹 retract, ㊣ approve, ...;
- the bells the agent sent during the job (Agency job records whose caller
  is the agent: recipient, mode, job id), which are the work orders;
- parks it set (awaiting job ids);
- commits it made (the Agent-Session / Agency-Job trailers);
- ownership claims in plain words ("I own", "my blocker", "will replay"),
  read by fixed patterns as :promise acts, and kept only when they name a
  next step.

Output: P24 acts with stable ids, so the same turn read twice gives the
same acts. The turn->acts adapter (kimi-1, job a6d2d865) is the first part
of 大象: marks and commits. Bells, parks and claims follow.

## The heads-up display: HAPPENED and DIDN'T HAPPEN (Joe, 2026-10-04)
Because 象 now reads after a turn has landed (the fast path: 小象's
provisional parse at send, 象's finalised reading after), 象 can keep the
marble-run state across turns: which ports closed this turn and which are
still open from earlier ones. The stepper's HAPPENED pane becomes a HUD:
closed this turn, and DIDN'T HAPPEN -- what is still open, with its age.
It is the same judgement as requirements 3 and 7 (one judge: the P24
kernel), shown to Joe instead of belled to an agent.

Inputs, all already produced:
- Joe's side: 象's reading, an intent per fragment (ask-action, accept,
  redirect, ...).
- The agent's side: the reply proforma marks, read classically
  (🈸 offer, 🈳 unresolved, ㊭ propose, 🈡 withdraw, ...; commit 50c6b7c1
  reads them into 象).
- Commits: the happened note lists them; a commit can carry out an
  accepted offer, which closes the gap the first P24 run found.

Live example, this session (claude-17, 2026-10-03/04): open with no closing
answer -- the xiang-trace false alarm under the jvm recorder, the operator
evidence id not recorded, making `jvm` the saved default, the
natural-deduction lab note. Each was a 🈸 or 🈳 that nothing recorded as
open.

Missing piece: the adapter from a settled turn (record + reading +
happened) and the agent's marked reply to P24 acts. P24's note lists it as
not built.

## Order of work
1. Write both cases up as P24 fixtures (`test/futon3c/logic/xiang_fixtures/`):
   the claude-4/codex-19 half-done order (locate it first), expectation
   "open after codex-19's job ends"; and the codex-18 chain above,
   expectations "Joe's order open, codex-19 owes the fix, codex-18 parked on
   it" after 23:38.
2. Kernel: let a report or commit that cites an order and claims completion
   close it (`:fulfil`), and refuse an agreement for a missing seat with its
   own reason rather than `:no-visible-offer`.
3. Agency: record orders at bell time; at job end, ask the kernel; bell the
   reminder.
4. The listing route and the lighter count.

## Live cases to test against
Easy to find: any work bell whose bellback summary says "first half",
"next I will", or lists unchecked items. The ledger query is
`GET /api/alpha/invoke/jobs?agent=<id>`.
