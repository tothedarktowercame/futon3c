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
5. **Visible.** `GET /api/alpha/work-orders?agent=` lists open orders, owed
   by and owed to; the REPL lighter can show a count.

## Order of work
1. Locate the claude-4/codex-19 case in the job ledger or evidence, and write
   it up as a P24 fixture (`test/futon3c/logic/xiang_fixtures/`) whose
   expectation is "open after codex-19's job ends".
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
