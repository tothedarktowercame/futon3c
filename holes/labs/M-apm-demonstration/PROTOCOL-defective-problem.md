# PROTOCOL — a flaw in the problem stops the line

**Ruled by Joe, 2026-08-19.** Standing, and it supersedes how f11 and f12 were
handled.

> "If we find a flaw in the problem, stop the line and repair it, move the
> problem to a new frame, mark the previous one as void, and retry."

## The rule

The moment a frame establishes that the **problem** is defective — as opposed to
hard, or blocked on missing Mathlib — the frame stops. It does not run to close.

1. **STOP.** No further dispatches. In particular no student dispatch: the
   student arm measures "can zai solve this aided by memories", and on a broken
   problem it measures nothing about transfer while costing a dispatch.
2. **REPAIR the problem**, or mark it unrepairable and remove it from the
   selectable pool (t01A05's disposal).
3. **NEW FRAME** for the repaired problem, with its own registration and pin.
4. **VOID the previous frame** in the series.
5. **RETRY.**

## What "void" means, precisely

`:void` is a **series-level** mark, not a cycle disposition. The allowed cycle
dispositions are `#{:closed :tier-a :tier-b :defective}` (`problem.clj:1637`) and
that set is not changing for this. So:

- the cycle's disposition stays **`:defective`**;
- the `series.edn` entry carries **`:void true`** with `:void-reason`, and
- **a void frame is never counted as a measurement of solving capability.**

### What a void frame still counts for — this distinction is load-bearing

Voiding the problem outcome does **not** void the instrument findings. A void
frame's harness measurements remain valid, because they measure the machine and
not the problem. Concretely, f13 is void for `:problem-solved` and yet:

- `:memory-contributes-to-close` is **CONFIRMED with attestation** — two f12
  memories `USED`, all five surfaced accounted for, machine-checked. The solver
  used accumulated memories to *discover the defect*. That is a genuine transfer
  observation and it survives the void.
- D57–D61 stand.

So a void frame records: **problem outcome VOID, instrument findings RETAINED.**
Anything else would throw away the only attested transfer the series has
produced, on the grounds that the problem it was measured on turned out broken.

## Why stop rather than finish

Three consecutive frames — f11 t01A05 (statement FALSE), f12 m03J01 (statement
VACUOUS), f13 m99J06 (model INCONSISTENT) — ran to or near close on defective
problems. Each spent a student arm, a promotion pass and an adjudication on a
measurement that could not mean what it appeared to mean. A frame that closes a
vacuous statement and is filed as a close corrupts the corpus; a frame that
closes one and is filed as `:defective` is honest but still spent its budget
measuring nothing.

The cost of stopping is one frame's remaining phases. The cost of continuing is
a frame's full budget plus a record that a later reader has to be careful with.

## Retroactive application

- **f13 m99J06** — VOID. Stopped at `:promote-solver`, no student dispatched.
- **f12 m03J01** — VOID retroactively. Disposition was `:defective` and correct;
  the frame ran to close, including a student arm, on a vacuously true statement.
- **f11 t01A05** — VOID retroactively. The problem was already marked
  `statement-defective` and removed from the selectable pool.

f9 (a01J06) and f10 (m93J02) are unaffected — both closed on sound problems.

## The consequence nobody should have to rediscover

**The selectable pool has an unmeasured defect rate**, and on the evidence so far
it is not small. Every frame drawn from it is a coin flip between measuring the
machine and finding a broken formalisation. A formalisation audit is therefore
not housekeeping; it is the precondition for the series measuring what it claims
to measure.


## A cost the protocol creates, found by running it once (f13-guide, 2026-08-19)

`:promotion-result` is harvested into cycle outputs **only by `dispatch-student!`**
(`conductor.clj:504-509`). So a frame that halts before the student **loses the
trace record of promotions that actually landed.** The store is correct — the
memory is there, `:reviewed`, with its review evidence — but the cycle trace
under-reports the frame's gain, and an Analyst reading only the trace would score
`:reviewed-attachment-gained` as unearned.

This is not a pre-existing defect. **Stopping the line created it**, because the
harvest was written on the assumption that every frame reaches the student. Any
frame halted under this protocol must therefore carry its promotion record in the
halt REPORT, and the series entry must take it from there rather than from the
trace. f13's does.

Fixing it properly means moving the harvest to the promotion itself rather than
to the student dispatch. Queued.

## Scoring rule: `:not-reached` is not `:refuted`

A halt makes some predictions unreachable. f13's `:offer-disposition-populated`
is the case: `:write-use` is available only at `:adjudicate` (`problem.clj:56`),
which the halt precedes.

**An unreachable prediction is scored `:not-reached` and never `:refuted`.**
Refuting it would charge the protocol's cost to the machine's capability, and the
series would show the apparatus getting worse each time we correctly stopped a
frame early.
