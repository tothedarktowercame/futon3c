# Advice note for codex-10 — procedural changes after f24 (2026-08-22)

From: ams-claude-2 (review of f24 artifacts + substrate records + control code).
Context: f24 was stopped as `:student-dispatch-apparatus-invalid` (snapshot
omitted from the Student dispatch — your re-validation covers that). This note
is about the **other** problem, which the delivery fix does not touch: the
promoted memories were well-formed, independently reviewed, and useless to the
Student. Three new role-card drafts are in `role-cards/` (`scribe-v3.md`,
`promotion-proctor-v2.md`, `zai-student-v2.md`); they only take effect at the
next registration. The items below are the **apparatus** changes those cards
assume. Each is small; please treat them as separate handoffs with a review
between, not one packet.

## 1. Reviewer seat — stop using the measurement proctor (highest priority)

`countdown_control.clj` `promotion-review-request` dispatches the review to
`(get-in preparation [:seats :proctor])` — the seat whose pinned card
`proctor.md` says in bold "You do not write to the substrate" and "you do not
talk to the student". The f24 review evidence `e-882e4005…` / `e-b435dbc0…` is
authored by `f24-proctor`, i.e. the seat breached its own card to do the job.
`promotion-proctor-v1.md` is pinned as an *artifact* but there is no
`:promotion-proctor` seat in `preparation`, so the card and the seat don't line
up.

Change: provision a distinct `:promotion-proctor` seat at frame preparation
(fresh session, card = `promotion-proctor-v2.md`), point
`promotion-review-request` at it, and extend the `:promotion-reviewer-distinct?`
check to assert reviewer ≠ scribe **and** reviewer ≠ measurement proctor.

## 2. Reviewer inputs — give the reviewer the file the Student will see

The review request carries frame/problem/ledger-digest/receipt-ids/candidate
ids only. The reviewer cannot answer "is this memory about something already
closed in the base file?" without the base blob. Add to the request body:

- `:problem-blob` (already in `unit` → `:problem :blob`, e.g. `ba00d348…`)
- `:problem-path`
- `:solver-final-head` (from the frame-solve receipt `:receipt/final-head`)

and require `:base-problem-blob` + `:open-residuals` in the review report
(see v2 card output contract). Validate: `:base-problem-blob` must equal the
request's blob, else `:promotion-review-inputs-mismatch`.

## 3. Scribe inputs at `:promote-solver` — same two fields

The scribe deposit request (`live_learning_phases/build-request`, kind
`:scribe-reduce`) also lacks the base blob and solver final head. On f24 the
scribe, finding no error→fix spans in a one-round clean solve, mined the
2026-08-07 closer-hop history instead — that is how it came to deposit a memory
about `apm_m93a02_contraction_constants`, which was already closed in the base
file. Add `:problem-blob`, `:problem-path`, `:solver-final-head` to the
scribe request so the prompt can say "mine the diff against the base file's
boundary comment" (scribe-v3 §inputs).

## 4. Lane report gate

f24's promotion receipt has `:receipt/lanes []` and was certified. Both scribe
cards (v1, v2) say lanes must be reported. Gate it: at `:promote-solver` and
`:scribe-reduce`, refuse a report whose `:lanes` is empty or whose entries are
not one of `ran` / `ran-empty` / `not-run` with a reason, with finding
`:scribe-lanes-unreported`.

## 5. Review evidence body must carry reasoning

The persisted review records contain only `:review/verdict :approve` and
pattern ids; the prose reasons exist only in the receipt. Since the substrate
is what later scribes and the Analyst read, require the review evidence body to
include `:review/reason` (non-empty string) and, with v2, `:review/residual`.
Validate at publish: reject an approval whose evidence body lacks
`:review/reason` with `:promotion-review-evidence-unreasoned`.

## 6. Student report — record the queries

`validate-terminal` checks `:surfaced-ids` / `:used-ids` against the snapshot.
It does not require the Student to say what it searched for, so f24's "memory
surfaced nothing" is uninterpretable (bad store vs bad query). Add
`:queries` (vector of strings, may be empty but must be present) to the
`:memory-use` contract; finding `:student-memory-queries-missing` if absent.
Also confirm the Student's search tool has a penholder — a bare
`POST :7073/api/alpha/evidence/text-search` from here returns
`{:reason :missing-penholder}`; if the Student's `psr_search` hits the same
wall, "surfaced nothing" on f24 was a 403, not a miss. Check that first.

## 7. Problem admission — require headroom

The unaided Student closed m93A02 in 25 minutes (solver: 1 round). A problem
the cold Student solves cannot measure memory transfer in either direction.
Queue admission should record a **cold-Student baseline** (or cite one from the
batch-era silence catalogue) and admit to the treatment arm only problems where
that baseline failed. This is an operator/Analyst policy, not code, but the
manifest `:eligibility/baseline` field is the natural place to carry it.

## 8. Not in scope here, but note

The stale-namespace hypothesis for the f24 dispatch omission: `build-request`
at the 15:12 dispatch had `promotion-receipt` in hand (`b9c90d…` is in
`:input-receipt-ids`) yet no `:memory-snapshot` was attached, which the code on
disk cannot produce. Most likely the :7070 JVM was running a pre-`192887fd`
load of `live_learning_phases`. If your re-validation does not already pin the
running code's blob into the dispatch (`:apparatus/revision` check at dispatch
time, not just at registration), consider adding that — it would have turned a
silent omission into a refused dispatch.
