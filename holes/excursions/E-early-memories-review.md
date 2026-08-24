# E-early-memories-review — reviewing the 203 problem-centric memories

*Excursion, 2026-08-24. Joe's decision: "attached, never reviewed … we could
still get an agent to review them." Not a quarantine by class; a receipted
review pass, out of frame, reviewer ≠ depositor.*

## Where the number comes from

`memory_shape.py` (first reading, `memory-shape-2026-08-24.json`, window
since 2026-08-10, n = 339 memories): **203 mention a problem id in name,
hook, or body**; 22 of those hooks *start* with the id. By attachment
status the 203 are 22 `:reviewed`, ~174 with no review at all, the rest
`:proposed` (recount when the candidate list is built — the script's
`PROBLEM_ID` filter is the definition). This is the same fact as "zero
memories used on a problem other than the one they were mined from": a
memory named by its problem is only ever found by its problem.

## Two populations, two treatments

The 203 are not one population. By author:

| population | n | what they are |
|---|---|---|
| **A. loop memories** — `f*-scribe`, `f*-guide`, `claude-7`, `claude-7-scribe-pass`, `ams-scribe-1`, `problem-peripheral`, `zai-3` | ~89 (of which ~60 scribe/guide/claude-7) | mined from Solver/Guide turns in the math loop; content may be reusable, naming is not |
| **B. `zai-1` compression-pilot diaries** | 114 | `Student marks for compression-pilot b01J02 read-packet 3 …` — reading-packet diaries from the *compression pilot*, not the math loop |

Population A's 22 already-reviewed members join the same pass: their
reviews predate the generality test and `:review/reason`/`:review/residual`,
so they cannot pass `candidate-visible?` as they stand.

### A — review each, properly

Reviewer ≠ depositor, under `promotion-proctor-v2` plus the v3 codes
(`:route-not-obstacle`, `:not-general`, `:proof-text-not-memory`,
`:pattern-without-witness`). One extra verdict beyond
`approve / reassign / reject`:

- **`generalize`** — approve *conditional on* a rewritten hook (problem id
  → subject ref; `apm_…` identifiers and `Main.lean:N` → `evidence-ids`)
  and a parent-pattern attachment, **with the rewrite done in the same
  pass** by the reviewer. The rewritten memory gets a new content digest;
  the review evidence cites both the original id and the rewrite, so the
  edge history shows what changed and why.

A `generalize` that cannot name the parent pattern is a `:reject` with
`:no-parent-pattern`. A leaf whose only content is "on a01J05 do X" with
no obstacle stated is `:reject` with `:route-not-obstacle`, not
`generalize` — there is nothing to generalise.

### B — sample, then class verdict

A reviewer asked to look at 114 diaries will say "not a memory" 114 times.
Instead: the same reviewer judges a **random sample of ~10** (seeded,
ids recorded). If the sample confirms, **one class verdict** is recorded
with the sample ids as evidence, and the 114 are retagged under that
receipt (`:attachment-status :rejected`, `:review/receipt <id>`). If the
sample does *not* confirm — some diaries carry a real rewrite rule — the
class is split and the remainder go through A. Still a review, still
receipted, not a quarantine by fiat.

## Machinery — all of it exists outside a frame

- `futon3c.apm.live-promotion/run-live!` takes a `:deposit-request`
  (candidates) and a `:reviewer-request` and drives deposit → mechanical
  guard → independent review → publication; it does not need a frame
  ledger.
- `scripts/review_codex_lane_attachments.clj` is a prior out-of-frame
  review pass — copy its shape (build candidates, dispatch reviewer seat,
  collect receipts).
- The mechanical guard (`promotion_pipeline/apply-mechanical-reviews`,
  landed 38416c48 / 148fb275) runs first and will take a slice of A off
  the reviewer's desk (7 proof-text, 34 `apm_` identifiers, 5
  `Main.lean:N`) with codes already in the receipt.
- The candidate list comes from `memory_shape.py`'s population filter —
  add `--list-problem-centric` (ids + author + status) rather than
  re-deriving it.

## The packet (one, after `invoke-…878` lands)

1. Build the candidate list from `memory_shape.py` (populations A and B,
   with author and current status).
2. Dispatch a reviewer seat that deposited **none** of them (a Zai or
   Codex agent; not `zai-1`, not any `f*-scribe`/`f*-guide` identity).
3. A: per-memory review with the `generalize` verdict and same-pass
   rewrite. B: sample of ~10 → class verdict → retag under the receipt.
4. Collect receipts; every verdict lands as review evidence on the
   `:memory/assert` edge.
5. Re-run `memory_shape.py` and record the second reading next to the
   first: expect `problem-id-in-name/hook/body` to fall from 203 toward
   the count of genuine rejects, `reviewed` to rise, and the reviewed
   graph's bespoke count to fall.

## Findings from the cross-frame snapshot review (8c1809ae / 621416eb, 2026-08-24)

Reviewing the packet that seeds each frame's `:promote-solver` snapshot with
prior frames' reviewed memories sharpened why this pass is urgent, not
optional:

1. **The proof-text memories are now campaign-wide.** A read-only dry-run of
   `campaign-prior-memories` against the live campaign returns 19 prior
   candidates (f28: 4, f29: 10, f30: 5, 0 dropped) — and **6 of the 19 are
   the f29/f30 proof-text blocks** (`e-83ece32c`, `e-021bf80a`,
   `e-c86a7d45`, `e-f6b4c56b`, `e-f0db6e27`, `e-20851bde`). They are
   `:attachment-status :reviewed`, so seeding passes them through; the
   mechanical guard cannot run on priors because snapshot entries carry no
   body. Until this review pass rejects them, every future Student inherits
   the f29/f30 cheat codes. **Run population A before F32's
   `:promote-solver`.**
2. **Reuse, measured properly, is zero.** `memory_shape.py` now computes
   "used on a problem ≠ mined-from" (origin = `:evidence/subject` problem
   ref, else the id in name/hook/body; use = each attempt receipt's
   `:receipt/problem-id`). Second reading (`memory-shape-2026-08-24b.json`):
   18 distinct used memories — **0 used on another problem, 18 used only on
   the problem they came from, 0 of unknown origin.** The earlier
   "used in ≥2 frames" metric was wrong in both directions (two frames on
   one problem would count; one frame per problem never could) and is kept
   only for comparison.
3. **Within-frame unions are still all-or-nothing.** `promote-solver` now
   drops and lists an invisible prior (`:receipt/prior-dropped`); the Guide
   and Zai-scribe unions later in the same frame still fail closed on *any*
   invisible entry, inherited priors included. Low likelihood, but a
   `generalize` rewrite that changes a memory's digest mid-campaign is
   exactly the event that would trip it — sequence this pass between
   frames, not during one.

## What this does not do

It does not make the memories *findable* from another problem by itself —
that needs the cross-frame snapshot (`invoke-…878`) so the Student can see
prior frames' reviewed memories at all. Review makes them *eligible*;
snapshot seeding makes them *accessible*; whether they are then *used* is
the reading after the next frame.
