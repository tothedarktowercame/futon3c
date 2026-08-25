# TN: F33 / a94A07 — the "30-minute student budget" was never wired in; the real ceiling is 60 minutes, uniform across every seat

Author: Claude (Fable 5, session referred to as "sonnet"), 2026-08-25T10:25Z.
Live babysitting observation of campaign `jit-all-open-nontopology-v1`, frame
`f33`, problem `a94A07`, prompted by Joe noticing `student-attempt-1` running
long and asking whether chasing full early Zai solves (rather than measuring
progress within a fixed, strategically short budget) risks inflating frame
cost well past 3 hours.

Apparatus under review: futon3c `master` at `782d112b`.

## Method

1. Checked `f33-student`'s live Agency registration (`GET /api/alpha/agents`)
   for its actual turn-timeout and elapsed time.
2. Read `live/student-attempt-1.edn` for the dispatched request's
   `:terminal-budget` and timeout fields.
3. Grepped `src/` for every definition and consumer of `turn-timeout-ms` to
   find where the enforced value actually comes from.
4. Read `generated_contract.clj`'s `required-bounds` (the canonical,
   Lean-emitted contract this campaign qualifies against).
5. Read `campaign_qualification.clj`'s seat-config handling, which reports
   `:solver-minutes` and `:student-minutes` as separate fields.
6. Read `role-cards/zai-student-v2.md` for the Student's own stated bound.
7. Searched the whole `M-apm-demonstration` lab tree for any "30 minute"
   reference to find where that figure originated.

## Finding — three different numbers exist, and none of them is 30 minutes

**What's actually enforced:** `f33-student`'s live registration shows
`turn-timeout-ms: 3600000` (60 minutes), `turn/source:
:frame-seat/code-default`. This traces to `generated_contract.clj`:

```clojure
(def required-bounds
  {...
   :seat-turn-timeout-ms 3600000
   ...})
```

This single value is applied **uniformly to every seat** — solver, student,
guide, proctor, promotion-proctor, scribe, zai-scribe, analyst all get the
same 60-minute ceiling. Confirmed by grepping every consumer of
`turn-timeout-ms` (`live_learning_phases.clj`, `countdown_control.clj`,
`live_proof_phases.clj`, `library_lane_launch.clj`, `library_lane_phases.clj`,
`live_launch_preparation.clj`, `queued_frame_adapter.clj`) — all either read
the same flat contract value or default to the same literal `3600000`.

**What the role card says:** `role-cards/zai-student-v2.md` line 41 states a
different, third number: **"3 attempts. Hard stop at 120 minutes."** — worded
ambiguously as to whether that's 120 minutes *per attempt* or *total across
all 3 attempts*, and it's a self-reported instruction to the agent, not
something the harness enforces.

**What's nowhere:** searching the entire `M-apm-demonstration` lab tree
(design docs, proposals, role cards) for "30 min" / "30-minute" / "30min"
returns zero hits. The 30-minute figure that both Joe and I have been
carrying tonight as the assumed student budget does not appear to be written
down anywhere, and nothing in the running system enforces it.

**Concrete instance that surfaced this:** f33's `student-attempt-1` started
at `09:28:36Z`; by the time Joe asked about it (`10:10Z`) it was 42 minutes
in, still `:invoking`, no coordinator or watchdog alert — legitimately still
running, within the actual 60-minute ceiling, just past whatever informal
30-minute expectation was in the room. It also had an unusually large memory
snapshot (22 `:accessible-memory-ids`, versus single digits on f28–f30),
which may be part of why this attempt is running long relative to earlier
ones — not confirmed, just a candidate contributing factor.

## Why this matters (Joe's framing, and it holds up)

The strategic point: a short, fixed student budget is meant to measure how
much of a gap toward the solver's own (much longer) solve the Student can
close, not whether the Student can eventually solve it unboundedly — "if we
happen to compress a 1h30m solve from Codex into 30m on Zai, that's a big
achievement," but that signal only exists if the budget is actually short and
actually enforced. Letting each of 3 attempts run to the current 60-minute
ceiling (rather than a deliberately smaller number) both destroys that
measurement and multiplies frame cost: 3 × up to 60 min for the student
phase alone, on top of the solver's own solve time, guide interventions, and
scribe/proctor overhead — comfortably over 3 hours per frame even before
counting any stall/repair time, which is consistent with what's actually
been observed tonight (f28 and f30 solve phases alone ran 1h19m and multiple
hours respectively).

## The plumbing for a role-specific budget already half-exists

`campaign_qualification.clj` reads `[:student :turn-timeout-ms]` as a value
*distinct* from `[:solver :turn-timeout-ms]` and reports them as separate
`:student-minutes` / `:solver-minutes` fields in its qualification report —
implying per-role differentiation was anticipated by whoever wrote that
check. But because `generated_contract.clj` currently emits one flat
`:seat-turn-timeout-ms` applied to all roles, that distinction is a no-op
today: both fields will always read the same 60 minutes until the contract
itself becomes role-aware.

## What would need to change (not attempted tonight — handoff, not a fix)

- `generated_contract.clj`'s `required-bounds`: either add a student-specific
  bound alongside `:seat-turn-timeout-ms`, or restructure it into a per-role
  map. I did not trace how far upstream this contract is actually generated
  from Lean vs. hand-maintained here — worth checking before assuming this
  file is the true source of truth.
- Every consumer that currently reads `turn-timeout-ms` as a flat scalar
  (the list in Finding 1 above) would need to become role-aware.
- `campaign_qualification.clj`'s reporting already expects `[:student
  :turn-timeout-ms]` to potentially differ, so that half should work
  unmodified once seat-configs actually vary by role.
- `role-cards/zai-student-v2.md` line 41 would need updating to match
  whatever number is chosen, with the per-attempt-vs-total ambiguity
  resolved explicitly rather than left implicit.

## Finding 2 (Joe, 2026-08-25T10:32Z) — "120 minutes" is almost certainly per-attempt, not a total

120 doesn't factor cleanly against either candidate per-attempt number: it is
neither 3×30 (=90) nor 3×60 (=180). If the card meant "120 minutes total
across the 3 attempts," you'd expect a number that actually divides by 3
against whatever the intended per-attempt figure was. It doesn't — which
argues the card means each attempt individually gets up to 120 minutes.

A second, independent argument points the same way: the card is written as
direct instruction to the agent itself ("## Bounds — 3 attempts. Hard stop at
120 minutes."), and the same document states two paragraphs earlier that
*"you will not remember previous attempts"* — a fresh cold-start session has
no mechanism to track a running total against a cross-attempt budget even if
it wanted to. A "hard stop" instruction only makes sense as guidance the
agent can act on *within its own session*, which structurally forces the
per-attempt reading.

So this isn't a documentation ambiguity to resolve later — it reads as
someone deliberately authorizing **up to 2 hours per student attempt**,
which is even more generous than the 60-minute ceiling Agency currently
enforces (the harness's flat `:seat-turn-timeout-ms 3600000` cuts every
attempt off at 60 min regardless, so the card's 120-minute allowance is
currently moot in practice — but it tells us what the *intended* design was,
and it was not a tight compression-measurement budget). That's the opposite
end of the range from Joe's 30-minute strategic figure: intended-by-card
(120) > enforced-by-harness (60) > intended-by-Joe (30), a three-way
mismatch, not a two-way one.

## Open questions for whoever picks this up

- What number? Joe raised 30 minutes as the illustrative strategic figure
  (measure compression, don't chase a full solve); the role card's own
  120-minutes-per-attempt reading (Finding 2) is roughly the opposite intent.
  Those two need to be reconciled explicitly, not silently overridden — ask
  whoever authored the v2 card's "Hard stop at 120 minutes" line whether
  that was a considered choice or copied from the solver's own more generous
  round budget, before just replacing it with 30.
- Worth considering a number scaled to the matched solver's own solve time
  rather than a flat constant, given solve-phase duration varies hugely by
  problem (f28: 1h19m; f29: 22m — already flagged in
  `TN-sonnet-F29-finding.md`).
- Does "hard stop" mean Agency kills the turn externally at the boundary
  (student submits whatever partial state exists), or is the agent expected
  to self-monitor and submit early? Turn-timeout-ms today is an
  externally-enforced Agency ceiling, so presumably the former — worth
  confirming the "ran out of turn budget, repair job recovered the
  submission" pattern already seen once on f29 (`TN-sonnet-F29-finding.md`,
  Finding 1) still behaves cleanly at a much shorter boundary, since each
  repair job triggered adds real wall-clock cost.

## Not yet raised with codex-10

Per Joe: this is a design decision he wants to hand to Codex "at an
appropriate point in time," not tonight's operational stall queue — no bell
was sent. This note is the handoff artifact for that later dispatch.
