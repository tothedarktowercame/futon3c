# T-forward-model-vs-active-work — the cascade and the clock disagree

Raised by joe + claude-13, 2026-08-24, out of the futon-2026 paper work
(`p4ng/sec-operator.tex` Fig. `fig:cascade`, `p4ng/sec-turn-chain.tex` Fig. 2).

**Status: FINDING, not a defect. Do not hotfix.** Joe's call, and the reason is
the substance of the ticket: the mismatch is itself a high-precision signal, and
flattening it into a marking convention destroys the thing worth reading.

## The observation

The merged backlog cascade (`p4ng/empirics-futon/cascade-map.edn`, 23 missions in
six lanes) is the stack's forward model of what it is building. Exactly one rung
carries evidence of being current: `m3` = `M-text-sidecar`, `:shipped true`,
*"query surface live; verified by exercising it"* — i.e. a scored query returns
and a nonsense control returns nothing.

Measured over the window 2026-08-19 → 08-23, across all agents, the clocked
missions were:

| mission | turns | a cascade rung? |
|---|---:|---|
| `M-apm-demonstration` | 21 | no |
| `M-futon-problems` | 6 | no |

**0 of 23 cascade rungs carried an active clock-in.** The forward model and the
attention have empty intersection.

This replicates, at mission granularity, the paper's Figure 2 finding at pattern
granularity: route A (clock-in) and route C (documented mission) agree on 0 of
11. The cascade is the sharper case, because it is *specifically* the artifact
that claims to say what should be worked on next.

## What the operator supplied that the record does not have

Joe, in the turn that raised this — and his framing is the point, so it is quoted
rather than paraphrased:

- `M-apm-demonstration` is **crucial for an APM capability star** — "even if
  that's not written down anywhere besides this turn."
- `M-futon-problems` is **a kind of successor to `M-interim-director`** — "even
  if that's not written down anywhere." `M-interim-director (family)` is rung
  `m16` ("conversion economics").

So the two clocked missions are not off-model; they are *unmapped*. The edges
that would place them exist, and they exist **only in the operator's head**.
Joe: "these not-written-down facts that I'm talking about now are signals coming
from the outside world (where I live)."

That is the futon-2026 thesis instantiated on its own forward model: the loop
records observables about the operator it never consults, and the missing
variable here is the mapping, not the mission.

## Why this must not be hotfixed

The obvious repair — mark rungs with two predicates, `capability-live`
(exercised, matched null fails) and `attention-live` (clocked in window) — would
take the count from 1/23 + 0/23 to something tidier and **would delete the
signal**. A persistent gap between the forward model and where the operator
actually is, is exactly the kind of prediction error an active-inference loop
should be *reading*, not smoothing. Record it; do not zero it.

The two-predicate marking is noted here as **one candidate resolution**, not the
plan, and it has an independent argument for it if it is ever adopted: folding
"the built thing answers" and "someone is attending to it" into a single `live`
flag reproduces precisely the error the paper indicts — a capability ledger
where one invoice and a repeatable revenue capability both read `:satisfied`.
Same status, incomparable evidence. If two marks are ever added, they must stay
two.

## The actual ask

Expand `M-points-de-fuite` and the FTS methods to cover operator-supplied
mappings of this kind — a channel where "X is a successor to Y", "X serves
capability star Z" can be deposited when the operator says them, and read back
when the forward model is scored. Today such a statement exists only in a REPL
turn and is lost at the end of it.

## Verification notes (what was actually checked)

- Rung membership by **name match** against `cascade-map.edn`. A rung could
  overlap conceptually under a different mission name — `M-apm-demonstration`
  and the `M-aif-faithfulness + M-war-machine-aif-completion` rung are
  plausibly related work under different names. **Not checked.**
- One window (2026-08-19 → 08-23). A rung clocked last month would not appear.
- `m3` is the only rung with any currency evidence because it is the only one
  re-checked that way; the figure caption is explicit that absence of a mark is
  absence of evidence, not of progress.

## Related

- Paper: `p4ng/sec-operator.tex` (cascade fig), `p4ng/sec-turn-chain.tex` (Fig 2,
  routes A/B/C), `p4ng/app-argument-outline.tex` step ⟨1.2.6⟩.
- Open elsewhere: whether the `0.009`-across-108-emissions action class stayed
  at that value or recovered. Bears on whether the harness's own collapse is an
  absorbing state. Run record not located — see below.
