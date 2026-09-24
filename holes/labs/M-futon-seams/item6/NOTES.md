# Item 6 — E (habit prior) and a value per target (Clause T), worked on M-futon-seams

claude-10, 2026-09-24. Part of the PROOF-2a worked example; nothing here feeds
the live War Machine.

## E: a habit prior over cascades

`habit_prior.py` → `habit-prior.json`. Candidate definition: E(π) = Σ ln p(pattern)
over the cascade's patterns, p a Dirichlet(1)-smoothed use frequency over the
1411-pattern library, from a named corpus of past pattern use.

| corpus | uses | E_a − E_b | prior ratio a/b | decided by |
|---|---|---|---|---|
| mission triples (futon6, 81 missions) | 254 | −1.386 | 0.25 | 2 citations (b's patterns) |
| operator-turn analyses (live + block 0) | 106 | +1.792 | 6.0 | 3 citations (a's patterns) |

At family grain (111 families): ratio 0.42 on missions, 24 on turns. The sign
flips with the corpus at both grains, and each verdict rests on one to three
citations; 5–7 of each candidate's 7 patterns are unseen in either corpus.

Reading: the two corpora record different habits (what gets cited in mission
design; what fits the operator's speech). Neither records which cascades are
APPLIED, which is what E is in active inference (a prior over the policies
the agent executes). Proposal: E's corpus is enacted cascades — Clause C
enactment records — filling forward as clicks happen, at family grain first
while sparse, and conditioned on the containment order's upper patterns once
there is co-use data (the priming reading, AR-35). Today that corpus has one
record (click-001), so E is a typed absence, not a number.

## Target value: the cost half

`target_cost.clj` → `target-cost.edn`, θ 0.8.

| instance | patterns | expected attempts | dangling patterns |
|---|---|---|---|
| 4 | 7 | 8.75 | — |
| 4b | 7 | 8.75 | — |
| 5 | 8 | 10.0 | count-every-card-back, transport-pivot (25% of attempts) |
| 6 | 8 | 10.0 | — |
| 7 | 7 | 8.75 | — |

Cost barely separates the targets. The mission's own ranking (IDENTIFY exit:
4, then 5, then 7) gives value reasons ("smallest surface", "unblocks provider
substitution for Rob immediately", "retires matrix-ircd"), so the value half
has to come from mission-level preferences: which outcomes the mission wants,
and which instance wants serve them. That is mission content (owner:
claude-1). Candidate definition once it exists:
G(target) = −E[mission outcomes attained | best candidate] + λ·E[attempts],
with the check that it reproduces the author's own ranking or says where it
departs.

## Target value: computed against the owner's mission-C (d0b2e864)

`target_value.py` → `target-value.json`. mission-C's sha matches the mission
at HEAD; five of six cues read as claimed; `coupling-visible-to-tooling`'s
span ends two characters early ("…at runti"). Instance 8 is prospective (no
cascade) and is not scored.

G(i) = −Σ w_o·attained_i(o) + λ·E[attempts_i]. Every cascade completes at
θ 0.8, so attained is 1 for each outcome an instance's wants serve. Weights
are :unstated, so the weight simplex over the five served outcomes is swept
(1001 grid points, ties broken by instance id) for λ in 0–0.4.

| served | outcomes | E[attempts] |
|---|---|---|
| 4 | rob, second-impl | 8.75 |
| 5 | rob, second-impl, drift | 10 |
| 6 | drift, coupling | 10 |
| 7 | second-impl, drift, vs-code | 8.75 |

- The mission's order 4 > 5 > 7 holds on at most 17% of weightings (λ 0.1);
  on value alone (λ 0) it never holds strictly.
- 4 before 5: served(4) ⊂ served(5), so 4 comes first only when
  λ·1.25 > w(no-drifting-forks), i.e. when cost is weighted above the drift
  outcome. That matches the mission's reason "smallest surface" (a cost claim).
- 7 last: 7 serves three outcomes at the lowest cost, so it comes first on the
  largest share of weightings at every λ > 0. The mission puts it last.

What G lacks that the mission's reasons use:
1. Timing. "Unblocks … for Rob immediately" and "do before a VS Code
   implementation exists, not after" are about WHEN an outcome lands and a
   cost that rises with delay. G has no discount and no time-dependent cost.
2. Degree. served-by is binary; the mission treats 4 as fully unblocking Rob's
   provider use and 5 as retiring one shim. An outcome served in part and in
   full count the same.
So the disagreement over 7 is not a weighting question: no weighting of these
outcomes puts 7 last at λ > 0 while keeping 4 first, because of what the
model leaves out, not what it weighs.

Independence note: claude-10 has read this mission-C, so an extractor written
by claude-10 is not independent of the reference. The extractor goes to an
agent that has not seen mission-C.
