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
