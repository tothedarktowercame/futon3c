# E-operator-turn-modelling — measurements from the 2026-08-25 session

*Joe + claude-13, evening of 2026-08-25. Written 2026-08-26 after Joe's Emacs
buffer was lost in a crash. The design work of that session is in two committed
artefacts — `futon3c/holes/labs/M-apm-demonstration/role-cards/joe-scribe-v1.md`
(ffc114b8, 56f07386) and
`futon3/library/features/operator-turns-enter-the-observation-vector.flexiarg`
(90d6c87, 134a0e3). This note holds the four things that were measured in the
conversation and written into neither.*

## 1. Operator-driven → AIF-driven transfer: a negative result

Joe's framing: transfer from operator-driven sessions to AIF-driven sessions is
the equivalent of the Codex→Zai transfer in the maths realm, where
`zai-scribe-v2` deposits when the Student stumbles on something the Solver did
cleanly ("the gap between them is the deposit").

The operator act under test was a **lateral structural connection**: Joe saw
that R2's nag gate and inbox-zero's tier-1 escalation were the same shape — a
ladder built top-down whose top rung's precondition has no producer.

Put each side through the same futon3a retrieval that runs per turn:

    "nag gate never fires; acknowledged conjunct has no producer…"
       0.4142  measurement/ghost-as-typed-sorry
       0.3763  orchestration/pattern-warranted-choice-point
       0.3591  features/operator-turns-enter-the-observation-vector
       0.3487  cycle-machine/job-port

    "tier-1 never fires; the plan's seat id joins to no claim…"
       0.4016  agent/escalation-cost-vs-risk
       0.3950  agency/single-routing-authority
       0.3923  sidecar/bridge-triple-escalator
       0.3614  structure/interest-event-vocabulary

**Zero overlap**, and it survived the index refresh (§4), so it is not
staleness. Both retrievals are locally sensible; the failure is that embedding
similarity matches on TOPIC (nag gates, escalation tiers) while what the
operator supplied was SHAPE. Querying the shape itself returns
`pattern-warranted-choice-point` (0.4268), `mana-gated-work` (0.4263),
`aif/admissibility` (0.4140) — adjacent, none of them it. The shelf does not
hold the abstraction.

**The acceptance test this implies** (falsifiable, cheap, not yet run): write
the missing pattern, re-embed, and check that BOTH queries retrieve it. If they
do, an operator-driven session has transferred a connection an AIF-driven
session can reach on its own. If they don't, the deposit is prose.

**Caveat on generality.** This tests one kind of operator act. Three others from
the same evening are not pattern-shaped at all: rejecting a commit-time lint
gate on where a check belongs in a lifecycle; rejecting mid-turn commits by
consequence severity rather than a recorded metric; and "I don't have time for
that", a resource constraint the loop does not model. Expect the shelf half to
transfer and the card half not to — see the two-destination split in the
Joe-Scribe card.

## 2. Novelty judged by the cascade — calibration, and how it fails

Joe's proposal: Air mints new flexiargs from operator turns, using the cascade
to judge novelty.

Calibration on the refreshed index — `cascades/on-the-fly-cascade` retrieves its
own conclusion text at **0.5644**, with its three siblings behind it; ordinary
unrelated neighbours sit at **0.33–0.42**. So the usable band between "this
exact pattern exists" and "nothing like this exists" is narrow but real, and
novelty-by-score is possible **only as a calibrated comparison, never as a fixed
threshold**.

**The failure mode to design against.** A stale index fails in the worst
direction: everything written since the last embed pass looks novel, because the
thing doing the judging cannot see it. On 2026-08-25 that was 145 patterns
(§4), so Air would have minted 145 duplicates with high confidence — precisely
the bloat `cascades/edges-earn-permanence` names ("skeleton bloats into noise").
Any novelty check must first assert the index covers the shelf.

## 3. session-mode.el as the operator-facing consumer

`session-mode.el` already holds both hard parts: it reads the per-turn
`context-retrieval` evidence and resolves sigils from it, and it has a display
surface (posframe hover card). What it does not do is ACCUMULATE — it is
per-turn markup, not structure across turns.

**The APM analogy does not transfer directly.** `conductor.clj` builds the
memory cascade that guides Zai by traversing `memory/assert` hyperedges filtered
by `attachment-status = "reviewed"`, then `pattern/has-semantic-why` outward,
capped at 100. Every edge in it has passed review. A live per-turn cascade is a
different object: derived, unreviewed, `:proposed`. The existence proof sets the
bar rather than transferring to it.

**And Zai is cold; Joe is not.** Zai arrives at each problem with no memory of
prior attempts, so the cascade supplies what it cannot remember. A cascade
surfacing what the operator already knows is noise. The value for the operator
is a different organ: not recall but ORIENTATION — which patterns this session
is converging on. That is M-points-de-fuite's concentration field, i.e. the
`field-read` organ, sitting beside `act+witness` in the same unstaffed list.

Joe's rendering note: Tufte sidenotes sit BESIDE the text they annotate, with no
reference marks and nothing to jump to. The posframe hover is the opposite —
on-demand, cursor-driven, one at a time. A margin rendering would let an
accumulating cascade be read as a field rather than queried node by node.

**Condition on building it.** `zai-scribe-v2` requires a `:memory-use` report
(`:queries`, `:surfaced-ids`, `:used-ids`). An operator-facing cascade needs the
same or it is instrument theatre: surfaced and never used is a decoration that
costs a glance per turn.

## 4. The retrieval index was 15 days stale, and why

`minilm_pattern_embeddings.json` was dated 2026-08-10 and held 1,247 pattern
ids against 1,219 shelf flexiargs, with **145 on the shelf and not embedded** —
including all five `cascades/*` patterns, committed 2026-08-17. The node layer
of every cascade could not see the cascade patterns.

Cause: `index_patterns.sh` prefers `clj`, which needs `rlwrap`, which is not
installed on zone; the default path exits 1 before writing. Fail-closed worked
— the old index survived — but nothing reported the gap. Fixed by running with
`CLJ_CMD=clojure`; a daily `futon-pattern-index.timer` now carries it.

**Precision is still unmeasured, and a cascade compounds it.** A sampled turn
returned `iching/hexagram-39-jian` at rank 2 (0.4744) against rank 1's 0.4809 —
a 0.0065 gap — and nothing in the stack knows whether that is a hit. A cascade
inherits every error in its nodes and turns it into edges. An operator-facing
one spends attention, which is the resource this design is shortest of. Measure
top-3 retrieval precision against turns whose right answer is known BEFORE
building either consumer.

## Open, in the order they gate things

1. Measure top-3 retrieval precision (§4). Gates both consumers.
2. Run the transfer test in §1 — write the pattern, re-embed, re-query.
3. Bridge the 58 `p4ng/*` control patterns to shelf retrieval, or scope the WM
   consumer out of the first cut (see the R2 flexiarg).
4. The Joe-Scribe card's five wiring gaps; it has no force until the first four
   land.
