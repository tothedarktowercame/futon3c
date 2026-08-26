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

---

# Addendum 2026-08-26 — the WM side, measured on the mechanism itself

*Joe, 2026-08-26: the §1 experiment "was a scratch-paper exercise that has very
little to do with what actually goes on inside the War Machine when it runs …
we need to build from both sides." He is right, and the flaw is specific: §1
queried `notions_search.py` with my own paraphrases. That tests the per-turn
`context-retrieval` path. It is not how a WM cascade is built. What follows runs
the WM's own constructor.*

## How cascades are actually used in the WM

Two distinct uses, not one:

1. **`strategic-cascade/outer-frontier`** — control patterns (`p4ng/*`) as
   ordered lenses. Each step runs a Phase-4 adapter query returning candidate
   and excluded missions; the result is a frontier of `:ready`/`:held` missions
   with reasons, plus `:no-witnessed-mission` holes for steps that yielded
   nothing. Budget truncates: `(take budget shown)` runs, the rest are
   `skipped-patterns`.
2. **`chipwitz/find-warrant`** — the deliberation gate. Given a circumstance
   `{:psi <text> :choice-point {…}}` it calls `cascade-policy-for psi budget`,
   takes the top `:rel`, and if `rel >= *warrant-threshold*` (0.45) the choice
   is DETERMINED and the WM proceeds without deliberating.

`:rel` is **raw MiniLM cosine**: `cascade_construct.py` loads
`resources/notions/minilm_pattern_embeddings.json` — the file that was 15 days
stale — and encodes psi with `normalize_embeddings=True`. So the staleness in §4
was not a side issue; it was degrading `rel` for every choice point the WM made.

## Weakness 1 — the live fallback psi is a mission slug, and it carries no signal

`portfolio/effect.clj:78`:

    psi      (or (:psi effect) mission-id "")
    psi-text (-> (str psi) (str/replace #"^M-" "") (str/replace #"-" " "))

With no `:focus`/`:circumstance`, the cascade for a mission is constructed from
its **name**. Same mission, two psi sources, through `cascade_serve.py 20 0.15`:

| mission | psi = name | psi = first 890 chars of the mission doc |
|---|---|---|
| apm-demonstration | size 2 · wholeness 0.395 · top **0.2440** `iching/hexagram-16-yu` | size 5 · wholeness 2.101 · top **0.5150** `invariant-coherence/shape-first-identify` |
| dionysus-winddown | size 2 · wholeness 0.340 · top **0.2040** `iching/hexagram-33-dun` | size 3 · wholeness 1.354 · top **0.4910** `exotic/live-sync-source-truth` |
| case-studies | size 3 · wholeness 0.987 · top **0.4310** | size 2 · wholeness 0.869 · top **0.4840** |

Mission text clears the 0.45 warrant threshold in all three; the slug clears it
in none. And the slug scores are indistinguishable from nonsense of the same
length:

    "apm demonstration" 0.2440   |  "banana forklift"  0.2410
    "dionysus winddown" 0.2040   |  "purple tuesday"   0.1360

So on the fallback path the WM constructs a policy cascade whose top pattern is
an I Ching hexagram matched to a two-word slug at cosine ~0.22. (iching is 64 of
1371 patterns, median text 1904 chars against 1346 overall — long, evocative,
weakly similar to everything, which is why it surfaces when there is no signal.)

**Unmeasured, and it bounds this finding:** how often the live path actually
falls back to the slug rather than carrying a real `:focus`/`:circumstance`.
Measure that before sizing the fix.

## Weakness 2 — cascade depth is a function of psi length

Coverage-saturated `size` was 1 for one-sentence circumstances, 2–3 for slugs,
2–5 for 890-char mission text. A cascade is only as structured as its input is
long. Any per-turn cascade built from operator turns — which are short — is
degenerate by construction unless psi is enriched first.

## Weakness 3 — the threshold is calibrated for mission text, the gate fires on choice points

0.45 sits just under mission-text tops (0.48–0.52) and far above
circumstance-sentence tops (~0.30). `find-warrant`'s docstring says the
motivating example "had expected-information-gain at rel ~0.5". So the gate is
tuned to the long-psi regime while being invoked on choice points, whose
descriptions are short.

## Weakness 4 — two smaller ones

- `find-warrant` defaults to **budget 6**; `cascade-lane/default-budget` is
  **20** by operator ruling of 2026-07-05, because invariant-grade patterns
  arrived at greedy ranks 10–16. `shown` is in greedy-coverage order, not rel
  order, so budget 6 can exclude the pattern that would have topped by rel.
- `cascade-policy-for` memoizes on `[psi-text budget epsilon]` and
  `clear-cache!` is called from nowhere outside its own namespace. After a
  re-embed the live JVM keeps scores computed against the old index.

## What this says about the operator model

The cascade machinery is not the weak part; **the psi is**. The constructor
produces real structure when handed a real circumstance description and noise
when handed a slug — and the slug is the live default. A better model of the
operator contributes exactly the missing thing: operator turns are the richest
available statement of what the current circumstance IS, in the operator's own
terms, at the moment the choice is live.

That reframes the Joe-Scribe seat's output. Not only candidate memories — a
**psi constructor**: turning a session's operator turns into a circumstance
description long and specific enough for the cascade to have wholeness. Joe's
earlier "Air composes the FTS query" idea is the same move one layer over.

**The test to run before building anything:** take a mission where the WM
currently falls back to the slug, construct psi from that session's operator
turns instead, and compare `size`, `wholeness` and top `rel` against both the
slug and the mission doc. That is the two-sided experiment, and it is cheap.
