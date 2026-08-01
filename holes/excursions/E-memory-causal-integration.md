# E-memory-causal-integration — joint modeling with Rob's causal engine

**Opened 2026-08-01 by claude, on receipt of the first reply from Rob's agents
("facadebootstrap") to the V2 cover note** — same day the note was pushed.
This doc banks that reply's load-bearing content and tracks the joint
modeling work it opens. Strategy/sequencing consequences are applied in
`E-memory-resourcing-and-strategy.md`; experiment designs stay in
`E-memory-v3-programme.md`.

**Status of their side, as reported:** the engine supports typed
interventions, conditioning, d-separation, backdoor checks, identification,
structural counterfactuals, mediation/effect algebra, persistence, replay,
and navigable source-linked cards. Structure is **authored, then revised
from evidence** — not autonomous discovery — so V3's job for them is the
explicit DAG, drawn properly. Their compiler-repair work runs exactly the
cycle we want: author a coarse model, observe failures, identify the
upstream cause, revise implementation and graph together, test predicted
downstream consequences.

**Their documentation pointers (mfuton, not on our disk — zip offered and
accepted):**

- `M-book-of-why-causal-memory-graph-for-compiler-repair.md`
- `M-book-of-why-worked-causal-graph-examples-in-memory.md`
- `…/M-book-of-why-operational-causal-semantics-engine.md`
- `…/M-lean-parser-compiler-causal-model.md`

---

## 1. The working DAG (theirs, adopted as the joint starting point)

```
problem/pre-solution state + corpus
    -> query and candidate generation
    -> memory availability
    -> surfacing
    -> information supplied AND/OR policy/route changed
    -> runner trajectory
    -> independent Lean witness
```

with a separate **measurement layer**: receipt fields and instrumentation
are sensors on these nodes, not nodes themselves, and their **recency-biased
missingness must itself be modeled** — an absent receipt value is not
evidence the causal event did not happen. (This is V2 §4.6's
"third field to fail the same way," returned to us as a modeling
obligation rather than a lament.)

**Tidy alignment worth stating: the Phase-0 repair list is exactly a sensor
per DAG node.** B2 (seed capture) instruments query/candidate generation;
A4 (`surfacing-via` everywhere) instruments surfacing; B4
(`:memory-use/kind`) records *which channel* — information vs policy — the
memory acted through; A1 (`record-outcome!`) instruments use; the Lean
compiler is the witness node by construction. The repairs were derived from
V2's failures; that they tile the DAG is independent confirmation both were
cut at the right joints.

**Route-relativity:** confirmed expressible, with the correction that route
must be an **explicit moderator/parent of trajectory**, not metadata on a
memory. Open item on their side: whether their current public operation
request exposes this conveniently, "rather than merely claiming the
underlying model can encode it." Treat as unverified until the worked
example exists.

**Regulative/substitutive as edge-target: confirmed, very cleanly.**

```
substitutive: Memory -> AvailableInformation -> Decision
regulative:   Memory -> Policy/Route/StoppingDecision -> Decision
```

And their gloss is the explanation V2 wanted: `used-ids` observes the
information channel while many memories act through policy.

## 2. Their qualifications, each accepted, with consequences

| # | qualification | consequence for us |
|---|---|---|
| Q1 | `:blocked-on` is a **dependency** relation, not automatically a causal arrow; it becomes causal only when intervening on the blocker has defined consequences downstream | the queue's conative edges (staging §G2, §G7) enter the joint model as *candidate* arrows requiring interventional semantics, not as given structure |
| Q2 | **E2's `do(withhold M)` identifies the effect of memory *availability*, not memory *content*** — distinguish intention-to-treat from effects mediated by surfacing and actual use | E2's estimand must be stated as ITT before it runs; mediation through surfacing/use is a separate, second estimand. Same discipline applies to the cohort (§3 below) |
| Q3 | the 38% stays an **adjudication** until a structural model *and* intervention data exist — E2 alone does not promote it | matches our framing; the promotion path is SCM + E2 jointly, which is the argument for encoding the model first |
| Q4 | V2's "the corpus indexes advice, not artifacts" is a **valid frozen-baseline claim**; the shift to Lean proofs as a main memory source is a **later regime** V3 must model separately, not silently update | V3 gets an explicit two-regime structure: advice-era (V2's frozen corpus) vs artifact-era (the cohort's regime). The prospective cohort runs entirely in the new regime and must say so; cross-regime comparisons are between-model, not within |
| Q5 | three structurally different graphs do not establish a general richness law for λ₂ | already V2's own hedge; carried |

## 3. Estimands, made explicit while we are at it

The reply's ITT distinction applies to the cohort as much as to E2, and it
lands well for us:

- **Cohort primary = ITT of memory availability**, identified by the
  randomization itself. This is the clean, defensible headline estimand —
  no mediation assumptions needed.
- **Cohort secondaries = mediated effects** (through surfacing; through the
  information vs policy channel, readable once B4 exists) — these need the
  model, which is what makes the joint encoding worth doing *before* the
  cohort registers, not only before E2.
- **Granularity note:** the cohort intervenes on the whole recall channel
  (availability of the system); E2 intervenes on availability of *one
  memory*. Same node, different granularity — the model should carry both.

## 4. The new critical-path item

> **Encode the DAG, the measurement/missingness layer, and E2's estimands
> in their engine before the ~1M-token intervention run** — their words:
> "that could genuinely alter what E2 identifies rather than merely
> analyzing its results afterward."

Sequencing effect (applied in the strategy doc): E2 is now blocked on the
`ValidatedTrace` refactor **and** the estimand encoding; the cohort
registration wants the encoding too, for its secondaries. Neither blocks
the Phase-0 instrument repairs or the frozen-data experiments, which
proceed regardless.

## 5. Open items

1. ~~Receive and read the four mfuton mission docs~~ **DONE 2026-08-01 — see
   §6.** Pack retained at `~/code/external/rob-pearl-pack-2026-08-01/`
   (Rob's material; not committed here — his call).
2. ~~Draw our DAG properly~~ **DONE 2026-08-01 —
   `docs/memory-causal-graph-spec.json` (+ `.md` companion)**: 20 variables,
   34 evidenced arrows + 1 flagged conjecture (C1), 4 removable leak edges
   (the E2 isolation surgery), 9 sensors with missingness annotations, 2
   regime axes, both interventions, and the three requested receipts
   (Q1 cohort identification, Q2 E2-under-surgery + mediation, Q3
   filter-equivalence d-separation on both topologies, with a falsifiable
   divergence prediction). Verified valid JSON and acyclic; roots = exogenous
   contexts + interventions, sinks = the t+1 states. Awaiting their encoding.
3. Ask their side for the worked route-as-moderator example — **sharpened by
   §6.4**: the natural encodings are route-as-explicit-variable (their
   requirement 9-style context node) or route-as-regime (their transport
   machinery); which one their public surface makes convenient is exactly
   their own flagged unverified item.
4. Decide where the missingness model lives — **partly settled by §6.6**:
   their engine does exact finite inference and identification, not
   statistical estimation, so the missingness *model* (MNAR weighting etc.)
   stays in our analysis code; what their engine contributes is the
   measurement layer's *structure* (which sensor observes which node) and
   d-separation checks that a given analysis is licensed.

## 6. The pack, read — what their system actually is (2026-08-01)

Four mission docs, ~5,200 lines. The short version: **this is not a
framework that could express our problem; it is an operational discipline
with live machinery, and large parts of V3 can adopt it nearly wholesale.**

**6.1 The substrate.** A typed memory store (Neo4j + pgvector, MCP-served)
in which *The Book of Why itself* is a provenance-preserving hierarchy:
2,673 immutable source atoms, L1–L4 summary layers, page + editorial-index
graph, 8,274 typed relations, live-audited. The causal engine's semantics
were derived from 82 source obligations read off that hierarchy and
dispositioned to zero missing/partial — 36 live typed operations plus 3
source-justified refusals, 505 tests, every operation persisted with a
stable id and **replay-verified with zero writes**. Their receipts
discipline is at least as strict as ours.

**6.2 The engine's capability surface** (all live): query-rung
classification and refusal of causal-direction-from-association;
d-separation, backdoor enumeration, Markov-equivalence bounds, variable-role
classification; belief propagation; observational-vs-interventional regime
comparison; Cornfield sensitivity; do-calculus Rules 1–3 with
machine-readable mutilation/side-condition receipts; full identification
including sequential policies, surrogates, **transport across regimes, data
fusion, selection-bias recovery**; structural counterfactuals (potential
outcomes, treatment-on-treated, edge/path/split interventions); the full
mediation family (**TE/CDE/NDE/NIE/path-specific**); linear + LATE; and
causal-explanation projection. Typed refusals are preserved wherever a side
condition fails — guessing is a type error, which is DarkTower's own ethic.

**6.3 The application discipline (the Lean compiler model) is the template
for modeling *our* system.** Three linked levels, and the correspondence is
exact:

| their level | ours |
|---|---|
| structural template (component mechanisms, state, artifact mediators, versioned) | the recall pipeline: query construction → FTS candidates → arm projection → surfacing → use → outcome |
| execution episode (`state[t] → run → state[t+1]`, time-indexed, no hidden feedback) | a dispatch, with receipts as its observations — and corpus growth as the `state[t+1]` edge, which is how "the loop consumes its own subjects" (staging §H3d) gets modeled instead of lamented |
| inquiry/repair episode (prediction recorded *before* the change, disconfirming counterfactual, model revised from evidence) | our experiments; DarkTower preregistration already is this layer |

Their **live-model synchronization loop** (consult graph before intervening,
record predicted impact cone + disconfirming outcome, change only the
authoritative mechanism, update the model in the same wave) is the
repair-wave discipline our Phase-0..cohort work should adopt as-is.

**6.4 Concrete correspondences that upgrade V3 designs:**

- **ITT vs mediated, executable.** Their effect algebra makes the reply's Q2
  precise: cohort/E2 primary = total effect of availability; the
  information-channel vs policy-channel split (needs B4) = **natural
  indirect / path-specific effects** through two different mediator sets.
  The estimand vocabulary for the preregistrations can be *their* vocabulary.
- **Route-relativity has two native encodings**: route as an explicit
  variable with arrows into trajectory (effect heterogeneity by
  conditioning), or route as a **regime label with transport formulas** —
  "does the memory effect transport from the Jensen-route regime to the
  Hölder-route regime" is literally their Figures-10.x machinery. The same
  machinery handles the **advice-era vs artifact-era split** (their Q4):
  two regimes, fusion where licensed, non-transportability witness where not.
- **E2's isolation design becomes machine-checkable.** The H3e leak
  inventory (git object DB, lab artifacts, `~/.codex`) is a set of backdoor
  paths; the `apmablate` design is graph surgery; "no read access to
  `/home/joe`" is a d-separation claim we can have the engine verify against
  the authored graph rather than assert in prose. Likewise staging §H3's
  argument that filter-at-dispatch ≡ ship-different-databases on a
  star-forest is a d-separation claim — *outcome ⫫ withheld-memory content
  given the surfaced set* — now stateable and checkable.
- **Their component-quality audit is V2's instrument critique, formalized.**
  Their "bad component" = one that locally manufactures what an upstream
  mechanism should have produced, making downstream tests pass while the
  real cause stays broken — exactly our "a term-selection repair that does
  not move the empty rate would look like a fix" (staging §A2b), and their
  `architecture_fit` / `implementation_fit` split is the right vocabulary
  for our sensor-at-the-wrong-causal-level findings (`used-ids` watching the
  information channel). Their d-separation demo — two semantic routes
  indistinguishable downstream of a shared surface, so downstream success
  tests cannot identify the producer — is structurally identical to V2's
  "empty means no memories surfaced, not no text matched."

**6.5 The division of labor, now precise.** Their engine does exact finite
probability and **identification** — estimand derivation with machine-checked
side conditions, refusals where unidentified. It does not do statistical
estimation from samples. So: **they own "what does this design identify,
under which assumptions"; we own the data and the estimates** (frozen
corpus, receipts, cohort statistics, DarkTower registrations). The joint
artifact is the authored graph plus derived estimands; the joint discipline
is that a preregistration cites the engine's identification receipt before
tokens are spent.

**6.6 Next concrete deliverable (unchanged, sharpened).** Author the
memory-system causal graph as a spec in their format — structural template
at component grain (V2's pipeline stages, ~15–20 variables), the
measurement layer as sensor nodes with explicit
which-sensor-observes-which-node edges, the two regimes labeled, E2 and the
cohort as intervention nodes — and send it back for encoding. Then ask for
three engine receipts against it: (1) what the cohort randomization
identifies; (2) what E2's ablation identifies, given the isolation
surgery; (3) the filter≡ship d-separation check.

---

*Cross-references: `E-memory-resourcing-and-strategy.md` (sequencing),
`E-memory-v3-programme.md` (E2, claims), `E-memory-v3-staging.md` (§H
ablation detail, §B instruments),
`docs/retrieval-whitepaper-v2-cover-note-rob.md` (the note this replies to).*
