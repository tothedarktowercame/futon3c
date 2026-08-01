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

1. Receive and read the four mfuton mission docs (zip accepted).
2. Draw our DAG properly — one figure, nodes as above, measurement layer
   distinguished — as the shared artifact both sides revise. First concrete
   deliverable of the collaboration.
3. Ask their side for the worked route-as-moderator example (their own
   flagged unverified item).
4. Decide where the missingness model lives (their engine vs our analysis
   code) — for the historical corpus it is load-bearing; for the
   instrumented cohort it should shrink toward bookkeeping.

---

*Cross-references: `E-memory-resourcing-and-strategy.md` (sequencing),
`E-memory-v3-programme.md` (E2, claims), `E-memory-v3-staging.md` (§H
ablation detail, §B instruments),
`docs/retrieval-whitepaper-v2-cover-note-rob.md` (the note this replies to).*
