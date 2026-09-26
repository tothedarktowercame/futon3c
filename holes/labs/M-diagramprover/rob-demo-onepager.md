# Rob demo one-pager — causal receipts pipeline (2026-08-03)

*Prepared for Joe's call. Current as of the front-door capability
landing (reviewed, all oracles green, 52 tests / 188 assertions).*

## 1. The pitch, one sentence

Instead of porting *The Book of Why* directly, we built a **receipts
pipeline**: you author a causal DAG as JSON with the questions you want
answered attached; the engine computes verdicts with the witness paths
named, cross-checks every verdict against independent oracles
(NetworkX, dagitty, y0), reproduces byte-identically from one command —
and where it cannot answer, it **refuses with a computed proof of why
and a name for what's missing**.

## 2. The demo (one command)

`holes/labs/M-diagramprover/oracle-pass/run.sh` — regenerates
everything: six receipts on two real specs + the Book-of-Why fixtures.

| Fixture | Status | Oracles |
|---|---|---|
| Simpson / kidney stones | computed (backdoor, path named) | NetworkX agrees |
| Sprinkler collider | computed | NetworkX agrees |
| Monty Hall | computed | NetworkX agrees |
| Smoking → tar → cancer | **computed (front-door, mediator {tar})**: three criterion conditions individually computed, symbolic estimand emitted; was a proved refusal until this morning — the frontier moved | NetworkX agrees on all three conditions; dagitty agrees backdoor exhausted; y0 agrees identifiable |
| Firing squad rung 2 | computed (do-surgery) | NetworkX agrees |
| Firing squad rung 3 | **computed (deterministic SCM)**: abduction → action → prediction, each step in the receipt (evidence death=true forces the unique world court-order=true; do(soldier-A:=false) leaves death=true via soldier-B) — the third frontier move of the day | exhaustive world enumeration re-derives the semantics (a re-derivation, not an independent oracle — stated); underdetermined and inconsistent-evidence cases refuse with witness worlds |
| Napkin problem | **computed (general ID)**: Shpitser-Pearl recursion over the latent projection, symbolic estimand matching the published napkin formula; was a refusal until mid-morning — the frontier moved a second time today | y0 agrees identifiable |
| Bow graph | **PROVED IMPOSSIBLE**: the refusal carries the failing recursive subproblem as witness — not "we can't" but "provably nobody can, from observational data" | y0 agrees non-identifiable |

The refusal rows are the point, not the gap: a refusal proves its
exhaustion (every candidate adjustment set listed with the surviving
confounding path named) and names the exact missing capability. The
frontier is *in the artifact*.

## 3. Receipts on real systems, not just textbook fixtures

Two authored specs with all six requested receipts answered and
oracle-agreed: our memory-retrieval system (Q1 cohort identification,
Q2 isolation+mediation with an honest sensor-gated refusal, Q3
filter-equivalence on two corpus topologies — its divergence verdict
is already wired into a live experiment's registration) and our Lean
proof pipeline (R1 selection confounding with the adjustment set
verified, R2 provenance asymmetry making duplication debt a measured
quantity, R3 — see below).

## 3b. This morning's additions (post-9AM)

- **Your 60 worked examples, consumed.** The mfuton bundle's
  Book-of-Why graphs ran through our engine + three oracles: 60/60
  converted (40 fully, 20 graph-only with typed reasons — that
  boundary table is our priced capability backlog), **755/755 oracle
  checks, zero discrepancies**, byte-identical reruns. Review caught a
  semantic-fidelity gap (prose-marked latents flattened) whose
  correction produced a verdict delta table — generic-frontdoor
  flipped to front-door, jtpa to *proved-impossible* — each change
  re-triangulated. Your engine stays yours; your corpus became our
  regression harness.
- **The "so below" real-world example: our memory system's retrieval,
  causally.** A stage-level causal spec of the recall pipeline + four
  computed receipts (RS-1..4), which within ONE HOUR of delivery: (a)
  corrected a live experiment's preregistered interpretation (E8's
  arm-D reading was too strong — two residual causes, not one), (b)
  had a prediction validated on frozen data (RS-2: query cardinality
  inert, vocabulary the lever — the planned cohort arm would have run
  starved), and (c) got corrected BACK by the experiment's data (a
  static node refuted; "discriminator is free" refuted; two candidate
  deltas registered). The spec's revision trail — v1 → v2 →
  v3-candidate → v4-candidate, one day, every bend evidenced — is the
  definition-repair loop running live between two agents. The
  sharpest finding: the shipped retrieval never once surfaced a
  *named* target across the labelled cases — "retrieving a relevant
  memory" ≠ "retrieving the needed one," which is the retrieval twin
  of "generated code that compiles" ≠ "the generated code you needed."
- **In flight at call time:** the authored retrieval DAG is being
  falsification-tested against the frozen 129-dispatch corpus
  (dagitty localTests + DoWhy falsify_graph — real observational
  data). Violations, if any, land as spec deltas: the loop eats its
  own dogfood.

## 4. Rob's own point, with a receipt (the Lakatos frame)

"Finding the right definition is the end result of the proving
process, not the beginning" — that is Lakatos's lemma-incorporation,
and we have a mechanized instance: our R3 receipt **refuted our own
spec's prose**. We predicted a planned sensor (T05) would screen off
the old progress metric; the engine computed that no noisy measurement
child can ever screen off its parent — and the constructive repair
(record the underlying dependency set losslessly, not another reading
of it) went back into the spec as a versioned delta. The conclusion
failed; the *definition* got repaired; the receipt documents both.
That loop — his loop — is what the pipeline mechanizes.

## 5. The talk-vs-walk question (his complaint, our data)

Our answer is not "better instructions." It is structure:
- **Gates that quantify over artifacts, executed by the harness** —
  the only green is the built thing passing corpus-level checks at
  HEAD; the agent's per-instance claims are inadmissible as evidence.
  (His compiler analog: the binary processes the full test corpus;
  never per-file hand-verification.)
- **Author ≠ reviewer, with the runner leg carrying the gate** — both
  of our false-spec incidents were caught by runners, not readers.
- **Stalls classified, definition-repair routed to the human** —
  agents prove under gates; humans repair statements. (137/145 closed
  under this discipline; the residual is *typed*.)
- Measured, honestly: in our one preregistered probe cycle, a
  correction to the authoring agent **held across a session gap** —
  but the design never rests on that; the guarantee rides the gates.
  Re-instruction is a weak intervention; gate installation is a
  structural one. His repeated "YOU ARE RIGHT! → reversion"
  observations are data supporting exactly that distinction.

## 6. His problem, already encoded (the personalized exhibit)

Not an offer — done this morning: `rob-workflow-spec.json` +
`rob-workflow-receipts.edn` (this directory), an 11-node causal model
of the agent-compiler failure he described, receipts computed by the
engine:

- **RW-1 — "YOU ARE RIGHT!" explained structurally.** Acknowledgment
  has NO directed path to behavior; the engine names the only
  connecting path: `acknowledgment ← trained-propensity →
  per-file-hand-check`. Talk and walk are *confounded, not
  connected* — which is why re-explaining can never fix the behavior:
  it intervenes on the wrong node.
- **RW-2 — instruction fragility vs gate robustness, as graph
  surgery.** Sever the rule-in-context node (context loss): the
  instruction's influence on behavior disconnects ENTIRELY, while the
  gate's path survives — `gate-installed → harness-verdict →
  behavior` — because it re-enters through the harness, not through
  the agent's memory. His observed reversion pattern is this
  receipt's prediction.
- **RW-3 — gate installation is a clean experiment.** P(closure-rate
  | do(gate-installed)) is identified with the empty adjustment set.
  Installing the gate and measuring is methodologically sound as-is;
  re-instructing is not even an intervention on the right node.

Standing offers on top of it: the gate template as a one-pager; the
fixture pipeline itself (specs in, receipts out) pointed at his own
DAGs; and refining this workflow model WITH him — the spec is v1, and
the revision contract (deltas, not silent edits) applies to it too.

## 7. Live demo script (paste during the call)

```sh
# 1. The whole pipeline, deterministically, one command (~2 min):
holes/labs/M-diagramprover/oracle-pass/run.sh

# 2. A live identification in the serving JVM (napkin, post-ID-build):
./scripts/proof-eval.sh '(do (require (quote [futon3c.diagramprover.causal.bow :as bow])) (bow/front-door-receipt))'

# 3. Rob's own workflow model, receipts recomputed live:
./scripts/proof-eval.sh -f /tmp/rob-workflow-receipts.clj
```

(Adjust #2 to the napkin/general-ID receipt once that slice lands —
the point of #2 is a verdict computed in front of him, not replayed.)

## 8. Honest limits (say them before he finds them)

DAG-level only — the categorical/compositional layer is precisely the
funded work; rung-3 counterfactuals computed only for finite
deterministic Boolean SCMs — stochastic/unspecified-SCM counterfactuals
remain named capability refusals; general-ID estimand correctness is
literature-matched on the napkin and verdict-agreed with y0, but
deep-nesting estimand cases lack known-answer coverage yet;
data-dependent falsification (localTests, falsify_graph) installed but
awaiting real experiment data; the missingness oracle (dosearch) was
exercised to its actual boundary on our hardest question — its node
budget and compute cost make it structurally unable to settle the Q2
mediation query, and the REPORT records why a fitting projection's
negative verdict doesn't transfer (a review catch: projecting observed
variables away is sound only in the identifiable direction); and
DAG-level oracle agreement cannot catch a categorical-layer bug that
projects correctly — a permanent limit we state rather than hide.
