# What the 2026-08-01 evidence corpus says

**Answer in plain words.** Yes, this corpus contains evidence that parts of the
APM memory system succeeded. Memories were retrieved and reported as useful in
real proof work; a later adjudication classified 17 of 45 non-unclear use
instances as load-bearing — a rate *within* a doubly-filtered subset, since only
45 of the 130 jobs surfaced memory IDs at all and the observation channel yields
just 20 metric-bearing rows from 129 offered halves (15.5%), so it is not a rate
over the corpus; individual records show recalled memories supplying
the architecture or API that accompanied compiler-checked closure; and the
whitepaper records one same-problem end-to-end loop closure plus one cross-model
transfer. But the corpus does **not** establish that memory-augmented APM solves
more problems than a no-memory baseline. It contains no valid controlled
outcome-lift estimate, no frozen-corpus recall benchmark, and no independent
counterfactual showing that the cited memory caused each successful proof.

The fairest conclusion is therefore: **memory use and several useful mechanisms
worked in production, sometimes apparently decisively; causal improvement in
APM proof success remains unmeasured.** This is the distinction made explicitly
by the corpus's own synthesis: attribution plus compiler witness is not causal
attribution ([retrieval-whitepaper.pdf](../../docs/retrieval-whitepaper.pdf),
§§5.7 and 6).

## Scope and reading method

This note covers the 154 files frozen in
[evidence-manifest-20260801.tsv](evidence-manifest-20260801.tsv): 130 raw invoke
jobs, 12 memory-retrieval analysis files, six sorry-loop files, three
typed-memory files, two whitepapers, and one isolation probe. Counts below were
recomputed from the files rather than copied from a narrative report. The raw
jobs were parsed through their top-level `job` records and their final memory-use
sections; the analysis artifacts were checked against those records; the PDFs
were read as syntheses, not treated as primary measurements where a raw artifact
exists.

Evidence strength in this note uses four levels:

1. **Outcome witnessed:** a Lean/compiler or axiom check witnesses the proof
   outcome, while memory use remains solver attribution.
2. **Adjudicated association:** a later rubric labels a reported use
   load-bearing, but there is no counterfactual control arm.
3. **Controlled mechanism:** an ablation or control demonstrates an internal
   mechanism, possibly on a synthetic fixture rather than real proof outcomes.
4. **Descriptive/diagnostic:** logs, topology, retrieval state, or self-reports;
   useful evidence about the apparatus, not evidence of outcome lift.

## 1. The 130 raw job results

### What the experiment was

The files under
[`job-results-20260731/`](M-memory-retrieval/job-results-20260731/) preserve the
full invoke records for theorem-proving and construction-target jobs: dispatch
prompt, event stream, final result, execution metadata, commit reference, and
the runner's memory-use report. They cover several phases rather than one clean
randomised cohort. Direct parsing gives 126 terminal `done` jobs and four
`failed` jobs; the agent mix is 60 codex-6, 56 codex-7, 12 zai-5, and two zai-6.
The 130-file capture itself is documented by
[memory-reports-worksheet-20260731.json](M-memory-retrieval/memory-reports-worksheet-20260731.json),
which finds a memory report in 121 files, including 45 reports with surfaced
memory IDs and 76 without.

### What varies

The records vary in target (whole APM problems versus individual missing
lemmas), starting proof state, runner, retrieval availability, memories offered,
and completion class. They are not exchangeable arms. Among the later stamped
jobs, 30 prompts say `completed-with-memories`, 28 say `timeout`, one says
`store-unavailable`, and 71 older jobs have no stamped dispatch-recall outcome.
Consequently, “no memory used” mixes genuine non-use with missing or failed
recall. The larger receipt audit reaches the same diagnosis independently:
46 timeouts and 12 store-unavailable cases occur among 129 offered halves
([observation-channel-audit-20260731.edn](M-memory-retrieval/observation-channel-audit-20260731.edn)).

### What success they support

They support concrete, outcome-witnessed cases:

- A recalled three-memory bundle accompanied closure of the Young convolution
  frontier: `young_convolution_inequality_L2` went from one sorry to zero, Lean
  exited 0, and the axiom report had no `sorryAx`. The runner says one memory
  correctly identified the Fubini/integrability frontier and another stopped
  repeated API search
  ([invoke-1785243793353-233-a32962a2.edn](M-memory-retrieval/job-results-20260731/invoke-1785243793353-233-a32962a2.edn)).
- For a01A07, a recalled local half-disk route is reported as carrying the proof;
  the target and downstream declarations became axiom-clean, with sorry count
  1 to 0
  ([invoke-1785451020936-403-177f0408.edn](M-memory-retrieval/job-results-20260731/invoke-1785451020936-403-177f0408.edn)).
- For a95A04, the recalled dyadic-induction/Lebesgue-differentiation
  architecture is reported as the successful architecture
  ([invoke-1785467054646-455-b8371b8f.edn](M-memory-retrieval/job-results-20260731/invoke-1785467054646-455-b8371b8f.edn)).
- For a95J05, two recalled memories supplied the strict/closed-threshold bridge
  and the a.e.-convergent subsequence API in a completed commit
  ([invoke-1785469190846-464-39b04a8f.edn](M-memory-retrieval/job-results-20260731/invoke-1785469190846-464-39b04a8f.edn)).
- For a94J04, the recalled Young-inequality reduction exactly matched the route
  used to close `poissonConv_L2_contraction`
  ([invoke-1785473298737-474-6e1af56a.edn](M-memory-retrieval/job-results-20260731/invoke-1785473298737-474-6e1af56a.edn)).

These are real positive records on the measure “a relevant memory was used in a
job whose proof obligation closed.” Their strength is limited: Lean witnesses
the closure, but the same runner reports the memory attribution, and there is no
matched no-memory rerun. They establish successful use, not causal lift.

## 2. The 12 memory-retrieval analysis files

### Receipts, use, and load-bearing adjudication

[receipts-export-20260731-all-authors.edn](M-memory-retrieval/receipts-export-20260731-all-authors.edn)
is the offered/outcome evidence substrate. The audit finds 129 offered halves,
115 outcome halves, 114 joins, and only 20 metric-bearing rows; outcome-half
completion was 89.15%, while just 15.5% of offered rows became metric-bearing
([observation-channel-audit-20260731.edn](M-memory-retrieval/observation-channel-audit-20260731.edn)).
That is a major repair over the earlier slice—14 offered, two outcomes, one
metric-bearing row—but still a selective observation channel
([observation-channel-audit-20260730.edn](M-memory-retrieval/observation-channel-audit-20260730.edn)).

The strongest success count in the corpus comes from a two-step analysis. The
frozen candidate set contains 49 reported memory-use instances
([load-bearing-candidates-20260731.jsonl](M-memory-retrieval/load-bearing-candidates-20260731.jsonl));
the per-instance verdict file assigns 17 `LB`, 21 `CO`, five `TRAJ`, two `IN`,
and four `UN` verdicts
([adjudication-verdicts-p2-20260801.json](M-memory-retrieval/adjudication-verdicts-p2-20260801.json)).
Thus 17 of the 45 non-`UN` instances, 37.8%, were judged load-bearing. Examples
include the Young-convolution assembly, the a01A07 local half-disk route, the
a95A04 dyadic architecture, and the a95J05 threshold/API bridge.

This is **adjudicated positive evidence**, but its comparator is the other
reported-use instances, not a no-memory arm. It answers “did some memories
appear to matter?” with yes; it does not answer “how much did memory improve
the solve rate?”

### Rejection behavior

The same corpus also shows that surfacing was noisy and that the solver often
declined memories. The coding file extracts the relevant memory-report sections
from 45 surfaced rows
([coding-sections-20260731.json](M-memory-retrieval/coding-sections-20260731.json)).
Across 94 coded rejection statements, 61 are topical mismatch, 16 absent
precondition, six scope mismatch, four stage mismatch, three subsumption, and
four relevance-without-applicability
([rejection-coding-20260731.json](M-memory-retrieval/rejection-coding-20260731.json)).
This supports a descriptive result—agents discriminate among offered memories,
rather than blindly applying all of them—but the reasons are solver self-report,
not independently labelled relevance judgments.

[memory-reports-worksheet-20260731.json](M-memory-retrieval/memory-reports-worksheet-20260731.json)
and [cooccurrence-table-20260801.json](M-memory-retrieval/cooccurrence-table-20260801.json)
make the corpus analysable: the latter records 129 dispatch queries, 273 distinct
terms, and 1,864 term pairs under an exhaustive, analyst-blind extraction. Those
are corpus-description assets, not efficacy results.

### Retrieval-state ablation

The two-case D-state experiment freezes candidate generation and removes edges
or whole retrieval arms. In one case, removing the content arm loses the exact
historically missed a92J05 memory; removing the pattern arm loses three related
neighbors. In the Lemniscate case, content is redundant but the pattern arm
supplies three neighbors. Across single-edge perturbations, 5/55 changed the
top-five set (maximum Jaccard distance 0.889); across pattern-role removals,
1/55 changed it (maximum 0.75)
([damage-state-fixture-20260730.edn](M-memory-retrieval/damage-state-fixture-20260730.edn),
[damage-state-results-20260730.edn](M-memory-retrieval/damage-state-results-20260730.edn)).

This is a controlled success for **complementary candidate generation** against
arm-removal baselines, on two frozen cases. The artifact explicitly limits its
claim to `D-state` and excludes memory usefulness and outcome lift. It therefore
does not show that the additional candidates caused more proofs to close.

### Warrant audit

The warrant field failed its own audit. Of 62 attachments carrying a witness
status, 53 say independently witnessed, but 52 of those 53 have no witness
record; the only two actual witness records split across the two status classes
([warrant-audit-20260730.edn](M-memory-retrieval/warrant-audit-20260730.edn)).
Any success rate based on `:witness-status` alone is invalid. This negative
finding is important because it prevents an apparently clean but unsupported
success narrative.

## 3. The six Codex sorry-loop files

Five `harvest-dryrun-*.edn` files preserve turn-level context selected from five
Codex sessions: 70, 67, 14, two, and three turns respectively, 156 total
([019f9b12](M-codex-sorry-loop/harvest-dryrun-019f9b12.edn),
[019fa2c1](M-codex-sorry-loop/harvest-dryrun-019fa2c1.edn),
[019fb3b7](M-codex-sorry-loop/harvest-dryrun-019fb3b7.edn),
[019fb48a](M-codex-sorry-loop/harvest-dryrun-019fb48a.edn), and
[019fb49a](M-codex-sorry-loop/harvest-dryrun-019fb49a.edn)). They contain the
source text, event ranges, and tool digests from which self-correction memories
can be mined. Because they are dry-run capture artifacts, they demonstrate that
the raw correction trace is recoverable; they do not themselves test whether a
mined rewrite helps later work.

The sixth file classifies roughly 30 observed uses into seven modes:
interface translation, direct proof step, strategy override, work removal,
proof architecture, risk ordering, and stopping rule
([memory-use-modes-20260731.json](M-codex-sorry-loop/memory-use-modes-20260731.json)).
Five of the seven are non-substitutive—they alter what the solver does or stops
doing rather than supplying mathematics. This is a useful qualitative success:
the effect vocabulary is broader than “recalled lemma inserted.” Its own caveats
are decisive: one corpus, one Codex model, one day, approximately 30 uses, and an
unexhausted tail. It has no control arm or rate estimate.

## 4. The three typed-memory files

[live-graph-export-20260730.edn](M-typed-memories/live-graph-export-20260730.edn)
is a read-only snapshot of the deployed reviewed-memory graph. The corresponding
meter records 219 nodes, 233 edges, 227 reviewed-current versus six other review
states, and a largest reviewed component of 146 nodes. Its verdict is
`:component-limited`; the measured λ₂ is 0.0755, below the preregistered >0.1
criterion
([connectivity-meter-20260730.edn](M-typed-memories/connectivity-meter-20260730.edn)).
This demonstrates deployed graph construction and auditability, not APM proof
success.

The owner-stability check preregistered Spearman ρ ≤ −0.8 between λ₂ and time to
uniformity and measured approximately −0.8 over four settings, marked `passed?`
([ws2-owner-stability-check-results.edn](M-typed-memories/ws2-owner-stability-check-results.edn)).
That apparent positive must be read with the later correction: the whitepaper
reports that three of four settings were beyond the explicit-Euler stability
boundary, so the runs measured concentration rather than diffusion; a corrected
ε=0.1 rerun again obtained −0.8, but multi-seed replication remained outstanding
([retrieval-whitepaper.pdf](../../docs/retrieval-whitepaper.pdf), §5.4). It is a
mechanism check, not an outcome result.

## 5. The two whitepapers

[retrieval-whitepaper.pdf](../../docs/retrieval-whitepaper.pdf) is the corpus's
best integrated interpretation, and it is notably conservative:

- Hybrid content-plus-pattern retrieval is demonstrated as complementary on two
  frozen cases, but RQ1 is explicitly “not established;” there is no benchmark
  (§5.1).
- The original spectral-rent criterion is retracted because it rewards a
  degenerate one-hyperedge graph. Fourfold reach is retained as a structural
  positive, but functional damage and proof outcomes are unmeasured (§5.2).
- The coefficient-update experiment refuses its claim at n=1 despite a synthetic
  fixture proving the ranking mechanism can flip the intended item (§5.3).
- The exploration floor succeeds against its synthetic controls: floor-off puts
  all mass on the decoy, ε=0.2 recovers the planted target at step 2, and both
  fixed-order and one-step controls rank the decoy first. This is a genuine
  controlled mechanism result on one synthetic configuration, not real APM
  outcome lift (§5.4).
- The observation channel and warrant-status field fail their audits (§§5.5–5.6).
- One end-to-end same-problem loop closure and one cross-model transfer are
  compiler/axiom witnessed. The paper nevertheless states that memory use is
  runner attribution and causation is not independently established (§5.7).

[retrieval-whitepaper-v2-plan.pdf](../../docs/retrieval-whitepaper-v2-plan.pdf)
updates the data boundary after the overnight corpus. It says frozen historical
corpus state is unavailable, so recall@k, nDCG, and the planned chronological
benchmark cannot be reconstructed (§3.1). It identifies the production
used/unused/rejected labels as the valuable new asset, while warning that they
are one-model self-reports (§3.2), and explicitly adds a separate load-bearing
boundary because `used-ids` can score a correctly used memory that still led to
the wrong decision (§3.3). Thus the plan corroborates the conclusion here: the
corpus supports a use taxonomy and case evidence, not a causal success rate.

## 6. The E2 isolation probe

[e2-isolation-probe.edn](../experiments/e2-isolation-probe.edn) records an
isolated `apmablate` account, five frozen problem/revision targets, inaccessible
operator/source state, a clean runner-side store, and registration status 201.
Every listed isolation predicate is true. This is a successful **readiness
check** for an ablation; it contains no arm outcomes and therefore no evidence
that APM memory improved proofs.

The manifest row is stale. It records 628 bytes and SHA-256 `3af74f23…`, whereas
the file present on both surveyed machines is 1,002 bytes with SHA-256
`51dcd0d5…`; the added fields strengthen the isolation record. The mismatch is
an integrity defect in the catalog, not a divergent copy
([evidence-manifest-20260801.tsv](evidence-manifest-20260801.tsv),
[e2-isolation-probe.edn](../experiments/e2-isolation-probe.edn)).

## Overall judgement

### What succeeded

- **Production use:** real proof jobs cite memories that supplied direct proof
  steps, API bridges, proof architectures, search stopping rules, and strategy
  changes; several associated obligations closed with compiler/axiom evidence
  (specific job records above).
- **Adjudicated usefulness:** 17/45 non-unclear reported-use instances were
  classified load-bearing
  ([adjudication-verdicts-p2-20260801.json](M-memory-retrieval/adjudication-verdicts-p2-20260801.json)).
- **Retrieval mechanics:** content and pattern arms contribute different
  candidates on two frozen cases
  ([damage-state-results-20260730.edn](M-memory-retrieval/damage-state-results-20260730.edn)).
- **Anti-collapse mechanism:** the exploration floor beats two controls on one
  synthetic planted-decoy fixture
  ([retrieval-whitepaper.pdf](../../docs/retrieval-whitepaper.pdf), §5.4).
- **Loop closure:** one same-problem mine→recall→use→compiler-witness episode
  and one cross-model transfer are recorded
  ([retrieval-whitepaper.pdf](../../docs/retrieval-whitepaper.pdf), §5.7).

### Against what baseline

Only internal mechanisms have explicit baselines: retrieval-arm removal for
D-state, and floor-off/fixed-order/one-step controls for the synthetic
anti-collapse fixture. The 17 load-bearing cases are contrasted with other
adjudicated uses, **not** with matched no-memory attempts. The compiler proves
that work closed; it does not prove that memory caused closure.

### What remains unanswered

The central outcome question—whether Scribe-mined or other retrieved memories
increase one-shot proof closure, reduce time, or reduce attempts relative to a
clean no-memory baseline—is unanswered by these 154 files. Infrastructure
missingness, absent dispatch-time corpus snapshots, one-model self-report,
unreliable warrant metadata, heterogeneous jobs, and tiny controlled samples
prevent that inference. An honest halftime report should therefore say:

> The apparatus has produced witnessed useful episodes and passed several
> mechanism tests. It has not yet measured causal proof-outcome lift.
