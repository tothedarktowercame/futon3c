# E-futon-memories — the stack's own git history as a memory corpus

**Status: DERIVE (exploratory).** Opened 2026-07-31, claude-2, from Joe's
proposal. **Scoping only — nothing ingested, no writes.** Logic model first
per house discipline; the numbers below are measured, not estimated.

Related: `E-memory-whitepaper-v2-plan.md` §2.3.2 (as-of probes), V1 §5.2 (the
retracted spectral criterion), `E-memory-topology.md` (the hypergraph
Laplacian correction).

## 1. Why — the formalism is blocked structurally, not by volume

V1 retracted its spectral admissibility criterion after finding λ₂
**anti-correlated** with useful structure: the largest component *is a single
hyperedge* (one pattern holding 33 memories), and a blob attains λ₂ = 1.0 by
construction. We withdrew the criterion rather than answering the question,
because you cannot test whether a spectral quantity discriminates on a graph
with no topology to discriminate.

**More APM dispatching will not fix this.** The corpus grows as star-forests
by construction — every memory attaches to exactly one pattern. The deployment
is closed under adding memories.

So the operator theory of V1 §3 — `Δ_θ = Σ_r θ_r Δ_r`, k-step propagation, the
exploration floor at scale, the braiding question — is untestable for want of
a graph, and no amount of real work produces one.

## 2. Why git logs, and not a fictional corpus

The first proposal was to read *Dracula* and mint memories about it. Joe's
counter-proposal — the stack's own git logs — is better on the dimension that
decides it.

**The witness.** In APM the witness is the Lean compiler; in WM the external
adjudicator. A fiction corpus would need span-verification against the source
text: mechanical, but artificial and inconsequential. Git witnesses are
external *and* have teeth — does it build, was it reverted, did a later commit
fix these same lines. Same class as the compiler.

**Five things git has that our deployment does not:**

1. **Native as-of.** `git log --before`, `git show <sha>:<path>`. §2.3.2
   established our text index has *no* temporal capability, which blocks
   Experiment 0's recall *denominator* — we know what surfaced, not what was
   findable. On git, "what was in the corpus at time T" is exactly
   recoverable, so a benchmark is **constructible** here in a way it is not
   on the APM corpus.
2. **Multiple genuine relation types.** Parent-child DAG, same-file,
   same-subsystem, co-change coupling, temporal window, mission/ticket
   reference — five or six real `Δ_r` against the deployment's **one**
   (`θ_r` is currently declared `:inactive-degenerate`).
3. **Free mechanical labels** via blame/SZZ (§4).
4. **A second clock.** author-date ≠ commit-date on **10% of futon3c's last
   400 commits** — valid-time vs system-time in miniature (work done vs
   entered history; diverges on rebase, cherry-pick, amend). We probed the
   real store and found the two clocks *coincide*, so this machinery is
   currently unexercisable.
5. **Baselines.** Tornhill's change-coupling/hotspot methods and SZZ are prior
   art. "Does the operator beat change-coupling?" is a real comparison — which
   is exactly what RQ1 has lacked.

## 3. The corpus — futon2, measured 2026-07-31

| metric | value |
|---|---|
| commits | **915** |
| span | 2025-11-12 → 2026-07-31 (8.5 months) |
| distinct files ever touched | 939 |
| files at HEAD | 934 |
| mean files per commit | **2.6** |

2.6 is the number that matters for topology: sparse enough to avoid a
hairball, dense enough for real co-change structure. Contrast the deployed
memory graph, where the fan-out is 9:1 into a star.

Top churn: `scripts/futon2/report/war_machine.clj` (65),
`test/futon2/aif/full_loop_runner_test.clj` (62),
`src/futon2/aif/full_loop_runner.clj` (61).

**That last pair is a built-in positive control.** Source and test churning
61/62 is textbook logical coupling. Any co-change detector that fails to
surface `full_loop_runner.clj ↔ full_loop_runner_test.clj` is broken, and we
should assert it before trusting any other coupling it reports.

## 4. The benchmark n — measured, not inherited

The headline task is **"given a fix, retrieve the commit that caused it"**: a
real retrieval problem, with mechanical external ground truth, not answerable
from the query alone.

A naive `grep -i fix` over all 915 messages returns **89**. That number is
wrong, and the way it is wrong matters:

- this corpus is **prose-heavy** — `holes/*.md` mission docs are among the
  top-churn files — so "fix" in a message frequently means fixing a
  *document*;
- **SZZ needs deletions.** A pure-insertion commit has no blame-able
  antecedent. The first fix-ish commit sampled was 38 added lines of markdown
  and zero deletions.

Filtering properly:

| filter | n |
|---|---|
| all commits | 915 |
| touch `src/` `test/` `scripts/` | 388 |
| …and fix-ish message | 50 |
| …**and contain deletions (blame-able)** | **45** |

**45.** Scaling that rate (4.9%) across the 5,389-commit stack gives ≈ 265.

**Consequence for staging: futon2 alone is ample for the topology question and
marginal for the fix→cause benchmark.** Do not conflate them.

## 4a. futon3c scanned the same way — and it is the better corpus

Measured 2026-07-31 at Joe's suggestion. It wins on every axis that matters.

| | futon2 | **futon3c** |
|---|---|---|
| commits | 915 | **1,828** |
| span | 8.5 months | 5.7 months |
| density | 108/month | **320/month** |
| distinct files ever touched | 939 | **1,447** |
| files at HEAD | 934 | 1,381 |
| files deleted over history | 5 | **66** |
| mean files/commit | 2.6 | 2.7 |
| **blame-able fix instances** | **45** | **134** |

**134 versus 45 — three times the benchmark n**, and comfortably clear of the
n ≥ 20 minimum rather than sitting on it. On its own that settles the choice.

Three further advantages:

**Author diversity is real, if lopsided.** After alias resolution:

| identity | commits |
|---|---|
| Joseph Corneli + Joe Corneli | 1,734 |
| **Robert Meyers + Rob Meyers** | **86** |
| tothedarktowercame | 6 |
| electric-samurai | 2 |

Rob's 86 commits (4.7%) make `:same-author` **non-degenerate** here, where in
futon2 it is unusable. And the aliasing is a gift rather than a nuisance: a
memory system that treats `Joe Corneli` and `Joseph Corneli` as distinct
entities has an entity-resolution bug. **Alias detection becomes a validation
task** — every other relation (file, subsystem, co-change, temporal) should be
unable to separate the two, and if the graph *does* separate them, that is a
false distinction worth reporting.

**More supersession structure.** 66 files deleted over history against futon2's
5, so the corpus carries real retraction and replacement rather than being
almost purely additive.

**Denser temporal clustering.** 320 commits/month makes `:temporal` a
meaningful relation instead of a near-uniform smear.

### Two hazards this corpus introduces, both manageable

**Hub files could recreate the exact degeneracy we are trying to escape.**
`dev/futon3c/dev.clj` (162 commits) and `src/futon3c/transport/http.clj` (149)
are large hubs. Under a naive `:same-file` projection, a file touched by *k*
commits becomes a *k*-clique — **precisely the clique-expansion error
`E-memory-topology` caught**, where a hyperedge over *k* nodes was inflated
into k(k−1)/2 relations. Mandatory design consequence: **use the
degree-normalised hypergraph Laplacian (Zhou), never clique expansion.** A
file is one incidence relation over its commits. The prior finding transfers
directly and must be applied from the start rather than rediscovered.

**Self-reference.** futon3c is the repo this session commits to, so the corpus
changes underneath the study. The pinned-HEAD-sha requirement in §9 already
covers this, but it is now load-bearing rather than hygiene, and the pin
should be recorded in the artifact and quoted in any result.

**Recommendation: run S1 on futon3c.** futon2 remains a useful replication
target — if a topology finding holds on both, that is real; if it holds only
on futon3c, the hub structure is doing the work.

### Corpus pin (Joe confirmed full history, 2026-07-31)

```
repo   futon3c
sha    d722f772ede949719948aec76839d4d5e83586b0
scope  full history — 1,828 commits, 2026-02-09 .. 2026-07-31
```

Every S1 artifact must record this sha, and every reported number must be
recomputable from it. The pin covers committed history only; the working tree
was dirty at pin time, which is irrelevant to the corpus but is noted so the
record is exact.

**A memory** is a commit, recorded as a typed act with its message, author,
both timestamps, and the spans (files/hunks) it distills. This is the same
dialogue-act shape the deployment already uses — a commit *is* an assert with
provenance.

**Patterns** — the reusable retrieval handles — are *derived*, not authored:
subsystem, file, and co-change cluster. This follows V1 §5.2's commitment that
a mid tier must be computed from data already present, since a tier requiring
a fresh annotation pass decays as `:level` did.

**Relations** (`Δ_r`), all derivable, none requiring annotation:

| relation | derivation |
|---|---|
| `:parent` | commit DAG |
| `:same-file` | shared path |
| `:same-subsystem` | shared directory prefix |
| `:co-change` | files changed together above a threshold |
| `:temporal` | same time window |
| `:references` | sha or mission id in the message |

**The witness** is external and mechanical, never a model's judgement:
blame-identified antecedent, revert, or a later fix touching the same lines.

**Warrant** stays as the deployment defines it: derived edges do not widen the
warrant set. A relation nominates; it does not conduct until reviewed. For a
synthetic lane this means the reviewed set must be defined explicitly rather
than assumed.

## 6. Two slices

**S1 — futon2 topology (the question worth answering first).**
Build the corpus and ask: **can the retracted λ₂ criterion discriminate on a
non-degenerate graph, or is it inherently uninformative?** Self-contained,
uses data on disk, no witness needed. A null result is publishable in V2 as
"we built the graph the criterion needed and it still did not discriminate" —
which would settle a question V1 could only withdraw.

### S1 preregistration — written before any ingest, 2026-07-31

Without a stated criterion, "does λ₂ discriminate?" is unfalsifiable, and V1
§5.2 is a standing lesson in what happens when a spectral criterion is
adopted without one. So the test is fixed here, in advance.

**The question.** Does λ₂ carry structural information about this graph
*beyond its degree sequence*?

**The test — a configuration-model null.** Compute λ₂ on the real graph, then
on ≥ 200 degree-preserving random rewirings of it. The degree sequence is
held fixed; only the wiring is randomised.

| outcome | reading |
|---|---|
| real λ₂ falls **outside** the null's 95% interval | λ₂ carries structure beyond degree. Criterion is potentially rehabilitable. |
| real λ₂ falls **inside** the null interval | λ₂ is a restatement of the degree sequence on this graph. **Criterion stays retracted**, now with evidence rather than by withdrawal. |

**Preregistered expectation:** *inside* the interval — i.e. we expect the
criterion to fail again, for a deeper reason than the star-forest
degeneracy. Recording the expectation so that a pass is a genuine surprise
and a fail is not a post-hoc rationalisation.

**Both operators, always reported side by side** — unnormalised and
degree-normalised hypergraph (Zhou). V1 §5.2's finding was that the
unnormalised figure can be moved by edge inflation while normalised
conductance falls; any single-number report here repeats that error.

**Two positive controls, asserted before any headline number is trusted:**

1. **Co-change control.** `src/futon2/aif/full_loop_runner.clj` ↔
   `test/futon2/aif/full_loop_runner_test.clj` churn 61/62 in futon2; the
   futon3c analogue is `src/futon3c/transport/http.clj` (149) ↔
   `test/futon3c/transport/http_test.clj` (52). A co-change detector that
   fails to surface these pairs is broken and its other output is worthless.
2. **Alias control.** `Joe Corneli` and `Joseph Corneli` are one person. Every
   non-author relation should be **unable** to separate them. If the graph
   does separate them, that is a false distinction and must be reported as a
   defect in the relation set, not as a finding.

### S1 RESULT (2026-07-31) — preregistration disconfirmed, and the threshold confirmed inverted

Run by codex-5 at pin `d722f772`; reviewed and extended by claude-2. Artifacts
in `holes/labs/E-futon-memories/` (`c8af798e`, `16d9c948`). Corpus: 1,828
commit vertices, 3,014 typed hyperedges, 14,876 incidences; relation counts
parent 1,827 / same-file 646 / co-change 271 / same-subsystem 99 / temporal 97
/ references 74.

| operator | real λ₂ | null mean (sd) | null 95% interval | result |
|---|---:|---:|---:|---|
| unnormalised | 0.271811 | 1.173410 (0.198407) | [0.835495, 1.554879] | **outside, below** |
| Zhou degree-normalised | 0.035995 | 0.371910 (0.022643) | [0.324192, 0.409725] | **outside, below** |

**Finding 1 — our preregistered expectation was wrong.** We predicted the real
value would fall *inside* the null interval, i.e. that λ₂ merely restates the
degree sequence. It does not: the normalised figure sits **~15 SD below** the
null mean. λ₂ *does* detect wiring structure beyond incidence degree, and both
operators agree in direction — unlike V1 §5.2, where they disagreed. Recorded
as a disconfirmation, not softened.

**Finding 2 — the *threshold* is now confirmed inverted, with a third data
point.** This is the reading the run's own note gestures at but does not
complete. All three figures below are the **same** Zhou degree-normalised
operator, so they are directly comparable:

| graph | scale | λ₂ | vs the 0.1 floor |
|---|---|---:|---|
| deployed memories, patterns only | 33 memories, **1 hyperedge** | **1.0000** | **passes** |
| deployed memories + subjects | 126 memories, 51 hyperedges | 0.0689 | fails |
| futon3c git history | 1,828 commits, **3,014 hyperedges** | **0.0360** | **fails** |

**Monotone decreasing in richness across a 55× range in hyperedge count.** The
most degenerate topology available — a single blob — scores the maximum and
passes; the richest graph we have scores lowest and fails. The WS2 criterion
is not merely gameable by edge inflation (V1 §5.2); **as a threshold it is
backwards, and this is now three consistent points rather than one
retraction.**

The two findings are compatible and must be reported together: **λ₂ is
informative but its threshold rule is inverted.** "Potentially rehabilitable"
is right only in the sense that the *statistic* carries signal — the
*criterion as written* should stay retracted, and any rehabilitation must
first establish which direction is good.

**Controls.** Co-change passed convincingly: `transport/http.clj` ↔
`transport/http_test.clj`, 47 co-changes, Jaccard 0.3052, **2nd of 271**
qualifying pairs, with churn independently reproducing my 149/52.

*Alias control — passed, but by construction rather than by measurement.* All
six relations exclude author identity, so swapping `Joe Corneli` /
`Joseph Corneli` labels cannot alter the edge set; the assertion is true a
priori. The risk it was meant to catch — some *other* relation separating the
aliases without using author labels — was therefore untested. **Checked
separately by claude-2 and the risk is absent:** the groups are temporally
interleaved (Joseph 2026-02-10..07-31 n=1394; Joe 2026-02-09..07-16 n=342,
nested inside), so `:temporal` cannot separate them either. Conclusion safe;
the test was weaker than intended and should be strengthened if reused.

**Determinism verified**: hashes recomputed and match the report exactly —
corpus `777e2376…`, results `9636fcd7…`.

**S2 — fix→cause retrieval (gated on S1).**
The n ≈ 265 benchmark across the stack, with change-coupling as the baseline
to beat. Only worth building once S1 shows the operator behaves sensibly on
real topology.

## 7. What this cannot establish — state it in any writeup

- It tests retrieval over **code history**, not over **proof work**. Findings
  about the operator transfer; findings about what memory does for a solver
  do **not**.
- **Single project, and heavily single-author even at best.** On futon3c one
  person holds 95% of commits; Rob's 86 make `:same-author` non-degenerate but
  not balanced. On futon2 it is unusable outright. Report the skew rather than
  presenting author as a peer relation to file or co-change.
- **SZZ has known false-positive modes** — cosmetic changes, moves, reformats.
  Inherit them explicitly; do not pretend the labels are clean.
- This is a **testbed for the substrate**, not an additional lane of real
  work. It must not be read as evidence that the memory loop delivers value.

## 8. Separation requirement — non-negotiable

The corpus gets **its own lane and its own signal type**, structurally
prevented from pooling with the APM and WM corpora — exactly the contract
generalised in `futon2.aif.memory-contract` on 2026-07-31 (`7a69095a`,
`d722f772`). A synthetic corpus silently merged into the real one would
invalidate every number in the paper. The pooling guard is mutation-tested;
this lane must be added to it.

## 9. Gates

Read-only against the repos: no rewriting history, no commits to the studied
repo from the study. Corpus build is deterministic and byte-reproducible from
a pinned HEAD sha, recorded in the artifact. Any harness carries clj-kondo /
check-parens / tests per the workspace standard. Frozen artifacts under
`holes/labs/` are never modified by a rerun.

## 10. Open, for Joe

- Confirm S1-only to start, with S2 explicitly gated on its result.
- **Slice settled by measurement (§4a): futon3c**, on 134 blame-able instances
  against futon2's 45, plus real author diversity, real deletion structure and
  denser temporal clustering. futon2 is retained as a **replication target**,
  not the primary.
- The one genuine call left: whether S1 should run against the *whole* futon3c
  history or a windowed slice. The full history is 1,828 commits over 1,447
  files; a six-week window would be perhaps a third of that and still clear
  n ≥ 20 for S2. Full history is the better graph and the slower ingest.
  Default assumption unless told otherwise: **full history, pinned sha.**
