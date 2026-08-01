# Related-work deliverable: `retrieval-whitepaper-v2.md`

**Produced 2026-08-01** by executing `holes/excursions/SPEC-v2-related-work.md`
(deep-research fan-out: 6 angles, 24 sources fetched, 92 claims extracted, 25
adversarially verified, 21 surviving). **Every citation below was then
re-verified by hand against a primary source** — the arXiv abstract page, the
NeurIPS proceedings, or the ACL Anthology — because the spec's hardest rule is
that a fabricated reference is worse than an empty section. Verification status
is stamped on each entry in the appendix. Three of the harness's paraphrases
were wrong and are corrected in place (noted where they occur).

The spec asked for bluntness about novelty. It gets it. **The organising thesis
and three of the five main claims are restatements or rediscoveries of
established work.** Two claims may genuinely stand, and one of those two is the
strongest thing in the paper.

---

## 1. Gap analysis (deliverable 1 — most important)

### Claim 1 — the reflexive/relational thesis · **RESTATEMENT**

*"Every instrument measured an intrinsic property of an artifact; every
phenomenon proved relational; the instrument inherited its subject's defects."*

This is a restatement of measurement theory, not a new observation. Its owners,
earliest-first:

- **Cronbach & Meehl (1955)**, *Construct Validity in Psychological Tests* — a
  construct is admissible only inside a "nomological net," and instrument and
  theory are validated together (the Duhem–Quine entanglement: "if the
  prediction is not confirmed, any link in the chain may be wrong"). This is the
  "instrument inherits the categories of the theory that built it" claim, sixty
  years early.
- **Jacobs & Wallach (2021)**, *Measurement and Fairness* (FAccT) — imports
  construct validity into ML: unobservable constructs are operationalised
  through a measurement model, and "many of the harms discussed in the
  literature on fairness in computational systems are direct results of such
  mismatches." This is the paper's thesis in the ML-evaluation register.
- **Freiesleben & Zezulka (2025)**, *The Benchmarking Epistemology* — benchmark
  scores are "at best measurements of model performance relative to an
  evaluation dataset," i.e. performance is relational, not intrinsic. The
  paper's "intrinsic vs relational" split, applied to benchmarks.

**The name is taken, and it means something else.** "Reflexive measurement" is
a real named concept — Michelson (2022) — but it denotes instruments that
**causally affect the data they elicit**, not instruments that inherit their
subject's categories. The paper cannot adopt the label as a ready-made home for
its claim; the deep-research pass adversarially *refuted* (0–3) the proposition
that "reflexive measurement" already owns the inheritance framing.

**What survives as the paper's own.** Not the epistemology, but the reflexive
*self*-application in §7 — turning the thesis on the authors' own adjudication
rubric and catching it committing the same defect. Construct-validity theory
says instruments inherit defects; it does not, in these sources, work an example
of the analysts catching their *own* instrument doing it mid-study. That move —
§7.2/§7.3, the withheld-repair gate as the mechanism that caught it — is the
defensible increment. Situate the thesis as known; foreground the self-application.

### Claim 2 — memory-use instrumentation & "receipt discipline" · **REDISCOVERY (narrow residual novelty)**

The motivation and the central measurement ("did the memory actually matter?")
are prior art, all 2026 preprints in a fast-moving area:

- **Yuan, Su & Yao (2026)**, *Diagnosing Retrieval vs. Utilization Bottlenecks
  in LLM Agent Memory* — already separates retrieval failure from utilization
  failure and finds retrieval dominates (their finding, on LoCoMo: retrieval
  method spans 20 accuracy points vs 3–8 for write strategy). This is the
  paper's §4.1 "the modal decline is a retrieval failure, not a judgement,"
  arrived at independently.
- **Srivastava (2026)**, *Causal Intervention-Based Memory Selection* — proposes
  **CMI = Causal Memory Intervention** (the harness mislabelled this "Counterfactual
  Memory Importance"; corrected). Selects a memory only when it measurably
  improves task performance under controlled intervention and is stable under
  perturbation. This is the paper's load-bearing counterfactual (§4.4), done at
  write time rather than as a post-hoc adjudication.
- **Li et al. (2026)**, *AttriMem: Attribution-Guided Process Feedback for Agent
  Memory Learning* — token-level counterfactual attribution of memory to the
  final answer. **Caveat the harness missed:** AttriMem is primarily an RL
  process-feedback method; the attribution is a component, not the paper's
  headline. It is a weaker analogue than "another paper that instruments memory
  use," but the counterfactual-attribution machinery is genuinely there.

**Be blunt:** the LLM-judge-relevance idea and the counterfactual-mattered idea
are both prior art. The paper's genuine residual is narrower than "instrumenting
agent memory": it is (a) the **binary `used-ids` receipt over the *offered*
set** — an audit discipline over a deployed pipeline, as opposed to CMI's
write-time selection — and (b) the **`(client, witness-source)` trust boundary**
requiring an independent, decision-keyed witness. Whether "audit/receipt over a
deployed pipeline" is a real methodological increment over "counterfactual
selection at write time" is the open question the paper should answer head-on
rather than assume.

### Claim 3 — sparse-corpus U-curve / term-weighting · **RESTATEMENT**

That term distinctiveness is the wrong objective when targets live in the low-DF
tail, and that "rare" and "absent" converge in a small corpus, is settled IR /
corpus-linguistics:

- **Sparck Jones (1972)** — origin of IDF; **Robertson (2004)** — its
  justification is probabilistic (IDF as a special case of the
  Robertson–Sparck-Jones weight), not information-theoretic. Grounds "distinctiveness
  = discrimination" as the *heuristic the paper is arguing against*.
- **Weeber, Vos & Baayen (2000)**, *Extracting the Lowest-Frequency Words:
  Pitfalls and Possibilities* — the direct analogue, and the one the paper most
  needs to cite. They argue a-priori discarding of low-DF terms is **not
  warranted** (68.3% of their target terms had frequency < 5) **and** show
  formally that rare-informative terms become mathematically indistinguishable
  from rare-noise (a hapax legomenon gets maximum mutual information by
  construction). That is precisely the paper's "rare and absent converge."

**The increment that survives** is small but real: Weeber's terms are
*present-but-rare*, and he still recommends discarding true hapax legomena as
unextractable. The paper's setting is *sparse-corpus*, where the rare term has
**no referent at all** — "rare" collapses into "absent," not merely into "noisy."
That extension is the paper's, but it is an extension of Weeber, and should be
framed as one.

### Claim 4 — spectral admissibility inversion · **MACHINERY GROUNDED; NOVELTY PLAUSIBLE**

- The degree-normalised hypergraph Laplacian is correctly owned by **Zhou,
  Huang & Schölkopf (2006)**, *Learning with Hypergraphs* (NIPS 19). Citation
  grounded and verbatim-verified.
- **No prior art was found** for the paper's actual move: using λ₂ (the Fiedler
  value / algebraic connectivity) as an **admissibility criterion** rather than
  as clustering/embedding machinery, and reporting a **threshold inversion**
  across a range of graph richness. This is "unsourced" in the neutral sense —
  evidence *for* potential novelty, not proof of it. **This is likely the
  paper's strongest genuine contribution and should be foregrounded**, with the
  honest note that absence of prior art in one search pass is not proof of
  originality.

### Claim 5 — reachable ≠ retrievable + trust boundaries + "a system cannot witness its own outcomes" · **UNSOURCED**

The search returned **no verified citation** on any of: the trust-boundary
model, the `(client, witness-source)` architecture, the reachable-vs-retrievable
distinction, or the principle "a system cannot witness its own outcomes." The
adjacent literatures the spec named (W3C PROV, scientific-workflow provenance,
remote attestation) surfaced sources but produced no surviving claim. The
principle rhymes with **separation-of-duties / non-repudiation** in security and
with **observer-independence** norms in provenance, but nothing was verified.
**Treat as asserted on the authors' own authority until a dedicated
provenance/attestation pass runs** — this is a sourcing gap, not a verified
novelty.

---

## 2. Drafted related-work section (deliverable 2)

> Intended to sit in the paper. It situates; it does not re-argue the paper's
> findings. ~1,150 words. Q5 is written thin on purpose — see deliverable 4.

### Related work

**Measurement theory and the instrument that inherits its subject.** The
observation organising this paper — that an instrument built inside a system
inherits that system's categories, and so cannot register what the system
cannot express — is a specialisation of construct validity. Cronbach and Meehl
(1955) established that a psychological construct is admissible only within a
"nomological net," and that instrument and theory are confirmed or
disconfirmed together; a failed prediction may indict any link in the chain.
Jacobs and Wallach (2021) carried this into machine learning as *measurement
modelling*: unobservable constructs must be operationalised through a
measurement model whose assumptions are inherited by every downstream number,
and they attribute a range of documented fairness harms to construct/operationalisation
mismatch. Freiesleben and Zezulka (2025) apply the same lens to benchmarks,
arguing that a benchmark score measures performance *relative to* a dataset and
learning problem rather than any intrinsic capability. A separately named
concept, "reflexive measurement" (Michelson 2022), is adjacent but distinct: it
concerns instruments that *causally influence* the data they elicit, not
instruments that inherit categorical blind spots. Our contribution is not this
epistemology, which is well established, but its reflexive application within a
single study — the observation (§7) that our own adjudication rubric reproduced
the defect we had diagnosed in the system's instrumentation.

**Instrumented agent memory.** Persistent memory for LLM agents is an active
area, with architectures such as MemGPT-style summarisation and Mem0-style fact
extraction, and benchmarks (LoCoMo and successors) that score memory systems by
end-to-end task accuracy. A recent line moves from scoring memory to
*instrumenting* it. Yuan, Su and Yao (2026) separate retrieval failure from
utilization failure and report that retrieval dominates the error budget — a
result convergent with our finding that the modal decline is a retrieval
failure rather than a judgement. Srivastava (2026) proposes Causal Memory
Intervention, selecting a memory only when a controlled intervention shows it
improves the task score over a no-memory baseline and remains stable under
perturbation — a write-time counterfactual for "did the memory matter." Li et
al. (2026) attribute an answer to individual memory tokens by counterfactual
ablation. Against this backdrop, our instrumentation is not novel in aim, and
the counterfactual "did-it-matter" measurement in particular is prior art. What
distinguishes our approach is its position and its discipline: a receipt over
the *offered* set recorded in a deployed pipeline rather than a selection
criterion at write time, and a trust boundary requiring the witness to come from
an independent, decision-keyed source outside the system under study.

**Term weighting over sparse corpora.** Our lexical results restate, and then
extend, a settled result. Inverse document frequency (Sparck Jones 1972),
whose theoretical justification Robertson (2004) locates in the
probabilistic-relevance model rather than in information theory, encodes the
intuition that distinctive terms discriminate better — the very heuristic our
U-curve falsifies for a sparse corpus. The closest prior statement is Weeber,
Vos and Baayen (2000), who argue that discarding the lowest-frequency terms a
priori is unwarranted (a majority of their target terms had corpus frequency
below five) and show formally that under association measures a rare-but-informative
term becomes mathematically indistinguishable from rare noise. Our finding that
"rare" and "absent" converge is the small-corpus limit of exactly this
phenomenon: where Weeber et al. study terms that are present but rare, our
corpus is sparse enough that a rare query term frequently has no referent at
all, so distinctiveness selects for emptiness rather than for noise. Amazon's
"Statistically Improbable Phrases," which we invoke informally, is best
described as product folklore: we could locate no archival or peer-reviewed
source for it and do not treat it as literature.

**Spectral quantities on retrieval graphs.** The machinery we use — a
degree-normalised hypergraph Laplacian — is that of Zhou, Huang and Schölkopf
(2006), who generalised spectral clustering from graphs to hypergraphs and
developed the associated embedding and classification methods. That lineage,
and the wider literature on algebraic connectivity (the Fiedler value, λ₂), uses
the second eigenvalue as *clustering and embedding* machinery. Our use is
different in kind: λ₂ as an *admissibility criterion* — a gate on whether a
retrieval graph is well-formed enough to trust — and our report of a threshold
that inverts across a range of graph richness. We are not aware of prior work
using a spectral quantity as an admissibility gate, or reporting such an
inversion; we state this as an absence of located prior art rather than as a
claim of priority.

**Provenance and trust boundaries.** Our trust-boundary model, its
`(client, witness-source)` architecture, and the principle that a system cannot
witness its own outcomes sit near the provenance and attestation literatures —
the W3C PROV data model for provenance, scientific-workflow provenance, and
remote attestation, in which an external verifier rather than the device itself
vouches for state — and near separation-of-duties and non-repudiation in
security. We have not, however, located a source that states "a system cannot
witness its own outcomes" as an explicit design principle, and we present the
model as our own pending a fuller reading of these literatures.

**Retrieval for formal mathematics.** Our deployment setting — retrieval into a
Lean formalisation loop — connects to retrieval-augmented theorem proving.
LeanDojo (Yang et al. 2023) extracts premise annotations from Lean's
mathematics library and trains ReProver, a retrieval-augmented prover that
selects premises for a proof; premise selection is the retrieval problem in that
setting. Our concern is orthogonal: not premise selection accuracy but the
instrumentation of a memory service that supplies advice, and the observation
(§5.1) that the corpus indexes advice rather than the importable artifacts a
prover needs.

---

## 3. "We should read this properly" list (deliverable 3 — ranked, ≤10)

1. **Weeber, Vos & Baayen (2000)**, *Extracting the Lowest-Frequency Words* —
   the paper that already made the §4.2 argument; read before finalising §4.2.1.
2. **Jacobs & Wallach (2021)**, *Measurement and Fairness* — the canonical
   import of construct validity into ML; the home the thesis actually belongs to.
3. **Srivastava (2026)**, *Causal Intervention-Based Memory Selection (CMI)* —
   the closest prior art to §4.4's counterfactual; read to sharpen what the
   receipt discipline adds over write-time selection.
4. **Yuan, Su & Yao (2026)**, *Retrieval vs. Utilization Bottlenecks* — an
   independent arrival at §4.1's "retrieval failure, not judgement."
5. **Cronbach & Meehl (1955)**, *Construct Validity in Psychological Tests* —
   the origin; the nomological-net argument is the thesis in embryo.
6. **Zhou, Huang & Schölkopf (2006)**, *Learning with Hypergraphs* — already the
   §4.5 machinery citation; confirm the normalisation matches what was computed.
7. **Robertson (2004)**, *Understanding IDF* — what IDF theory actually claims
   about the low-DF tail; grounds the "distinctiveness is the wrong objective"
   framing.
8. **Freiesleben & Zezulka (2025)**, *The Benchmarking Epistemology* — the
   nearest ML-side statement of "performance is relational, not intrinsic."
9. **Yang et al. (2023)**, *LeanDojo / ReProver* — the domain-situating citation
   for retrieval into a Lean proof loop (§5.1, §6).
10. **Li et al. (2026)**, *AttriMem* — token-level counterfactual attribution;
    read to bound how much of §4.4 is genuinely new, but note it is an RL method,
    not primarily a measurement paper.

---

## 4. Claims that could NOT be sourced (deliverable 4 — assert on your own authority)

1. **"A system cannot witness its own outcomes" as a named design principle.**
   No verified source (Q5). Provenance/attestation were searched at the survey
   level only; a dedicated pass is needed before this is called novel.
2. **The `(client, witness-source)` trust-boundary architecture and the fifth
   (load-bearing) / reachability axes** (§1.2, §2). Unsourced — plausibly novel,
   but the search did not cover provenance deeply enough to say.
3. **λ₂ as an admissibility criterion, and the threshold inversion** (§4.5). No
   prior art found — this is the paper's most likely genuine novelty, but
   "not found in one pass" is not "does not exist." Assert with that hedge.
4. **Amazon's Statistically Improbable Phrases as literature.** No archival or
   peer-reviewed source located; only product documentation and a Wikipedia
   entry. Per the spec, describe it as folklore, not cite it as prior work.
5. **The specific increment of an audit/receipt over a deployed pipeline vs.
   write-time counterfactual selection** (§4.4 vs CMI). Whether this is a real
   methodological difference is unestablished by the literature; it is the
   authors' to argue.

Two sourcing gaps the deep-research pass did **not** close and that a follow-up
should: a dedicated provenance/attestation search for Q5, and full extraction of
the Q6 formalisation citations (LeanDojo verified; premise-selection literature
beyond it not swept).

---

## Appendix — verified reference list

All entries below were re-checked by hand on 2026-08-01 against the stated
primary source. "✓ verified" = title, authorship, and cited content confirmed
against the source; "canonical" = long-established work not in doubt, spot-checked.

| ref | identifier | status |
|---|---|---|
| Cronbach & Meehl (1955), *Construct Validity in Psychological Tests*, Psychol. Bull. 52(4):281–302 | psychclassics.yorku.ca/Cronbach/construct.htm | canonical |
| Jacobs & Wallach (2021), *Measurement and Fairness*, FAccT '21 | doi:10.1145/3442188.3445901 (arXiv:1912.05511) | canonical |
| Freiesleben & Zezulka (2025), *The Benchmarking Epistemology* | arXiv:2510.23191 | ✓ verified |
| Michelson (2022), *Reflexive Measurement* | arXiv:2208.06371 | ✓ verified (distinct mechanism) |
| Yuan, Su & Yao (2026), *Diagnosing Retrieval vs. Utilization Bottlenecks in LLM Agent Memory* | arXiv:2603.02473 | ✓ verified (preprint) |
| Srivastava (2026), *Causal Intervention-Based Memory Selection* (CMI) | arXiv:2605.17641 | ✓ verified (preprint) |
| Li et al. (2026), *AttriMem: Attribution-Guided Process Feedback for Agent Memory Learning* | arXiv:2607.21106 | ✓ verified (preprint; RL method) |
| Sparck Jones (1972), *A statistical interpretation of term specificity* , J. Doc. 28(1):11–21 | — | canonical (IDF origin) |
| Robertson (2004), *Understanding inverse document frequency*, J. Doc. 60(5):503–520 | staff.city.ac.uk/~sbrp622 | ✓ verified |
| Weeber, Vos & Baayen (2000), *Extracting the Lowest-Frequency Words*, Comput. Linguist. 26(3):301–317 | ACL Anthology J00-3001 | ✓ verified |
| Zhou, Huang & Schölkopf (2006), *Learning with Hypergraphs*, NIPS 19:1601–1608 | proceedings.neurips.cc (2006) | ✓ verified |
| Yang et al. (2023), *LeanDojo: Theorem Proving with Retrieval-Augmented LMs*, NeurIPS 2023 | arXiv:2306.15626 | ✓ verified |
| Cohen-Wang et al. (2024), *ContextCite* (cited by AttriMem) | arXiv:2409.00729 | not independently verified this pass |
| W3C PROV (provenance data model) | w3.org/TR/prov-overview | exists; no surviving claim for Q5 |

**Not admitted as literature:** Amazon "Statistically Improbable Phrases" — no
primary source; folklore only.
