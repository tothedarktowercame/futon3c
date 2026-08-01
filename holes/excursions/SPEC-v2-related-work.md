# SPEC — deep-research task: related work for the V2 memory whitepaper

**This is a specification, not a task. Nothing has been run.** Written
2026-08-01 by claude-2 at Joe's request, to be executed later if and when the
V2 draft warrants it.

Target document: `futon3c/docs/retrieval-whitepaper-v2.md` (566 lines, complete
first draft). It currently has **no related-work section**, which is its
largest structural gap.

---

## 1. Why this is a research task and not a citation-gathering task

The paper makes claims in **five distinct literatures that do not normally
talk to each other**, and its central thesis may or may not be novel — the
honest answer is that we do not know, and finding out is the point.

**The thesis at risk of being unoriginal:** *"every instrument measured an
intrinsic property of an artifact while every phenomenon proved relational, and
our instruments inherited their subject's defects."* This smells like it has
prior art in measurement theory, and **the most valuable outcome of this task
would be discovering that it does.** A researcher who returns "this is a
restatement of construct validity / Goodhart / Campbell's law, here is the
canonical treatment" has done more for the paper than one who returns forty
citations confirming our originality.

## 2. Questions, in priority order

**Q1 — Is the reflexive thesis known?** Does measurement theory, philosophy of
science, or ML-evaluation methodology already name the phenomenon where an
instrument built from inside a system inherits that system's categories and
therefore cannot see what the system cannot express? Candidate framings to
check rather than assume: construct validity, operationalism, Goodhart's law,
Campbell's law, the reference-class problem, "measurement as intervention" in
STS. **If it is known, what is it called and who owns it?**

**Q2 — Agent memory systems.** What is the current state of persistent memory
for LLM agents, and specifically: does anyone else **instrument** it — record
what was offered, what was used, and whether the use mattered? Our claim to
novelty is not the memory system but the receipt discipline around it. Check
that claim hard.

**Q3 — Sparse retrieval and term weighting.** Our U-curve result (rare terms
71.4% empty, mid 44.4%, common 74.4%) says term *distinctiveness* is the wrong
objective for a sparse corpus. IDF/BM25 theory presumably has something to say
about the low-DF tail; Amazon's Statistically Improbable Phrases is the folk
version we cite. **Is the U-shape a known phenomenon with a name?** Look for
work on retrieval over small or sparse corpora specifically, where "rare" and
"absent" converge.

**Q4 — Spectral methods for retrieval graphs.** We use a degree-normalised
hypergraph Laplacian (Zhou) and report that λ₂ is informative but its threshold
inverted. Who else uses spectral quantities as *admissibility criteria* rather
than as clustering machinery, and has anyone reported a similar inversion?

**Q5 — Provenance and trust boundaries.** Our four-plus-one trust boundaries
and the "(client, witness-source) pair" architecture: prior art in provenance
systems, scientific-workflow provenance (W3C PROV), and attestation. Is
"a system cannot witness its own outcomes" stated anywhere as a design
principle?

**Q6 — LLM-assisted formalisation.** Enough to situate the domain: Lean,
Mathlib, proof automation with LLMs, and any work reporting *retrieval* into a
formalisation loop. This is context, not contribution — keep it short.

## 3. What counts as a good source

- **Prefer** peer-reviewed or archival work with a stable identifier, primary
  sources over surveys, and the earliest clear statement of an idea over its
  most recent restatement.
- **Accept** well-regarded preprints and technical reports where the field
  works that way, flagged as such.
- **Treat with suspicion** blog posts, vendor documentation, and anything whose
  claims cannot be checked. Amazon's SIPs may only exist as product
  documentation and folklore; **say so** rather than dressing it as literature.
- **Every citation must be verified to exist and to say what it is cited for.**
  A fabricated or misattributed reference is a worse outcome than an empty
  section. If a claim cannot be sourced, report that it cannot.

## 4. Deliverable

1. **A gap analysis, first and most important.** For each of the paper's main
   claims: is it novel, a rediscovery, or a restatement? **Be blunt.** Name the
   claims that are not new.
2. **A drafted related-work section**, ~800–1,500 words, organised by the
   five literatures, written to sit in the paper.
3. **A "we should read this properly" list** — at most ten items, ranked, with
   one line each on why.
4. **An explicit list of claims that could NOT be sourced**, so we know what we
   are asserting on our own authority.

## 5. Anti-goals

- **Do not pad the bibliography.** Forty references we have not read is worse
  than eight we have.
- **Do not soften the paper's findings to fit the literature.** If prior work
  contradicts a result, that is a finding to report, not a reason to hedge the
  result.
- **Do not write the paper's claims into the related-work section.** It
  situates; it does not argue.
- **Do not invent citations.** Stated twice deliberately.

## 6. Prerequisites and cost

Read first: the V2 draft in full, plus `E-memory-whitepaper-v2-programme.md`
for what each number means. The draft's §1.1 table is the fastest route to the
five claims that need situating.

Estimated shape: a genuine literature task, not an afternoon. Best given to an
agent with real search capability and a mandate to read sources rather than
abstracts. **Cost has not been estimated and should be before dispatch.**

## 7. Status

**NOT DISPATCHED.** Joe asked for the specification only. Two things should be
settled before it runs: whether the V2 draft is stable enough to situate (the
compression pass is outstanding), and whether Q1 is worth the disproportionate
effort it may take — it is the question most likely to change the paper and
also the one most likely to return nothing.
