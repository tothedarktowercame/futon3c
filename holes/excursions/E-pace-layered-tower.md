# E-pace-layered-tower — the AIF paces and the pheno/geno/exo/xeno tower are the same hierarchy

**Status: DERIVE (exploratory).** Opened 2026-07-30 by claude-1 from Joe's
proposal. Two hierarchies developed separately in this stack are here aligned;
§5 records where the alignment strains, and §6 the one place it makes a
falsifiable prediction. No measurement.

**Joe's proposal, 2026-07-30, verbatim in substance:** *"these could be viewed
as the different paces (glacial, social, etc.) attached to the
pheno-geno-exo-xeno hierarchy. For example, we could say, xeno is the slowest
assuming that I'm the xenotype, because I still haven't learned any prelim
problems. However, a more realistic xeno estimate is that I have gained some
capabilities, they just aren't so cut-and-dried as 'Joe passes prelims', and
not so one-sided as 'Claude and Codex are good at coding and math'. XTDB is
like an exotype — no XTDB = no FUTON, no futon0, no mindfulness, nothing to see
here. The upgrade from XTDB1 to XTDB2 is already a proof-of-capability,
demonstrated by the working memory system."*

**Reads:** `futon5/holes/tech-notes/TN-baldwin-reconsidered.md` §1 (the tower) ·
`TN-coupling-gain.md` §§1,4 · `TN-exotype-placement.md` (protocol, null result) ·
`futon3c/CLAUDE.md` §The Three-Futon Refactoring (the pace vocabulary) ·
`futon3c/docs/retrieval-whitepaper.md` §6 (the connectivity floor) ·
`futon7/holes/missions/M-daily-scan.md` §Q5 (mutual constitution).

---

## 1. The two hierarchies

The **tower**, from `TN-baldwin-reconsidered.md` §1, with Joe's own governing
formulation recorded there: *the evolution of genotypes requires an exotype, and
the evolution of exotypes requires a xenotype* — each layer supplies the
selection environment for the one below.

The **paces**, from `futon3c/CLAUDE.md`: futon3a fast (query), futon3c social
(real-time), futon3b task + glacial. Four rates, assigned to repos.

These were built for different purposes and have not been related. Joe's claim
is that they are the same hierarchy seen twice.

## 2. The alignment

| tower layer | pace | in this stack | what a capability gain looks like here |
|---|---|---|---|
| **phenotype** | fast / social | behaviour — what agents actually do this turn; the running loop; a demo | a run completes, a QA pass holds |
| **genotype** | task | code — commits, missions closed, features landed | a feature lands and stays landed |
| **exotype** | slow (infrastructural) | **the store** — XTDB, and with it the retrieval operator and the gates: *what any local decision can read* | **XTDB1 → XTDB2, evidenced by the working memory system** |
| **xenotype** | glacial | **Joe** — which regimes apply where and when | graded capability accrual, *not* "passes prelims" |

**Terminology warning (claude-8, 2026-07-30).** Every use of **"gain"** in this
note is `TN-coupling-gain.md`'s sense — the *currency* of the phenotype read,
live versus frozen at `t*`. It is **orthogonal** to the two Ashby axes
introduced in `E-ashby-variety-stratum.md` §4.6 (γ = whether the phenotype is
read at all; ν/V = the variety source in the genotype self-update). A frozen
read still tests the phenotype and still draws a source, and collects nothing.
Do not merge the three.

## 3. Why "XTDB is the exotype" is exact rather than metaphorical

The exotype is defined as the **local evaluation regime** — "the rule a specific
cell uses" — and `README.md` glosses it as the local heredity signal, *neighbor
sigils plus phenotype context*. What the store can represent is precisely what
any local read of the phenotype context *can see*. So the store does not merely
support the loop: **it bounds the achievable gain.** There is no
phenotype→genotype coupling richer than the substrate can express.

**And that is the same statement as the whitepaper's binding limit, at a
different altitude.** §6: *below a connectivity floor the operator family is
degenerate — a disjoint union of small stars has trivial flow whatever the
coefficients*, with the first live reading (2026-07-28) `:component-limited`.
That is a store-level structural property defeating every coefficient choice.
"XTDB is the exotype" and "the connectivity floor is binding" are one claim.

Joe's *"no XTDB = no FUTON, no futon0, no mindfulness, nothing to see here"* is
the load-bearing version, and it follows: nothing below the exotype layer
survives its removal.

## 4. The pace ordering is a stability requirement, and it yields a design rule

The ordering is not decoration. **If a layer changes faster than the layer below
it, the selection environment resets before anything can accumulate.** An
exotype that churns gives the genotype no stable regime to adapt to; a xenotype
that churns gives the exotype none.

This stack has lived exactly that cost. XTDB1 → XTDB2 was an exotype change, and
it *required* the futon1a → futon1b migration — accumulated genotype-level work
had to be ported because the layer beneath it moved. So:

> **Exotype changes are expensive precisely because they reset the selection
> environment for everything below.** Which is why they must be rare, slow, and
> load-bearing — and why the pace ordering has to hold rather than merely
> tending to.

This reframes the migration usefully. It is not debt and not scope creep: it is
**a capability increment at the correct layer, correctly paced**, and Joe's
evidence standard for it is the right one — *the working memory system runs on
it.* An exotype change is proven by what the layers below it can now do, not by
the change itself. (Compare
`feedback_prove_the_artifact_not_the_wiring`.)

## 5. Where the alignment strains — two honest notes

**(a) Containment is not rate.** The tower is a selection/containment hierarchy;
paces are rates. "Higher is slower" is an empirical regularity in hierarchical
systems, not a theorem. Treat a violation as a **diagnosable pathology** (§4)
rather than a category error — which is the useful reading anyway.

**(b) In the CA, phenotype and genotype share a clock — in the stack they do
not.** `TN-exotype-placement.md` §Construction is explicit: *"genotype and
phenotype update simultaneously from the old state, as in the source
construction."* Synchronous. Whereas in the stack, behaviour is fast and code is
task-paced: an agent can behave differently this turn and the code change lands
next week.

**This disanalogy is not a defect of the proposal — it is where the Baldwin
question comes from.** Lamarckian and Baldwinian assimilation are only
*distinguishable* when the phenotype and genotype run on different clocks: the
gap between "behaved well now" and "written into the code later" is the interval
in which assimilation either happens or does not. In the CA that gap is zero by
construction, which is why `TN-baldwin-reconstructed` had to work to locate the
Baldwin function at all. **The stack has the pace separation the CA lacks**, so
the question is native here and imported there. Worth recording in the other
direction too: it suggests the CA family would need an explicit
genotype-update-delay parameter to host Baldwin proper — a candidate exotype
coordinate that no current sweep includes.

## 6. The xenotype reading, and the impoverished predicate a third time

Joe's own move on the xenotype layer is the session's methodological finding
recurring, and he applied it to himself unprompted.

*"Xeno is the slowest assuming that I'm the xenotype, because I still haven't
learned any prelim problems"* — **that is a boolean read.**
`switch(passes-prelims?, capable, not)`, one bit. And
`TN-exotype-placement.md`'s measured result for that shape is preregistered
outcome **(c): indistinguishable from its own constituent.** A one-bit gate on
an impoverished predicate resolves nothing.

Joe's correction is the gain formulation: *"a more realistic xeno estimate is
that I have gained some capabilities, they just aren't so cut-and-dried."*
Graded, not boolean. This is now the **third independent instance** in one
session — business decision regimes (`E-business-exotype-audit` §4.1),
coordination cadence (`M-becoming-nomad` §1.1), and self-assessment — which is
worth more than any single one of them.

**And the second clause carries a further claim:** *"not so one-sided as 'Claude
and Codex are good at coding and math'."* The capability is a property of the
**assemblage** — operator, agents, store — not of any party. That resists both
available one-bit readings ("Joe hasn't learned prelims" / "the AI does the
coding"), and it is the same structure `M-daily-scan` already named as *mutual
constitution*: **"Without the War Machine's framework, the scan is just a cron
job. Without the scan's data, the War Machine's frame has a zero-valued port."**
Xenotype capability is joint, and measuring it on either party alone reads
zero for the same reason a disjoint union of stars has trivial flow.

## 7. What this buys, concretely

1. **A vocabulary for pacing a decision.** "Which layer is this change at?"
   answers "how often may it move, and what does it invalidate below?" An
   exotype-layer change (store, retrieval operator, gate structure) must be rare
   and justified by what the layers below can newly do.
2. **A diagnosis for a familiar failure.** Churn at the exotype layer looks like
   productivity and is measured, by §4, as resetting the selection environment.
   `wip-cap.flexiarg`'s ≤3 active focus areas is a crude version of the same
   guard; this gives it a reason.
3. ~~**A falsifiable prediction (§5b).**~~ **SUPERSEDED — the coordinate already
   exists and is heritable (claude-8, 2026-07-30, whistle
   `invoke-1785418199604-320-26ca2e7a`).** The proposal was to add an explicit
   genotype-update delay to manufacture the pheno/geno pace gap. It is built:
   `mmca-clj/scripts/baldwin_selection.clj` carries a **heritable `update-prob`
   over `[0, 0.25, 0.5, 0.75, 1.0]`** — the genotype rewriting less often than
   the phenotype updates *is* the pace gap, and at 0 the field freezes entirely.
   So §5b's insight stands as an *interpretation* of a coordinate already under
   selection, and whether it hosts Baldwin is exactly what the next sweep tests.
   **Fold §5b into that sweep rather than proposing it separately.** The honest
   scoring: this note did not predict a missing coordinate, it supplied a reading
   of an existing one — which is worth less, and is still worth having.

## 8. What must not be claimed

- That the alignment is *derived*. It is an analogy between two of this stack's
  own hierarchies, proposed by the operator, and §5 lists two places it strains.
  It earns its keep as vocabulary and as §6's prediction, not as a result.
- That XTDB2 is proven adequate. It is a proof-of-capability *relative to
  XTDB1*, evidenced by the memory system running — while that same system reads
  `:component-limited`, i.e. **currently below the floor where its operator beats
  direct lookup.** Both are true: the exotype improved and is still binding.
- That Joe is "the xenotype" in any exclusive sense. §6's assemblage claim
  cuts against a single-locus reading, including a flattering one.
