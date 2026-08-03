# V3 arm-set sequencing receipts (computed 2026-08-03, claude-10)

The RS-2 product shape applied to the whole `V3-arm-design.md` arm set:
a partial order over arms and instruments, each constraint carried by a
computed receipt or a named data-contact finding — not intuition.
Sources: retrieval-stage-causal-spec.json (applied v2), the RS receipts,
E8's frozen results, and the falsification-with-data run (`046b6fb6`).
Companion EDN: `v3-sequencing-receipts.edn`.

## SEQ-0 — Instrumentation preconditions (top of the order; block arms)

1. **Common outcome endpoint, preregistered.** The falsification
   mapping table found `runner-outcome` UNMEASURED across the frozen
   corpus (heterogeneous result classes, 10 absences, no common
   endpoint). Until the cohort preregisters one, NO arm's primary
   endpoint is derivable — this precedes everything, including the
   control arm.
2. **Pull-side surfacing fields** before `:push+pull` / `:pull-only`
   activate — operational now as the cohort dispatch guard
   (`causal/cohort-guard`, 47166e23/65fef3ab): refuses at dispatch
   with the missing denominator field named.
3. **Per-dispatch attachment export** before axis 3 runs:
   `attachment-density` — axis 3's own treatment-adjacent variable —
   is UNMEASURED in current receipts (no joinable frozen export). An
   axis-3 cohort without it produces arms whose treatment cannot be
   verified from the record.
4. **Offer-stage measurement repair** before any mediation-through-
   offer analysis: both exact CI violations (v5: p=0.012; v6:
   p=2.4e-9) sit on the surfaced→offered→used chain, with
   "instrument field ≠ node" as a live reading for each.
5. **Loud ranking fallback (added post-rank-rerun):** receipt-ranking
   (Ψ) is INTERMITTENT — under store backpressure the ranking-stats
   fetch degrades silently to base ordering against an identical
   snapshot (two-run evidence, same store sha, different score-kind).
   Until the fallback is loud in the receipt, any Ψ-dependent arm
   difference is confounded with store load — a nuisance NO earlier
   sequencing receipt carried. Silent degradation is worse than
   absence: absence shows in a receipt, degradation does not.
   (Spec-side: candidate delta v8, store-load as latent cause of
   ranking-mode, registered pending this instrument.)

## SEQ-1 — Settled by E8 (frozen data; no arms to spend)

Cardinality is inert (40% flat across A/B8/B12/B16); vocabulary is the
lever (oracle 40%→80%). Axis 2's cardinality levels collapse;
`:structure-aware` is the one remaining build, measured against the
oracle ceiling. (RS-2's E8-before-C1 constraint, discharged and
vindicated.)

## SEQ-2 — Axis 2 is NESTED under axis 1, not crossed (computed today)

Receipt: severing the dispatch-time query edges (the `:pull-only`
regime) leaves query-vocabulary and query-cardinality with **no
directed path to runner-outcome** — axis 2 is causally inert by
construction in `:pull-only`. Crossing axis 2 with `:pull-only` spends
arms on a structurally null contrast; the design should assign axis-2
levels only within push channels.

**Bonus warning, path named:** in `:pull-only` arms the association
`[:query-vocabulary ← :problem-difficulty → :runner-outcome]` remains
open — any observed query-quality/outcome correlation there is PURE
difficulty confounding and must not be read as a query effect.

## SEQ-3 — Axis 3 last (RS-2 masking + SEQ-0.3)

The attachment effect's only route to surfacing shares its target with
the query stage (surfaced-set parents: attachment-density,
pattern-endpoints, pollution, reachability); under a starved query the
populated-vs-star contrast can null for query reasons. Axis 3 runs
after axis-2's vocabulary settlement is applied to the shipped builder
AND after SEQ-0.3's attachment export exists.

### SEQ-3 addendum (earned by the attachment export's first join)

The star-forest/populated contrast is ALREADY PARTLY REALISED in the
corpus: 4 endpoints populated (densities 16-40, carrying 106/167
attachments), 21 sparse (≤4). Randomizing axis 3 across problems
without blocking on endpoint density dilutes the contrast with
now-measurable effect modification. Constraint: STRATIFY on endpoint
density, or restrict axis 3 to sparse endpoints where the treatment is
an actual change. (claude-12 adopting into the arm design.) Note also:
the export's first use WEAKENED the conjecture axis 3 was built to
test - the sole arm-D failure sits on the densest endpoint, so
attachment starvation is refuted at that extreme; see spec deltas
v3-vindicated / v10-candidate.

## The resulting partial order

```
SEQ-0 instruments (endpoint, pull fields, attachment export, offer repair)
  └─> axis 1 channel arms (guard-licensed)
        └─> axis 2 (:structure-aware vs shipped, WITHIN push channels only)
              └─> axis 3 (populated vs star)
```

Every constraint above is citable to a receipt, a frozen result, or a
named violation; none is a preference.
