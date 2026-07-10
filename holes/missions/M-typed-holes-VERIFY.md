# M-typed-holes — VERIFY (2026-06-14)

Status: complete

*Phase 5 per `futon4/holes/mission-lifecycle.md`: check the design against
evidence — does it hold, what breaks it? ARGUE's handoff: "does one `fill` + the
proving loop actually cover all six projections, with no orphan hole-source and
no projection left reimplementing fill?" Exit: the design is certified, with the
gaps named honestly.*

## VERIFY is itself a scope — two holes, two certificates

The thing to verify splits into two claims at two altitudes, so VERIFY is a
**scope** (a partial hyperedge) with **two typed holes**:

```edn
{:verify/scope
 {?design-cert  {:hungry-for :proof-for-all}    ;; the model is coherent
  ?wiring-cert  {:hungry-for :checked-on-deployment}}}  ;; the running code matches
```

- **`?design-cert`** — *the six projections are one `fill`, no orphan, none
  reimplements it.* This is a coverage/factorization claim → a claim about **all**
  cases → discharged by a **Lean proof** (proof > model-check here, because the
  content is exhaustiveness, which a finite model only samples).
- **`?wiring-cert`** — *the deployed projections actually satisfy I1–I6 and don't
  regress grounded%.* This is about the running Clojure → discharged by a
  **runtime check** (Lean cannot see deployed code).

VERIFY passes exactly when **both holes are filled**. And — the reflexive payoff
— *each certificate is itself a fill*: a Lean proof discharging a goal, and a
runtime check discharging a query-as-scope. So VERIFY does not merely assert the
thesis, it **dogfoods it twice**: the phase verifies the framework using the
framework. (This is why Lean-alone would leave the VERIFY-scope half-open — the
wiring hole would stay hungry.)

## Certificate 1 — design (LANDED)

**Artifact:** `mathlib4/DarkTower/Coverage.lean` (commit `84b61e9`, authored by
codex-1, reviewed PASS by claude-2). `lake build DarkTower` green (8510 jobs);
**0 `sorryAx`** globally (independently re-built + axiom-checked); all axioms are
standard mathlib (`propext` / `Classical.choice` / `Quot.sound`, from
`Fintype`/`Finset`/`simp`).

What it certifies (`Projection` = the six hole-sources as a 6-constructor
inductive; `fillFacet` = a total, **no-catch-all** classifier into the *existing*
DarkTower facets — no new fill is defined):

| projection (DERIVE) | routes through | theorem | strength |
|---|---|---|---|
| discharge / proof | comonad counit `Discharge.filled` | `discharge_routes_through_filled` | **strong** |
| ground / symbol | the **same** counit | `ground_routes_through_filled` | **strong** |
| reply / bell | the **same** counit | `reply_routes_through_filled` | **strong** |
| compose / comb | `Comb.comp` (dependent lens) | `compose_routes_through_comb_comp` (+ `_direction`) | **strong** |
| answer / query | `ScopeQuery.fills` (= I4) | `answer_routes_through_fills` (reuses `answers_eq_fills`) | **strong** |
| cascade-feed / mining | `TypedHole.satiety` grade | `cascadeFeed_routes_through_satiety` | **schematic** |

- **"No orphan hole-source" is discharged *by construction*:** `fillFacet` is an
  exhaustive match with no catch-all, so adding a seventh projection stops the
  file compiling until classified; `noOrphan : Fintype.card Projection = 6`
  pins the count. Lean's totality checker *is* the no-orphan verifier.
- **"None reimplements fill" holds for 5/6:** discharge/ground/reply are
  *literally the same* `Discharge.open_filled` (they vary only by an unused
  `DischargeKind` tag — proof that the three illocutions are **one** counit, not
  three operators); compose is `Comb.comp`; answer is `answers_eq_fills`. No
  branch defines a fresh fill.

**Honest gap (carried, not hidden):** `cascade-feed/mining` is certified only as
a *static* satiety-grade view of a node (`T.satiety a.1 = grade`), **not** a
hungry→sated *transition* — DarkTower has `SatietyGrade` but no satiety
transition system yet. So mining is covered as "the graded hole exists," not as a
fill *act*. This is the one weak leg (same honesty bar as PaperExample's
single-node store / BV's schematic rules). Optional follow-up: add a satiety
transition to `TypedHole.lean` and upgrade this leg to a real fill proof. Does
not block VERIFY; recorded as a known schematism.

## Certificate 2 — wiring (PARTIAL — an open arrow)

The wiring certificate checks the **deployed** projections against I1–I6 + the
grounded% tripwire. It is **partially fillable now**, with a named gap:

- **Checkable today (already running):**
  - *answer = fill at runtime, AT SCALE* — `scripts/scope_query_dogfood.py`
    (c694d55) runs a structural ScopeQuery where answers ARE the fills of the
    scope's holes. **Gate (a) substrate-2a import LANDED** (2026-06-15, futon6
    `12fa355`, authored by codex-1, reviewed PASS by claude-1 — determinism
    re-run + source-fidelity recount): the mined CT first cut (9736 papers) is
    imported to the `hx/` schema (80-paper sample store, grounded% baseline
    67.2, `data/substrate-2a/QA-METRICS.md`), so the runtime now answers over
    real mined CT, not just the 8-paper golden. The runtime shadow of
    `answers_eq_fills`, deployed.
  - *proving loop witnessed in ArSE (I5) LANDED* — **gate (b)** (2026-06-15,
    futon3c `a573b54`, authored by codex-1, reviewed PASS by claude-1).
    `scripts/proving_loop.py` poses 5 grounding scopes over substrate-2a and
    records each answer as a witnessed `(ask → answer)` ArSE pair via the direct
    `/arse/ask`+`/arse/answer` endpoints (FUTON3C_TYPED_BELLS on; ArSE 69→74;
    `unanswered`=0). Independently verified: thread `ask-1781552434-69`'s witness
    equals `answers()` for that scope. **Proof = witnessed fill, demonstrated
    live** — the C-adopt-on-flag-ON step.
  - *reply/bell projection live* — M-typed-bells is COMPLETE; `type=query` →
    `type=answer --ref` is the deployed query/answer fill (I5's witness loop).
  - *ground/symbol precision gates live* — the grounding harvesters (appositive,
    def-eq, lexicon) enforce typed fills (I1/D3) on the gate papers; grounded%
    is measured and held (claude-1-endorsed discipline).
- **Owed (the open arrow `?wiring-cert ⊸ route-all-six-through-one-fill`):** that
  *every* deployed projection routes through a **single** runtime `fill` (D1) is
  not yet true — the projections still have their own discharge code; D1 is the
  build, and per D6 it is **gated** behind the mining wrap + claude-1 coordination.
  Target specified, construction owed.

**This open arrow is exactly failure-condition #1** from the HEAD/AIF workup
("dead if no projection actually routes through the single `fill`"). So the
wiring certificate's hunger IS the death clause — VERIFY makes the adoption gate
machine-checkable rather than rhetorical. The wiring cert is *green-when-D1-lands*,
not green now, and saying so is the honest VERIFY result.

## Verdict

- **`?design-cert`: FILLED** (Coverage.lean) — 5/6 projections proven to share one
  fill, no orphan by construction; cascade-feed schematic.
- **`?wiring-cert`: PARTIALLY FILLED** — the answer/bell/ground legs are checkable
  and pass today; the "all six through one `fill`" leg is an owed open arrow,
  gated on the D1 build (D6).

So the VERIFY-scope is **one hole filled, one hole hungry-with-a-named-target** —
which is the honest state of a *design-now/build-gated* mission (D6). The design
is certified for-all where it can be; the wiring is certified where deployed and
owes exactly the unification build the mission exists to motivate. **What VERIFY
hands to INSTANTIATE:** the owed artifacts are (1) the single runtime `fill` with
six adapters (closes `?wiring-cert` + failure-cond #1), and optionally (2) the
satiety-transition upgrade (closes the cascade-feed schematism in `?design-cert`).

*Cross-refs:* `M-typed-holes.md`, `M-typed-holes-ARGUE.md` (this answers its
exit question), `mathlib4/DarkTower/Coverage.lean` (the design certificate),
`scripts/scope_query_dogfood.py` (the runtime answer=fill check),
`E-typed-holes-example-self.md` (this fills its `:VERIFY` ghost),
`E-typed-holes-aif-alternate.md` §3 (the failure conditions the wiring cert makes
checkable), `futon4/holes/mission-lifecycle.md`.
