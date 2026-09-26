# M-typed-holes — ARGUE (2026-06-14)

Status: complete

*Phase 4 per `futon4/holes/mission-lifecycle.md`: synthesise — why is the DERIVE
design *right*, not merely workable? Technical synthesis (pattern cross-ref,
coherence, trade-offs, generalization) + a plain-language argument. Exit: the
design feels **inevitable** given the constraints. (Note: this is the "as
planned" ARGUE; an AIF-driven re-run is queued for after, as a comparison.)*

## Pattern cross-reference (`futon3/library/`, `futon3c/library/`)

| pattern | where it applies in DERIVE | how |
|---|---|---|
| **`realtime/single-authority-registration`** | D1 (one `fill`) + D3 (precision gate) | Its thesis — *enforce single authority at registration time, not post-hoc through eviction watches; pre-hoc is deterministic and testable* — is exactly D1: enforce one `fill` at the datatype, not reconcile six fills after the fact. The window where "an agent exists in two routing stores" is the window where six fills drift. Same with D3: type-check the fill at fill-time (I1), not audit afterward. |
| **`math-informal/construct-an-explicit-witness`** | D4 / I5 (proof = witnessed fill) | *To prove existence, build the object.* A fill doesn't assert a hole is closed; it **exhibits the filler** (the ArSE record IS the constructed witness). Proof-by-witness, not proof-by-assertion. |
| **`meta/exotype-determines-behaviour`** | the whole thesis (Poly typing) | Behaviour = genotype + **exotype**. The hole's *type* (`:hungry-for`, the Poly direction-type) determines what may fill it and thus the behaviour — the datatype is the exotype that makes the six projections one. |
| `realtime/verify-after-start`, `realtime/surface-map` | VERIFY gate; the views (ArSE/CLI/overlay) | the verification discipline + the surface contracts for the fill loop's UI. |

### PSRs (catch-up — DERIVE skipped them; per the lifecycle)

- **PSR-1 — `single-authority-registration` → the single `fill` operator (D1).**
  *Context:* six projections each implement "discharge a hole." *Decision:* one
  runtime `fill`, projections as views. *Why:* the pattern proves post-hoc
  reconciliation is racy/undeterministic; a single pre-hoc authority is
  deterministic and testable — the unification's whole value.
- **PSR-2 — `construct-an-explicit-witness` → witnessed answer (D4/I5).**
  *Context:* `answers ≡ fills` could be merely definitional. *Decision:* require
  an ArSE record per production answer. *Why:* the pattern says existence is shown
  by *building* the object; a proof is the witness, not the claim.

## Theoretical coherence

The design serves IDENTIFY's anchoring exactly, it has not shifted: a typed hole
*is* a Poly position-with-directions; the single `fill` *is* Poly substitution /
the comonad counit (already formalised — `DarkTower/Fill.lean`, `Discharge.lean`);
the precision gate I1 *is* "well-typed substitution"; `answers = fill` (I4) is the
`ScopeQuery` lemma. So DERIVE is not a new theory bolted on — it is the *runtime
shadow* of the Lean datatype. Coherence is strong: every DERIVE decision has a
formal correlate already proved.

## Trade-off summary (what we gave up, why)

- **D2 store-agnostic** gives up per-store optimisation → accept it; schema
  uniformity (`hx/`) makes one engine correct and per-store engines waste.
- **D3 typed-fills-only** gives up *recall* (some real fills with weak type
  evidence are refused) → accept it; precision-over-recall is the campaign's
  settled, claude-1-endorsed discipline (false fills poison the substrate).
- **D5 ArSE-wiring (not abstract Lean illocution)** gives up a self-contained Lean
  proof of the coord projection → accept it; the runtime already exists
  (M-typed-bells COMPLETE) and the concept activates by *use*.
- **D6 design-now/build-gated** gives up immediacy → accept it; the mining is
  gated and heavy, design is free, and this keeps us off the loaded CPU.

## Generalization notes

The design is not MO/CT-specific. It works for **any hyperedge store exposing the
`hx/` schema** (substrate-2 the stack, substrate-2a math.ct, MO, golden-graphs —
and future stores). The `single-fill + explicit-witness` pair generalises to any
`(hole, fill)` system: a build/CI pipeline (a failing job = a hole, a fix = a
fill), an issue tracker, a curriculum. To apply elsewhere: supply a store in the
`hx/` schema and a HoleType lexicon; the operator and the witness discipline are
unchanged.

## Confronting the alternative (the "inevitable, not possible" bar)

The only real alternative is **keep six fills** (status quo). It is *possible* but
not *right*: `single-authority-registration` shows post-hoc reconciliation across
six implementations is racy and untestable; the six already disagree in small
ways (e.g. grounding's precision gates exist, the comb's didn't, the query's
were definitional). Unifying is the move that makes the disagreements impossible
by construction rather than caught by audit. Given (a) the stores already share
one schema, (b) the datatype is already one Lean object, and (c) the witness
runtime (ArSE) already exists — building anything *other* than one `fill` over
one schema with one witness discipline would be re-deriving the disunity we just
removed. That is what makes the design feel inevitable.

## Plain-language argument (the elevator pitch)

> The system kept building the same thing six different times — a slot waiting to
> be filled: a question waiting for an answer, a proof waiting to be finished, a
> symbol waiting for its definition. They're all the same shape, and "fill the
> slot" is one action. So we build it **once** and let each use be a view of that
> one thing. The payoff: **asking a question and proving a theorem become the same
> act** — find what fills the slot — and every answer is recorded so it can be
> trusted.

## Exit

Technical synthesis (3 applied patterns + 2 PSRs, coherence with the formalised
datatype, trade-offs, generalization) and the plain-language argument are in
place; the keep-six-fills alternative is confronted and the design reads as
inevitable given the shared schema + the one Lean datatype + the existing witness
runtime. **What ARGUE hands to VERIFY:** check the design against the (sketched)
wiring + the IDENTIFY completion criteria — does one `fill` + the proving loop
actually cover all six projections, with no orphan hole-source and no projection
left reimplementing fill?

*Cross-refs:* `M-typed-holes.md`, `M-typed-holes-DERIVE.md`,
`E-typed-holes-example-self.md` (this fills its `:ARGUE` ghost),
`futon3/library/math-informal/construct-an-explicit-witness.flexiarg`,
`futon3c/library/realtime/single-authority-registration.flexiarg`,
`futon3/library/meta/exotype-determines-behaviour.flexiarg`.
