# f13-guide working notes — m99J06

Cycle `m99J06-a0a723b1aef69731014d0b797f7f8ff0da1a8be419e8f064649dd606e8dbff0f`.
Not frozen material. Written so the analysis survives a pouch eviction.

## Solver dispatch 1

- action-id `f13-guide/dispatch-solver/1`, at version 9 -> 10.
- job `invoke-1787138965830-5047-ae5ec610`, recipient `f13-solver`.
- park `park-c016b9a7-6172-45ec-b363-b6f47a6a0aab`, deadline +3600s.
- **`[dispatch-recall-outcome=completed-with-memories]`** — first non-empty
  recall in the f9..f13 series. Two memories surfaced, both f12 harvest:
  - `e-1b72bb47-1575-4a08-ba3b-2a40735c2b86`
    propagate-local-api-mismatches-to-global-theorem-semantics (rewrite-rule)
  - `e-7c6631c9-caf0-4ee1-a1ce-2c7b7c6b88a6`
    audit-elaborated-regularity-semantics-before-proof-search (technique)
- The packet demands an inhabitation witness as first deliverable, WITHOUT
  supplying the argument below. Deliberate: whether the solver finds it (with
  or without the two surfaced memories) is the measurement.

## HYPOTHESIS (mine, NOT COMPILED): `apm_m99J06_H01Model H` is uninhabited

If this holds, `theorem apm_m99j06` is VACUOUSLY TRUE for every `H`, and the
frame confirms `:problem-closed-on-artifact` while refuting `:problem-solved`
— the f12 pair, by a different mechanism.

Argument. Everything `apm_m99J06_isH01Pair u du` requires constrains `u` only
on `[0,1]`:
- `AbsolutelyContinuousOnInterval u 0 1` is stated over `uIcc 0 1` only
  (Mathlib/MeasureTheory/Function/AbsolutelyContinuous.lean);
- `MemLp u 2 (volume.restrict (Icc 0 1))` sees only `[0,1]`;
- `HasDerivAt u (duRep x) x` for a.e. `x` in `[0,1]` is LOCAL, so for a point
  of `(0,1)` it constrains `u` only on a small neighbourhood inside `(0,1)`;
- `u 0 = 0`, `u 1 = 0`.

So take `u = 0` and `u' = Set.indicator {2} (fun _ => 1)`, both with `du = 0`.
`u'` is identically `0` on a neighbourhood of every point of `[0,1]` (the ball
of radius 1/2 avoids `2`), so both are `isH01Pair _ 0`.

`M.realizes` then gives `w w' : H` with `M.val w = u`, `M.val w' = u'`,
`M.weakDeriv w = M.weakDeriv w' = 0` — and `val w = u` is POINTWISE function
equality on all of `ℝ`.

`M.inner_eq` integrates over `Icc 0 1` only, where `u = u' = 0`, so
`⟪w,w⟫ = ⟪w,w'⟫ = ⟪w',w'⟫ = 0`, hence `‖w - w'‖² = 0`, hence `w = w'`, hence
`M.val w = M.val w'`, hence `0 = u' 2 = 1`. Contradiction.

The defect class: `realizes` pins `val` POINTWISE on all of `ℝ`, while
`inner_eq` measures only on `[0,1]`, so the structure demands an isometry
separate points that its own inner product identifies.

**Corroboration from the file's own header.** The "Statement repairs" note says
the former model "stored weak derivatives as literal functions even though its
inner product observed them only almost everywhere, making the hypothesized
structure inconsistent", and repaired it by moving `weakDeriv` into `Lp`. That
is EXACTLY this defect — and the repair was applied to `weakDeriv` and not to
`val`, which is still a raw `H → ℝ → ℝ`.

Status: HYPOTHESIS. Not compiled by me (card forbids scratch-file evals).
The solver owes a compiled witness in either direction. If it returns (A) with
a genuine model, this note is wrong and that is the better outcome.

If the solver misses it, this is the content to deliver at `:intervene` as an
`:answer` to its reported residual — content is permitted there because it
changes the route.

## Machine findings so far

- **The engine-owned park did not fire.** `park-dispatch` only posts when the
  cycle context carries `:agent`; `/api/alpha/parked` was empty after the
  dispatch, so I parked by hand. Report at close.

## Update, 11:34 — the solver is reproducing the argument unaided

`problems/m99J06/lean/Inhabitation.lean` appeared in the solver's worktree
(untracked, in progress) within ~2 minutes of dispatch. It builds
`test_isH01Pair` / `test_H01Model` as a local copy and is proving

    example {H} [NormedAddCommGroup H] [InnerProductSpace ℝ H] :
      IsEmpty (test_H01Model H)

by exactly the route in the hypothesis above, with the same `spike` witness
(`fun x => if x = 2 then 1 else 0`) — and slightly cleaner: instead of
`w = w'` it shows `⟪w,w⟫ = 0` and `⟪ws,ws⟫ = 0` separately, so `w0 = ws = 0`
and then `M.val 0 = 0` versus `M.val 0 = spike`.

**No guidance was given.** The solver got there from the packet's demand for a
witness alone. The measurement is intact.

### Review point that makes or breaks the witness (mine, for adjudication)

The witness is stated about a COPY (`test_H01Model`), not about
`apm_m99J06_H01Model`. Diffed the copied block against Main.lean lines 32-57
modulo the `apm_m99J06_`/`test_` prefix: **textually identical except one line
break** (`abbrev X_L2 :=` on one line rather than two). So the copy is
faithful and an `IsEmpty` result transfers.

But at review I require the CLOSE in `Main.lean` to be stated against the real
`apm_m99J06_H01Model` — a green `Inhabitation.lean` alongside an unchanged
`sorry` is not a close, and a close routed through the copy is not a proof of
the frozen theorem.

## Corrections to the notes above

- The engine-owned park finding STANDS, but my evidence was sloppy:
  `GET /api/alpha/parked` with no params returns `[]` with
  `more-pending: true`. The real query is `GET /api/alpha/parked?agent=f13-guide`,
  which shows exactly ONE park — mine (`park-c016b9a7-…`, mode `within-turn`).
  So: no engine park, my hand park is alive.
- Job telemetry reads `execution {:executed? false :tool-events 0}` while the
  solver is demonstrably writing files. The live signal is the agent record's
  `invoke-activity-at` (11:34:08, 21s quiet), not the job's execution counters.
  Do not read `executed? false` as wedged.

## RETRIEVAL FINDING — the OR-join fired, the query did not

From the offered receipt `e-4b862952-6668-4db9-9aea-e5b65cab1a7a` (authoritative;
the job event log truncates the prompt at ~1511 chars, which is why my first
report said "two memories" — **five were surfaced**, not two, and not merely
eligible).

The term extractor found the right vocabulary. `:term-sources`:

- problem-md: finding, strong, **sobolev**, uniqueness, **weakly**, dense,
  sufficient, weak, limit, equivalent, **subspace**, interval, converges,
  dimensional
- proof-outline-md: equality, **galerkin**, apm_m99j06_model_ext, combinations,
  **finite-dimensional**, gives, identities, **inner_eq**,
  representative-level, **riesz**
- stdin-packet: clause, lake, report, **vacuous**, verbatim, **apm_m99j06**,
  **apm_m99j06_h01model**, blocked

The query actually issued was

    finding OR equality OR clause OR strong

i.e. the first term of each of the three sources plus the second term of the
first. Mechanism, read from source, not inferred:

1. `dispatch_with_recall.clj:552` round-robins the three source term-lists.
2. `default-query-term-limit` is **4** (line 21), applied at line 557.
3. Round-robin position 5 is `galerkin`. The cap cuts at 4.

So every distinctive term — galerkin, sobolev, riesz, inner_eq,
apm_m99j06_h01model — is one slot or more past the cap. `text-keywords`
(line 320) sorts each source rarest-first *by problem-corpus IDF*, and against a
corpus of mathematics problems the rare words are the PROSE words. The source
comment at line 424 already names this: "problem-corpus IDF selects artifact
vocabulary and INVERTS relevance — e-retrieval-miss-a01A12-slit-wedge".

**Correcting my orientation on the anchor band.** The briefing said
`anchor-df-band [3 150]` "is not doing selection". True — but not for the reason
given. The band lives in `query-anchor-term-memory-df`, which runs ONLY when
`--anchor-source memory-df`. The default is `:problem-idf` (line 562) and the
receipt confirms `:anchor-source :problem-idf`. The band was never on the path.
Same for the df-scoping fix `6bfe5808`: it only affects the memory-df path.

**And the wave-2 rung as built would not have repaired this dispatch.**
`--anchor-source memory-df` changes `required-term` only — the term that gets a
ranking boost (`rank-with-anchor-boost`, line 689). The query terms are
unchanged. Flipping it yields the same `finding OR equality OR clause OR strong`.
The damage is in the query; the built fix targets the anchor.

**What actually delivered the memories.** `:memory-use/surfacing-via`:
- `e-1b72bb47…` (the one the solver used first) — `:content-match`
- the other four — `:pattern`

So lexical recall contributed exactly ONE memory, and it ranked **27th of 30**
in the lexical seed (score -9.88); 27 of the 30 seed hits were
`:evidence-type :coordination` (chat turns, invoke receipts), not memories. The
cascade contributed four. The read path returned the right thing largely by
luck of the four generic words appearing in it.

STORE-MODE: this is an OBSERVATION, not a change. I have not touched retrieval
and will not this frame.

## SOLVER RESULT — job done 11:36:43, verdict (B) UNINHABITED

Commit `25186f291b25c98a0c5e6e2280a951ab107be277` on
`exp/frame-13-m99J06-solver`. Solver's own report: close is **VACUOUS**, no
sorry/admit/added axiom, worktree clean, no residuals.

Per-memory attestation returned unprompted and complete over all five:
- USED `e-1b72bb47…` — propagated the whole-function/interval-semantics
  mismatch through `realizes` and `inner_eq` to model emptiness.
- USED `e-7c6631c9…` — audited the model's inhabitation BEFORE PDE/Galerkin
  proof search.
- IGNORED the three `e-codexpilot-…` (typeclass preconditions; ContDiff — not
  in this statement; pair-refutation-with-repair — contract forbade repair).

### My review (gate, not rubber stamp)

- `git diff a92ffb6..HEAD`: +77/-1, one file. The five frozen definitions and
  the `apm_m99j06` signature are **byte-identical** to the pin (checked by
  extracting the block and comparing with the inserted lemma removed:
  `FROZEN BLOCK IDENTICAL: True`).
- `grep sorry|admit|^axiom|sorryAx` over Main.lean: **NONE**.
- The witness is stated against the REAL `apm_m99J06_H01Model`
  (`theorem apm_m99J06_H01Model_isEmpty`, Main.lean:83-152), not a `test_` copy —
  the copy file was deleted. The close is
  `letI : IsEmpty (apm_m99J06_H01Model H) := …; exact isEmptyElim M`.
  My stated review bar is met.
- Independent compile + axiom audit: bg job `bg-1787139511615-1`.
