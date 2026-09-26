# E-prove-salingaros-cascade-scorer — is the cascade wholeness L=T·H *really* a free energy?

**Date chartered:** 2026-06-24
**Type:** E-prefix excursion (bounded scope-out, single owner end-to-end).
**Owner:** claude-2 (proposed — holds the operational-scorer context; Joe to confirm/assign).
**Parent:** M-G-over-cascades (futon2) — grain-2 grounding. **Source:** deep-research open-Q3.
**Status:** CHARTER.

## Why it exists

M-wm-policies omission 2 built an **operational** AIF cascade scorer: `F = accuracy − λ·complexity`
(`accuracy` = ψ-coverage; `complexity` = Σ −log(co-application base-rate inclusion prior)), gating `gap-lane`
on F≤0 (the Bayesian-Occam accept criterion). That **grounds `(10−H) → complexity` operationally** and is live.
What it does **not** do is *prove* the formal identity the deep-research report names as its **open-Q3** (and its
headline literature-wide gap, Thread 4): **is Salingaros life `L = T·H` (equivalently disorder `C = T·(10−H)`)
recoverable as `accuracy × (−complexity)` on a generative model of the circumstance `|ψ⟩`?** This excursion
settles that — proof, monotone-equivalence, or a clean negative.

## The hard part (why this is a real problem, not a rubber stamp)

1. **Multiplicative vs additive (the domain bridge).** Salingaros `L = T·H` is a **product**; variational free
   energy `F = accuracy − complexity` is a **sum** (log domain). A product of probabilities is a sum of
   log-probabilities, so the natural conjecture is `L ∝ exp(F)` — i.e. `T·H` lives in *probability* space and
   `F` in *log* space. The proof must either establish that bridge or show it fails.
2. **Definition slippage to pin FIRST.** The codebase is loose: `cascade_construct.py` returns `T*H` under the
   key `"C"` — but `T·H` is Salingaros **L (life)**, while Salingaros **C** is `T·(10−H)` (disorder). And the
   code's `H = mean 4·s·(1−s)` over pairwise cosine `s` (a variance-like coherence that peaks at s=0.5), which
   is **not** Salingaros harmony nor obviously `−complexity`. Pin: the generative model `M(ψ)`, `accuracy =
   E_q[log p(ψ|patterns)]`, `complexity = KL(q(patterns|ψ) ‖ p(patterns))`, and exactly which of T / H / (10−H)
   maps onto which.
3. **Harmony ≠ prior-expectedness?** `H` (pairwise coherence / low redundancy) and `−complexity` (KL from the
   *base-rate* prior) are both "coherence," but one is *internal* (patterns agree with each other) and the other
   is *external* (patterns are a-priori expected). Prove they coincide, relate, or diverge.
4. **The live data already hints the naive identity is FALSE.** `cascade-lane` reports both `C(=T·H)` and `F`,
   and they do **not** co-rank on the 2026-06-24 spectrum (e.g. a mission can be high-F / modest-C). So a *clean
   positive identity is unlikely*; the realistic outcomes are (b) monotone-equivalence sufficient for the gate,
   or (c) genuinely-distinct quantities (geometric `L` ≠ variational `F`) — which would *confirm* the report's
   gap is a real difference, not merely unbuilt. Any of the three is a valid, valuable result if proven.

## Proof obligations / success criteria

- **O1 Definitions pinned** — `M(ψ)`, accuracy, complexity, and the T/H/(10−H) map written down unambiguously.
  *Naming half DONE (2026-06-24):* all the cryptic single-letter scorer names are now descriptive
  letter-word compounds (Joe: "single variable names are a liability"). The cascade-policy scorer emits
  **`wholeness`** (= T·H = Alexander wholeness = Salingaros life L; the old `"C"` key was a misnomer since
  Salingaros C is `T·(10−H)`), **`T-intensity`**, **`H-coherence`**, **`accuracy`**, **`complexity`**,
  **`F-free-energy`** (= accuracy − λ·complexity). Renamed across `cascade_construct.py`, `cascade_serve.py`,
  `futon2.report.cascade-lane` (cascade-lane + gap-lane + the `gap-free-energy-threshold` def),
  `vwm_sim_runs.clj`, `war_machine_pilot.clj`, `portfolio/effect.clj` (py_compile + clj-kondo 0/0 + parens OK +
  verified live; policy/core tests 21/75/0). *NB the separate `:C` in `flight_spec_verify.clj` is a
  DIFFERENT structure (flight-spec sampler-cascades, E-cascade-sampler-sampler) — left untouched, out of scope.*
  The remaining O1 work is the **mathematical** pinning (the generative model + the accuracy/complexity formulas
  as theorems, not just code).
- **O2 The identity, settled** — ONE of: (a) `L = g(accuracy, complexity)` for an explicit `g` (e.g. `L = exp(F)`
  up to constants) **proven**; (b) `L` and `F` shown **monotone-equivalent over cascades** (same ordering ⇒ the
  F≤0 gate ⇔ an L-threshold — sufficient for the act-gate); or (c) a **counterexample** proving they are distinct
  orderings, with a characterization of *when/why* they diverge.
- **O3 Empirical witness** — the conclusion checked against real cascades via the live scorer (`cascade_construct`
  already emits both `C` and `F`): the proof's prediction matches the data, or the divergence cases are exactly
  the counterexample of O2(c).
- **O4 Verdict back to the parent** — if (a)/(b): grain-2's `F` is *formally* AIF (close the residual in
  M-wm-policies / the grounding audit). If (c): record that `L=T·H` is a genuinely *non-free-energy* geometric
  quantity and the AIF gate must use `F`, not `L` (and update [[aliveness-synthesis]], which currently treats
  them as projections of one quantity).

## Scope

**In:** the formal relationship between the cascade wholeness (`T`, `H`, `L=T·H`, `C=T·(10−H)`) and the
variational `F = accuracy − complexity`, plus the empirical witness on real cascades.
**Out:** the broader L=T·H↔AIF link for the *whole* stack (FutonZero value=C, the full aliveness synthesis) —
this excursion settles the **cascade scorer** instance; the general identity is a larger research target. Also
out: changing the operational scorer's behaviour (it's live and grounded; this is about *proof*, not code) unless
O1 requires fixing the `C` naming.

## Pointers
- Operational scorer: `futon3a/holes/labs/M-memes-arrows/cascade_construct.py` (`base_rate_prior`,
  accuracy/complexity/F), `cascade_serve.py`, `futon2.report.cascade-lane/gap-lane`.
- Theory: `deep-research-AIF-morphogenesis.md` §"Grounding audit" + Thread 2 (EFE = complexity−accuracy; the
  Millidge wrinkle) + Thread 4 (L=T·H, the unbuilt link) + open-Q3.
- Parent: `futon2/holes/M-wm-policies.md` Track 3 (omission 2). Sibling: [[E-vwm]].
