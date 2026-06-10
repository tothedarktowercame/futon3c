# The Grounded Learning Specification (shared — claude-3 / claude-1 / claude-4)

*The contract the mesh works toward. Joe (2026-06-10): the shared **store** is proven; what needs a
clear shared spec is **how the machine-learning works**, and that's for claude-3/1/4 to decide
(Fable on tap). This doc is the convergence target. Coordinated under `C-falsifiable-missions`.*

## 0. The proven foundation (not in question)
Agents and the Cyborg (Joe) write **closures** — `:correlated → :open → :constructed` maturations
(cascade → sorry → wiring diagram) — into one **shared store** (`futon3a/meme.db` + the
`closure-ledger`/`closure-folds`), under one standard (realized closure, no laundering). Demonstrated:
5 agent closures + E-mission-head's 7 Cyborg arrows; `meme.db` 3→10.

## 1. The loop (the architecture all three build toward)
```
closure (real fold, in the shared store)
   │  signal = "did it help?" — dense, real, external to the prior, PERADAM-FREE
   ▼
LEARN     pattern-utility (posteriors) · edge-correctness (phylogeny upvote/seed) · pattern-missing (gaps)
   ▼
ACT       EXPLOIT (close what you can)  +  EXPLORE (EFE info-gain: seek what teaches most)
   ▼
AUDIT     peradams (sparse, OUTSIDE the loop) periodically check the grounding hasn't drifted
   └──────────────────────────────────────────────────────────────► better next closure
```

## 2. Per-agent contracts (who owns which surface)
- **claude-3 / E-ground-G** — the cascade-fold loop: phylogeny-grounded `construct_cascade` (Build 1 ✓),
  the closure-learning hooks (Build 2 ✓ — posteriors + phylogeny-learns + gap-log), the curriculum
  coupling to the star-map (Build 3). Owns the **edge-correctness** + **cascade** surfaces.
- **claude-1 / M-pattern-posteriors** — the **pattern posteriors**: consumes the fold-utility signal
  (now `pattern_posteriors.grounded.json`, grounded in fold-usage NOT self-grading). MUST
  **compose with wholeness scoring, not replace it** (the pointwise-greedy = cursor-bug risk). Owns
  **pattern-utility** + the A/B credit term in `construct_cascade`.
- **claude-4 / the car (M-peradam-grounding)** — the **peradam audit**: sparse, Goodhart-safe,
  outside the loop. Periodically check high-grounded trajectories also (later, independently) earn
  peradams; correlation high ⇒ grounding honest, drift ⇒ re-ground. Owns **the auditor**, NOT the
  reward.
- **the star-map (M-capability-star-map)** — the **EFE-over-graph scheduler** (G = risk + ambiguity +
  **info** + cost). Owns **EXPLORE** (picks the next hole by pragmatic+epistemic value).

## 3. The data interfaces (the contracts between surfaces)
- **Closure record** — what a closure carries. *(THE open schema question, §4.1.)*
- `pattern_posteriors.grounded.json` — `{stem → {α,β,mean}}`, Beta(1,1) prior. (claude-3 emits, claude-1 owns.)
- `pattern-phylogeny-learned.json` — the `{co_app,descent}` overlay (upvote/seed). (claude-3.)
- `cascade-coverage-gaps.edn` — the missing-pattern backlog → curriculum. (claude-3 emits, star-map consumes.)
- the **curriculum proposal** — ranked holes-to-learn by EFE, surfaced for operator ratification. (star-map.)

## 4. The OPEN ML questions (for the mesh to decide; Fable-relay candidates marked ★)
1. **★ Unified closure schema / grain-bridge.** Closures arrive in two grains — pattern-stem fold
   records (`closure-folds.edn`) and entity-keyed mission-diagram arrows (`meme.db`). What is the ONE
   closure schema both reduce to, so agent + Cyborg closures are a single learning signal?
2. **★ Credit assignment.** A fold uses a *subset* of the cascade. How is utility/credit assigned —
   to used patterns, to the edges among them, to the cascade-as-whole? (Ties to claude-3's
   don't-double-count cross-check: R2 move-grain vs pattern-grain must not double-count.)
3. **★ The EFE epistemic term.** Concretely, expected-information-gain for a candidate hole: how much
   would closing it teach (a new pattern? a cross-cluster edge? a capability)? The formula that makes
   EXPLORE real, not hand-waved.
4. **Posterior composition.** How do per-pattern posteriors compose with wholeness `C` in selection —
   so trust sharpens the cascade without reintroducing pointwise-greedy? (claude-1's named risk.)
5. **Drift / Goodhart safety.** The audit's cadence + threshold: when does "grounding drifted" fire,
   and what re-grounds it? (Keeps the peradam strictly the auditor.)

## 5. Status
Builds 1+2 live (the LEARN + part of the store↔loop). Build 3 (EXPLORE coupling) designed. The five
questions in §4 are the convergence work — claude-3/1/4 agree here before building further.
