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

> **THE SAFETY INVARIANT (claude-4, load-bearing — state it, don't imply it):** *the auditor NEVER
> feeds peradams back as gradient.* Drift triggers re-grounding via the **dense closure-fold signal +
> the operator** — never via peradam-as-reward. If re-grounding trained on peradams, they'd be pulled
> into the loop = the exact Goodhart this whole architecture exists to prevent. The peradam DETECTS;
> the closure-fold + operator CORRECT.

## 2. Per-agent contracts (who owns which surface)
- **claude-3 / E-ground-G** — the cascade-fold loop: phylogeny-grounded `construct_cascade` (Build 1 ✓),
  the closure-learning hooks (Build 2 ✓), the curriculum coupling to the star-map (Build 3). Owns the
  **co-app EDGE surface** (phylogeny upvote/seed overlay) + the cascade.
- **claude-1 / M-pattern-posteriors** — the **pattern posteriors** (contract **AGREED** 2026-06-10):
  consumes the fold-utility signal (`pattern_posteriors.grounded.json`, grounded in fold-usage). Owns
  the **pattern-NODE surface** (α/β trust) + the A/B credit term; MUST compose with wholeness, not
  replace it.

> **THE NODE/EDGE SURFACE SPLIT (claude-1, makes non-double-count STRUCTURAL):** one closure projects
> **orthogonally** onto two surfaces — **vertices** (claude-1's per-pattern α/β) and **co-app edges**
> (claude-3's phylogeny overlay). They are non-double-counting **by construction, not by bookkeeping**:
> a node-trust scalar and an edge-weight are never the same number. **claude-3 holds edges, claude-1
> holds nodes — confirmed.**
- **claude-4 / the car (M-peradam-grounding)** — the **peradam audit** (contract **AGREED**
  2026-06-10): sparse, Goodhart-safe, outside the loop, bound by the safety invariant above. Owns
  **the auditor**, NOT the reward; `ac4ae5d` (the discharge-half) is the realized-closure emitter into
  `meme.db` the audit reads. Auditor design = Q5 (resolved, §4).
- **the star-map (M-capability-star-map)** — the **EFE-over-graph scheduler** (G = risk + ambiguity +
  **info** + cost). Owns **EXPLORE** (picks the next hole by pragmatic+epistemic value).

## 3. The data interfaces (the contracts between surfaces)
- **Closure record — RESOLVED (Fable, 2026-06-10): the endpoint-keyed BHK-triple arrow IS the unified
  schema; `closure-folds` and `meme.db` are two PROJECTIONS of one `promote!` event stream.**
  `closure = {endpoint {have,want} (identity/join) · construction {components [pattern-id…],
  composition <rewrite-graph>, artifact <code-ref>} · provenance {agent|cyborg, …} · grain (a facet)}`.
  A fold record is a closure whose `construction.components` are library stems; a mission-arrow
  `promote!` is one whose components may be empty + artifact points at code. **The learning signal =
  the `promote!` event stream**: pattern-grain reads `construction.components`, mission-grain reads
  `endpoint`. (Implication: `cascade_learn` should consume the `meme.db` stream; `closure-folds.edn`
  becomes a pattern-grain *view* — this also unifies agent + Cyborg closures into one signal. Build:
  the grain-bridge, §5.)
- `pattern_posteriors.grounded.json` — `{stem → {α,β,mean}}`, Beta(1,1) prior. (claude-3 emits, claude-1 owns.)
- `pattern-phylogeny-learned.json` — the `{co_app,descent}` overlay (upvote/seed). (claude-3.)
- `cascade-coverage-gaps.edn` — the missing-pattern backlog → curriculum. (claude-3 emits, star-map consumes.)
- the **curriculum proposal** — ranked holes-to-learn by EFE, surfaced for operator ratification. (star-map.)
- **per-move grounded-G** — the loop's value prediction keyed by `:move/id`; the **audit's x-axis**.
  *(Open interface, claude-1 ↔ claude-4: claude-1's posteriors/the cascade must expose it; claude-4's
  lift-audit joins peradams onto it.)*

## 4. The OPEN ML questions (for the mesh to decide; Fable-relay candidates marked ★)
1. **Unified closure schema / grain-bridge — RESOLVED (Fable, 2026-06-10; full answer in
   `C-falsifiable-missions.md` §7 A1).** Don't build a third schema: the **endpoint-keyed BHK-triple
   arrow IS the unified closure** (see §3 "Closure record"). A fold record and a `meme.db`
   `promote!` are the *same kind* of event, differing only in a `grain` facet; the **learning signal
   is the one `promote!` event stream**, read by two projections (pattern-grain ← `construction.components`,
   mission-grain ← `endpoint`). This unifies agent + Cyborg closures and preserves anti-laundering
   structurally (a closure is a mode-crossing; attestation can't constitute one). **Build:** the
   grain-bridge — `cascade_learn` consumes the `meme.db` stream, `closure-folds` becomes a view.
2. **Credit assignment — RESOLVED (claude-1 + claude-3, 2026-06-10).** Credit the **USED SUBSET** of
   the cascade (NOT cascade-as-whole — too coarse; NOT edges — claude-3's surface). **Per-pattern
   Bernoulli** on the used subset: used-and-closed → `α+=1`; used-and-**didn't**-close → `β+=1`.
   Cross-stage **non-double-count invariant** (claude-3's R2 cross-check, stated testably): no
   closure-derived scalar enters BOTH a move-weight AND a pattern-posterior later **summed in one
   selection score** — satisfied by construction (R2 at MOVE-RANKING, the posterior at WITHIN-CASCADE
   CONSTRUCTION; different stages, never summed into one `G`), reinforced by the node/edge split.
   **Fold-record answer (claude-3):** `closure-folds.edn` *already* records the **used subset**
   (`:used`); `cascade_learn` credits only those — so the `(1,0)/0.667` uniformity in `grounded.json`
   is **all-success-each-once data, not cascade-as-whole crediting.** **Discrimination unlock (next
   refinement):** `cascade_learn` must process **failed** folds (used-and-didn't-close → `β+=1`);
   today it skips `success:false`, so there's no β yet — record failed folds and discrimination appears.
   **Fable's sharpening (three currencies, never summable):** **utility** → the *composite* (the used
   subset *as* a new first-class wiring-fragment arrow), credited **once**; **attestation**
   (statistics, not reward) → the used components; **calibration credit** (a proper scoring rule —
   e.g. log-loss of the used-set under the proposal) → the proposing cascade, never utility. Different
   units ⇒ move-grain and pattern-grain *cannot* double-count; semilattice overlap only affects
   attestation shares, not reward.
3. **The EFE epistemic term — RESOLVED (Fable, 2026-06-10; full answer in `C-falsifiable-missions.md`
   §7 A3).** `info(h) = expected posterior contraction over the units the hole's REQUIRED construction
   would touch` — Beta-Bernoulli **closed form**: `Σ_{u∈S(h)} E_o[KL(Beta(α,β|o) ‖ Beta(α,β))] +
   Σ_{e∈structure(h)} E[log BF(edge-exists vs not)]`. Large exactly when **pseudo-counts thin +
   predictive p≈0.5**. The three candidate signals fall out as *cases*: coverage-gap pattern = a fresh
   unit (max prior entropy → max IG); missing cross-cluster edge = first obs on an untested `θ_e` +
   the structure-discovery BF; frontier capability = `H(cap)` reduction only (its *value* lives in the
   PRAGMATIC term — don't leak it in twice). **Two guards:** (i) `S(h)` from the hole's *required*
   construction, never "could mint novel units" (else EXPLORE farms novelty — the anti-farming line);
   (ii) IG peaks at p≈0.5 on thin posteriors → the scheduler selects at the **edge of competence**
   (EOC) — and that's the **T-run verification signature**: if EXPLORE keeps picking sure-things or
   sure-failures, the info term is mis-implemented. *(Consumes claude-1's α/β variance — the surfaces meet.)*
4. **Posterior composition — RESOLVED (claude-1, 2026-06-10) — provably non-greedy via 3 invariants:**
   (I) **trust-neutral at prior** — an unclosed pattern at Beta(1,1) mean 0.5 → centered 0 →
   multiplier 1.0 → `m'(p)` unchanged; (II) **multiplicative on positive margin only** — trust
   re-weights `m'(p)=rel·connectivity` (already wholeness-coherent); a high-trust LOW-connectivity
   pattern still gets low `m'` → not chosen; **trust cannot manufacture a place, only re-order among
   candidates that already earn one by wholeness** (modulate only when `m'(p)>0`); (III) **bounded +
   saturation-external** — multiplier in `[1−0.5w, 1+0.5w]`; the posterior never alters the
   coverage-saturation stop or budget. **Exploit/explore split at this surface:** composition consumes
   the **mean** (EXPLOIT); the star-map's EXPLORE consumes the **variance** (α,β) for the epistemic
   boost (Thompson-compatible) — a partial answer to Q3 from the posterior side (see Q3).
5. **Drift / Goodhart safety — RESOLVED (claude-4, 2026-06-10; τ pending operator ratification).**
   The auditor:
   - **Metric:** an out-of-sample, *independent* prediction-check. The loop predicts value
     (grounded-G) at `t`; the peradam (dokusan-certified, operator-supplied, LATER + independent) is
     the outcome at `t+k`. **Lift = P(peradam | high-grounded-G) / base-rate.** Lift≫1 ⇒ grounding
     honest; lift→1 ⇒ peradams uniform across grounded-G = value decoupled from reality = **drift**.
     Use **lift / precision@k**, not raw per-move correlation (peradams too sparse). **Join on
     `:move/id`** (move-grain → consistent with R2; no double-count with pattern-grain learning — the
     Q2 cross-check).
   - **Cadence:** **peradam-accrual-event-driven** (fire when K≈5–10 new certified peradams accrue),
     not wall-clock — respects sparsity + dokusan gating; long-interval backstop for quiet periods.
   - **Threshold:** drift fires when **lift < τ, sustained across 2 consecutive windows**, each
     ≥ N peradams. **τ is data-calibrated from the first batches' lift distribution + operator-ratified
     — an `:O-*` observable** (like `:O-cascade-budget`), NOT guessed.
   - **Re-grounding (neither action uses peradam-as-gradient — the safety invariant):** (1) re-weight
     toward the **dense closure-fold** — drift implies the prior over-trusted self-estimate, so shrink
     it back toward fold-grounded evidence; (2) **surface** the drifted theses/holes to the
     operator/curriculum (a dokusan at the meta-grain). Peradam detects; closure-fold + operator correct.
   - **Operator touchpoints (Joe):** (a) ratify τ from data; (b) the re-grounding dokusan when drift fires.
   - **One interface to nail (claude-1 ↔ claude-4):** claude-1's posteriors must expose the **per-move
     grounded-G** that is the audit's x-axis. *(Open interface — §3 addition.)*

## 5. Status — SPEC CONVERGED (2026-06-10)
**All three contracts AGREED** (claude-3 · claude-1 · claude-4); **all five ML questions RESOLVED**
(Q2/Q4 claude-1, Q5 claude-4, **Q1/Q3 Fable** — full Fable answers in `C-falsifiable-missions.md` §7).
Builds 1+2 live. **Next builds, now fully specified:**
- **The grain-bridge (Q1)** — `cascade_learn` reads the `meme.db` `promote!` event stream (the unified
  closure); `closure-folds` becomes a pattern-grain view. Unifies agent + Cyborg closures into one signal.
- **Failed-fold β (Q2)** — record `success:false` folds so per-pattern discrimination appears (the
  three-currency credit model: utility→composite, attestation→components, calibration→proposer).
- **Build 3 / EXPLORE (Q3)** — the EFE `info` term = expected posterior contraction (Beta-Bernoulli
  closed form), coupled to the star-map; verify via the **edge-of-competence** signature in T-runs.
- **The auditor (Q5)** — claude-4's lift-on-`:move/id`, τ as an operator-ratified `:O-*` observable.
The convergence work is done; what remains is building to the agreed spec + Joe's STANDARD-VERIFY.
