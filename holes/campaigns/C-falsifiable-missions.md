# Campaign: C-falsifiable-missions

*Ground the closed FutonZero loop in external reality — make "it worked" something the
world says (the peradam), and make every architectural fork a recorded falsifiable
decision (the observables). No single mission owns the grounded yardstick or the
observable ledger; that is why this is a Campaign.*

**Status:** CHARTERED (RALLY exited 2026-06-10, operator-ratified) — coordination shape: **convergent** (keystone-first).
**Coordinator:** claude-3. **Principal:** Joe.
**Home:** futon3c/holes/campaigns/ (coordination layer). Constituent of the same family as
`C-pudding-prover` (which owns the peradam *recognition*; this campaign owns its *grounding
into the learning loop*).
**Provenance:** the two Fable (Opus2x) reviews — of `M-differentiable-substrate` (the
closed-loop synthesis) and `futon2/docs/futonzero-alphazero.md` §1/§5 — plus the 2026-06-10
Fable planning session that chartered the three constituent missions and added the observable
ledger. `M-futonzero-grounding` (claude-3, the earlier umbrella sketch) folds in here as the
diagnosis; it is **retired as a standalone mission** (this campaign is its capstone form,
resolving `:O-capstone-form` toward *campaign*, pending operator ratification).

---

## 0. RALLY  *(proposed new phase-0, before CHARTER — muster + assign + coordinate)*

> **RALLY is the assembly phase** — gather the agents, sketch the lanes, establish the
> coordination protocol, *before* the formal CHARTER locks the joint goal + standard. It is to
> CHARTER what HEAD is to a Mission's IDENTIFY: the form-up. (Joe, 2026-06-10 — proposed as a
> campaign-lifecycle addition; see §6. "This initial coordination plan is early, like a new RALLY
> phase.") **RALLY exits when** the lanes are assigned, the coordination protocol is named, and
> the operator has ratified the assignments — *then* CHARTER formalises the standard.

### The mesh (lanes)

| Lane | Owner(s) | Mission | Build | Status |
|---|---|---|---|---|
| **Grounding** (keystone, critical path) | **claude-4 + claude-3 (co-led)** | `M-peradam-grounding` (futon7; the car) | Codex: `promote!`→certificate emission, MUST-B-aware | RALLY → IDENTIFY |
| **Dialectic** | **claude-1** | `M-arguing-worlds` (futon2) | Codex: buildout-generator + referee harness | RALLY → IDENTIFY |
| **Learning** | **claude-1 (lean)** / claude-3 (cross-check) | `M-pattern-posteriors` (futon3a) | Codex: PUR-extractor + Beta posterior + A/B term | RALLY → IDENTIFY *(owner-confirm pending)* |

- **Co-led car** (Joe-ratified): claude-4 owns the discharge site (`promote!`/MUST-B); claude-3
  owns the CH2 consumption + the role-fork's training arm; the pudding-prover certify/anti-laundering
  logic is reused (futon7), not rebuilt.
- **claude-3** also: coordinator/reviewer; keeps `M-differentiable-substrate`'s producer ready to
  consume CH2; holds the **don't-double-count** cross-check (R2 move-grain learning vs
  pattern-posteriors pattern-grain learning must not double-count).
- **Codex** builds under each PM (scope-bounded-handoff: declared `:in`/`:out`; author ≠ reviewer).
- **In parallel** (Joe, 2026-06-10): another agent runs `M-agency-hardening` to fix the
  dropped/crossed bells *as we work* — so the coordination substrate improves under the collab.

### Coordination protocol (the patterns this mesh runs on)

`futon3/library/social/`: scope-bounded-handoff, explicit-exit-over-abandonment,
verify-before-compose, tension-before-code — **plus two new** grounded in this session's overhead
(crossed bells, silent semantic drift): `social/idempotent-handoff` and
`social/derive-the-contract`. The drift-proof handshake (`derive-the-contract`) is how the
scope-grain seam stayed zero-drift across three agents; idempotent handoffs are how we survive
crossed bells until agency-hardening lands.

### Operator checkpoints (Joe checks here — pause-and-ask, not autonomous)

1. **Assignments ratified?** Co-led car ✓ (done). `M-pattern-posteriors` owner — claude-1 (lean) or
   claude-3? **Open.**
2. **The role-fork** (`:O-peradam-role`) — peradam as *training target* (CH2 gradient) vs *exogenous
   calibration anchor* (the Lee-Sedol reading). This is the **first joint DERIVE** the mesh resolves;
   it shapes pattern-posteriors' credit-assignment and arguing-worlds' yardstick. Operator-visible.
3. **RALLY → CHARTER** — does this campaign formalise (CHARTER the standard), and is RALLY adopted
   into `campaign-lifecycle.md` as phase-0?
4. **`M-war-machine` disposition** (parked) — folded under `:O-capstone-form`.

---

## 1. CHARTER  *(draft — formalises at RALLY exit)*

- **Joint goal / gap.** The FutonZero loop is *closed and self-referential* (both Fable reviews):
  the spec is authored, the value self-graded, the reward never external. Make it touch reality.
  **No single mission can own this:** the grounded yardstick (the peradam) is delivered by one
  mission but *consumed* by two others who each need it fit for their own use — the cross-mission
  adequacy check no constituent can perform alone.
- **The shared standard.** (a) **The grounded yardstick** — a realized peradam emitted at live
  discharge (`promote!` → `:constructed`), keyed by `:move/id`, typed distinctly from a
  search-internal `:q` (anti-laundering). (b) **The observable-falsification discipline** — every
  architectural fork discharges as a recorded *choice-on-evidence* (the 8 observables in
  `pudding-prover-registry.edn`), never a predetermined answer.
- **Joint completion criterion** (cross-mission, not "all members done"): the peradam-yardstick's
  contract is **verified fit for both consumers** (STANDARD-VERIFY) **AND** ≥1 paired requirement
  is released-and-consumed — e.g. one grounded peradam moves one pattern-posterior, OR arguing-worlds
  runs its tournament on the grounded (non-circular) yardstick.
- **Membership.** Keystone = `M-peradam-grounding` (delivers the yardstick). Paired/dependent =
  `M-pattern-posteriors`, `M-arguing-worlds` (consumers; grounded paths held in escrow). Sibling:
  `M-differentiable-substrate` (the producer/prior that R2 trains), `M-pudding-peradams` (recognition).

## 2. ESCROW ledger (the held grounded-paths)

```
{:from M-pattern-posteriors :on the-grounded-yardstick
 :requirement "peradam-attributed outcome moves a per-pattern posterior (the grounded update path)" :status :held}
{:from M-arguing-worlds :on the-grounded-yardstick
 :requirement "a non-circular tournament yardstick OUTSIDE the wholeness-C loop" :status :held}
```
Two-step release: **STANDARD-VERIFY** (the role-fork + the certificate contract verified
fit-for-both) → `:contract-released` (paired missions build their grounded paths to the verified
spec); **RUN/DELIVER** (the car emits real peradams) → `:satisfied`. Both paired missions' **v0**
(self-graded; the label travels) is *not* escrowed — it ships now, in parallel; only the **grounded**
completion is held.

## 3. The observable ledger (the shared assurance standard)

8 observables in `futon7/holes/pudding-prover-registry.edn` `:observables` (Fable, 2026-06-10):
`:O-rollout-kill-criterion`, `:O-cross-mission-unlocking`, `:O-prior-consumption-witness`,
`:O-cascade-budget`, `:O-semilattice-rollout-reconciliation`, `:O-scheduled-cadence`,
`:O-peradam-role`, `:O-capstone-form`. **The stance:** an observable discharges when the measurement
happened *and a decision was recorded — whichever way it goes*. The certificate certifies that a
choice was made on stated evidence, **never which choice ought to have been made.** This is the
campaign's governance backbone: each constituent mission's open fork registers here.

## 4. Log

- **2026-06-10 — RALLY opened (claude-3, Joe-directed).** Three constituent missions chartered by
  Fable; co-led car ratified; coordination protocol = social patterns + 2 new (idempotent-handoff,
  derive-the-contract); `M-futonzero-grounding` folded in + retired as standalone. Pending operator
  checkpoints (§0): pattern-posteriors owner, the role-fork, RALLY→CHARTER, capstone form.
- **2026-06-10 — RALLY exited → CHARTERED (operator decisions).** (1) `M-pattern-posteriors` owner =
  **claude-1** (leads both cascade-side lanes: dialectic + learning). (2) The role-fork
  (`:O-peradam-role`) is **operator-visible and held for a Joe discussion** — the car designs BOTH
  arms up to the fork and surfaces it; does NOT decide unilaterally. (3) CHARTER formalised + RALLY
  adopted into `campaign-lifecycle.md` as phase-0. Collab dispatched to claude-1 (arguing-worlds +
  pattern-posteriors) and claude-4+claude-3 (the car). Transport-failure escalation: dropped bells →
  **claude-6** (on `M-agency-hardening`).
- **2026-06-10 — cascade lanes rolling; observables registered + don't-double-count ratified.**
  claude-1 dispatched both v0 Codex builds (codex-1 → M-arguing-worlds: generator + diversity-check-
  first + referee on the realized-G(π) floor; codex-3 → M-pattern-posteriors: PUR-extractor + drop-log
  + Beta posterior + A/B term, self-graded label travels). **Observables registered ACTIVE** (homed to
  claude-1's lanes; discharge = fill `:decision`/`:evidence` in `pudding-prover-registry.edn` when the
  missions resolve them): `:O-cascade-budget`, `:O-rollout-kill-criterion`,
  `:O-semilattice-rollout-reconciliation`. **Don't-double-count RATIFIED** (claude-1's
  evidence-source boundary): pattern-grain credits a PATTERN from a PUR-outcome; move-grain (R2)
  credits a MOVE from a rollout realized-G(π) — distinct sources ⇒ no double-count in v0.
  **Sharpening (claude-3):** the real collision is the GROUNDED peradam (one discharge could credit
  both the move *and* the proposing cascade's member patterns). That's the **car's credit-assignment
  rule** (M-peradam-grounding, co-designed claude-1 ↔ claude-4 ↔ claude-3). It must **CONSERVE**: the
  total credit assigned across move + patterns ≤ the one certified peradam unit — no inflation
  (anti-laundering for credit). Witness (baked into both builds): (v0) same work → exactly one grain
  per source; (grounded) the credit-assignment rule splits ≤1 unit, never double-credits.
- **2026-06-10 — M-pattern-posteriors v0 PASS (claude-1 review, real gate; codex-3, futon3a
  `codex/m-pattern-posteriors-v0` `6402ceb`).** Both anti-glibness gates hold: posterior MULTIPLIES
  m(p) centered at 1.0 (byte-inert at weight=0 / no-evidence) — composes-not-replaces; credit-seam
  encodes v0 pattern-grain-only + the grounded ≤1.0-conservation witness baked in. 5 tests,
  deterministic rebuild (accepted 21 / dropped 28 w/ reasons / 12-of-1071 patterns with evidence),
  self-graded label everywhere. **HONEST FINDING (the legitimate sparse-data exit):** the machinery
  is SOUND but the PUR corpus is too sparse — WM cascade surfaces mostly hit n=0 → posterior inert →
  NULL; it moves only on PUR-overlapping queries. **This CONFIRMS the campaign thesis by measurement:**
  the learning layer is genuinely *inert without grounding* — it needs the car's grounded peradams
  (+ more PURs) to move anything. The escrow/grounding is **load-bearing, not bureaucratic.** Branch
  byte-inert + mergeable (land-call: Joe). Escrow entry stays `:held` (grounded posterior path); the
  v0 self-graded machinery is done.
- **2026-06-10 — M-arguing-worlds v0 PASS + the campaign's FIRST CLEAN FALSIFIABLE NEGATIVE**
  (claude-1 review, real gate; codex-1, futon2 worktree `codex/arguing-worlds-v0` `2eca617`). Both
  anti-glibness gates verified IN CODE: yardstick = realized rollout G(π), NOT C (no circular grade);
  diversity-precheck returns `:monotone-generator` *before* judging (the root-seeded max-overlap-1.0
  case fired it). 5 tests/17 assertions, clj-kondo 0/0, worktree-isolated (no main disruption).
  **THE NEGATIVE:** on a GENUINELY-diverse generator (4 lenses, max-overlap 0.11, disagree=true) the
  dialectic does NOT beat single-best → `:single-best-holds` (winner `:fast-terminal`). NOT circular,
  NOT a monotone-generator artifact (the diverse case *was* diverse). So **on the realized-G(π)
  floor, arguing-across-possible-worlds (Joe's E3b leading hypothesis) is CEREMONY** — recorded
  honestly (a recorded negative = a successful completion, per the charter). **CAVEAT (not
  permanent):** the grounded peradam yardstick (escrowed on the car) could FLIP it; the
  peradam-yardstick seam is correctly escrowed; revisit grounded. Completion-criterion-4:
  `futonzero-alphazero.md` §1 updates "open, early" → "negative on the realized-G floor, revisit
  grounded". **THE INTERLOCK (independent confirmation from TWO lanes):** both cascade v0s point at
  `M-peradam-grounding` (the car) as the unlock — pattern-posteriors is inert without grounded
  peradams; arguing-worlds' negative is revisit-able under the grounded yardstick. The campaign spine
  confirmed from two directions. Both v0s reviewed-PASS, both honest, both mergeable (land-call: Joe).
