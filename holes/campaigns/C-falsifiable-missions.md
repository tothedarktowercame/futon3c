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
resolving `:O-capstone-form` → *campaign*, **operator-ratified 2026-06-10**).

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
- **2026-06-10 — both cascade lanes CLOSED-for-v0; futonzero-alphazero §1 measured.** M-arguing-worlds
  v0 (`3b40370`) + M-pattern-posteriors v0 (`ff04342`) both reviewed-PASS + recorded; `futonzero-
  alphazero.md` §1 updated "open, early" → MEASURED (`6e0b624`: dialectic = ceremony on the realized-G
  floor, guarded against both failure modes, revisit-able under the grounded yardstick). Both grounded
  paths stay `:held` (escrowed on the car); claude-1 standing by for the grounded-yardstick bell.
  **Campaign convergence:** every grounded path now waits on `M-peradam-grounding` (the car) — which
  waits on Joe's DIRECT confirm to claude-4. The campaign's entire forward motion is gated on that one
  operator-go.
- **2026-06-10 — `:O-peradam-role` DISCHARGED → CALIBRATION ANCHOR (operator decision, Joe).**
  The peradam is a **calibration** (Lee-Sedol-to-AlphaZero), NOT a training target / currency.
  **Evidence (Joe's reasoning):** (a) enough drive-signal already exists — patterns (normative) +
  missions (optative); the loop doesn't need the peradam as a *third* drive. (b) The peradam's value
  is the proof-is-in-the-pudding *un-game-ability*; used as an audit it can't be gamed (the
  anti-laundering core). (c) Daumal's peradam-as-currency (spend-to-climb = Arm A/training) is set
  aside in favour of the Buddhist pāramī reading (a perfection, always-perfecting, never spent =
  the standing audit). **Why it's the deeper resolution:** the three-witness scarcity that's fatal
  for *training* (cf. pattern-posteriors' inert null) is ideal for *calibration*; and it answers
  Fable's closed-loop critique as the un-game-able falsifier the loop can't optimize away, with
  drive from real signals (patterns/missions). **Loop-closure (wrinkle, op-confirm pending):**
  calibration does not auto-correct — the audit feeds back THROUGH the operator + pattern-library
  (human-in-the-loop), not an automatic gradient. **Shapes:** CH2 = audit-not-train
  (M-differentiable-substrate §8.7); pattern-posteriors learns from PURs, *calibrated* by peradams;
  arguing-worlds' grounded yardstick = the audit. Hybrid (sparse trained-arm gated by the audit) =
  reversible future option iff the patterns/missions drive proves too weak. Registry
  `:decision`/`:evidence` for `:O-peradam-role` to be filled by the car (futon7).
- **2026-06-10 — calibration resolution recorded in both mission docs (claude-1: futon3a `82de774`,
  futon2 `76ccc72`); pattern-posteriors null SHARPENED to TWO AXES.** Under calibrates-not-trains,
  the v0 null disaggregates along exactly Joe's training-vs-calibration distinction: (1)
  **coverage/training** — only 12/1071 patterns have any PUR, so most of the library is *untrained*
  (remedy: more PURs); (2) **calibration/audit** — even the trained 12 are self-graded until the
  car's peradams audit them (remedy: grounded peradams). So "needs more evidence" resolves into
  "low-coverage AND un-calibrated" — two distinct axes, two distinct remedies. A cleaner finding
  than the original null, and a direct product of the calibration choice. ≤1.0-conservation applies
  on the calibration path; escrow stays `:held`.
- **2026-06-10 — GOAL-DEFINITION RESOLVED + Campaign RATIFIED + v0 merges greenlit (operator
  decisions, Joe).** Three decisions cleared in one turn:
  - **(1) `:O-goal-definition` DISCHARGED → SPARSE + EXPLICIT + DOKUSAN.** How a substrate-2
    discharge earns a peradam: **sparse** (a discharge certifies a curated Mount-Analogue thesis
    *only if* it satisfies that thesis's `:pudding-requires` — peradams land only at real
    altitude-achievements, not routine code-work); **explicit** (the thesis names its own
    discharge-condition in plain terms — NO semantic matcher, which would reopen the Goodhart
    surface the campaign exists to close); **DOKUSAN** (the recognition is *interactive*: the
    system can only **propose a claim** — "this discharge may have reached thesis T"; the operator
    **certifies it in an interactive examination**, the live sibling of the three-witness
    certificate — you can't fake insight to the examiner, which is where the human-grain
    un-game-ability comes from). **Shareable:** a qualified peer can run the dokusan → distributed
    recognition (the peer-learning core). This makes "the loop closes through the operator" concrete
    — the peradam is minted by interactive certification, not an automatic gradient. **Shapes the
    car (futon7 cert-half, claude-3):** the half is a *dokusan interface* — surface candidate claims
    (discharge event matches a thesis `:pudding-requires`) → operator interactively certifies
    (provides the witness `certify` checks) → mint peradam bound to `:move/id`. Reuses existing
    `certify`/3-witness; dokusan is the interactive witness-provision step.
  - **(2) `:O-capstone-form` DISCHARGED → CAMPAIGN (ratified as specified).** `C-falsifiable-missions`
    stands as a Campaign; `M-futonzero-grounding` retirement confirmed; `M-war-machine` disposition
    rides under this objective (parked).
  - **(3) v0 merges GREENLIT.** M-arguing-worlds v0 + M-pattern-posteriors v0 cleared to land on
    their main branches (delegated to claude-1, the lane PM).
- **2026-06-10 — both cascade v0s LANDED on main (claude-1); escrow discipline caught the
  discharge path.** Merge shas: **futon2 main @ `83e9a30`** (NON-TRIVIAL: v0 was split — docs on
  `wm-rollout-v2-root-seed` (76ccc72), code on `codex/arguing-worlds-v0` (2eca617); landed ff→docs
  then `--no-ff` ort merge of the code, conflict-free new-files-only; `arguing_worlds.clj` on main
  byte-identical to the tested 2eca617; done in a throwaway worktree so claude-4's dirty checkout
  was untouched). **futon3a main @ `82de774`** (fast-forward to exactly the reviewed v0:
  6402ceb/ff04342/82de774). Consistent with CHARTER §2: the **v0s are NOT escrowed** (self-graded,
  the label travels) — they ship now; only the **grounded** completion stays `:held` on the car.
  **THE CATCH (escrow machinery working):** `ac4ae5d` "Emit CH2 discharge events on meme
  construction" (the discharge-EMISSION = claude-4's grounded half) sat ONE commit above the futon3a
  v0 tip and nearly rode into main on a plain ff; claude-1 held it **branch-only** per "only v0s
  land / don't touch escrowed grounded-paths." **Ratified hold:** `ac4ae5d` stays branch-only
  (escrowed); it lands only when the car ARMS — Joe's DIRECT confirm + the cert-half ready +
  STANDARD-VERIFY. Also held branch-only: `e8dc3e8` (unrelated flexiarg parser fix; out of campaign
  scope; routes separately under its own review). **Both mains updated LOCALLY, NOT pushed**
  (shared-remote caution; push held for Joe).
- **2026-06-10 — car DISCHARGE-HALF complete + real-gate reviewed (claude-4).** `ac4ae5d` (codex-2)
  + review-fix `e45c1a6` (claude-4), branch `codex/m-pattern-posteriors-v0`, held branch-only
  (escrowed). **Review was a real gate:** read the diff (meme.ch2 + promote! hook + ch2_test);
  MUST-B verified STRUCTURALLY by test (sim `meme.step/step` constructs an arrow but emits nothing —
  sink empty; only the live `promote!` path emits); assurance core verified (`discharge-event?`
  rejects bare number / `{:q}` / missing tag / bad `:sorry-ref` / anything carrying `:peradam`/`:q`;
  `emit-discharge-event!` THROWS on a laundering attempt — rejection-witness passes BY throwing;
  semantic-regression rejects near-misses by meaning not shape); idempotent (emits once on real
  `:open->:constructed`, re-promote noop does NOT emit — no double-count); full futon3a suite
  35 tests/129 assertions/0 fail, clj-kondo 0/0, parens clean. **claude-4 found + fixed one issue
  himself** (`e45c1a6`): codex-2's emission wasn't isolated — if `arrow-sorry-ref!` threw it would
  break an already-committed promotion AND lose the event (retry hits the idempotent noop); now
  wrapped to surface-loud-to-`*err*` + return nil, promotion unaffected, strict validation untouched.
  **The seam (my consumption interface):** sink `futon3a/data/ch2-discharge-events.edn`
  (append-only, one EDN/line); `(meme.ch2/read-events sink)` → events `{:ch2/discharge-event true
  :move/id "<have>-><want>" :discharged? true :at "<iso-ts>" :sorry-ref "<label>/sorry/meme-arrow-<slug>"}`;
  join keys `(:move/id, :sorry-ref, :at)`; no `:peradam` from his side. **Car status:** discharge-half
  DONE; the car now waits only on (a) my futon7 cert-half (sparse `:sorry-ref`→thesis map + dokusan +
  mint) and (b) Joe's DIRECT confirm to ARM. Note: claude-4's completion bell DROPPED (interleaving);
  caught by roster-polling — another live datapoint for M-agency-hardening (claude-6).
- **2026-06-10 — cert-half spec APPROVED (Joe) + Invariant-Queue concern → RUN/DELIVER bar.** Joe
  approved the dokusan cert-half spec (futon7 M-peradam-grounding Checkpoint 1, `c190c6e`) and
  flagged a concern (not a blocker): the **Pudding Prover is structurally akin to the
  operational-invariant inventory** (`M-invariant-queue-unstuck`), which was built well but **ended
  up underused — a fatberg / "list of claims about the past," not a dashboard of present binding**.
  Recorded as a carried concern + a sharpened **RUN/DELIVER bar** (futon7 `5a83f40`): RUN/DELIVER
  must prove the Prover ITSELF functional+meaningful, not just that the cert-half wiring fires —
  (1) fires on a real (non-seeded) discharge; (2) DISCRIMINATES — Ashby accept-out-of-blanket (T2.3
  cold-EOI-sent) AND reject near-miss/laundering, on real cases not fixtures (accept-only or
  reject-only = the fatberg signature); (3) peradam CONSUMED not write-only (the campaign's joint
  completion criterion, sharpened — consumption distinguishes a live yardstick from a fatberg);
  (4) CANARIED (the after-each-WM-run cycle as the present-binding signal). Honesty label until
  cleared: "a built+wired registry," not "a functional, meaningful prover." This raises the bar on
  the campaign's STANDARD-VERIFY/RUN reason-to-exist. **Still open for the v1 build:** the v1 thesis
  pick + the curated-map home; **and** the v0-mains push (claude-1 holding local).
