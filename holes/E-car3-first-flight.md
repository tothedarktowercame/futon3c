# E-car3-first-flight — does the WM hit the Car-3 criteria? (a logged pilot flight)

**Date:** 2026-06-24
**Pilot:** claude-5 (inhabits the WM). **Ground Control:** claude-2 (on bell if stuck; judges the logs).
**Operator / consent:** Joe. **Relation:** Car-3 of `M-wm-policies` (CLOSED — Car-3 acting was HELD for arming);
flight model = `futon3c/holes/missions/M-first-flights.md`.

## Why

`M-wm-policies` built and shipped Track-3 (read-only): on a flat field the WM **acquires** a cascade-policy
(`:acquire-patterns`, real EIG) instead of freezing; the cascade carries an AIF act-gate leg
`F-free-energy = accuracy − λ·complexity`; the rollout gives the other leg `G(π)`. **Car-3 = closing the loop**:
the WM *acts* on a selected policy under the **consent gate** (`ΔF ∧ ΔG`, WM-I4) — the R16 "closed
action–perception loop" criterion. The documented run (`futon2/holes/M-wm-policies-documented-run-2026-06-24.md`)
shows everything *up to* acting; this flight tests whether the loop can actually close — or surfaces exactly where
it can't (which is the learning).

## How to fly (read `README-pilot.md` first — it is the map)

Inhabit `:war-machine-pilot`; run ONE cycle: **READ** (`wm-api-query` → `:judgement :ranked-actions`; also note
`:cascade-policies` and `:pattern-gaps`) → **EVAL** (choose `v`; mint a `cg-id`; at a stall consider the
acquired/cascade candidate; **check the act-gate `ΔF ∧ ΔG`** — the cascade's `F-free-energy` AND the move/rollout
`G`) → **PRINT** (execute the action citing the `cg-id`) → **LOOP** (`request-tick!` → `close-live-cycle!`; the
field must actually move) → **VERIFY** (`bb scripts/repl_spec_verify.clj <frame>`). Frames land in
`data/repl-traces/<run-id>.edn`; inhabitation log `futon5a/data/pilot-inhabitations.edn`.

## Car-3 acceptance criteria (what Ground Control will judge from the logs)

- **C3.1 — Acquire-on-stall.** The cycle engages a stall/flat or pattern-poor situation and an **acquired /
  cascade policy** (not a flat-cosine single pick) is the candidate `v`. *(Hit / partial / not-reached + where.)*
- **C3.2 — Act-gate `ΔF ∧ ΔG`.** EVAL's consent decision is informed by **both** legs — the cascade
  `F-free-energy` AND the rollout/`G` — not one alone; the `cg-id` records the warrant. *(Is the conjunction
  actually computable/computed at EVAL, or only one leg available?)*
- **C3.3 — Earned action + loop closes.** PRINT executes a real action citing the `cg-id`; LOOP shows the field
  **moves** (top-shift TRUE; addressed item rotates off `ranked-actions[0]`); `realised-discharge` +
  `prediction-error` recorded; **V2 honest-closure** (the gap is actually gone — cites a persisted registry sorry,
  no fake-finished).
- **C3.4 — Logged + verifiable.** The γ-frame conforms to the verifier (V1–V4) and the inhabitation log is updated.

A **partial / blocked** result is a valid, valuable outcome — if the apparatus can't act on an *acquired cascade*
(e.g. there's no "apply-cascade" action, or `ΔF ∧ ΔG` isn't wired at EVAL), **record exactly where it stops**:
that names the remaining Car-3 build.

## Disciplines (hard — `README-pilot.md` §"Hard disciplines")

Never restart the serving JVM (reload via Drawbridge); `request-tick!` never `tick!`; **substantive substrate
writes are consent-gated — held for Ground Control / operator (Joe), do NOT autonomously merge to master**
(gate-at-merge / WM-I4 — this is the whole point of Car-3); honest closure (V2); don't flood (E-cheesemonger).

## Ground Control verdict (claude-2, 2026-06-24) — Car-3 BLOCKED at 3 verified seams

claude-5 flew run `live-923940d9` (PILOTS-LOG **Turn 24**), supervised-proposal, held the act per WM-I4.
Ground Control verified each finding against ground truth (frame + served judgement + code), not the self-report:

- **C3.1 acquire-on-stall — NOT REACHED.** WM-judge `ranked-actions` types = `{advance-mission 99, address-sorry
  4, open-mission 2, learn-action-class 1, no-op 1}` — **no acquire/apply-cascade class.** The acquired/cascade
  policy exists only in the *read-only* `:cascade-policies` / `:pattern-gaps` lanes, never as an executable `v`.
  *(Architectural root, GC-confirmed: `:acquire-patterns` lives on the `futon3c.portfolio` AIF surface, which is
  **disjoint** from the futon2 WM-judge the pilot drives — the field was `:multiplied`, not flat, so the portfolio
  stall-path was not even in scope.)*
- **C3.2 act-gate ΔF∧ΔG — PARTIAL.** ΔF leg IS served (`:pattern-gaps` `F-free-energy`, e.g.
  `M-bayesian-structure-learning` F=−0.01). ΔG leg ABSENT — `:cascade-policies` carry `:wholeness`, not `G(π)`;
  the frame's `:prediction :policy` is literally `{:blocked-by :rollout-engine}`. And `consent-gate-emit`
  (`war_machine_pilot_backend.clj:215`) has **no slot for either leg** (GC-grepped).
- **C3.3 earned action + loop closes — NOT REACHED.** Correctly HELD for operator (WM-I4); no act, no movement
  (predicted=realised=−4.78).
- **C3.4 logged + verifiable — ✅ HIT.** Frame **CONFORMS** (verifier V1–V4 pass; Σ pred-error 1.6e-4; V4
  niche-construction-legible ok); honest typed-holes (`:sorry {:kind :not-yet}`) for the unreached organs.

**Verdict: Car-3 = BLOCKED — but the flight is a success as an experiment** (it converted a vague held-item into
a precise, verified build-list). **The remaining Car-3 build = R16 loop-closure wiring, 3 seams:**
1. **Join the surfaces** — make the acquired/cascade policy an executable `v` in the WM-judge `ranked-actions`
   (today `:acquire-patterns` is on the disjoint `futon3c.portfolio` loop; an `apply-cascade`/`acquire` action
   must enter the WM judge's differential).
2. **Serve `G(π)`** — join `futon2.aif.rollout`'s rollout into the served judgement next to the cascade `F`
   (today only wholeness/F is served; G(π) was a hand-built demo).
3. **ΔF∧ΔG slot in the consent gate** — `consent-gate-emit` records the conjunction (both legs + the warrant),
   so the act-gate is auditable at EVAL.

*(Side-finding, not Car-3: `data/wm/needs-you.edn` carries a live "WM needs Joe" signal — the rank-1
`:learn-action-class` (action class "unknown") wants operator framing (a bootstrap/input-source decision).
Unblock = "Confirm." This is the WM's own R6 gap-surfacing, separate from Car-3.)*

## The 3 seams — BUILT + live-verified (claude-2, 2026-06-24)

All three Car-3 seams are wired and confirmed end-to-end on the live JVM (Codex paused → built directly;
clj-kondo 0/0 + check-parens OK on all three files; reloaded via Drawbridge, no JVM restart):

- **Seam 2 — `G(π)` joined (ΔG beside ΔF).** `futon2.report.cascade-lane/rollout-g-for` computes the best-rollout
  `G(π)` from a mission's root in the v2 move-set (`futon2.aif.rollout`, read-only/sim-only, zero `:7071`
  writes), memoized; `cascade-lane`/`gap-lane` now emit `:G-rollout` next to `:F-free-energy`. Verified:
  hypergraph −0.0043, substrate-metric −0.0015, canon `nil` (no move-set path → ΔG genuinely unavailable).
- **Seam 1 — `:apply-cascade` in the differential.** `generate-war-machine` synthesizes an `:apply-cascade`
  action per cascade-policy, **appended** to the served `ranked-actions` (so the WM's own auto-selection
  `wm-decision`/`wm-admissible` is unaffected), each carrying `:act-gate {:delta-F :delta-G :pass?}`, the cascade
  `:shown`/`:wholeness`, and `:held-for-arming? true`. **Live-verified:** the served judgement now lists 2
  `:apply-cascade` actions (`M-canon-fingerprint-store`, `M-bayesian-structure-learning`), both `:pass? false`
  (ΔG `nil` — no rollout path → gate abstains, correctly); a path-having mission (hypergraph-operator) computes
  `:act-gate {:delta-F 1.676 :delta-G -0.0043 :pass? true}` = actionable.
- **Seam 3 — ΔF∧ΔG slot in the consent gate.** `consent-gate-emit` accepts `:act-gate`, computes a
  `:gate-verdict` (`:pass` = F>0 ∧ G<0; `:fail`; `:abstain-missing-leg` = a leg nil), and records both in the cg
  warrant + bell. Verdict logic verified (pass / fail / abstain-missing-leg).

**Status of Car-3 after the seams:** **C3.1 (acquire reachable as `v`) and C3.2 (ΔF∧ΔG computed + recordable at
EVAL) are now SATISFIED** — a re-flight will find `:apply-cascade` in the differential with both legs and can mint
a consent gate recording the conjunction. **C3.3 (earned action + loop closes) remains HELD** — there is
deliberately *no executor* for `:apply-cascade` (`:held-for-arming? true`); actually applying a cascade =
Part B = the explicit operator-arm step (WM-I4). So the seams make Car-3 *reachable up to the gate*, not armed.
Honest edge: actionability is move-set-coverage-bound (the two currently-served missions lack a rollout path →
abstain), which ties to the R14/R15 frontier (serve a richer reachable set).

## Flight #2 — Ground Control verdict: C3.1 + C3.2 now HIT (claude-2, 2026-06-24)

claude-5 re-flew after the seams landed (run `live-e9809257`). GC-verified from the begin-frame + the minted
consent gate (artifacts, not self-report):
- **C3.1 acquire-on-stall — HIT (was not-reached).** `:apply-cascade` actions are now in the READ differential
  (`dT-snapshot`): `M-canon-fingerprint-store` `{:delta-F 0.005 :delta-G nil :pass? false}`,
  `M-bayesian-structure-learning` `{:delta-F −0.01 :delta-G nil :pass? false}` — selectable as `v`.
- **C3.2 act-gate ΔF∧ΔG — HIT (was partial).** The pilot minted `cg-ba4c4702-…` and `consent-gate-emit`
  recorded `:act-gate {:delta-F :delta-G}` + **`:gate-verdict :abstain-missing-leg`** — the conjunction is
  computed AND recorded at EVAL. Abstain because ΔG (rollout `G(π)`) is `nil` for both served missions (no
  rollout path in the v2 move-set) — the gate correctly refuses when a leg is missing.
- **C3.3 earned action + loop closes — still NOT-REACHED (by design).** Execution held (no `:apply-cascade`
  executor — Part B); and the gate abstained, so even armed it would not act on these missions.
- **C3.4 — begin-frame written; held at the gate (no close-cycle act, correctly).**

**Net: the 3 seams are confirmed live — Car-3 is now *reachable to the gate*, and the gate is *auditable*
(records ΔF∧ΔG + verdict).** Two things remain for a *closed* loop (C3.3): **(a)** a `:pass` case to exercise —
needs a path-having mission in the open-mission set (the **R14/R15 move-set-coverage** frontier; today's served
missions all abstain on missing ΔG); **(b)** the `:apply-cascade` **executor + operator arming** (Part B / WM-I4).

## Flight #2 follow-on — the "4th seam": pilot auto-loop wiring (claude-5 found, GC-recorded)

The 3 seams are live at the **judgement + gate-tool** level, but **not yet wired into the pilot auto-loop**
(`war_machine_pilot/begin-live-cycle!` / `close-live-cycle!`) — claude-5 had to drive apply-cascade via the
README-sanctioned witness pattern (hand-composed begin-state citing the real `cg-id`). Three bounded gaps:
- **(a) target-only selection collision.** `begin-live-cycle!`/`close-live-cycle!` match the chosen `v` by
  `:target` ONLY → an `apply-cascade` (rank ~111) collides with a same-`:target` `advance-mission` (rank ~96)
  picked first; close-cycle would also mis-measure realised against the advance-mission. **Fix:** disambiguate by
  `(:type, :target)`, not `:target` alone.
- **(b) act-gate not threaded.** `begin-live-cycle!` does not pass the chosen action's `:act-gate` into
  `consent-gate-emit`, so seam-3's verdict only fires when the act-gate is supplied explicitly (claude-5 called
  the gate tool directly). **Fix:** `begin-live-cycle!` threads `(get-in chosen [:action :act-gate])` into the gate.
- **(c) V2 attribution mismatch.** `begin-live-cycle!` sets `:v-attribution :operator-directed` when a `:target`
  is passed, but `repl_spec_verify` V2 accepts only `#{:operator :pilot-autonomous}` → any target-directed
  flight's frame FAILS V2 (claude-5 used `:operator` to conform). **Fix:** V2 accepts `:operator-directed`, or
  `begin-live-cycle!` emits `:operator`.

These make Car-3 **auto-flyable** (no hand-workaround) — the completion of "reachable via the pilot loop." Small,
in-apparatus, no arming. Distinct from Part B (the executor + operator arm), which stays held.

**4th seam — BUILT + verified (claude-2, 2026-06-24).** (a) `begin-live-cycle!`/`close-live-cycle!` now take an
`:action-type` and match the chosen/post `v` by **(:type, :target)** — verified: selecting
`M-bayesian-structure-learning` with `:action-type :apply-cascade` returns `selected-type :apply-cascade` (not
the colliding advance-mission). (b) `begin-live-cycle!` threads `(:act-gate v)` into `consent-gate-emit` — the
chosen apply-cascade `v` carries `:act-gate` (`has-act-gate true`), so the gate records ΔF∧ΔG + verdict on a real
auto-flight. (c) `repl_spec_verify` V2 now accepts `:operator-directed` — the flight-#2 frame still CONFORMS.
clj-kondo 0 errors (2 pre-existing warnings), check-parens OK, reloaded live. **Car-3 is now auto-flyable end to
end up to the gate; remaining = a `:pass` case (move-set coverage, R14/R15) + Part B (executor + arming).**

## Bell back to claude-2 (Ground Control)

When done (or stuck): bell `claude-2` with a summary + the **run-id and frame path(s)** + the
`pilot-inhabitations.edn` entry + a **self-assessment against C3.1–C3.4** (hit / partial / not-reached, and where).
If stuck mid-flight, bell `claude-2` — Ground Control will unstick or escalate to Joe.
