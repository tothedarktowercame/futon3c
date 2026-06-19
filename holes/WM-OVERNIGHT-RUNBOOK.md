# WM Guardrailed Overnight Run — pilot runbook (claude-1, 2026-06-06)

The `/loop`-driven pilot protocol for a guardrailed overnight outing. Composes the
first-outing harness (`M-war-machine-first-outing.md` §8.7–8.9) with guardrails mode
(`WM-GUARDRAILS-SPEC.md`) + the new pieces (Units 3/4). This is the inductive-step
instrument for pudding-prover **T4.2** (`:cadence :base-case` → `:capability`).

Interfaces this depends on (finalize signatures when the units land):
- `begin-live-cycle! {:guardrails? true :guardrails-ctx {...} :mode :substantive}` (Unit 1, done)
- `needs-you/emit-needs-you!` → `data/wm/needs-you.edn` (Unit 1, done; seam live on restart)
- mission-status-fn w/ `:open-hole-count` so `:open-mission` is advanceable (Unit 3, codex-2)
- `outing/run-cycle-gates!` → `{:verdict :pass|:quarantine|:hard-halt …}` (Unit 4, codex-4)

## Pre-flight (operator-gated)
1. **futon3c restart** (Joe's call) — makes claude-7's bulletin seam, the `mission_registry`
   leading-token fix (`2cd7445`), and the needs-you emitter all live together.
2. **Staging branch** `wm-outing/<date>` — pilot commits here only; Joe merges in the morning
   (the consent gate). Use a git worktree (§8.2 INSTANTIATE finding) to avoid the
   shared-working-tree hazard; multi-repo (futon2/futon3c/futon5a) edits stay on the branch.
3. **Bounds** — ~10–15 substantive cycles OR ~60–90 min wall-clock, whichever first (§8.6).
4. **Pre-launch expectations** — a short scored prediction (§8.15): expected disposition
   (self-improvement vs outward), first-few-cycles call.

## Per-cycle protocol (the `/loop` body)
1. **READ+EVAL** — `begin-live-cycle! {:guardrails? true :mode :substantive
   :guardrails-ctx {:mission-status-fn <Unit-3>} :tick? true}`.
   - `:reason :no-autonomous-action` ⇒ the queue is all operator-dependent; NAGs were emitted
     → **soft-stop** (surfaced the queue, did not wedge). Not a failure.
   - else `top` = the engaged autonomous action; `stepped-past` operator-dependent actions were
     emitted as NAGs to the operator view automatically.
3. **PRINT (earned)** — do the real edit citing the `cg-id`. For an `:open-mission` advancement:
   advance **one** open hole of the target mission (a genuine, bounded build). **Pilot judgment
   applies frontier priority here** — among advanceable missions, prefer the dependency spine
   (substrate-metric → aif2 INSTANTIATE), input-sources, WM refinements — *not* raw EFE order
   (operator's priorities are signals, [[feedback_operator_not_sovereign]]). The discharge must
   be EARNED (V2 no-teleport).
4. **LOOP** — `request-tick!` (never blocking `tick!`), wait for the tick, `close-live-cycle!`
   → frame + `:top-shift?`.
5. **GATES** — `outing/run-cycle-gates!` (Unit 4):
   - `:pass` ⇒ commit this cycle to `wm-outing/<date>` (one commit; msg cites cg + target + frame run-id).
   - `:quarantine` ⇒ record loudly, **do NOT commit**, **continue on other work** (containable friction).
   - `:hard-halt` ⇒ G3 fake-finish (claimed discharge, field didn't move) — **end the run**.
6. **Soft-stop check** — bounds reached, stuck-detector (K cycles no progress; a decompose counts
   as progress), regression-storm (≥3), or operator sentinel ⇒ clean end.

## Guardrails (the hard NOs — enforced by Unit 1 `autonomous-admissible?`, never relaxed here)
- Never master directly (staging only; Joe merges). Never restart the serving JVM (Drawbridge
  reload of own edits; never `:reload` a third-party/protocol ns). No outward/irreversible acts
  (sends/EOI/invoice → NAG). No ∇-deform / protocol-defining / niche-construction (→ NAG). No
  fake-finish (→ hard-halt). Stay out of `<repo>/.state` + the futon7-private boundary.
  Don't-flood (one hole/cycle; E-cheesemonger).

## Morning review (§8.9)
- `wm-outing/<date>` branch `git log` — the discharge ledger (one commit/earned cycle).
- `data/repl-traces/live-*.edn` frames — each G1-conformant.
- `data/wm/needs-you.edn` — the NAGs surfaced to the operator view (the "what needs you" list).
- run-summary `data/outings/<date>.edn` — cycles attempted/committed, halt reason, Σ pred-error,
  top-shifts, discharges (cg + frame refs), quarantines, wall-clock — read against the
  expectations doc.
- Manual VSATARCS regen if pheno/geno moved (§8.12).
