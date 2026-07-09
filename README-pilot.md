# README — Inhabiting the War Machine Pilot

You are about to inhabit the **`:war-machine-pilot`** peripheral: drive the War
Machine (WM) as a **REPL**, where the four turns Read / Eval / Print / Loop are
a differential operator over the active-inference manifold. This file is the
map — it points at the canonical sources; read them, don't trust this summary
over them.

## Read first

- **`holes/specs/repl.spec.edn`** — THE spec. The REPL is `v·∇` over the WM
  field: `READ` discloses `dT` (ranked-actions = the differential), `EVAL`
  contracts it with a chosen direction `v` (mint a consent-gate), `PRINT`
  deforms the connection `∇` (emit a typed artefact), `LOOP` integrates the
  flow (tick → re-read). Invariants **V1–V6**, **§8 fork-resolution**, and the
  **LOOP `:autonomy`** clause are the contract you operate under.
- **`futon3c/holes/missions/M-war-machine-pilot.md`** — the parent mission; the
  inhabitation ingredients (AIF workup, sorry registry, peripherals, conductor,
  bells, Playwright).
- **`futon3c/holes/missions/M-pilot-appearance.md`** — what the pilot's
  appearance IS (the contract / four-card cycle); §8.4.2 is the plain-language
  thesis you are enacting (AI-as-reskilling, not deskilling).
- **`futon3c/README-missions.md`** — the Mission Doc **format**: how a section
  header becomes an eightfold-phase frame (so your PRINT-turn edit actually
  discharges the phase hole), and how to observe the change immediately. READ
  THIS before you edit a mission doc — it is the format zai-1 had to
  reverse-engineer mid-flight.
- **`futon3c/README-drawbridge.md`** — how to evaluate in the live JVM
  (`scripts/proof-eval.sh`); the `emit_mission_clean.sh` tracker + its
  `--refresh` callback both go through Drawbridge.

## The apparatus (what each file does)

| File | Role |
|---|---|
| `src/futon3c/peripheral/war_machine_pilot.clj` | the loop: `begin-live-cycle!` (READ→EVAL→PRINT-proposal→request-tick) / `close-live-cycle!` (LOOP→emit frame). Two-phase so it never blocks on the tick. |
| `src/futon3c/peripheral/war_machine_pilot_backend.clj` | the tools: `wm-api-query` (READ), `consent-gate-emit` (EVAL, mints `cg-…`), `anchor-flip`/`coherence-row-author`/`pilot-action`/`hop-trigger` (substantive PRINT — all require a `:consent-gate-event-id`, "Pilot-I1"). |
| `src/futon3c/aif/repl_trace.clj` | γ-frame emission: `record-step!` / `frame` / `write-frame!` / `add-learning!`. Frames land in `data/repl-traces/<run-id>.edn` (daily-scan-frame shape + `:learning`). |
| `src/futon3c/aif/loop_learning.clj` | LOOP `:autonomy` auto-miner: derives `:learning` (patterns-applied + sorries-mined, rated) from the WM judgement's own gap-signals. |
| `scripts/repl_spec_verify.clj` | the verifier: `bb scripts/repl_spec_verify.clj <frame.edn>` → asserts V1–V4 (hard), reports V5/V6. Every cycle's frame should CONFORM. |
| `src/futon3c/wm/scheduler.clj` | the tick. **Use `request-tick!` (async). NEVER call `tick!` synchronously** — the 45s scan + a shell timeout has killed the JVM before. |

## Running a cycle (the live shape)

1. **READ** — `wm-api-query` → `:judgement :ranked-actions` (each carries `:G-total` and an `:action`). The top is your candidate `v`.
2. **EVAL** — choose `v`; `predicted-discharge` = its `G-total`; mint a `cg-id`.
   At a *fork* (more than one defensible `v`), resolve by **§8**: pattern-warrant
   over `futon3/library/`, AIF² contest for close calls, record the warrant.
3. **PRINT** — execute the action citing the `cg-id`. Substantive substrate
   edits outside the pilot envelope are done by you as a hand-edit + Drawbridge
   reload (the witness pattern), still citing the `cg-id`. **The discharge must
   be earned** — if the action claims to close a gap, the gap must actually be
   gone (V2 no-teleport; no fake-finished).
4. **LOOP** — `request-tick!` → wait for the tick to land → `close-live-cycle!`
   re-reads, computes `realised-discharge` + `prediction-error`, auto-mines
   `:learning`, and writes the frame. **The field must actually move** for a
   real discharge: top-shift TRUE, the addressed item rotates off `ranked-actions[0]`.
5. **VERIFY** — run the verifier on the emitted frame.

A worked, conforming example: run `live-2d50834b…` (discharged
`sorry/mission-aif-head-not-served`); the inhabitation log is
`futon5a/data/pilot-inhabitations.edn`.

## Hard disciplines (guardrails — violating any is a design error)

- **Never restart the serving JVM** (futon3c I-0). Reload your own edits via
  Drawbridge `load-file`. Never `:reload` a third-party `defprotocol`.
- **No synchronous heavy calls** — `request-tick!`, never `tick!`; no 45s blocking eval.
- **Consent gate / gate-at-merge** — full-auto is expressible (autopen at the
  gate), but the *operator decides what replays into master*. Substantive tools
  cite a `cg-id` (Pilot-I1).
- **Honest closure** — V2. A closure claim cites a *persisted* registry sorry;
  you cannot "close" an ephemeral auto-mined candidate (anti-backdoor — see
  `E-cheesemonger.md`).
- **Don't flood** — mining holes faster than closing them is governed by
  `holes/missions/E-cheesemonger.md` (the hole-lifecycle charter).

## Layers (why this is documentation, not just code)

The stack is a coupled, hierarchical AIF system: **pheno** (code) → **geno**
(AIF / WM) → **exo** (VSATARCS readable scene-form) → **xeno** (the pilot-facing
artefact, incl. this README). The LOOP turn's `:autonomy` clause requires the
exo layer be **kept current automatically** — when the loop changes the stack,
the VSATARCS documentation (`futon5a/holes/stories/` → `bb
~/code/futon4/scripts/generate_vsatarcs_md.bb` → `futon7a/vsatarcs.html`) should
reflect it. The Operator's Foreword (VSATARCS §0) points readers here.

## Governance + provenance

- `holes/missions/E-cheesemonger.md` — the hole-lifecycle charter (identify /
  no-fake-finished / don't-flood / closure-must-stick / verify / spurious-reject /
  anti-Goodhart / provenance-ledger / GC).
- `futon5a/data/pilot-inhabitations.edn` — the living inhabitation log.
- `data/repl-traces/*.edn` — your γ frames (the data the LOOP tooling is built on).
