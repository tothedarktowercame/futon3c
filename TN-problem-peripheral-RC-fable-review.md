# TN — Problem peripheral RC review (Fable)

**Reviewer:** claude (Fable 5), 2026-08-15
**Scope:** audit of `README-problem-peripheral.md` (commit `ba917b7e`) against the
code it describes, before the first run on the APM corpus.
**Verdict:** the README is substantially accurate and the machine matches its own
description on every subtle claim I chased. Two small code fixes and one operator
decision are needed before the corpus run (findings 1–3 below).

## What was checked

- `src/futon3c/peripheral/problem.clj` (847 lines) — full read
- `src/futon3c/peripheral/cycle.clj` (generic engine) — full read
- `src/futon3c/apm/preregistration.clj` (validator) — full read
- `src/futon3c/apm/cycle_harness.clj` (`derive-trace`, `run-cycle!`) — full read
- `scripts/frames.bb` (worktree provisioner) — full read
- `src/futon3c/peripheral/runner.clj` — full read
- `holes/labs/M-apm-demonstration/round1-registration.edn` — full read
- `test/futon3c/peripheral/problem_test.clj` close-phase tests (lines 700–1139)
- `src/futon3c/dispatch_with_recall.clj` — targeted read (caller identity,
  `run-dispatch!`, memory-channel handling)

Verification steps re-run independently:

- **Test suite:** `clojure -X:test` over the five named namespaces →
  **119 tests, 349 assertions, 0 failures** — exactly as the README claims.
- **Lean pin:** `git -C ~/code/mathlib4 log -n1 --format=%H --
  DarkTower/APMDemonstrationPreregistration.lean` →
  `4331becf6f0c8471537adcc264a8fde7e12c94c9`, matching both
  `preregistration/required-lean-revision` and the registration EDN.
- **Pin enforcement:** grep for `reg/environment-revision` /
  `reg/harness-revision` across `src/`, `test/`, `scripts/` → **no reader
  exists** (finding 1).

## Claims verified as accurate

- `:completed` is a terminal sentinel with no tools; `:close` is inhabitable
  (phase-order and `base-phase-tools` in `problem.clj`, plus the
  `close-phase-is-inhabitable` test).
- The five derived close tools are computed from engine state and never touch
  the backend — tests assert `(empty? (tools/recorded-calls backend))`.
- Caller-supplied `:measurement`, `:capability-probes`, `:trace`, `:validation`,
  `:authorization` are stripped at close; the engine's retained step results
  win. The engine clock, not the caller, stamps `:cycle/closed-at`.
- The recorded `:assign-checkouts` result overrides a forged advance payload
  (tested directly, `problem_test.clj:700–720`).
- Save/load: write-once versioned files under `data/problem-state/<cycle-id>/`,
  temp-file + `ATOMIC_MOVE` publication behind a JVM lock, branch markers on
  load, guards against cross-session / cross-cycle / cross-mode loads, and the
  `..` path-containment fix (canonical-path comparison, not just the regex).
- Isolation: solver worktree at `:assign-checkouts`, one fresh worktree per
  student attempt at dispatch; all-or-nothing rollback at `:register` only; a
  mid-cycle student provisioning failure rolls back nothing (tested — the
  solver's tree survives).
- Memory channels fixed by role (`:dispatch-solver` → `:push+pull`,
  `:dispatch-student-fresh` → `:pull-only`), assoc'd last so a caller cannot
  override; a failed bell surfaces as a structured tool failure.
- `frames.bb` arm vocabulary matches the engine (`solver`, `student-<uuid>`);
  branches are batch-qualified to avoid second-cycle collision.
- Measurement summary "4 measured / 13 unset" matches `measurement-values`
  (4 derivable fields of the 17 registered).
- A worry chased and **dismissed**: the validator's `:direct-channel-used` check
  flags `claude-*` → `zai-*` Agency jobs, but the engine's own student
  dispatches carry caller `"ground-control"` (CLI default) or nil
  (programmatic path) — the machine will not trip its own gate.

## Findings

### 1. The registration's resource pins are decorative (fix before run)

`:reg/environment-revision` and `:reg/harness-revision` exist only in
`round1-registration.edn` — nothing in `src/`, `test/`, or `scripts/` reads
them. `:assign-checkouts` takes a **caller-supplied** `:base-rev`, and nothing
compares the provisioned revision (or the attempts' harness revisions) against
the frozen pins. The existing invariants check arms against *each other*
(`environment-arms-match`, F5, `harness-changed-in-store-round`), never against
the *registration*. A cycle provisioned at the wrong revision validates
cleanly.

**Fix:** derive `:base-rev` from
`(get-in outputs [:registration :reg/environment-revision])` in the engine, or
add a trace-content failure comparing attempt revisions to the pins. ~5 lines.

### 2. `:launchable? true` is unreachable by construction — and only two of the three permanent blockers are documented (operator decision)

The README's "Known gap" section covers the missing retrieval-probe producer
and the resulting `:f9-capability-probe-missing`. But
`:guidance-measurement-mismatch` is equally permanent:
`record-measurement` structurally cannot populate `"attempts or closer hops"`
(its own unset reason: "Agency-derived guidance evidence is not available to
this tool"), while the validator requires that measured value to **equal** its
independently computed guidance count. With Agency evidence available the
mismatch fires; without it, `:guidance-evidence-unavailable` fires instead.

Consequence: **no cycle run through this peripheral can ever produce a written
authorization.** Before the corpus run, decide:

- (a) the run is gated on a written authorization → the run cannot start as
  built; or
- (b) refused closes are the accepted normal for round 1 → then the README
  should enumerate which refusal codes are *expected-and-benign*
  (`missing producer: retrieval-probe`, `:f9-capability-probe-missing`,
  `:guidance-measurement-mismatch`) versus *disqualifying* (everything else),
  so a reviewer of frame-40 can tell a healthy refusal from a broken one.

This is Joe's call, not a code fix.

### 3. The retrieval-probe gap is quietly caller-bypassable (fix before run)

At close, `emit-trace` strips caller-supplied `:capability-probes` and
`:measurement` from outputs — **but not `:retrieval-probes`**. Any earlier
advance payload can inject `:retrieval-probes []`, which clears the
`missing producer: retrieval-probe` failure ("an empty collection proves its
producer ran" — except here nothing ran), and a non-empty fabricated probe set
would vacuously satisfy F7 and `:need-retrieval`. The one entity type with no
producer is the one the caller can supply.

**Fix:** add `:retrieval-probes` to the close-time strip list in
`stamp-environment-outputs` / the `emit-trace` dissoc, so the gap bites
honestly. ~2 lines.

### 4. "The one rule" oversells its scope (doc)

Frame hashes, dispositions, containment probes, snapshots, and offers all
arrive as caller-relayed advance payloads and are only shape/consistency
checked. Notably, the emitter-side F1 gate that hashes actual files
(`cycle-harness/emit-frame!`) is on the `run-cycle!` path, **not** the
peripheral path — the peripheral's validator just compares two relayed
strings. Defensible for a Codex-as-REPL process, but the README's framing
("anything a gate later reads is written by the engine from its own records")
is true only of the derived fields; a reader could mistake relayed claims for
measured ones.

Relatedly: the README's example `(make-problem)` uses the **mock backend** for
every tool not intercepted by the provisioning / ground-control / state-IO
wrappers. Worth one sentence in the README saying which tools are real in
production (provisioning, dispatch, state IO, derived close tools) versus
relay.

### 5. Minor

- (a) The `:solver-seat-mismatch` guard is dead code on the peripheral path —
  `validate-trace-from-state` passes `(:reg/solver-seat registration)` as the
  invoked seat, comparing the pin against itself. Consistent with
  engine-ownership, but comment it so nobody relies on it firing there.
- (b) The README says `:emit-trace` "refuses" if a producer is absent; it
  actually completes and records `producer-failures`, with refusal deferred to
  `:write-authorization`. The behavior is better than the doc (auditable);
  the doc should match.
- (c) `:f7-missed-available-artifact` is still computed in
  `trace-content-failures` even though F7 was dropped from
  `required-runtime-invariants`. Vacuous today (`[] ⊆ []`), but combined with
  finding 3 a caller-injected probe set could make dropped-F7 fire or pass
  invisibly.

## Recommendation

Fix findings 1 and 3 (both small edits in `problem.clj`) and the doc items in
4/5 before the corpus run; make the finding-2 decision explicitly and record it
in the README. Per the handoff protocol these review fixes belong to the Claude
owner directly, not a re-bell.
