# Mission: Peripheral Model

## Derivation

INSTANTIATE step of the social exotype derivation xenotype.

Prior:
- ARGUMENT.flexiarg R10 (mode-gate): coordination talk and action talk must be distinguishable
- social-exotype.edn §S-mode (lines 151-162): mode classification component
- peripheral-spec.md: five core peripherals (explore, edit, test, deploy, reflect)
- mode-gate.flexiarg: DISCUSS → DIAGNOSE → EXECUTE transition protocol
- M-agency-refactor Part I complete: shapes, registry, test infrastructure exist

## What This Mission Produces

1. **S-mode component** — classifies messages as coordination or action (R10)
2. **Peripheral specs** — Malli-validated capability envelope definitions
3. **Mode transition machine** — validates DISCUSS → DIAGNOSE → EXECUTE flow
4. **Hop protocol** — session-id transfer across peripheral boundaries
5. **Integration with shapes.clj** — ClassifiedMessage output used by S-dispatch

## Scope In

- S-mode: Message classification (R10, agency reading of G3)
- Peripheral definitions: capability envelopes with structural constraints
- Mode transition validation: explicit state machine for DISCUSS/DIAGNOSE/EXECUTE
- Hop protocol: session-id continuity, context transfer, entry/exit conditions
- Mode shapes: extend shapes.clj with peripheral/mode types

## Scope Out

- S-validate: Coordination outcome validation — needs forum (M-forum-refactor)
- S-default: Default mode network — needs all components first
- Transport layer: HTTP/WS/IRC connections — subsequent mission
- Agent invocation: run-codex!, run-claude! — M-agency-refactor (invokes.clj)
- Forum integration: post-forum-reply! — M-forum-refactor
- Pattern search peripheral runner — needs futon3a integration
- codex_mirror — not in social pipeline
- Context rollover / token tracking — stays in invocation layer

## Source Material (futon3 → futon3c mapping)

| futon3 file | Lines | Maps to | Notes |
|-------------|-------|---------|-------|
| agency/service.clj (run-peripheral!) | 659-797 | futon3c/social/mode.clj (classification) + futon3c/social/peripheral.clj (specs) | Split: classification logic vs dispatch mechanics |
| agency/service.clj (get-peripheral) | ~50 | futon3c/social/peripheral.clj | Peripheral spec lookup |
| resources/agency/peripherals.edn | 33 | resources/peripherals.edn | Rewrite with Malli-validated specs |
| docs/peripheral-spec.md | 215 | Design reference (not ported) | Informs the shapes |
| library/realtime/mode-gate.flexiarg | 24 | Informs S-mode implementation | DISCUSS → DIAGNOSE → EXECUTE |

## Key Design Decision: Split Classification from Dispatch

futon3 mixes mode classification with agent invocation in `run-peripheral!`.
futon3c separates them:

- **S-mode** (`mode.clj`): Takes a message + patterns → produces ClassifiedMessage
  with `:msg/mode :coordination` or `:msg/mode :action`. Pure function, no side effects.
  Also validates mode transitions (DISCUSS → DIAGNOSE → EXECUTE).

- **Peripheral specs** (`peripheral.clj`): Defines what each peripheral CAN do
  (tools, scope, entry/exit conditions). These are the structural constraints —
  not behavioral warnings but machine-enforced envelopes.

- **S-dispatch** (M-agency-refactor Part IV): Uses the ClassifiedMessage mode
  to route appropriately. Dispatch consumes mode classification but doesn't
  produce it.

This split follows social-exotype.edn: S-mode feeds into S-dispatch via typed
edge (`:xtdb-entity`), and mode guides both dispatch and validation.

## Parts

### Part I: Mode Shapes + Peripheral Specs (Claude)

**Status:** Complete (26e625b)

:in  — src/futon3c/social/shapes.clj (READ-ONLY, extend with new shapes)
       library/social/ARGUMENT.flexiarg (READ-ONLY, R10 spec)
       futon5/data/missions/social-exotype.edn (READ-ONLY, §S-mode)
       futon3/docs/peripheral-spec.md (READ-ONLY, design reference)
       futon3/library/realtime/mode-gate.flexiarg (READ-ONLY, pattern)
:out — src/futon3c/social/shapes.clj (ADD new shapes, preserve existing)
       resources/peripherals.edn
       test/futon3c/social/shapes_test.clj (ADD new shape tests)

What to do:
- Add to shapes.clj:
  - `PeripheralSpec`: Malli schema for peripheral definitions
    {:peripheral/id, :peripheral/tools, :peripheral/scope, :peripheral/entry,
     :peripheral/exit, :peripheral/context}
  - `ModeTransition`: {:mode/from, :mode/to, :mode/actor, :mode/at,
     :mode/exit-criteria, :mode/approval-token}
  - `HopRequest`: {:hop/to, :hop/reason, :hop/session-id, :hop/context}
  - `HopResult`: {:hop/from, :hop/to, :hop/session-id, :hop/at, :hop/success?}
- Write resources/peripherals.edn with five core peripherals as Malli-validated specs
- Add shape tests for all new types

### Part II: S-mode Component (Codex handoff)

**Status:** Complete (f0a939f)

:in  — src/futon3c/social/shapes.clj (READ-ONLY)
       test/futon3c/social/test_fixtures.clj (READ-ONLY)
       src/futon3c/agency/registry.clj (READ-ONLY)
       resources/peripherals.edn (READ-ONLY)
       futon5/data/missions/social-exotype.edn (READ-ONLY, §S-mode)
:out — src/futon3c/social/mode.clj
       test/futon3c/social/mode_test.clj

Criteria:
- [x] R10 (mode-gate): coordination and action are distinguishable
- [x] Mode transitions validated: DISCUSS → DIAGNOSE → EXECUTE
- [x] EXECUTE requires approval token or explicit approval
- [x] Input conforms to shapes, output is ClassifiedMessage|ModeTransition|SocialError
- [x] 7 tests pass (5+ met)
- [x] No EXPECTED FAIL markers

Scope compliance: clean — two :out files created, no :in files modified.
Review notes:
- `patterns` argument in `classify` is validated but unused: classification uses a
  hardcoded heuristic (coordination-types set), not pattern library rules. Correct
  for now — the heuristic works — but pattern-informed classification is a future
  enhancement. The parameter is a placeholder.
- Signature extension: `validate-transition` takes keyword opts (`:approval-token`,
  `:exit-criteria`, `:summary`) beyond the 3-arg spec. Reasonable — needed for
  approval token enforcement.
- EXECUTE → DISCUSS does not enforce mandatory summary: `:mode/summary` is optional.
  The mission state machine says "exit: must include summary". Part IV integration
  should decide whether to enforce this or keep it advisory.

### Part III: Peripheral Specs + Hop Protocol (Codex handoff)

**Status:** Ready for handoff

:in  — src/futon3c/social/shapes.clj (READ-ONLY)
       test/futon3c/social/test_fixtures.clj (READ-ONLY)
       src/futon3c/social/mode.clj (READ-ONLY)
       resources/peripherals.edn (READ-ONLY)
:out — src/futon3c/social/peripheral.clj
       test/futon3c/social/peripheral_test.clj

Function signatures:
```clojure
(defn load-peripherals
  "Load and validate peripheral specs from EDN."
  [path]
  ...)

(defn get-peripheral
  "Look up a peripheral by ID. Returns PeripheralSpec or SocialError."
  [peripherals peripheral-id]
  ...)

(defn validate-hop
  "Validate a hop request: is this transition allowed from the current peripheral?
   Returns HopResult or SocialError."
  [current-peripheral hop-request]
  ...)

(defn transfer-context
  "Build context for the target peripheral, carrying session-id and relevant state."
  [hop-result source-context]
  ...)
```

Criteria:
- [ ] Five core peripherals load and validate against PeripheralSpec shape
- [ ] Hop validation: only allowed transitions succeed (explore → edit, edit → test, etc.)
- [ ] Session-id preserved across hops
- [ ] Entry conditions checked before allowing peripheral entry
- [ ] 5+ tests pass
- [ ] No EXPECTED FAIL markers

### Part IV: Integration (Claude)

**Status:** Blocked on Part III

:in  — All component files from Parts I-III
       src/futon3c/social/presence.clj (from M-agency-refactor Part II)
       src/futon3c/social/authenticate.clj (from M-agency-refactor Part III)
:out — test/futon3c/social/mode_integration_test.clj

Criteria:
- [ ] Mode classification feeds into dispatch pipeline (ClassifiedMessage shape)
- [ ] Peripheral hop preserves session-id end-to-end
- [ ] Mode transition state machine prevents invalid paths
- [ ] Integration with S-presence → S-authenticate → S-mode pipeline segment
- [ ] Decide: enforce mandatory summary on EXECUTE → DISCUSS transitions?
  (mode.clj makes :mode/summary optional; mission spec says "must include summary")
- [ ] Decide: should classify use patterns for heuristic enrichment?
  (currently patterns are validated-but-unused; hardcoded coordination-types set)

## Peripheral Definitions (for resources/peripherals.edn)

Derived from futon3/docs/peripheral-spec.md, validated against PeripheralSpec shape:

```clojure
{:peripherals
 {:explore
  {:peripheral/id :explore
   :peripheral/tools #{:read :glob :grep :bash-readonly :web-fetch}
   :peripheral/scope :full-codebase
   :peripheral/entry #{:default :from-reflect}
   :peripheral/exit #{:found-target :ready-to-edit :user-request :hop-reflect}
   :peripheral/context {:session-id :inherit}}

  :edit
  {:peripheral/id :edit
   :peripheral/tools #{:read :edit :write :bash}
   :peripheral/scope {:paths ["src/" "docs/" "scripts/"]}
   :peripheral/entry #{:from-explore :user-request}
   :peripheral/exit #{:tests-pass :ready-to-commit :blocked :hop-test :hop-reflect}
   :peripheral/context {:session-id :inherit :target-files :from-explore}}

  :test
  {:peripheral/id :test
   :peripheral/tools #{:read :bash-test}
   :peripheral/scope :test-commands-only
   :peripheral/entry #{:from-edit :user-request}
   :peripheral/exit #{:pass :fail :flaky :hop-edit :hop-deploy :hop-reflect}
   :peripheral/context {:session-id :inherit :changed-files :from-edit}}

  :deploy
  {:peripheral/id :deploy
   :peripheral/tools #{:bash-git :bash-deploy}
   :peripheral/scope :git-push-only
   :peripheral/entry #{:from-test :tests-passed}
   :peripheral/exit #{:deployed :blocked :hop-reflect}
   :peripheral/context {:session-id :inherit :commit-message :from-edit}}

  :reflect
  {:peripheral/id :reflect
   :peripheral/tools #{:read :musn-log}
   :peripheral/scope :session-log-only
   :peripheral/entry #{:session-close :user-request :agent-request :from-any}
   :peripheral/exit #{:par-generated}
   :peripheral/context {:session-id :inherit :session-log :fetch-from-musn}}}}
```

## Mode Transition State Machine

```
             ┌──────────────────────────┐
             │                          │
             v                          │
         DISCUSS ──→ DIAGNOSE ──→ EXECUTE
             ^                     │
             │                     │
             └─────────────────────┘
                   (exit only)
```

Valid transitions:
- DISCUSS → DIAGNOSE (always allowed)
- DIAGNOSE → EXECUTE (requires approval token or explicit approval)
- EXECUTE → DISCUSS (exit: must include summary)
- Any → DISCUSS (reset/timeout)

Invalid:
- DISCUSS → EXECUTE (must go through DIAGNOSE)
- EXECUTE → DIAGNOSE (must exit to DISCUSS first)
- DIAGNOSE → DISCUSS (backward, use reset instead)

## Relationship to Other Missions

- **M-agency-refactor**: S-mode output (ClassifiedMessage) feeds S-dispatch input.
  S-dispatch is Part IV of M-agency-refactor. The two missions share shapes.clj
  as the contract boundary.

- **M-forum-refactor**: S-validate needs mode classification to validate
  coordination outcomes. Forum posts carry mode tags for auditability.

- **social-exotype.edn**: S-mode has edges from I-patterns (mode classification
  rules) and feeds into both S-dispatch (routing) and S-validate (validation
  criteria). This mission implements the S-mode node.

## Exit Conditions

- S-mode classify function produces shape-validated ClassifiedMessage
- Mode transition state machine enforces DISCUSS → DIAGNOSE → EXECUTE
- Five core peripherals load from EDN and validate against PeripheralSpec
- Hop protocol preserves session-id across peripheral transitions
- All new tests pass, existing 45 tests unaffected
- `clojure -X:test` passes cleanly
