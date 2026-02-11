# Mission: Peripheral Behavior

## Derivation

INSTANTIATE step, building on M-peripheral-model (complete) and
futon-theory/reverse-morphogenesis.flexiarg (new axioms A8-A12).

Prior:
- M-peripheral-model: specs + hop protocol complete (explore/edit/test/deploy/reflect)
- M-forum-refactor: evidence landscape complete (store + threads + validate)
- M-agency-refactor: social pipeline complete (presence → authenticate → dispatch → persist)
- futon-theory/reverse-morphogenesis.flexiarg: ← operator, constraint-from-fruit
- futon-theory/futonic-logic.flexiarg: 象, 香, 部, 鹽, A7 (compositional salience)
- resources/peripherals.edn: five peripheral specs (structural constraints)
- src/futon3c/social/peripheral.clj: hop validation + context transfer (pure functions)

## Why This Mission Exists

M-peripheral-model built the garage: specs, shapes, hop protocol, entry/exit
validation. But the garage has no cars. The five peripherals are structurally
constrained (tools, scope, entry/exit) but have no behavioral implementation —
no code that actually acts within those constraints.

The reverse-morphogenesis insight (← operator) provides the design method:
each peripheral is a constrained situation of action. The constraints are not
limitations — they are what makes each peripheral generative. The explore
peripheral can't write files; that's what makes exploration productive rather
than chaotic. The test peripheral can only run tests; that's what makes test
results authoritative.

Design method for each peripheral:

```
reverse-morphogenesis(form, intended-fruit) → constraints
```

- **form** = the codebase/system state (象)
- **intended-fruit** = what a successful session in this peripheral produces
- **constraints** = the tool/scope/entry/exit spec (already in peripherals.edn)
- **behavior** = realize(象, constraints) — the code that acts within the envelope

The ← round-trip test: run the peripheral, observe the fruit, apply ← backwards.
Do the inferred constraints match the spec? If yes, the behavior is correct.

## What This Mission Produces

1. **PeripheralRunner protocol** — Clojure protocol for peripheral execution
2. **Five peripheral implementations** — explore, edit, test, deploy, reflect
3. **Evidence emission** — each peripheral emits evidence entries during execution
4. **← round-trip tests** — verify that behavioral output matches spec constraints
5. **Integration with hop protocol** — behavioral state transfers across hops

## Scope In

- PeripheralRunner protocol: `start`, `step`, `stop` lifecycle
- Tool dispatch: map tool keywords (`:read`, `:glob`, `:grep`, etc.) to operations
- Scope enforcement: structural guarantee that tools respect scope boundaries
- Evidence emission: peripheral actions produce EvidenceEntry with `:evidence/type`
  matching peripheral function (`:reflection` for reflect, `:coordination` for others)
- Session context: peripheral reads/writes session state via context map
- Hop integration: `start` receives context from `transfer-context`, `stop` produces
  context for next peripheral

## Scope Out

- HTTP/WebSocket transport: peripherals operate in-memory, no network
- Agent invocation: peripherals don't invoke external agents (run-claude!, run-codex!)
- Real file I/O: tool dispatch uses pluggable backends (mock for tests, real for production)
- MUSN integration: reflect peripheral emits PAR-shaped evidence, not MUSN protocol
- CLI wrappers (fuclaude, fucodex): separate concern, uses PeripheralRunner

## Conceptual Model

### Each Peripheral as ← Reading

| Peripheral | 象 (form) | Fruit (intended yield) | Constraints (from spec) | ← verification |
|------------|-----------|----------------------|------------------------|----------------|
| **explore** | Codebase | "Found the right file/pattern" | read/glob/grep only, full-codebase scope | Did the session find a target without modifying anything? |
| **edit** | Target files | "Changes made, ready to test" | read/edit/write/bash, scoped paths | Did the session modify only scoped files? |
| **test** | Changed files | "Test results: pass/fail/flaky" | read/bash-test, test-commands-only | Did the session produce test results without modifying code? |
| **deploy** | Passing tests | "Committed and pushed" | bash-git/bash-deploy, git-push-only | Did the session only perform git operations? |
| **reflect** | Session log | "PAR generated" | read/musn-log, session-log-only | Did the session produce a structured reflection? |

### Dylan/GZA Principle: Constraints as Generativity

The peripheral's constraints are not safety rails bolted onto general-purpose
action. They are the decomposition regime (部) that makes action generative.

- Explore without write: forces the agent to understand before changing
- Edit without deploy: forces the agent to stay in the change, not skip to shipping
- Test without edit: forces the agent to observe results, not fix-and-rerun in a loop
- Deploy without edit: forces the agent to ship what was tested, not sneak in changes
- Reflect without action: forces the agent to observe, not fix what it observes

Each constraint is a question the peripheral asks: "What can you accomplish
within this envelope?" The answer is the fruit. If the fruit is good, the
constraints were right. If the fruit is bad, either the constraints need
revision (spec change) or the agent needs to hop to a different peripheral
(reframe — A10).

### Evidence Emission

Every peripheral action produces an EvidenceEntry:

```clojure
{:evidence/type     :coordination    ;; or :reflection for reflect
 :evidence/subject  <session-ref>    ;; ArtifactRef to the session
 :evidence/author   <agent-id>
 :evidence/body     {:tool :glob :args ["**/*.clj"] :result 42}  ;; tool + args + result summary
 :evidence/tags     [:peripheral :explore]
 :evidence/session-id <session-id>}
```

This connects peripherals to the evidence landscape: peripheral actions are
evidence entries that threads can project, validate can assess, and L1 can mine.

## Parts

### Structure: Diamond with Wiring Step

```
              Part I: Foundation (Claude)
             /                            \
Part IIa: ← Verification          Part IIb: 5 Peripherals
(Claude — theory + scaffold)       (Codex — implementations)
             \                            /
         Part IIc: Claude Wiring (Claude)
                       |
         Part III: Integration (Claude)
```

Parts IIa and IIb run **in parallel** after Part I completes. Part IIc is
blocked on IIb — it builds the Claude-specific adaptations that let Claude
operate within the peripherals (registry, prompt construction, chain
orchestration). Part III is blocked on IIa + IIc and brings everything
together with end-to-end integration tests.

### Part I: Foundation — PeripheralRunner + Tool Dispatch + Evidence Emission (Claude)

**Status:** Complete (b69ede4)

:in  — src/futon3c/social/shapes.clj (READ-ONLY)
       src/futon3c/social/peripheral.clj (READ-ONLY, hop protocol)
       src/futon3c/evidence/store.clj (READ-ONLY, evidence API)
       resources/peripherals.edn (READ-ONLY, peripheral specs)
:out — src/futon3c/peripheral/runner.clj
       src/futon3c/peripheral/tools.clj
       src/futon3c/peripheral/evidence.clj
       test/futon3c/peripheral/runner_test.clj

PeripheralRunner protocol:
```clojure
(defprotocol PeripheralRunner
  (start [this context]
    ;; Initialize peripheral with context from hop.
    ;; Returns {:ok true :state <initial-state>} | SocialError)
  (step [this state action]
    ;; Execute one action within the peripheral's constraints.
    ;; action: {:tool :keyword :args [...]}
    ;; Returns {:ok true :state <new-state> :result <tool-result>
    ;;          :evidence EvidenceEntry} | SocialError)
  (stop [this state reason]
    ;; Finalize peripheral, produce exit context.
    ;; Returns {:ok true :context <for-next-peripheral>
    ;;          :evidence EvidenceEntry} | SocialError))
```

Tool dispatch (`tools.clj`):
- `dispatch-tool [tool-id args scope backend]` — route tool keyword to backend
- `allowed? [tool-id peripheral-spec]` — check tool against spec's tool set
- `in-scope? [tool-id args peripheral-spec]` — check args respect scope
- Backend protocol: pluggable (mock for tests, real for production)

```clojure
(defprotocol ToolBackend
  (execute-tool [this tool-id args]
    ;; Execute tool, return result or error.))
```

Evidence emission (`evidence.clj`):
- `make-start-evidence [peripheral-id session-ref author]` → EvidenceEntry with :claim-type :goal
- `make-step-evidence [peripheral-id session-ref author tool args result]` → EvidenceEntry with :claim-type :step
- `make-stop-evidence [peripheral-id session-ref author fruit reason]` → EvidenceEntry with :claim-type :conclusion

Evidence emission is in Part I (not IIa) because both branches need it:
Codex's peripherals call it during step/stop, Claude's ← framework reads it.

Mock backend (`tools.clj`):
- Returns canned results per tool-id
- Records all calls (tool-id + args) for ← verification
- Implements ToolBackend

Criteria:
- [ ] PeripheralRunner protocol defined with start/step/stop lifecycle
- [ ] ToolBackend protocol with execute-tool
- [ ] Tool dispatch routes keywords to backend, respects tool set
- [ ] Scope enforcement: tools reject out-of-scope args structurally
- [ ] Mock backend for tests (returns canned results, records calls)
- [ ] Evidence emission: helpers produce shape-valid EvidenceEntry
- [ ] 8+ tests (lifecycle + dispatch + scope + evidence)

### Part IIa: ← Verification Framework (Claude — parallel with IIb)

**Status:** Complete (a6d56ef)

:in  — src/futon3c/peripheral/runner.clj (READ-ONLY, from Part I)
       src/futon3c/peripheral/tools.clj (READ-ONLY, from Part I)
       src/futon3c/peripheral/evidence.clj (READ-ONLY, from Part I)
       src/futon3c/social/shapes.clj (READ-ONLY)
       resources/peripherals.edn (READ-ONLY)
:out — src/futon3c/peripheral/round_trip.clj
       test/futon3c/peripheral/round_trip_test.clj

This is the theory-to-code bridge. The ← operator from reverse-morphogenesis
(A11) says: run a peripheral, observe the fruit, apply ← backwards. Do the
inferred constraints match the spec? If yes, the behavior is correct.

`verify-constraints [peripheral-spec evidence-entries]` → {:ok true} | {:violations [...]}
- Checks every tool invocation was in the spec's tool set
- Checks every tool arg was within scope
- Checks evidence entries have correct :evidence/type for the peripheral
- Checks the fruit (stop evidence) matches the peripheral's intended output shape

`run-and-verify [peripheral mock-actions]` → {:ok true :fruit ... :evidence [...]} | {:violations [...]}
- Convenience: runs start→step*→stop with mock actions, then verify-constraints
- This is the full ← round-trip in one call

← test pattern (used by Part III integration):
```clojure
(let [result (round-trip/run-and-verify explore-peripheral mock-explore-actions)]
  (is (:ok result))
  ;; ← reading: inferred constraints match spec
  (is (empty? (:violations result))))
```

Criteria:
- [ ] verify-constraints checks tool set, scope, evidence type, fruit shape
- [ ] run-and-verify executes full lifecycle and verifies
- [ ] Violations are specific: which tool, which arg, which constraint
- [ ] Works with any peripheral (not hardcoded to specific implementations)
- [ ] 6+ tests (tool violation, scope violation, evidence violation, clean run)

### Part IIb: Five Peripheral Implementations (Codex — parallel with IIa)

**Status:** Complete (e721e9e)

:in  — src/futon3c/peripheral/runner.clj (READ-ONLY, from Part I)
       src/futon3c/peripheral/tools.clj (READ-ONLY, from Part I)
       src/futon3c/peripheral/evidence.clj (READ-ONLY, from Part I)
       src/futon3c/social/shapes.clj (READ-ONLY)
       src/futon3c/evidence/store.clj (READ-ONLY)
       resources/peripherals.edn (READ-ONLY)
:out — src/futon3c/peripheral/common.clj
       src/futon3c/peripheral/explore.clj
       src/futon3c/peripheral/edit.clj
       src/futon3c/peripheral/test_runner.clj
       src/futon3c/peripheral/deploy.clj
       src/futon3c/peripheral/reflect.clj
       test/futon3c/peripheral/explore_test.clj
       test/futon3c/peripheral/edit_test.clj
       test/futon3c/peripheral/test_runner_test.clj
       test/futon3c/peripheral/deploy_test.clj
       test/futon3c/peripheral/reflect_test.clj

Each peripheral implements PeripheralRunner with its spec from peripherals.edn:

| Peripheral | Tools | Scope | Fruit | Exit context |
|------------|-------|-------|-------|-------------|
| explore | :read :glob :grep :bash-readonly :web-fetch | :full-codebase | {:found, :summary} | :target-files |
| edit | :read :edit :write :bash | {:paths [...]} | {:changes, :ready-to-test?} | :changed-files |
| test | :read :bash-test | :test-commands-only | {:result :pass/:fail/:flaky, :test-count, :failures} | — |
| deploy | :bash-git :bash-deploy | :git-push-only | {:committed?, :pushed?, :sha} | — |
| reflect | :read :musn-log | :session-log-only | {:par EvidenceEntry} | — |

Each peripheral's lifecycle:
- **start**: validates context, initializes state, emits :goal evidence via evidence.clj
- **step**: dispatches tool through tools.clj, emits :step evidence per action
- **stop**: produces fruit, emits :conclusion evidence, returns context for next peripheral

Scope enforcement per peripheral:
- explore: rejects any write tool (:edit, :write, :bash)
- edit: rejects paths outside {:paths [...]} scope
- test: rejects any tool except :read and :bash-test
- deploy: rejects any tool except :bash-git and :bash-deploy
- reflect: rejects any tool except :read and :musn-log

Criteria:
- [ ] All 5 peripherals implement PeripheralRunner
- [ ] Each peripheral's start/step/stop produces shape-valid results
- [ ] Each step emits evidence via evidence.clj helpers
- [ ] Scope enforcement: edit rejects out-of-scope writes, test rejects edit tools, etc.
- [ ] Reflect emits :reflection type evidence (others emit :coordination)
- [ ] 22+ tests (4+ per peripheral, 2+ scope violation tests)

### Part IIc: Claude Wiring — Registry + Adapter + Chain Orchestration (Claude)

**Status:** Ready (IIa + IIb complete)

:in  — src/futon3c/peripheral/explore.clj (READ-ONLY, from Part IIb)
       src/futon3c/peripheral/edit.clj (READ-ONLY, from Part IIb)
       src/futon3c/peripheral/test_runner.clj (READ-ONLY, from Part IIb)
       src/futon3c/peripheral/deploy.clj (READ-ONLY, from Part IIb)
       src/futon3c/peripheral/reflect.clj (READ-ONLY, from Part IIb)
       src/futon3c/peripheral/common.clj (READ-ONLY, from Part IIb)
       src/futon3c/peripheral/runner.clj (READ-ONLY, from Part I)
       src/futon3c/peripheral/tools.clj (READ-ONLY, from Part I)
       src/futon3c/peripheral/evidence.clj (READ-ONLY, from Part I)
       src/futon3c/social/peripheral.clj (READ-ONLY, hop protocol)
       src/futon3c/evidence/store.clj (READ-ONLY)
       resources/peripherals.edn (READ-ONLY)
:out — src/futon3c/peripheral/registry.clj
       src/futon3c/peripheral/adapter.clj
       test/futon3c/peripheral/registry_test.clj
       test/futon3c/peripheral/adapter_test.clj

Codex built abstract PeripheralRunner implementations; this Part wires them
for Claude. Three concerns:

**1. Peripheral Registry** (`registry.clj`):

Unified factory that maps peripheral-id to concrete PeripheralRunner:

```clojure
(make-peripheral :explore backend) ;; → ExplorePeripheral
(make-peripheral :edit backend)    ;; → EditPeripheral
;; etc.
```

Plus chain orchestration — runs a sequence of peripherals with hop
validation and context transfer between each:

```clojure
(run-chain backend peripherals-map context
  [{:peripheral-id :explore :actions [...] :stop-reason "found target"}
   {:peripheral-id :edit    :actions [...] :stop-reason "ready to test"}
   {:peripheral-id :test    :actions [...] :stop-reason "tests pass"}])
;; → {:ok true :evidence [...] :fruits [...] :final-context {...}}
```

run-chain uses social/peripheral.clj's validate-hop + transfer-context to
enforce hop validity at each transition. Each peripheral's stop context
feeds the next peripheral's start context. Evidence accumulates in an
optional evidence store.

**2. Claude Adapter** (`adapter.clj`):

Claude-specific translation layer — pure functions, no Claude invocation:

- `tool-mapping [peripheral-spec]` → `{"Read" :read, "Glob" :glob, ...}`
  Maps Claude Code tool names to peripheral tool keywords. Only includes
  tools in the peripheral's tool set (explore won't map "Edit").

- `tool-call->action [peripheral-spec tool-call]` → `{:tool :read :args ["src/a.clj"]}`
  Translates a Claude Code tool-use result into the action format that
  PeripheralRunner.step expects. Validates the tool is allowed.

- `peripheral-prompt-section [peripheral-spec context]` → string
  Generates the system prompt section that expresses constraints to Claude:
  available tools, scope boundaries, what's forbidden, exit conditions.
  This is the prompt engineering that makes Claude operate within the
  peripheral's envelope.

- `describe-constraints [peripheral-spec]` → constraint map
  Structured representation: `{:allowed-tools [...], :scope "...",
  :forbidden [...], :exit-conditions [...]}`. Used by prompt construction
  and potentially by hooks.

- `detect-exit [peripheral-spec text]` → nil | `{:exit-condition :kw :reason "..."}`
  Analyzes Claude's non-tool output for signals that it wants to hop or
  has completed the peripheral's goal. Maps to exit conditions from spec.

**3. Why this is Claude-specific:**

Codex doesn't need prompt sections, tool-call translation, or exit
detection — Codex receives explicit instructions via GitHub issues and
operates on code directly. Claude operates interactively within
constraint envelopes, so it needs: (a) to be told its constraints via
prompt, (b) its tool calls translated to the protocol, (c) its natural
language output parsed for hop signals.

Criteria:
- [ ] make-peripheral dispatches to all 5 peripheral factories
- [ ] run-chain orchestrates multi-peripheral sessions with hop validation
- [ ] run-chain transfers context via social/peripheral.clj transfer-context
- [ ] run-chain collects evidence across all peripherals in session
- [ ] tool-mapping produces correct mapping for each peripheral's tool set
- [ ] tool-call->action translates Claude tool calls to PeripheralRunner actions
- [ ] peripheral-prompt-section generates constraint-expressing prompts
- [ ] detect-exit identifies hop signals from output text
- [ ] Invalid hops produce SocialError (via validate-hop)
- [ ] 10+ tests (registry dispatch, chain orchestration, tool mapping, prompt, exit detection)

### Part III: Integration — Hop Chain + Evidence Flow + ← Verification (Claude)

**Status:** Blocked on Parts IIa + IIc

:in  — All files from Parts I, IIa, IIb, IIc
       src/futon3c/peripheral/registry.clj (from Part IIc — chain orchestration)
       src/futon3c/peripheral/adapter.clj (from Part IIc — Claude adapter)
       src/futon3c/peripheral/round_trip.clj (from Part IIa — ← verification)
       src/futon3c/social/peripheral.clj (hop protocol)
       src/futon3c/evidence/store.clj
       src/futon3c/evidence/threads.clj
:out — test/futon3c/peripheral/integration_test.clj

End-to-end scenarios:

1. **Full hop chain**: explore → edit → test → deploy → reflect
   - Each peripheral starts with context from previous
   - Session-id preserved throughout
   - Evidence entries accumulate in store

2. **← round-trip on every peripheral**: run each through run-and-verify
   - Confirms all 5 peripherals pass the constraint check
   - This is the ← operator applied as a test method (A11 in code)

3. **Evidence thread**: project a thread from the full chain's evidence
   - Thread has goal (explore started), steps (edits, tests), conclusion (deployed)
   - thread-patterns extracts patterns used across the chain
   - Proof-tree invariants hold

4. **Scope violation rejected**: edit attempts out-of-scope write → SocialError
   - ← test: the error IS the evidence that constraints are working

5. **Reframe scenario**: test fails → hop back to edit → fix → re-test → deploy
   - The hop-back is a reframe (A10): the expected exit (tests-pass) was wrong,
     so the agent returns to edit with new information

6. **Reflect produces mineable PAR**: reflect's output is an evidence entry
   that thread-patterns can read — patterns used, outcomes observed

Criteria:
- [ ] Full hop chain preserves session-id and context across 5 peripherals
- [ ] ← round-trip passes for all 5 peripherals (via run-and-verify)
- [ ] Evidence entries from all peripherals form a projectable thread
- [ ] Proof-tree invariants hold on the peripheral evidence thread
- [ ] Scope violation produces SocialError, not silent failure
- [ ] Reframe (test-fail → edit → re-test) works as hop cycle
- [ ] Reflect PAR is a valid evidence entry mineable by thread-patterns
- [ ] 8+ integration tests
- [ ] All existing tests unaffected

## Proof-Tree Invariants on Peripheral Evidence

The same 7 invariants from M-forum-refactor apply to peripheral evidence threads:

1. Tree validity: each step's in-reply-to references the previous step
2. Root invariant: explore's start entry has :claim-type :goal
3. No orphans: every peripheral action belongs to the session thread
4. Claim ordering: deploy's conclusion replies to test's evidence, not to another conclusion
5. Author tracking: participants = all agents that touched the session
6. Monotonic timestamps: peripheral steps are chronologically ordered
7. Entry count consistency: thread entry count matches actual entries

## Relationship to Other Missions

- **M-peripheral-model**: Built the garage (specs, hops). This mission fills it with cars.
- **M-forum-refactor**: Evidence landscape is the medium peripherals write to.
  Peripheral actions are evidence entries; threads project peripheral sessions.
- **M-agency-refactor**: S-dispatch routes messages to agents in peripherals.
  The dispatch receipt is the entry point; the peripheral session is the response.
- **futon-theory/reverse-morphogenesis**: ← is the design method AND the test method.
  Design: infer constraints from intended fruit. Test: verify output matches constraints.

## Exit Conditions

- PeripheralRunner protocol with start/step/stop lifecycle
- ToolBackend protocol with pluggable mock/real backends
- Evidence emission helpers produce shape-valid EvidenceEntry for start/step/stop
- All five peripherals implement the protocol with correct tool/scope constraints
- Tool dispatch is pluggable (mock backend for tests, real backend deferred)
- Each peripheral step emits shape-valid EvidenceEntry
- Scope enforcement is structural (rejected by dispatch, not by convention)
- ← round-trip: `run-and-verify` passes for all 5 peripherals (verify-constraints clean)
- ← violations are specific and actionable (which tool, which arg, which constraint)
- Full hop chain works end-to-end with evidence accumulation
- Peripheral evidence threads satisfy proof-tree invariants
- Part IIc wiring: registry dispatches to all peripherals, chain orchestration works
- Claude adapter: prompt generation, tool mapping, exit detection
- Parts IIa and IIb completed independently (no cross-dependency)
- All new tests pass, existing 167 tests unaffected (currently at 244)
- `clojure -X:test` passes cleanly
