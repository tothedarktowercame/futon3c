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

### Part I: PeripheralRunner Protocol + Tool Dispatch (Claude)

**Status:** Ready

:in  — src/futon3c/social/shapes.clj (READ-ONLY)
       src/futon3c/social/peripheral.clj (READ-ONLY, hop protocol)
       src/futon3c/evidence/store.clj (READ-ONLY, evidence emission)
       resources/peripherals.edn (READ-ONLY, peripheral specs)
:out — src/futon3c/peripheral/runner.clj
       src/futon3c/peripheral/tools.clj
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

Criteria:
- [ ] PeripheralRunner protocol defined with start/step/stop lifecycle
- [ ] Tool dispatch routes keywords to backend, respects tool set
- [ ] Scope enforcement: tools reject out-of-scope args structurally
- [ ] Mock backend for tests (returns canned results)
- [ ] Evidence emission: each step produces shape-valid EvidenceEntry
- [ ] 8+ tests (lifecycle + dispatch + scope + evidence)

### Part II: Explore + Edit Peripherals (Codex handoff)

**Status:** Blocked on Part I

:in  — src/futon3c/peripheral/runner.clj (READ-ONLY, from Part I)
       src/futon3c/peripheral/tools.clj (READ-ONLY, from Part I)
       src/futon3c/social/shapes.clj (READ-ONLY)
       src/futon3c/evidence/store.clj (READ-ONLY)
       resources/peripherals.edn (READ-ONLY)
:out — src/futon3c/peripheral/explore.clj
       src/futon3c/peripheral/edit.clj
       test/futon3c/peripheral/explore_test.clj
       test/futon3c/peripheral/edit_test.clj

Explore peripheral (`explore.clj`):
- Implements PeripheralRunner
- Tools: :read, :glob, :grep, :bash-readonly, :web-fetch
- Scope: :full-codebase (no path restrictions for reading)
- Fruit: `{:found <files/patterns>, :summary <what-was-found>}`
- Exit context: `:target-files` for edit peripheral

Edit peripheral (`edit.clj`):
- Implements PeripheralRunner
- Tools: :read, :edit, :write, :bash
- Scope: `{:paths ["src/" "docs/" "scripts/"]}` — structural enforcement
- Fruit: `{:changes <file-list>, :ready-to-test? <bool>}`
- Exit context: `:changed-files` for test peripheral

← round-trip test for each:
- Run peripheral with mock backend
- Observe output (fruit)
- Verify constraints were respected (no writes in explore, no out-of-scope in edit)
- Verify evidence entries emitted for each step

Criteria:
- [ ] Explore: read-only operations, full-codebase scope, produces target context
- [ ] Edit: write operations scoped to declared paths, rejects out-of-scope writes
- [ ] Both emit evidence entries for each tool invocation
- [ ] ← test: inferred constraints from output match spec
- [ ] 10+ tests (5+ explore, 5+ edit)

### Part III: Test + Deploy + Reflect Peripherals (Codex handoff)

**Status:** Blocked on Part I

:in  — src/futon3c/peripheral/runner.clj (READ-ONLY, from Part I)
       src/futon3c/peripheral/tools.clj (READ-ONLY, from Part I)
       src/futon3c/social/shapes.clj (READ-ONLY)
       src/futon3c/evidence/store.clj (READ-ONLY)
       resources/peripherals.edn (READ-ONLY)
:out — src/futon3c/peripheral/test_runner.clj
       src/futon3c/peripheral/deploy.clj
       src/futon3c/peripheral/reflect.clj
       test/futon3c/peripheral/test_runner_test.clj
       test/futon3c/peripheral/deploy_test.clj
       test/futon3c/peripheral/reflect_test.clj

Test peripheral (`test_runner.clj`):
- Tools: :read, :bash-test
- Scope: :test-commands-only
- Fruit: `{:result :pass|:fail|:flaky, :test-count <n>, :failures <details>}`
- Receives `:changed-files` from edit context

Deploy peripheral (`deploy.clj`):
- Tools: :bash-git, :bash-deploy
- Scope: :git-push-only
- Fruit: `{:committed? <bool>, :pushed? <bool>, :sha <commit-sha>}`
- Receives `:commit-message` from edit context

Reflect peripheral (`reflect.clj`):
- Tools: :read, :musn-log
- Scope: :session-log-only
- Fruit: `{:par EvidenceEntry}` — a PAR-shaped evidence entry
- Receives `:session-log` context; produces :reflection type evidence
- Entry from any peripheral (:from-any)

← round-trip test for each:
- Verify constraint adherence (test can't edit, deploy can't test, reflect can't edit)
- Verify evidence emission
- Verify fruit shape matches intended output

Criteria:
- [ ] Test: produces pass/fail/flaky result, reads only, runs tests only
- [ ] Deploy: git operations only, scoped to git-push-only
- [ ] Reflect: produces PAR-shaped evidence, read-only, session-log scope
- [ ] All three emit evidence entries
- [ ] ← test for each: output matches constraint spec
- [ ] 12+ tests (4+ each)

### Part IV: Integration — Hop Chain + Evidence Flow (Claude)

**Status:** Blocked on Parts II-III

:in  — All component files from Parts I-III
       src/futon3c/social/peripheral.clj (hop protocol)
       src/futon3c/evidence/store.clj
       src/futon3c/evidence/threads.clj
:out — test/futon3c/peripheral/integration_test.clj

End-to-end scenarios:

1. **Full hop chain**: explore → edit → test → deploy → reflect
   - Each peripheral starts with context from previous
   - Session-id preserved throughout
   - Evidence entries accumulate in store

2. **Evidence thread**: project a thread from the full chain's evidence
   - Thread has goal (explore started), steps (edits, tests), conclusion (deployed)
   - thread-patterns extracts patterns used across the chain
   - Proof-tree invariants hold

3. **Scope violation rejected**: edit attempts out-of-scope write → SocialError
   - ← test: the error IS the evidence that constraints are working

4. **Reframe scenario**: test fails → hop back to edit → fix → re-test → deploy
   - The hop-back is a reframe (A10): the expected exit (tests-pass) was wrong,
     so the agent returns to edit with new information

5. **Reflect produces mineable PAR**: reflect's output is an evidence entry
   that thread-patterns can read — patterns used, outcomes observed

Criteria:
- [ ] Full hop chain preserves session-id and context across 5 peripherals
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
- All five peripherals implement the protocol with correct tool/scope constraints
- Tool dispatch is pluggable (mock backend for tests, real backend deferred)
- Each peripheral step emits shape-valid EvidenceEntry
- Scope enforcement is structural (rejected by dispatch, not by convention)
- ← round-trip: for each peripheral, running it produces output consistent with its spec
- Full hop chain works end-to-end with evidence accumulation
- Peripheral evidence threads satisfy proof-tree invariants
- All new tests pass, existing 157 tests unaffected
- `clojure -X:test` passes cleanly
