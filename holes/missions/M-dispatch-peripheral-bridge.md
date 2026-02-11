# Mission: Dispatch–Peripheral Bridge

## Derivation

INSTANTIATE step, wiring the interior of S-dispatch.

Prior:
- M-agency-refactor: social pipeline complete (presence → authenticate → dispatch → persist)
- M-peripheral-model: specs + hop protocol complete (5 peripherals, validate-hop, transfer-context)
- M-peripheral-behavior: all behavioral code complete (runner, tools, evidence, 5 implementations,
  ← verification, registry, adapter, chain orchestration, integration tests)
- social-exotype.edn: S-dispatch specified as "route within constraints" + session state mutation
- futon-theory/reverse-morphogenesis.flexiarg: ← operator, constraint-from-fruit

The AIF+ analysis (futon5/docs/chapter0-aif-as-wiring-diagram.md) reveals that the
dispatch→peripheral gap is not a missing component in the social exotype — it is the
**interior wiring of S-dispatch itself**, which is abstractly specified but only
half-concretized.

Currently:
- dispatch.clj concretizes the message-routing half (ClassifiedMessage → invoke-agent! → DispatchReceipt)
- peripheral/registry.clj concretizes the constrained-execution half (run-chain → hop validation → evidence)
- Nothing connects them: dispatched messages never enter peripheral sessions, and
  peripheral fruits never flow back through dispatch

## Why This Mission Exists

Three completed missions form a triangle with one missing edge:

```
M-agency-refactor              M-peripheral-model
(dispatch pipeline)    ←────→   (specs + hop protocol)
       │                              │
       │     ✗ no bridge ✗            │
       │                              │
       └──────────────────────────────┘
                    │
           M-peripheral-behavior
           (implementations + chain)
```

M-peripheral-model connects to both (it defines the shapes dispatch uses AND the specs
peripherals implement). But dispatch and peripherals never meet. dispatch.clj calls
`reg/invoke-agent!` with a payload. Peripherals expect `run-chain` with structured
`{:tool :kw :args [...]}` actions and a session-id.

Seven specific gaps collapse into one architectural finding: **S-dispatch's interior
needs to route action-mode messages into peripheral sessions and return the fruit.**

### AIF+ Reading

The social exotype says S-dispatch:
- "Routes typed coordination events to recipients with delivery receipts" (R1)
- "The ONLY component that mutates session state" (I2)
- Produces `:http-request` to O-task-submissions (social→task boundary)
- Reads/writes `:xtdb-entity` from/to I-session-state

The peripheral system IS how S-dispatch "routes within constraints." The constraints
are the peripheral specs. The evidence entries are the structured events (R9). The
fruit is the coordination outcome that feeds S-validate.

### What the Invariants Say About Size

- **I1 (Boundary):** Needs one typed interface at the dispatch→peripheral surface.
  Two orphaned ports (`:msg/mode` unused; peripheral evidence unlinked to receipts).
- **I2 (Observation-Action):** Currently two independent session-state mutation paths
  (invoke-agent! and evidence store). Should be one. Single concern.
- **I3 (Timescale):** No issue. Both operate at `:social`. No new timescale.
- **I4 (Exogeneity):** No issue. Neither writes to constraint inputs. No new risk.
- **I6 (SPOF):** No new SPOF. Dispatch and peripherals already both reach outputs.

Conclusion: **one mission, three parts.** Route selection, session lifecycle, integration.

## What This Mission Produces

1. **PeripheralDispatch** — route action-mode messages into peripheral sessions
2. **SessionLifecycle** — create/join/close sessions at the dispatch boundary
3. **DispatchReceipt extension** — receipt carries session-id and peripheral fruit
4. **Evidence linkage** — dispatch emits a root evidence entry that peripheral evidence replies to
5. **Mode-aware routing** — `:msg/mode :action` enters a peripheral; `:coordination` routes directly

## Scope In

- Route selection: `:msg/mode` determines whether message enters peripheral or direct-invoke
- Session creation: dispatched action-mode message → session-id → peripheral context
- Peripheral selection: agent type + message metadata → which peripheral to start in
- Fruit return: peripheral chain result → enriched DispatchReceipt
- Evidence linkage: dispatch emits root EvidenceEntry; peripheral evidence chains reply to it
- DispatchReceipt shape extension: optional `:receipt/session-id`, `:receipt/fruit`

## Scope Out

- Transport (HTTP/WebSocket): messages arrive pre-classified via S-mode
- Real agent invocation: still uses MockBackend for tests, real backends deferred
- S-validate wiring: the coordination outcome feeds S-validate, but that wiring is S-validate's concern
- S-default: the default-mode parallel path is a separate concern (I6 closure)
- Multi-agent sessions: one agent per peripheral session for now
- Persistent session store: sessions live in atoms for this mission; durable store deferred

## Conceptual Model

### S-dispatch Interior Decomposition

S-dispatch (the abstract social exotype component) decomposes into:

```
ClassifiedMessage
       │
       ├─ :msg/mode :coordination ──→ direct-invoke (existing dispatch.clj path)
       │                                      │
       │                                      └──→ DispatchReceipt
       │
       └─ :msg/mode :action ──→ peripheral-dispatch (NEW)
                                      │
                                      ├─ select-peripheral (agent-type + metadata → peripheral-id)
                                      ├─ create-or-join-session → session-id
                                      ├─ emit root evidence entry (claim-type :goal)
                                      ├─ run peripheral (registry/run-chain or single)
                                      ├─ collect fruit + evidence
                                      └──→ DispatchReceipt + session-id + fruit
```

The coordination path is unchanged — `invoke-agent!` continues to handle direct
message delivery. The action path is new — it enters the peripheral system.

### Mode-to-Peripheral Mapping

`:msg/mode` is classified by S-mode but currently unused by S-dispatch. This mission
makes it load-bearing:

| Mode | Routing | Session | Evidence |
|------|---------|---------|----------|
| `:coordination` | Direct invoke (existing) | No peripheral session | Receipt only |
| `:action` | Peripheral dispatch (new) | Creates/joins session | Receipt + root evidence + chain evidence |

### Peripheral Selection

Which peripheral does an action message start in? Not hardcoded:

| Agent Type | Default Peripheral | Rationale |
|------------|-------------------|-----------|
| `:claude` | `:explore` | Claude starts by understanding before changing |
| `:codex` | `:edit` | Codex receives specific edit instructions |
| `:mock` | configurable | Test flexibility |

Override via `:msg/payload {:peripheral :edit}` for explicit peripheral targeting.

### Session Lifecycle

```
create-session [dispatch-context] → {:session/id "s-..." :session/peripheral-id :explore ...}
join-session [session-id action-msg] → existing session context
close-session [session-id fruit evidence] → final session record
```

Sessions are created at dispatch time, not peripheral time. This ensures the
session-id exists before the peripheral starts, so the root evidence entry
(emitted by dispatch) can reference it, and all peripheral evidence chains
reply to that root.

### Evidence Linkage

Currently: peripheral evidence starts with a `:goal` entry from `make-start-evidence`.
The dispatch message that triggered it is invisible.

After this mission: dispatch emits a `:dispatch` evidence entry BEFORE the peripheral
starts. The peripheral's `:goal` entry has `:evidence/in-reply-to` pointing at the
dispatch entry. This makes the full chain: dispatch → explore:goal → explore:steps →
explore:conclusion → edit:goal → ... visible as one thread.

```clojure
;; Dispatch root evidence
{:evidence/type     :coordination
 :evidence/claim-type :goal
 :evidence/body     {:event :dispatch
                     :msg/id "m-123"
                     :peripheral-id :explore
                     :session-id "s-456"}
 :evidence/tags     [:dispatch :session-start]}

;; Peripheral start evidence (replies to dispatch)
{:evidence/type     :coordination
 :evidence/claim-type :goal
 :evidence/body     {:peripheral :explore :event :start}
 :evidence/in-reply-to "e-dispatch-root-id"
 ...}
```

## Parts

### Structure: Linear (3 parts)

```
Part I: Route Selection + Session Lifecycle (Claude)
                    │
Part II: Peripheral Dispatch + Evidence Linkage (Claude or Codex)
                    │
Part III: Integration + Existing Test Preservation (Claude)
```

### Part I: Route Selection + Session Lifecycle

**Status:** Ready

:in  — src/futon3c/social/dispatch.clj (MODIFY — add mode-aware routing)
       src/futon3c/social/shapes.clj (MODIFY — extend DispatchReceipt, add SessionRecord fields)
       src/futon3c/social/mode.clj (READ-ONLY — understand classification)
       src/futon3c/agency/registry.clj (READ-ONLY — understand invoke-agent!)
       resources/peripherals.edn (READ-ONLY)
:out — src/futon3c/social/session.clj (NEW — session lifecycle)
       test/futon3c/social/session_test.clj (NEW)

**Session lifecycle** (`session.clj`):

```clojure
(defn create-session
  "Create a new peripheral session from a dispatch context.
   Returns {:session/id \"s-...\" :session/peripheral-id :explore
            :session/agent-id TypedAgentId :session/created-at Instant}
   or SocialError."
  [agent-id peripheral-id]
  ...)

(defn session-context
  "Build the context map a peripheral needs from a session.
   Returns {:session-id \"s-...\" ...}"
  [session]
  ...)

(defn close-session
  "Close a session with fruit and evidence summary.
   Returns updated session record or SocialError."
  [session-store session-id fruit evidence-summary]
  ...)
```

**DispatchReceipt extension** (shapes.clj):

```clojure
;; Add optional fields to DispatchReceipt:
[:receipt/session-id {:optional true} :string]
[:receipt/peripheral-id {:optional true} PeripheralId]
[:receipt/fruit {:optional true} :any]
```

**Route selection logic** — pure function, not yet wired into dispatch:

```clojure
(defn select-route
  "Given a ClassifiedMessage and agent record, determine routing.
   Returns {:route :direct :target agent-id}
   or {:route :peripheral :peripheral-id :explore :session-id \"s-...\"}."
  [classified-message agent-record]
  ...)

(defn select-peripheral
  "Given agent type and message metadata, choose starting peripheral.
   Returns PeripheralId."
  [agent-type message-metadata]
  ...)
```

Criteria:
- [ ] create-session produces shape-valid session records
- [ ] session-context builds correct peripheral start context
- [ ] close-session records fruit and evidence summary
- [ ] select-route uses `:msg/mode` to choose direct vs peripheral
- [ ] select-peripheral uses agent type with metadata override
- [ ] DispatchReceipt shape extended with optional session/peripheral/fruit fields
- [ ] Existing dispatch tests still pass (shape extension is additive)
- [ ] 8+ tests (session CRUD, route selection, peripheral selection)

### Part II: Peripheral Dispatch + Evidence Linkage

**Status:** Blocked on Part I

:in  — src/futon3c/social/dispatch.clj (MODIFY — wire peripheral path)
       src/futon3c/social/session.clj (READ-ONLY, from Part I)
       src/futon3c/peripheral/registry.clj (READ-ONLY — run-chain, make-peripheral)
       src/futon3c/peripheral/adapter.clj (READ-ONLY — tool mapping, prompt section)
       src/futon3c/peripheral/evidence.clj (READ-ONLY — evidence helpers)
       src/futon3c/evidence/store.clj (READ-ONLY — append*)
:out — src/futon3c/social/dispatch.clj (MODIFIED — peripheral-dispatch added)
       test/futon3c/social/dispatch_test.clj (MODIFIED — new tests for peripheral path)

**Peripheral dispatch** — the new action-mode path inside dispatch:

```clojure
(defn peripheral-dispatch
  "Dispatch an action-mode message through the peripheral system.
   1. Select peripheral (select-peripheral from Part I)
   2. Create session (create-session from Part I)
   3. Emit root evidence entry
   4. Run peripheral via registry/run-single or registry/run-chain
   5. Close session with fruit
   6. Return enriched DispatchReceipt

   config: {:backend ToolBackend, :peripherals loaded-map, :evidence-store atom, :session-store atom}
   Returns DispatchReceipt (with :receipt/session-id, :receipt/fruit) or SocialError."
  [classified-message agent-record config]
  ...)
```

**Root evidence emission:**

```clojure
(defn- emit-dispatch-evidence
  "Emit the root evidence entry for a peripheral dispatch.
   This entry becomes the in-reply-to target for the peripheral's start evidence."
  [session-id msg-id peripheral-id agent-id evidence-store]
  ...)
```

**Modified dispatch function** — the existing `dispatch` function gains a branch:

```clojure
;; Inside dispatch:
(if (= :action (:msg/mode classified-message))
  (peripheral-dispatch classified-message agent-record config)
  ;; existing direct-invoke path
  (let [resp (reg/invoke-agent! target (coerce-prompt (:msg/payload classified-message)))]
    ...))
```

The existing coordination path is untouched. The new peripheral path
produces the same DispatchReceipt shape (with optional extra fields).

Criteria:
- [ ] peripheral-dispatch creates session, runs peripheral, returns enriched receipt
- [ ] Root evidence entry emitted before peripheral starts
- [ ] Peripheral's start evidence has :in-reply-to pointing to dispatch root
- [ ] DispatchReceipt for action messages includes :receipt/session-id and :receipt/fruit
- [ ] DispatchReceipt for coordination messages unchanged (backwards compatible)
- [ ] Evidence store accumulates dispatch root + all peripheral evidence
- [ ] SocialError from peripheral propagates correctly through dispatch
- [ ] 8+ tests (peripheral dispatch happy path, error propagation, evidence linkage,
      coordination-mode still works, receipt shape validation)

### Part III: Integration + Existing Test Preservation

**Status:** Blocked on Part II

:in  — All files from Parts I, II
       src/futon3c/social/pipeline.clj (READ-ONLY — understand full pipeline)
       src/futon3c/peripheral/round_trip.clj (READ-ONLY — ← verification)
       src/futon3c/evidence/threads.clj (READ-ONLY — thread projection)
       test/futon3c/social/pipeline_test.clj (READ-ONLY — existing pipeline tests)
       test/futon3c/peripheral/integration_test.clj (READ-ONLY — existing peripheral tests)
:out — test/futon3c/social/dispatch_integration_test.clj (NEW)

End-to-end scenarios:

1. **Full pipeline → peripheral → receipt**: S-mode classifies action message →
   S-dispatch routes into explore peripheral → peripheral produces fruit →
   enriched DispatchReceipt returned with session-id and fruit

2. **Coordination mode bypasses peripherals**: S-mode classifies coordination
   message → S-dispatch invokes agent directly → standard DispatchReceipt
   (no session-id, no peripheral)

3. **Evidence thread spans dispatch + peripheral**: dispatch root entry →
   peripheral goal → steps → conclusion form one projectable thread.
   Thread projection works. Proof-tree invariants hold.

4. **Peripheral error propagates as SocialError**: peripheral scope violation
   inside a dispatched session → SocialError at dispatch boundary with
   :error/component :S-dispatch

5. **Session lifecycle**: create at dispatch, close after peripheral completes,
   session record has fruit summary

6. **← verification on dispatched peripheral**: run-and-verify on the evidence
   from a dispatched session passes constraint check

Criteria:
- [ ] Full pipeline → peripheral → receipt works end-to-end
- [ ] Coordination mode unchanged (regression test)
- [ ] Evidence thread spans dispatch + peripheral boundaries
- [ ] Proof-tree invariants hold on dispatch+peripheral thread
- [ ] Peripheral errors surface as dispatch SocialErrors
- [ ] ← round-trip passes on dispatched peripheral sessions
- [ ] All 254 existing tests still pass
- [ ] 6+ integration tests
- [ ] `clojure -X:test` passes cleanly

## Relationship to Other Missions

- **M-agency-refactor**: Built S-dispatch as message routing. This mission adds the
  peripheral path inside S-dispatch. Existing coordination path unchanged.
- **M-peripheral-behavior**: Built the peripheral system (runner, tools, evidence,
  implementations, chain orchestration). This mission calls it from dispatch.
- **M-peripheral-model**: Built specs and hop protocol. This mission's route selection
  uses the specs; peripheral dispatch uses the hop protocol via registry.
- **M-forum-refactor**: Evidence landscape is the medium. Dispatch emits root evidence;
  peripheral evidence chains reply to it. Thread projection spans both.

## What This Unlocks

With this bridge in place:
- A message arriving through the social pipeline can enter a constrained peripheral session
- The full evidence chain (dispatch → peripheral → fruit) is projectable as one thread
- S-validate (future) can validate coordination outcomes that include peripheral fruits
- S-default (future) can check for pending peripheral sessions during inter-coordination
- Interactive Claude sessions (fuclaude) can route through dispatch into peripheral mode
- The social exotype diagram's S-dispatch component is fully concretized

## Exit Conditions

- Action-mode messages route into peripheral sessions via dispatch
- Coordination-mode messages route directly (existing behavior preserved)
- DispatchReceipt shape extended with optional session-id, peripheral-id, fruit
- Session lifecycle: create at dispatch, close after peripheral completes
- Root evidence entry links dispatch to peripheral evidence chain
- Evidence threads span dispatch + peripheral boundaries
- Proof-tree invariants hold on combined evidence
- ← verification passes on dispatched peripheral sessions
- All existing tests (254) unaffected
- `clojure -X:test` passes cleanly
