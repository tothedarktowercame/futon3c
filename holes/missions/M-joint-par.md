# Mission: Joint PAR — Claude + Codex Paired Session with IRC Coordination

**Date:** 2026-02-19
**Status:** IDENTIFY (mission proposal)
**Depends on:** M-futon3-last-mile (seams 1-4 wired), M-IRC-stability (F1-F6)
**Exercises:** Multi-agent coordination, IRC relay, shared evidence, PAR synthesis

## Motivation

The self-evidencing trial proved a single agent can drive a peripheral
session over WS and produce a queryable evidence chain. But futon3c's
distinguishing claim is *multi-agent coordination with shared evidence*.
That claim is untested end-to-end.

This mission pairs Claude and Codex in a joint session where:
1. Both agents connect via WS and register with the relay bridge
2. They coordinate over IRC (`#futon-par`)
3. Each runs a peripheral session (explore, edit) against the live codebase
4. They produce a jointly authored PAR that synthesizes both perspectives
5. The PAR and all coordination evidence land in the evidence landscape

The result is the first real exercise of the tri-layer stack:
transport (IRC + WS) → peripherals (explore + edit) → evidence (shared
landscape with cross-agent reply chains).

### Why IRC for coordination?

IRC is the lowest-common-denominator protocol that both agents can use
without custom tooling. The relay bridge (`irc.clj`) already handles
the routing. Using IRC for the coordination channel makes the exchange
visible to humans (connect any IRC client to `localhost:6667` and watch
`#futon-par`), auditable (every message becomes a `:forum-post` evidence
entry), and testable (the existing M1-M16 + P-6 tests cover the transport).

### Why a joint PAR?

A PAR (Post-Action Review) is the natural unit of collaborative
reflection. Each agent has a different view of the session: Claude
explored the codebase, Codex edited files. Neither has the full picture.
The jointly authored PAR forces them to exchange perspectives, negotiate
what-worked / what-didn't, and produce a single artefact that represents
their shared understanding. This is the coordination problem in miniature.

## Scope

### In scope

1. **Dual-agent WS registration** — Claude and Codex register with the
   runtime, each with their default peripheral routing (explore, edit)

2. **IRC relay bridge** — Both agents join `#futon-par`. The relay
   bridge routes messages between them and emits `:forum-post` evidence

3. **Parallel peripheral sessions** — Claude runs an explore session
   (read, glob, grep against the futon3c codebase), Codex runs an edit
   session (write a small summary file based on Claude's findings)

4. **PAR coordination protocol** — After peripheral work completes,
   agents exchange PAR fragments over IRC:
   - Claude proposes `what-worked` and `what-didnt` from the explore
   - Codex proposes `what-worked` and `what-didnt` from the edit
   - One agent (Codex) synthesizes the joint PAR and emits it via
     `:par-punctuate`

5. **Evidence verification** — Query the evidence landscape and verify:
   - Two distinct peripheral session chains (goal→step*→conclusion)
   - IRC coordination messages as `:forum-post` entries
   - One joint PAR as a `:reflection` entry
   - Cross-agent `in-reply-to` threading

### Out of scope

- Live Claude/Codex CLI invocation (use mock invoke-fns that return
  scripted responses, or WS-connected real agents if available)
- Hop protocol (explore→edit within a single agent session)
- XTDB persistence (AtomBackend sufficient for the trial)
- Gate pipeline validation of the PAR

## Design

### Architecture

```
  IRC client (human observer)
       │
       ▼
  IRC Server (:6667)
       │
       ├──── relay-bridge ──┬─── WS agent: claude-1 (explore peripheral)
       │                    │
       │                    └─── WS agent: codex-1  (edit peripheral)
       │
  Evidence landscape (AtomBackend)
       │
       ▼
  HTTP API (:5060) — query evidence
```

### Phase 1: Setup (scripted)

```clojure
;; Register agents
(runtime/register-claude! {:agent-id "claude-1" :invoke-fn ...})
(runtime/register-codex!  {:agent-id "codex-1"  :invoke-fn ...})

;; Start IRC + relay bridge
(irc/start-irc-server! {:port 6667})
(irc/make-relay-bridge  {:agents {"claude-1" {...} "codex-1" {...}}
                          :evidence-store evidence-store})

;; Start HTTP + WS server
(start-server! combined-handler 5060)
```

### Phase 2: Peripheral work (WS frames)

**Claude (explore):**
```
→ peripheral_start {peripheral_id: "explore"}
→ tool_action {tool: "read", args: ["README.md"]}
→ tool_action {tool: "glob", args: ["src/futon3c/peripheral/*.clj"]}
→ tool_action {tool: "grep", args: ["defprotocol", "src/"]}
→ peripheral_stop {reason: "exploration-complete"}
```

**Codex (edit):**
```
→ peripheral_start {peripheral_id: "edit"}
→ tool_action {tool: "write", args: ["/tmp/futon3c-summary.md", "..."]}
→ peripheral_stop {reason: "edit-complete"}
```

### Phase 3: PAR coordination (IRC)

```
#futon-par <claude-1> PAR:what-worked: Peripheral read/glob/grep
  executed cleanly against live codebase. Evidence chain complete
  (1 goal, 3 steps, 1 conclusion). Self-referential exploration worked.

#futon-par <codex-1> PAR:what-worked: Edit peripheral accepted write
  to scoped path. Summary file produced from explore findings.

#futon-par <claude-1> PAR:what-didnt: Could not verify evidence
  persistence to XTDB (AtomBackend only). No cross-peripheral
  evidence linking yet (explore and edit chains are independent).

#futon-par <codex-1> PAR:what-didnt: Edit scope constraint meant
  summary had to go to /tmp, not into the repo docs/ directory
  (scoped edit requires pre-configured paths).

#futon-par <codex-1> PAR:synthesize: Emitting joint PAR now.
```

### Phase 4: Joint PAR emission

Codex calls `:par-punctuate` with merged content from both agents.
The critical structural detail: the PAR's `in-reply-to` references both
agents' conclusion entries, creating the crossing point in the evidence
graph.

```clojure
{:tool :par-punctuate
 :args [{:in-reply-to [claude-1-conclusion-id codex-1-conclusion-id]
         :what-worked "Dual peripheral sessions (explore+edit) executed
                       cleanly. IRC coordination produced visible,
                       evidence-backed exchange. Self-evidencing pattern
                       validated: the system documented its own functioning."
         :what-didnt  "XTDB persistence not exercised (AtomBackend only).
                       Edit scope constraint limits output location.
                       PAR negotiation was turn-based, not concurrent."
         :suggestions ["Add XTDB variant of trial for durable persistence"
                       "Consider hop protocol to unify explore→edit into
                        single evidence chain"
                       "Explore CRDT-based concurrent PAR editing in Emacs"]
         :contributors [{:agent-id "claude-1"
                         :contributed [:what-worked :what-didnt]
                         :via :irc}
                        {:agent-id "codex-1"
                         :contributed [:what-worked :what-didnt :suggestions]
                         :via :irc}]
         :synthesized-by "codex-1"
         :coordination-channel "#futon-par"
         :mission-id mid}]}
```

### Phase 5: Evidence verification

Query the evidence landscape and assert:

| Assertion | Source |
|-----------|--------|
| >= 5 entries for Claude's explore session | peripheral evidence |
| >= 3 entries for Codex's edit session | peripheral evidence |
| >= 4 `:forum-post` entries from IRC coordination | IRC relay evidence |
| 1 `:reflection` entry (joint PAR) | par-punctuate evidence |
| All IRC entries have subject `{:ref/type :thread :ref/id "irc/#futon-par"}` | IRC relay |
| PAR entry has `:what-worked`, `:what-didnt`, `:suggestions` in body | PAR shape |
| PAR entry has subject `{:ref/type :mission :ref/id mid}` | mission subject |
| PAR `in-reply-to` contains both agents' conclusion entry IDs | **crossing point** |
| Query by mission-id returns entries from both sessions + IRC + PAR | graph convergence |

## Implementation

### Script: `scripts/joint_par_trial.clj`

Standalone script (like `self_evidence_trial.clj`) that boots its own
infrastructure, drives both agents programmatically, and verifies the
evidence landscape. No external dependencies beyond `make dev` for
optional XTDB persistence.

### Test: `test/futon3c/transport/joint_par_test.clj`

Integration test using mock send-fn/close-fn (like ws_peripheral_test.clj)
that verifies the full protocol without starting real servers. Tests
the evidence chain assertions from Phase 5.

## Success criteria

1. Both agents complete peripheral sessions with no errors
2. IRC coordination messages visible in evidence landscape
3. Joint PAR lands as a `:reflection` entry with both agents' perspectives
4. Evidence query returns all expected entries, correctly typed and tagged
5. A human IRC client connected to `#futon-par` can observe the exchange
   in real time

## Design decisions

### D1. Session-ids stay separate; mission-id links contributors

Each agent's peripheral session keeps its own session-id — sessions are
per-connection, and conflating them would break the evidence chain semantics
(a session is one agent's continuous lifecycle).

Instead, a **mission-id** groups the work. At mission start, emit a
`:coordination` evidence entry on subject `{:ref/type :mission :ref/id mid}`:

```clojure
{:evidence/id (str "mission-" mid)
 :evidence/subject {:ref/type :mission :ref/id mid}
 :evidence/type :coordination
 :evidence/claim-type :goal
 :evidence/author "mission-orchestrator"
 :evidence/body {:event :mission-start
                 :mission-id mid
                 :contributors [{:agent-id "claude-1" :role :explore}
                                {:agent-id "codex-1"  :role :edit}]
                 :channel "#futon-par"}
 :evidence/tags [:mission :joint-par]}
```

Each agent's peripheral session then includes `:mission-id mid` in its
context, which flows into evidence entries. Querying by mission-id
returns all entries across both sessions + IRC coordination + the PAR.

`:mission` is already in `ArtifactRefType`, so no shape changes needed
for the subject reference. The contributors list lives in the mission
goal's body — lightweight registry, no new infrastructure.

### D2. PAR is a co-authored artifact, attributed as such

Each agent's evidence chain is linear: `goal → step* → conclusion`.
The chains are independent — they live on separate session subjects and
don't reference each other. The joint PAR is where those independent
linear histories **cross**:

```
claude-1:  goal → step → step → step → conclusion ─┐
                                                     ├──→ joint PAR
codex-1:   goal → step → conclusion ────────────────┘
                (session subject)              (mission subject)
```

The PAR lives on the mission subject (`{:ref/type :mission :ref/id mid}`),
not on either agent's session subject. Its `in-reply-to` links reach
back into both linear chains — specifically, each agent's conclusion
entry (or their final IRC contribution). This makes the PAR the
**topological junction** in the evidence graph: the node where two
independent histories converge.

This is not just attribution metadata. The crossing *is* the evidence
of coordination. Without it, you have two agents that happened to run
at the same time. With it, you have a shared object that proves their
histories were entangled — each agent's conclusion feeds the PAR, and
the PAR cannot exist without both chains completing.

The authoring process happens on IRC (or in future via CRDT in Emacs),
making the negotiation visible and auditable as `:forum-post` evidence.
These IRC entries form a secondary crossing: they're on the IRC thread
subject (`{:ref/type :thread :ref/id "irc/#futon-par"}`), interleaving
messages from both agents, creating another shared object that both
linear histories pass through.

The PAR evidence entry carries:
- `:evidence/author` — the agent that emitted the final synthesis
  (whoever called `:par-punctuate`)
- `:evidence/in-reply-to` — references to both agents' conclusion
  entries (the structural crossing)
- `:par/contributors` in the body — all agents who contributed, with
  what they contributed:

```clojure
{:evidence/subject {:ref/type :mission :ref/id mid}
 :evidence/in-reply-to ["claude-1-conclusion-id" "codex-1-conclusion-id"]
 :evidence/body
 {:what-worked "..."
  :what-didnt "..."
  :suggestions [...]
  :contributors [{:agent-id "claude-1"
                  :contributed [:what-worked :what-didnt]
                  :via :irc}
                 {:agent-id "codex-1"
                  :contributed [:what-worked :what-didnt :suggestions]
                  :via :irc}]
  :synthesized-by "codex-1"
  :coordination-channel "#futon-par"}}
```

Querying by mission-id returns the full diamond: two independent chains
converging at the PAR. Querying by either session-id returns just one
chain. The graph structure emerges from the references — no special
"joint" type needed, just standard `in-reply-to` links that happen to
cross session boundaries.

### D3. Scripted vs. live: open question

Both have value, and neither subsumes the other:

- **Scripted**: deterministic, CI-runnable, tests the protocol not the
  agents. Good for verifying the infrastructure works.
- **Live**: tests real agent behavior, but non-deterministic and slow.
  Good for validating the coordination is actually useful.

The script (`joint_par_trial.clj`) should support both modes via a
flag. Start with scripted for the initial wiring, graduate to live
when the protocol is stable. The evidence landscape doesn't care
which mode produced the entries — it's the same shapes either way.

## Open questions

1. **Mission-id propagation:** How does the mission-id flow into
   peripheral evidence? Options: (a) include in the WS `peripheral_start`
   frame and thread through context, (b) set it in the evidence store
   as ambient context, (c) post-hoc tag via a query+update pass.
   Option (a) is cleanest.

2. **CRDT PAR editing:** For the Emacs path, the PAR would be a shared
   buffer that both agents edit (via CRDT or turn-taking). The evidence
   entry is emitted when the buffer is "committed." This is future work
   but the mission should not preclude it.

3. **IRC vs. peripheral for PAR negotiation:** Should the PAR negotiation
   happen as IRC messages (visible, auditable) or as tool actions within
   a shared peripheral (structured, typed)? IRC is simpler and already
   wired. A "par-negotiation" peripheral would be more structured but
   is new infrastructure.

## References

- `scripts/self_evidence_trial.clj` — single-agent trial pattern
- `scripts/irc_claude_relay.clj` — Claude IRC relay pattern
- `scripts/irc_codex_relay.clj` — Codex IRC relay pattern
- `src/futon3c/transport/irc.clj` — IRC server + relay bridge
- `src/futon3c/peripheral/reflect.clj` — Reflect peripheral + PAR
- `src/futon3c/peripheral/real_backend.clj` — par-punctuate tool
- `test/futon3c/transport/irc_test.clj` — IRC stability tests (M1-M16, P-6)
- `holes/missions/M-futon3-last-mile.md` — predecessor mission
- `holes/missions/M-IRC-stability.md` — IRC failure modes + fixes
