# TN-federated-whistle

**Technical note — deferred design for request/response coordination across
Agency hosts.** Recorded 2026-11-13 after the federation proxy attempted to
resume codex-2 while its local turn still owned the thread store. Status:
analysis captured; no federated-whistle implementation authorized yet.

## Preceding bell repair

Federation used one surface-blind proxy invoke function for every cross-host
turn. Its HTTP fallback posted directly to `/api/alpha/invoke`, bypassing the
origin host's per-agent turn queue. During the incident, a proxy resume of
thread `01a1d68e-95f8-7a71-8feb-572c3e981cb2` met the real codex-2 process
already writing that thread. Codex's single-writer guard rejected the proxy;
the original turn continued and completed.

Commits `3d92d5a67` and `d38591822` repair the bell case. The queued job engine
now identifies the dispatch kind explicitly, and the HTTP federation proxy
posts bell jobs to the origin's `/api/alpha/bell`. The origin therefore owns
admission, durable FIFO ordering, execution, and its thread writer. Non-bell
dispatch remains synchronous for now so the bell repair does not silently
turn whistles into fire-and-forget messages.

This repair must be deployed on every sending host. Reloading one host changes
only the proxy functions constructed by that host; it cannot correct stale
code on a peer.

## Current whistle paths

A whistle is an ask: its caller waits for a response, unlike a bell, which is
accepted asynchronously.

The local HTTP `/api/alpha/whistle` path already has the useful shape. It:

1. creates a canonical invoke job;
2. admits it through the target's durable turn queue;
3. waits up to the caller's response deadline;
4. returns the response when the job finishes; and
5. returns HTTP 504 with `job-id` and `status-url` when the wait expires while
   leaving the work alive and pollable.

Federation does not preserve that shape:

- The HTTP proxy sends non-bell work to `/api/alpha/invoke`. A federated
  whistle is therefore synchronous but bypasses the target host's queue.
- The WS uplink sends a `fed_invoke` frame. The receiving host calls
  `registry/invoke-agent!` directly, also bypassing the target queue.
- The WS sender stores `invoke-id -> promise` only in an in-memory atom. A
  socket or process loss removes the rendezvous even if the target work later
  completes.
- `fed_invoke` does not carry a dispatch kind, durable source request identity,
  or target job identity. Its terminal result frame cannot be reconciled after
  reconnect.

Thus the current cross-host whistle has both hazards: it can collide with an
active target turn, and its caller can lose the answer when either transport
endpoint disappears.

## Required invariants

1. **One admission authority.** Every target turn, including a federated
   whistle, enters the target host's ordinary per-agent queue. A federation
   transport never resumes or invokes the target thread directly.
2. **Target-owned execution.** The target host owns job state, queue position,
   execution, terminal result, and thread persistence.
3. **Source-owned waiting.** The source host owns the caller's wait and its
   durable reference to the target job. Ending a wait does not end the work.
4. **Idempotent submission.** Retrying after a lost acknowledgement resolves
   to the same target job rather than creating another turn.
5. **Transport-independent recovery.** HTTP timeout, WS reconnect, or either
   host restarting does not erase the relation between source request and
   target job.
6. **Terminal results are replayable.** A terminal target result may be read
   again until the source records delivery; it is not consumed by the first
   result message.
7. **Messages carry deltas, not authority.** Frames may announce acceptance,
   progress, or terminal state. Durable state remains in each host's substrate.

## Proposed two-phase protocol

### 1. Submit

The source creates and persists a globally unambiguous `request-id`, then
sends:

```clojure
{:type :federated-whistle/submit
 :request-id "<source-site>/<durable-id>"
 :source-site "ams"
 :target-agent "lon-claude-4"
 :prompt "..."
 :caller "codex-9"
 :response-deadline-ms 600000}
```

The target atomically maps `request-id` to one canonical invoke job and queues
that job through the same service used by `/api/alpha/whistle`. Repeated
submission returns the existing mapping.

The target replies with acceptance, not an execution result:

```clojure
{:type :federated-whistle/accepted
 :request-id "<source-site>/<durable-id>"
 :target-job-id "invoke-..."
 :state :queued}
```

The source durably records at least
`{request-id, target-site, target-agent, target-job-id, state}` before telling
its caller that submission succeeded.

### 2. Observe and deliver

The source may keep a WS subscription for low-latency state deltas, but it
must also be able to poll the target job by `target-job-id`. Reconnection
replays the source's unresolved records and asks the target for current state.
The target answers from its job ledger; it does not restart execution.

On a terminal target state, the source records the result and delivery status
locally, then acknowledges delivery to the target. An acknowledgement may
govern retention, but must not be the only copy of the result.

The caller's synchronous deadline is a view over this durable exchange. If it
expires, return a local pollable handle containing both identities. The
cross-host job continues and can still release a parked caller later.

## Relation to parking

A whistle and a park are separate records. The whistle creates a target job;
the source agent may park on the durable source request or its mapped target
job. Completion reconciliation must call the source host's ordinary
`parked-on/note-completion!` path exactly once. A terminal message received
twice after reconnect must not resume the parked agent twice.

This also means a target-host result must not directly manipulate a park on
another host. It changes the source whistle record; the source substrate then
performs its local park transition.

## Decisions deliberately deferred

- Whether the durable source record belongs in the invoke ledger, a dedicated
  federation exchange ledger, or a typed extension of the promise substrate.
- The precise HTTP resources and WS frame vocabulary.
- Authentication and authorization for cross-host submission, observation,
  result retrieval, and acknowledgement.
- Result-retention and garbage-collection bounds after delivery.
- Whether progress events are retained remotely or only terminal state is
  replayable.
- How cancellation authority crosses hosts without treating a response
  deadline as cancellation.

## Acceptance cases for later implementation

1. A whistle sent to an agent already executing remains queued and never
   attempts a second writer for that agent's thread.
2. Losing the submission acknowledgement and retrying creates one target job.
3. Dropping the WS connection after acceptance, then reconnecting, returns the
   same target job and eventual result.
4. Restarting the source after acceptance reconstructs its unresolved wait
   from durable state.
5. Restarting the target after queue acceptance drains the preserved job once.
6. A response deadline returns a pollable overrun handle while the target work
   continues.
7. Duplicate terminal deltas record and resume the source caller exactly once.
8. A bell still returns after target acceptance and never waits for whistle
   completion semantics.

## Resume point

Before implementation, choose the durable source record and write the state
transition table for submit, accept, observe, terminal, deliver, acknowledge,
timeout, reconnect, and cancellation. Then make both HTTP proxy and WS uplink
adapters call one target-side queue service; neither adapter should duplicate
the invoke lifecycle.
