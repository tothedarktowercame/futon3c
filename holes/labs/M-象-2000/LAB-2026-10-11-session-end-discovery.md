# HOLE 18 discovery: where a session's end becomes observable

Date: 2026-10-11. Scope: read-only source inspection and GET probes. No live
state was changed.

## Result

A session end is not presently a first-class observable event. The Agency
registry knows an old session ID at reset time, and it can infer replacement
when an invoke or external-status report supplies a different ID, but it stores
only the new roster projection. There is no append-only evidence record or
session-change callback. Consumers that do not synchronously call reset must
poll and retain their own previous roster; deregistration and TTL reaping make
even that observation lossy.

The right seam is a registry lifecycle hook/outbox emitted atomically with a
successful old-to-new session transition and consumed by the turn service.
The turn service should append a session-end envelope plus a targeted `:lapse`
act for every still-open agent-authored port. Operator-authored ports must not
be targeted by a closing act; they remain open and receive a presentation mark
of `:orphaned` or `:unread` based on a new per-session marked-reply-paragraph
count.

## 1. Where session end is observable today

### Registry mutations

| Path | Registry write and timestamp | Previous session retained? |
|---|---|---|
| Registration | `register-agent!` installs the supplied current `:agent/session-id` and stamps both `:agent/registered-at` and `:agent/last-active` with one `now` value (`src/futon3c/agency/registry.clj:550-581`). | No history; this establishes the current value. |
| Generic update | `update-agent!` accepts `:agent/session-id`, merges it into the record, and auto-stamps `:agent/last-active (now)` unless explicitly supplied (`registry.clj:675-705`). | No. The old value is overwritten and is not returned as a transition. |
| Invoke completion | The private `mark-idle!` stores the invoke result's session ID (falling back to the session captured before invoke), stamps `:agent/last-active (now)`, and clears invoke state (`registry.clj:1088-1103`). This is where a newly created/resumed session normally enters the roster. | No comparison or old value is retained. `!on-invoke-complete` and `!on-idle` exist (`registry.clj:42-52,253-274,1190,1265-1266`), but their payload is an invoke outcome, not an atomic old/new session transition. |
| Explicit reset | `reset-session!` captures `old-sid`, calls the backend reset function, then stores `:agent/session-id nil`, stamps `:agent/last-active (now)`, and clears external invoke state (`registry.clj:931-980`). | Only in the synchronous return map as `:old-session-id`; not in registry/evidence. The timestamp is not returned. |
| External status | While status is `:invoking`, `report-external-invoke!` may overwrite `:agent/session-id` and stamps both external `:updated-at` and `:agent/last-active` from `now*` (`registry.clj:1462-1534`). An idle report clears external state but does not clear the registered session. | No. |
| Deregistration | `unregister-agent!` dissociates the entire record (`registry.clj:640-659`); the compatibility wrapper returns only the agent ID (`registry.clj:663-673`). | No session ID and no end timestamp survive or appear in the response. |
| TTL expiry | `reap-expired!` dissociates expired records using wall-clock milliseconds (`registry.clj:1278-1298`). | No; its return is only reaped typed agent IDs. |

The public `mark-agent-idle!` used in HTTP cleanup only changes invoke status
and `last-active`; it does not change `session-id`
(`registry.clj:1357-1378`).

### HTTP routes

- `POST /api/alpha/agents/:id/reset-session` directly exposes the ephemeral
  `old-session-id` returned by `reset-session!`, but no transition timestamp
  (`src/futon3c/transport/http.clj:7624-7643`). This is the only endpoint where
  the old ID is directly observable after the call.
- `DELETE /api/alpha/agents/:id` calls `unregister-agent!` and responds with
  only agent ID plus `deregistered: true` (`http.clj:7608-7622`).
- `POST /api/alpha/agents/:id/status` can set the current session through
  `report-external-invoke!` while reporting `invoking`; its response does not
  contain old/new session IDs or a timestamp (`http.clj:7645-7665`).
- Registration/auto-registration accepts an initial session ID and may call
  `update-agent!` to install it (`http.clj:4539-4675`). Restore/rebind chooses
  an effective current ID and overwrites the registry projection
  (`http.clj:4686-4872`). Neither route records a replaced ID as a lifecycle
  event.
- `POST /api/alpha/agents/:id/compact` is not a session-end mutation. The cold
  result may report `:session-id` (`http.clj:7774-7787`), but the queued compact
  path invokes `/compact` with the current ID and finally calls only
  `mark-agent-idle!`; it does not install a returned ID
  (`http.clj:7799-7849`). Its finalizer prints the result to stdout, which is
  diagnostic output, not a lifecycle contract.

### Durable roster projection

The registry watch rewrites a complete roster snapshot on every atom mutation
(`src/futon3c/agency/roster_store.clj:242-260`). A snapshot has one
snapshot-level `:generated-at` (`roster_store.clj:120-127`), and each restore
payload contains only the current non-nil `session-id`
(`roster_store.clj:70-105`). Persistence rotates one `.bak` before replacement
(`roster_store.clj:179-207`), but that backup is incidental recovery state,
not an ordered event stream or supported previous-session field. Thus a reset
is durably visible only as absence of the current ID, deregistration as absence
of the whole agent, and replacement as the new ID; none carries a semantic
`session-ended-at`.

### Live GET probe

Read-only probe:

```console
$ curl -sS http://127.0.0.1:7070/api/alpha/agents \
    | jq '{ok,count,agent:.agents["claude-16"]}'
{
  "ok": true,
  "count": 127,
  "agent": {
    "session-id": "00e653e3-28f7-4add-8304-47ca0475b5c2",
    "status": "idle",
    "mission-id": "M-象-2000",
    "last-active": "2026-10-11T16:57:48.900406426Z",
    "registered-at": "2026-10-11T02:43:53.635705904Z",
    "type": "claude",
    "id": {"id/value": "claude-16", "id/type": "continuity"},
    "capabilities": ["explore", "edit", "test", "coordination/execute"],
    "campaign-id": null,
    "excursion-id": null,
    "metadata": {"session-file": "/tmp/futon-session-id-claude-16", "...": "..."},
    "...": "routing and delivery fields"
  }
}
```

The actual payload includes current `session-id`, `registered-at`,
`last-active`, status, seat/campaign metadata, capabilities, and routing fields
(`registry-status`, `registry.clj:1727-1818`). It has no previous session ID,
session generation, end reason, or session-ended timestamp.

## 2. Existing event/log

There is no existing session-change evidence event. Registry session writes
are atom swaps; neither reset, generic update, external status, deregistration,
nor TTL reaping appends to the evidence store. The continuous roster watch is
a mutable snapshot, not a consumable event log. The invoke hooks and agency
bell signal invocation completion/idle, not that `old-session-id` ended, and
the compact stdout line likewise lacks an atomic old/new transition.

Consequences:

- a reset caller can act on its synchronous response, but a separate consumer
  cannot discover that response;
- a poller can notice old ID versus new/nil only if it retained the prior poll;
- rapid changes can collapse into one observed state;
- deregistration and TTL reaping erase the session identity needed to emit a
  correct terminal act.

## 3. Proposed act shape

Xiang's `:target` names another **act**, not a session: `target` is an
act-to-act relation, while `seat` separately records agent/session
(`src/futon3c/logic/xiang.clj:82-89,167-181`). Therefore a single lapse whose
target is a session ID would not close any port. Also, any explicit target is
treated as an answer and defaults to `:closes` if adjacency has no entry
(`xiang.clj:140-159,338-359`), so operator marks must not be encoded as
ordinary targeted acts.

I propose one informative envelope followed by concrete per-port closure acts:

```clojure
[{:id "session-end:claude-16:00e653e3:20261011T170000Z"
  :kind :notice
  :author "system"
  :at "2026-10-11T17:00:00Z"
  :agent "claude-16"
  :session "00e653e3-28f7-4add-8304-47ca0475b5c2"
  :port? false
  :session-end/reason :reset
  :session-end/marked-reply-paragraph-count 2
  :session-end/operator-port-marks
  [{:port "turn-101/frag-0" :mark :orphaned}]}

 {:id "session-end:claude-16:00e653e3:20261011T170000Z/lapse/turn-99-frag-1"
  :kind :lapse
  :author "system"
  :at "2026-10-11T17:00:00Z"
  :agent "claude-16"
  :session "00e653e3-28f7-4add-8304-47ca0475b5c2"
  :target "turn-99/frag-1"
  :port? false
  :session-end "session-end:claude-16:00e653e3:20261011T170000Z"}]
```

There is one `:lapse` act for **each port authored by the agent in the ended
session that is open immediately before `:at`**. This is preferable to making
the envelope target a session or placing a vector in `:target`: the present
fact schema accepts a singular target, and `openo` closes ports through
explicit answer links. `:lapse` is already a known closing kind
(`xiang.clj:66-76`) and an accepted answer to a `:promise`
(`xiang.clj:108-119`); explicit targeting makes it close other agent-authored
port kinds as well under the current default.

`turn-ports` can include these acts in the session act vector and use its
existing `db`/`open-ports` before-and-after calculation; no change to `openo`
is required for the lapse closures (`src/futon3c/xiang/turn_acts.clj:512-575`).
It should additionally expose the closing act's kind as the effect so the
inbox sees `:lapse`, rather than only the current `:closed-this-turn` act IDs.
For operator-authored ports it should read
`:session-end/operator-port-marks` from the envelope and decorate matching
entries in `:still-open`; these entries must never become `:target`s or
`answers` facts.

The kernel can decide from the act history:

- which ports are open at session end;
- which openers were authored by the ended-session agent versus `operator`;
- that the per-port lapse acts close the former while the latter remain open.

It cannot reliably decide `:orphaned` versus `:unread` from today's settled
act facts. That distinction requires a new authoritative fact, proposed above
as `:session-end/marked-reply-paragraph-count`, counting marked reply
paragraphs carried by the ended session. The turn service chooses `:orphaned`
when the count is positive and `:unread` when it is zero. Recording the count
on the envelope also makes the decision replayable instead of depending on
later access to mutable/raw reply evidence.

Implementation detail for the later build: `:notice` is not currently in
Xiang's `kinds`, so either add a non-effectful `:session-end`/`:notice` kind or
keep the envelope outside `lx/db` while retaining it in the stored act vector.
The per-port `:lapse` acts already fit the kernel.

## 4. Who should emit it

Recommendation: the registry should synchronously publish a typed
`session-ended` transition (preferably through an outbox/hook committed with
the successful mutation), and the turn service should consume it and append
the envelope/lapses. The registry is the only component that atomically has
the agent ID, old session ID, new/nil ID, reason, and transition time; the turn
service is the component that owns act history and can compute the open-port
set and marked-reply count.

Polling the registry is lossy across rapid replacement and deletion and has no
authoritative end timestamp. Continuing to let minus40 fault intake emit and
having 象 read its edge makes a downstream fault detector the lifecycle
authority, duplicates the decision, and cannot cover normal reset/rebind/end
paths consistently.

## Open questions

1. Is replacement on every successful invoke result a true end of the old
   session, or only reset, deregistration/reaping, and a changed non-nil ID?
   The hook needs an explicit reason policy.
2. Must registry mutation wait for durable act append, or should a small
   durable outbox give at-least-once delivery? Idempotent end/closure IDs above
   support the latter.
3. What is the canonical source for marked reply paragraphs, including replies
   not yet parsed into settled acts? That source must define the count.
4. On deregistration/reaping, should all open ports for the last session end,
   or only ports in the current mission/seat?
5. Should `:orphaned`/`:unread` live as durable act-envelope facts, a separate
   port-state relation, or a derived inbox projection? They must remain
   non-closing whichever representation is chosen.
