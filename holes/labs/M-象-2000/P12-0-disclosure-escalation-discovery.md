# P12-0 — disclosed choices and escalation

Read-only discovery, 2026-09-28. Joe split P12 into visible gap-filling inside
the request, escalation of blockers or extra work to the orchestrator, and
DOCUMENT-phase explanation (`holes/missions/M-象-2000.md:1023-1042`). A disclosure
records a choice; it never grants permission.

## What a completion report is today

The durable primary record is the invoke job. Creation stores caller, recipient,
prompt commission, mode and lifecycle events (`transport/http.clj:1606-1674`).
Finalization adds the full result, a short summary, commit/file artifact reference,
execution evidence and terminal event (`:1948-2004`). The completion report is thus
addressable as `<job-id>.result`; it is not itself an evidence entry.

When eligible, completion also creates a second invoke job addressed to the original
caller. Its prompt contains the result and original job ID; `:bellback-of` joins it to
the original (`transport/http.clj:1185-1235,1261-1285`). Every job creation also
writes a mesh edge (`:1726-1740`). Bellback text is delivery, while the original job
result remains the report authority.

No current job or report field carries structured assumptions or choices. Free prose
cannot provide per-item identity, withdrawal or an absence check. Add separate minted
hyperedges, one `:disclosure/choice` act per choice, endpointed by source job, author,
and affected artifact. A list embedded only in `result` would remain unqueryable and
could not be targeted independently.

## Small closed disclosure shape

```clojure
{:id "act:..." :kind :disclosure/choice :schema 1
 :author "codex-5" :at "..."
 :source-job "invoke-..."
 :unspecified "which stable queue ordering to use"
 :chosen "sort by due-at, then promise id"
 :affects {:kind :git-commit :id "abc..." :path "..."}
 :inside-request {:basis :source-span :quote "..." :text-sha256 "..."}
 :act/stamp {...} :act/harness {...}}
```

Keys are closed; all prose fields are nonblank and bounded; `:source-job`, author,
stamp signer and affected artifact are required. `:inside-request` does not prove a
semantic entailment. It makes the classification challengeable against immutable
request bytes: the quote must occur in the source prompt whose full hash is stored,
and the author explicitly claims the choice implements that span. Missing/mismatched
span is invalid; disagreement with the classification is handled by a withdrawal.
The disclosure remains non-authoritative even when valid.

## Negation and routing

Reuse P10's two-record distinction. A negating utterance is an interpretation that
terminates nothing; an authorised `:act/withdrawal` names the disclosure act and is
the effect. Withdrawal records already keep target, status, basis, valid time, stamp
and harness, and map to hyperedges without deleting the target
(`agency/pattern_card_record.clj:47-69,72-107`). The existing provisional route shows
the required grant check and separate interpretation basis, but it is card-specific
and may only resolve the active card (`transport/http.clj:9815-9865`); P12 needs a
generic disclosure-target route rather than pretending a disclosure is a card.

The orchestrator shown by the target job's unique stored dispatch edge may challenge
the choice. Negating another party's act still needs a P3 grant; Joe uses operator
authority only when the edge says Joe orchestrated that job. The resulting effect is
routed to the report author by a bell whose `in-reply-to` is the source job, producing
another job and mesh edge. The exact-seat turn-notice queue is insufficient: its
payload vocabulary is closed to withdrawal/agreement messages and it is consumed by
one later header (`agency/turn_notice.clj:32-81,106-152`).

The BUILD-PLAN bad case becomes a query: an operator/orchestrator negation
interpretation names disclosure D, but no authorised withdrawal targets D and no
routing job links the effect to D's author. Report
`:negation-without-effect` (and separately `:effect-not-routed`). Conversation prose
alone cannot satisfy either join. The acceptance's two disclosures therefore remain
two acts; negating one leaves the other untouched.

## Finding the orchestrator

`record-invoke-edge!` writes evidence tagged `[:coordination :mesh-edge]`; its body
stores `:edge/id` (the invoke job ID), `:edge/from`, `:edge/to`, surface and time
(`social/coordination_ledger.clj:84-117`). `create-invoke-job!` calls it with
`edge-id=job-id` after durable job creation (`transport/http.clj:1686-1740`). Thus the
direct orchestrator is `from` on the unique `kind=invoke` edge for that job. Unknown,
missing or duplicate edges must yield typed unknown/ambiguous, never inference from
the prompt.

The existing dispatch graph normalizes exactly those edges and preserves unknown
callers (`agency/dispatch_graph.clj:22-52`). Its `upstream` extension uses bounded job
running intervals and explicitly labels the result a reconstruction
(`:62-91`); direct dispatch does not need that temporal inference.

Read-only probes against `/api/alpha/coordination/edges?limit=1000` on 2026-09-28:

| Job | Stored edge evidence | From → to | Result |
|---|---|---|---|
| `invoke-1790630567265-26416-199e94fd` | `e-80447f15-fd0d-4cfb-8f5e-c6769816933e` | `claude-17 → codex-5` | orchestrator `claude-17` |
| `invoke-1790630322906-26405-e0e0ae72` | `e-3ea45b9e-8643-48f0-8bde-9a5ffd79b919` | `claude-17 → codex-5` | orchestrator `claude-17` |

Both job GETs independently report caller `claude-17`. The edge endpoint is current
and limit-bounded, so these probes establish the returned records, not historical
absence outside the window.

## Block escalation

An agent can already bell the stored orchestrator and correlate the request using
`in-reply-to`; the server persists that as `:bellback-of` (`http.clj:5701-5705,
5780-5802`). Typed bells include `:query`, `:challenge`, `:request` and `:suggest`, but
the rollout defaults off (`:1066-1085`). `agency_send.py` supplies caller, type, ref,
mission and mode (`scripts/agency_send.py:189-225`), but does not currently expose
`in-reply-to`, so callers use the HTTP body or need one small CLI flag.

Make the blocker checkable with one `:escalation/blocker` record:
`{id source-job author orchestrator blocker at pattern-search-id pattern-id
pattern-use-result escalation-job-id}`. `pattern-id` and a successful/failed attempt
are required before `escalation-job-id`; absence becomes `:pattern-unblock-untried`,
not permission to improvise. The orchestrator may answer the block or ask for a P11
offer. New work is never smuggled into this record.

## Packet order

1. **P12-1:** pure closed validator and hyperedge mapping for one
   `:disclosure/choice`. Acceptance: two choices round-trip with distinct act IDs;
   querying the source job finds both. Bad case: a result containing disclosure prose
   but no act returns `:disclosure-unrecorded`.
2. Write path and report attachment: mint each validated disclosure after completion,
   without making report success depend on it; expose typed missing-record status.
3. Generic negation interpretation/effect join, P3 authorisation, and bell routing to
   the source author; acceptance negates one of two disclosures and proves both the
   effect and routed job.
4. Blocker record plus pattern-first check, dispatch-edge orchestrator lookup and
   correlated escalation bell. Add `agency_send.py --in-reply-to`; extra work uses
   the existing P11 offer subsystem.
