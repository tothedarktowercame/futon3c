# P3-3 — harness at act write time (discovery only)

2026-09-27. Owner: claude-17; survey: codex-5. No implementation, API mutation,
reload, notification or runner execution. Source inspected at futon3c `81d20f42`
and futon2 `02fde89b3`; inspection does not establish which Vars the live JVM has
loaded. P6o commits fbbe1ab5/d1ff0ebc/0124668f and futon1b f998b6e were checked.

## Decision

Harness kind is **orthogonal to origin**, not a refinement of `origin.kind`.
An operator click can run under War Machine; an agent response can run under War
Machine; a harness-origin bellback can belong to a plain session. Keep
`:war-machine`, `:zai`, `:none` as execution values; retain explicit `:unknown`
when evidence cannot distinguish them. `:none` must not mean “field missing”.
Zai remains reserved: the `zai-api` model adapter and `zaif_controller` names do
not demonstrate deployment of Joe's proposed Zai harness.

`futon1b_origin.clj:6–25` defines a closed origin map: operator/agent/harness/unknown,
writer, attributed author, source/surface and authorization. Adding kind inside
that map without changing its validator would be rejected. P6o does not cover
minted hyperedge properties automatically. The grant reference remains separate;
a harness stamp confers no grant.

## Write-time inventory

Paths below are relative to /home/joe/code. Values are proposed **at the named
producer**, not classifications inferred for every existing record with its name.
Generic boundaries need an explicit execution context from their caller.

| Write path | Source site | Available signal | Proposed value / missing information |
|---|---|---|---|
| Emacs user/assistant turn capture and correction | futon3c/emacs/agent-turn-origin.el:13,29; session-mode.el:985; P6o2 changes in claude/codex/kimi/zai-repl.el | Buffer input origin, speaker, source id; emitter stamps emacs-repl | `:none` for an explicitly plain session. Surface alone cannot exclude an invoked WM turn in that buffer; inherit per-invoke context when present. |
| Agency invoke context | futon3c/src/futon3c/agency/registry.clj:1117; transport/http.clj:1690 | caller, surface, registration; job id at job creation | Context can be captured here, but caller/agent/session alone do not prove WM membership. Unknown without a trusted dispatch binding. |
| CLI lifecycle evidence | futon3c/dev/futon3c/dev/invoke.clj:79–95 | invoke-start uses dynamic input origin; other events use invoke-lifecycle | Inherit invoke execution context; standalone explicitly plain CLI is `:none`. Do not translate origin harness to WM. |
| Z.ai transcript | futon3c/src/futon3c/agents/zai_api.clj:1098–1105 | turn-start origin; agent identity for round/commits; zai-transcript for other events | Same execution context; never `:zai` from adapter/model name. |
| Promise/history and outcome bookkeeping | futon3c/src/futon3c/agency/promise_history.clj:112; promise_outcome.clj:91 | promise id and producer identity | Plumbing; carry originating execution reference or unknown. |
| Mesh invoke edges and coordination state | futon3c/src/futon3c/social/coordination_ledger.clj:115,162 | from/to/surface/time/edge id | Caller names identify routing, not harness; preserve trusted source-job context if provided. |
| HTTP evidence ingress | futon3c/src/futon3c/transport/http.clj:3125,3189 | supplied origin and normalized payload | Cannot invent harness at ingress. Validate and preserve explicit execution provenance. |
| Common evidence boundary/store | futon3c/src/futon3c/evidence/boundary.clj:317; evidence/store.clj:92–102 | entry and supplied origin; absent source becomes unknown | Central persistence seam, not execution authority. Unknown unless caller passed context. |
| Futon1b evidence persistence | futon1b/futon1b_evidence.clj:45–71 | validates/preserves supplied origin | Schema must independently validate/preserve harness field; no inference from author. |
| WM click entry | futon3c/src/futon3c/transport/http.clj:8941–9028 | exact WM click route, issuer provenance, admitted config, author/reviewer cast | `:war-machine` for this execution; issuer identity may still be unknown. |
| WM runner click/run records | futon3c/src/futon3c/wm/runner_service.clj:237,294–334 | click id, run id, run-record identity/digest validation | `:war-machine`, tied to the actual click/run binding. Local phase/run files are context evidence, not automatically evidence-store acts. |
| WM agent dispatch | futon2/src/futon2/aif/full_loop_runner.clj:1234–1251 | runner owns dispatch; outgoing agent/caller/mission/prompt; returned job id | `:war-machine` can be asserted at this producer and bound to returned job. Current payload lacks execution-harness binding. |
| WM readiness wake | same file:890–903 | explicit runner call; caller wm-full-loop | WM producer is known from code; string alone is not authority at a general endpoint. |
| WM tick evidence | futon2/src/futon2/aif/evidence_emit.clj:15,36,179–220; scripts/wm_scheduled_run.clj:158 | WM-specific emitter, tick trigger, wm-tick tag; FUTON2_WM_EMIT_EVIDENCE enables emission | `:war-machine` at emitter. Environment flag is an enable switch, not a general session classifier. |
| WM grounding writes | futon2/src/futon2/aif/full_loop_runner.clj:2980–3012 | run/attempt/reviewer job and wm-full-loop source | WM context known here. These writes are entities; do not silently count them as minted act hyperedges. |
| Generic substrate hyperedges | futon2/src/futon2/aif/substrate.clj:194–225; actuator_a3.clj:615 | document + opts; entity/hyperedge dispatch | Stamp at executing caller, preserve at substrate; generic API alone cannot distinguish standalone use. |
| Grant/rule/clearance minted acts | futon3c/src/futon3c/agency/grant_record.clj:169; rule_record.clj:78; incident_clearance.clj:78 | validated domain record, source evidence, valid time, mint request; no execution binding | CLI knows its explicit execution context only if supplied. Source grantor/author does not establish writer harness. Add properties at mint, never rewrite old acts. |
| Mint service | futon1b/futon1b_server.clj:212 | minted payload/idempotency receipt | Preserve validated properties, not infer from originating rule or act type. |

The WM cast is configurable (`http.clj:8953–8960`); `wm-author`, `wm-reviewer`,
`wm-repair-reviewer` are roles/names, not an exhaustive harness detector. No checked
path propagates a general execution-harness environment variable. Session ids are
correlation keys, not a discriminator. Agency jobs alone need a producer binding;
P3-2's time-interval reconstruction is explicitly not a stored causal authority.

`auto-bellback`, `turn-capture`, parked-resume, invoke-lifecycle, and promise-history
are transport/lifecycle plumbing. Their origin may be harness while execution is
none, WM, or unknown. Do not assign `:none` merely because a sender is plumbing;
carry the triggering job's context where that relationship is recorded.

## Unknowns and scope

An anonymous HTTP deposit, a historical turn without execution context, a bare
hyperedge CLI request, and a continuation with no source-job binding cannot be
classified at the common write boundary. They need an explicit producer context,
not text inspection, a global process flag or author-name lookup. Concurrent invokes
require per-job context; session-wide mutable state risks tagging a later plain turn
with an earlier WM run.

The inventory identifies production persistence boundaries and the P6o/WM/minted
act producers. It is not a proof that every ad-hoc script routes through P6o. In
particular scripts/deposit_runner_gate_memory.clj:18 posts hyperedges directly;
its :memory/assert ids are caller supplied, not minted act ids. Such callers must
carry context too if their records are included in the eventual “every act” rule.
An evidence LIST census does not enumerate the hyperedge/entity collections.

## Small implementation packet proposed

Add a sibling `:evidence/harness` (wire `harness`) map and matching `:act/harness`
inside minted `:hx/props`: `{:kind :war-machine|:zai|:none|:unknown,
:execution-id ..., :basis :producer-context, :source-ref ...}`. Require a run/job
binding for WM; explicit plain-session context for none; unknown includes a reason.
Reserve/reject live Zai assertions until its producer exists. Keep grant reference
and origin unchanged. Validate preservation at futon1b ingress and futon3c shapes,
normalization/store; stamp explicit producers rather than teaching the boundary
that a caller string implies a harness.

First bounded packet: contract + WM tick producer + plain-session producer and
minted rule-record CLI, with readback tests. Follow with separate per-job propagation
through WM dispatch, registry, CLI/transcript and lifecycle records. Do not claim
universal coverage until that propagation and the remaining writer audit pass.

Acceptance test: the same agent/session emits a WM-bound act then a plain-session
act; store/readback gives WM then none. Insert a harness-origin auto-bellback with
no binding: it stays unknown. Mutate only caller to wm-author or adapter to zai-api:
classification must not change. A WM operator click retains origin operator and
harness WM; neither result creates a grant. Test evidence and minted properties
through actual serializer/store seams, not just a pure classification stub.

## Pinned census

LIST read, sequential, JSON Accept header, limit 1000, with fixed filters:

- since = `2026-09-27T00:00:00Z`
- before = system-as-of = `2026-09-27T22:29:21.685918Z`
- base = `http://localhost:7073/api/alpha/evidence`
- Subsequent pages use returned next-cursor.at/id as cursor-at/cursor-id.
- 11 pages, **10,150 records / 10,150 distinct ids**, no incomplete pages,
  terminal page without cursor. Raw responses plus exact URLs preserved locally at
  `/tmp/p3-3-evidence/capture.json` (not committed: transcript bodies include unrelated
  work). SHA-256 `2cddfe50805206a0ea2bce2f80cb573e1eee44be1492cb045a371077c30985ff`.

| Proposed execution classification supportable from these records | Count |
|---|---:|
| war-machine, positively bound | 0 |
| zai (reserved) | 0 |
| none, positively declared plain execution | 0 |
| unknown / execution context not recorded | 10,150 |

These are **evidenced assignments under the proposed contract**, not a claim that
no WM or plain-session work happened today. There are no top-level harness fields,
no wm-tick tagged records and no authors containing wm or war-machine in this
window. 389 records have origin.surface emacs-repl, but that surface also carries
agent invocations/continuations: declaring all 389 plain would confuse transport
with execution. No author-name classifier was applied. Origin counts, independently:

| Stored origin.kind | Count |
|---|---:|
| absent | 6,706 |
| unknown | 1,224 |
| harness | 1,084 |
| agent | 970 |
| operator | 166 |

Of these records, 671 are origin/backfill (writer p6o3-reconstructed-v1); today's
record time does not make their provenance a new write-time execution stamp.
The census includes them as records, not new grants or WM executions. Existing
origin harness ≠ proposed execution war-machine: mapping the 1,084 that way would
be unsupported. Evidence LIST does not include minted hyperedges; no claim about
their number is made here. Both absent and unknown contexts need explicit migration
semantics in implementation, rather than defaulting old acts to none.

## Validation and limits

Only GET reads and local report/capture files were used. No runtime introspection
that evaluates forms, no code loading, no evidence/hyperedge writes. Source paths
above were read, not inferred from their names. The running evidence endpoint was
queried; that verifies stored fields, not runtime deployment of every inspected
producer. No tests required for this discovery-only packet.
