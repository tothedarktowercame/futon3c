# P3-3b-1 — preserve execution harness through futon3c

Implementation `59379f02`. No producer stamps or new defaults. Futon1b remains
the authority for the inner contract.

## Paths and exact preservation

- `evidence/store.clj:102`: args-map rebuilding now preserves a supplied bare
  or namespaced harness, including explicit nil. Absence does not add a key.
- `transport/http.clj:3129`: HTTP ingress now copies bare/namespaced harness,
  including string-key equivalents, with namespaced precedence. This was a
  second drop beyond the store rebuild.
- `evidence/http_backend.clj:75`: legacy HTTP payload builder also dropped the
  field. It now carries harness and origin; origin had the same omission.
  JSON naturally represents keyword enums as strings; no harness-specific
  conversion is performed.
- `evidence/boundary.clj:124,177`: coercion preserves unknown fields. Full valid
  maps pass through; partial maps reach the fixed store rebuild.
- `social/shapes.clj:326`: EvidenceEntry is an open Malli map; no schema addition
  needed. Supplied harness is not normalized or silently dropped here.
- `evidence/backend.clj:113`: AtomBackend retains the full validated map.
- `evidence/futon1b_backend.clj:203`: serializer uses pr-str on the full map;
  no change needed. Explicit invalid/null harness survives to authoritative
  futon1b rejection, rather than becoming an absent field.

## Gates and tests

clj-kondo: zero errors/warnings (one preexisting informational redundant boolean
at http.clj:8138). check-parens passed for all four touched Clojure files.
Code changed, so existing warrants cannot cover this revision (CLAUDE.md I-6).
Only evidence namespaces were run, individually:

| Namespace suffix | Tests | Assertions |
|---|---:|---:|
| harness-test | 3 | 102 |
| store-test | 16 | 30 |
| boundary-test | 13 | 59 |
| futon1b-backend-test | 22 | 94 |
| http-backend-test | 15 | 42 |
| origin-test | 4 | 31 |

All **73 tests / 358 assertions** passed. Logs: /tmp/p3-3b1-*-test.log.
The new test retains the formerly dropping args-map call shape through real
boundary/append!, store/append*, schema and Futon1bBackend serialization. Only
HTTP transport is substituted to inspect actual request bodies. AtomBackend and
HTTP ingress normalization are exercised too, including absent, explicit nil,
string-key maps, full entries and unchanged origin. An initial ingress fixture
used internal keyword enums rather than JSON values; corrected to the wire shape.

## Reload and live evidence

Reloaded from canonical master via scripts/proof-eval.sh:
`futon3c.evidence.store`, `futon3c.evidence.http-backend`,
`futon3c.transport.http`. Resource resolution confirmed
file:/home/joe/code/futon3c/src/futon3c/evidence/store.clj.
The installed HTTP handler identity was retained; no restart, handler rebuild or
bootstrap reload. Its POST branch at http.clj:9540 calls handle-evidence-create
through the Var, which calls the updated normalizer. Existing HttpBackend methods
likewise call the payload helper Var, so their instances need not be replaced.
Boundary/store and peripheral memory-lifecycle/store calls use Vars, not captured
append function values. No dependent namespace had a captured changed helper
requiring additional reload. In particular foreign-dirty bootstrap was untouched.

Authorized POST through :7070 returned 201, then GET :7073 returned the field:
`p3-3b1-f6ff72fb-ce45-48c7-b1f7-71dd9dcd674d`. Session `p3-3b1-harness-passthrough-20260927-codex5`.
Harness: `{:kind :none :basis :producer-context :source-ref "p3-3b1-harness-passthrough-20260927-codex5"}`.
Raw result /tmp/p3-3b1-live.json. This is a test declaration, not a newly installed
producer. Existing origin stamping remained independent (:unknown in this probe).

## Source dependency enumeration

Direct requires of changed namespaces (src/dev source audit). Dependents invoke
Vars or factories; requiring a namespace alone does not require reloading it.

- `futon3c.evidence.store`: `futon3c.agency.clock-decision`, `futon3c.agency.history-constraints`, `futon3c.agency.promise-outcome`, `futon3c.agents.apm-work-queue`, `futon3c.agents.arse-work-queue`, `futon3c.agents.memory-mcp`, `futon3c.agents.memory-mcp-test`, `futon3c.agents.tickle`, `futon3c.agents.tickle-logic`, `futon3c.agents.tickle-orchestrate`, `futon3c.agents.tickle-work-queue`, `futon3c.agents.zai-api`, `futon3c.aif.stack-generator`, `futon3c.apm.memory-caption-store`, `futon3c.apm.memory-snapshot`, `futon3c.apm.promotion-candidate-store`, `futon3c.apm.promotion-review-store`, `futon3c.dev.ct`, `futon3c.evidence.boundary`, `futon3c.evidence.threads`, `futon3c.live-efe-map`, `futon3c.logic.archaeology`, `futon3c.logic.ratchet`, `futon3c.logic.tracer`, `futon3c.peripheral.memory-lifecycle`, `futon3c.peripheral.memory-recall`, `futon3c.peripheral.mentor`, `futon3c.peripheral.mission-control-backend`, `futon3c.peripheral.pull-receipts`, `futon3c.peripheral.real-backend`, `futon3c.portfolio.core`, `futon3c.portfolio.observe`, `futon3c.social.bells`, `futon3c.social.coordination-ledger`, `futon3c.social.validate`, `futon3c.test-registry`, `futon3c.test-registry.validation`, `futon3c.transport.http`, `futon3c.transport.irc`, `futon3c.transport.ws.replication`.
- `futon3c.evidence.http-backend`: `futon3c.nlp.classical-pipeline`, `futon3c.test-registry`, `futon3c.test-registry.validation`.
- `futon3c.transport.http`: `futon3c.agency.r9-authority`, `futon3c.dev.bootstrap`, `futon3c.runtime.agents`, `futon3c.transport.bootstrap-handler-migration`, `futon3c.wm.scheduler`.
