# Futon3c Wiring Contract

Date: 2026-02-20

This is the system-building guide for coding agents.
Tests verify behavior. This contract defines structure.
Evidence ledgers verify that structural claims were actually exercised.

## Canonical Diagrams (Source of Truth)

These files are the canonical wiring specs:

1. `/home/joe/code/futon5/data/missions/social-exotype.edn`
2. `/home/joe/code/futon5/data/missions/coordination-exotype.edn`
3. `/home/joe/code/futon5/data/missions/futon3-coordination.edn`

Interpretation:
- `social-exotype.edn` defines the social loop topology (S-presence -> S-persist + S-default).
- `coordination-exotype.edn` defines task/glacial loop structure.
- `futon3-coordination.edn` defines concrete gate-pipeline structure used by integration boundaries.

## Machine-Readable Contract + Evidence

These files are part of the enforced contract:

1. `docs/wiring-claims.edn` (architectural claims and expected evidence modes)
2. `docs/wiring-evidence.edn` (claim evidence entries with commit/test/script/artifact witnesses)

Validation is enforced by:

- `test/futon3c/architecture/wiring_evidence_test.clj`

## Projection: Diagram -> Futon3c Code

| Diagram node | Primary implementation | Primary tests |
|---|---|---|
| `S-presence` | `src/futon3c/social/presence.clj` | `test/futon3c/social/presence_test.clj` |
| `S-authenticate` | `src/futon3c/social/authenticate.clj` | `test/futon3c/social/authenticate_test.clj` |
| `S-mode` | `src/futon3c/social/mode.clj` | `test/futon3c/social/mode_test.clj`, `test/futon3c/social/mode_integration_test.clj` |
| `S-dispatch` | `src/futon3c/social/dispatch.clj` | `test/futon3c/social/dispatch_test.clj`, `test/futon3c/social/dispatch_integration_test.clj` |
| `S-validate` | `src/futon3c/social/validate.clj` | `test/futon3c/social/validate_test.clj` |
| `S-persist` | `src/futon3c/social/persist.clj`, `src/futon3c/evidence/store.clj` | `test/futon3c/social/persist_test.clj`, `test/futon3c/evidence/store_test.clj` |
| `S-default` | `src/futon3c/agents/tickle.clj`, `src/futon3c/peripheral/mission_control.clj`, `scripts/codex-autowake` | `test/futon3c/agents/tickle_test.clj`, `test/futon3c/social/dispatch_test.clj` |

## Boundary Adapters (Transport is not topology)

| Adapter | File | Role |
|---|---|---|
| HTTP | `src/futon3c/transport/http.clj` | External request/response boundary into social pipeline |
| WebSocket | `src/futon3c/transport/ws.clj` | Session/frame boundary into social + peripheral runtime |
| IRC | `src/futon3c/transport/irc.clj` | Chat relay boundary; routes, does not create agents |

## Build Order (When Reconstructing)

1. `shapes`: `src/futon3c/social/shapes.clj`
2. `agency`: `src/futon3c/agency/registry.clj`, `src/futon3c/agency/federation.clj`
3. `social pipeline`: `presence -> authenticate -> mode -> dispatch -> validate -> persist`
4. `peripheral runtime`: `src/futon3c/peripheral/*`
5. `transport adapters`: HTTP, WS, IRC
6. `runtime wiring`: `src/futon3c/runtime/agents.clj`, `dev/futon3c/dev.clj`
7. `operational loop`: tickle + autowake + health/evidence endpoints

## Change Rule

Any change to topology, boundary responsibilities, or component identity must update:

1. this file (`docs/wiring-contract.md`)
2. `docs/wiring-claims.edn`
3. `docs/wiring-evidence.edn` with commit-scoped witness entries
4. the current-state map (`docs/system-now-next.md`)
5. tests proving the new projection

Without this, a green test run is not considered architectural convergence.
