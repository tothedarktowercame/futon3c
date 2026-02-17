# Implementation Inventory: Wired vs Shaped vs Referenced

Updated: 2026-02-17

Status categories:
- **Wired**: implementation + tests + integrated into pipeline or evidence flow
- **Shaped**: shape/spec exists, but no behavioral implementation
- **Referenced**: mentioned in missions/docs but no code exists

## Social Pipeline

| Component | Status | Evidence |
|---|---|---|
| Agent registry | Wired | `src/futon3c/agency/registry.clj`; tests: `test/futon3c/agency/registry_test.clj` |
| S-presence | Wired | `src/futon3c/social/presence.clj`; nil registry test for I3 enforcement |
| S-authenticate | Wired | `src/futon3c/social/authenticate.clj` |
| S-mode | Wired | `src/futon3c/social/mode.clj`; mode integration tests |
| S-dispatch | Wired | `src/futon3c/social/dispatch.clj`; structured payload passthrough (no lossy pr-str) |
| S-persist | Wired | `src/futon3c/social/persist.clj` |
| S-validate | Wired | `src/futon3c/social/validate.clj` |
| Evidence store | Wired | `src/futon3c/evidence/store.clj`; in-memory atom-backed |
| Thread projection | Wired | `src/futon3c/evidence/threads.clj` |
| Malli shapes | Wired | `src/futon3c/social/shapes.clj`; 9 peripheral IDs, all validated |
| Bell dispatcher | Wired | `src/futon3c/social/bells.clj`; standup bell + test-bell; 12 tests |

## Peripheral Runtime

| Component | Status | Evidence |
|---|---|---|
| PeripheralRunner protocol | Wired | `src/futon3c/peripheral/runner.clj` |
| Tool dispatch | Wired | `src/futon3c/peripheral/tools.clj` |
| Chain orchestration | Wired | `src/futon3c/peripheral/registry.clj`; multi-peripheral hop chains |
| Peripheral: explore | Wired | `src/futon3c/peripheral/explore.clj`; tests passing |
| Peripheral: edit | Wired | `src/futon3c/peripheral/edit.clj` |
| Peripheral: test | Wired | `src/futon3c/peripheral/test_runner.clj` |
| Peripheral: deploy | Wired | `src/futon3c/peripheral/deploy.clj` |
| Peripheral: reflect | Wired | `src/futon3c/peripheral/reflect.clj` |
| Peripheral: proof | Wired | `src/futon3c/peripheral/proof.clj` |
| Peripheral: discipline | Wired | `src/futon3c/peripheral/discipline.clj`; PSR/PUR with pattern-id tracking |
| Peripheral: chat | Shaped | Spec in `peripherals.edn`; no runtime impl (IRC fills this role) |
| Peripheral: alfworld | Wired | `src/futon3c/peripheral/alfworld.clj`; stepper with bell/whistle |
| Hop protocol | Wired | `src/futon3c/social/peripheral.clj`; explicit exit conditions only (no substring inference) |
| Adapter/prompt gen | Wired | `src/futon3c/peripheral/adapter.clj`; Claude + Codex tool mapping |
| Round-trip verify | Wired | `src/futon3c/peripheral/round_trip.clj` |

## Transport Layer

| Component | Status | Evidence |
|---|---|---|
| Protocol (shapes) | Wired | `src/futon3c/transport/protocol.clj`; 31 tests |
| HTTP adapter | Wired | `src/futon3c/transport/http.clj`; 15 tests |
| WebSocket adapter | Wired | `src/futon3c/transport/ws.clj`; 16 tests |
| IRC adapter | Wired | `src/futon3c/transport/irc.clj`; 40 tests incl. stability (F1-F6) |
| Transport integration | Wired | `test/futon3c/transport/integration_test.clj`; 10 tests |

## Alleycat Verification

| Gate | Status | Evidence |
|---|---|---|
| Peripheral inhabitation race | PASS | 20/20, 4 checkpoints + secret exchange |
| Transport pivot (Emacs + IRC) | PASS | Bidirectional joe+claude, shared session |
| Three-way chat (joe+claude+codex) | PASS | @-mention gated routing |
| P-4/P-6 structural closure | PASS | Chain + relay tests; 479 tests, 1540 assertions |
| IRC standup bell | STRUCTURAL | Wiring tested (12 tests); awaiting live demo to sign |

## Emacs UI Layer

| Component | Status | Evidence |
|---|---|---|
| Shared UI library | Written | `emacs/futon3c-ui.el` |
| Claude chat | Written | `emacs/futon3c-chat.el`; streaming via `--output-format stream-json` |
| Codex REPL | Written | `emacs/codex-repl.el`; streaming via JSONL |
| Sliding blackboards | Written | `emacs/futon3c-code-blocks.el` |

## Not Yet Extracted (from futon3)

| Component | Source | Target | Blocking? |
|---|---|---|---|
| ~~Forum service~~ | ~~`futon3/src/futon3/forum/` (3 files, 776 LOC)~~ | ~~futon3c~~ | **OBSOLETE** — superseded by evidence landscape (store + threads + validate). See TN-forum-to-evidence-landscape.md |
| ~~Drawbridge routing~~ | ~~`futon3/src/futon3/drawbridge/` (3 files, 1577 LOC)~~ | ~~futon3c~~ | **~80% OBSOLETE** — routing replaced by S-dispatch + transport adapters; IRC relay by `make-relay-bridge`; agent subprocess spawning now a peripheral concern. Genuine gaps: agency bell handlers (standup/PAR/test-bell), HTTP REST API. No MUSN drawbridge exists. |
| Agent invokes | `futon3/src/futon3/agency/invokes.clj` (329 LOC) | futon3c | Depends on dispatch |
| Disk persistence | N/A (new) | futon3c | XTDB/futon1a integration |
| L1 observer | Referenced in exotype | futon3c or futon3b | Glacial timescale |

## Test Summary

494 tests, 1604 assertions, 0 failures, 0 errors.

## Tech Debt (All Resolved)

| Item | Status | Resolution |
|---|---|---|
| I3 snapshot enforcement | RESOLVED | S-presence already fail-fast on nil registry; test added |
| Lossy coerce-prompt | RESOLVED | Passes structured data through; no pr-str |
| Hop exit inference | RESOLVED | Substring inference removed; explicit keywords required |
