# Implementation Inventory: Wired vs Shaped vs Referenced

Date: 2026-02-10

Status categories:
- **Wired**: implementation + tests + integrated into pipeline or evidence flow
- **Shaped**: shape/spec exists, but no behavioral implementation
- **Referenced**: mentioned in missions/docs but no code exists

| Component | Status | Evidence |
|---|---|---|
| Agent registry | Wired | `src/futon3c/agency/registry.clj`; tests: `test/futon3c/agency/registry_test.clj` |
| S-presence | Wired | `src/futon3c/social/presence.clj`; tests: `test/futon3c/social/presence_test.clj`; pipeline wiring: `test/futon3c/social/pipeline_test.clj:43` |
| S-authenticate | Wired | `src/futon3c/social/authenticate.clj`; tests: `test/futon3c/social/authenticate_test.clj`; pipeline wiring: `test/futon3c/social/pipeline_test.clj:57` |
| S-mode | Wired | `src/futon3c/social/mode.clj`; tests: `test/futon3c/social/mode_test.clj`; integration: `test/futon3c/social/mode_integration_test.clj:42` |
| S-dispatch | Wired | `src/futon3c/social/dispatch.clj`; tests: `test/futon3c/social/dispatch_test.clj`; pipeline wiring: `test/futon3c/social/pipeline_test.clj:72` |
| S-persist | Wired | `src/futon3c/social/persist.clj`; tests: `test/futon3c/social/persist_test.clj`; pipeline wiring: `test/futon3c/social/pipeline_test.clj:79` |
| S-validate | Wired (evidence flow) | `src/futon3c/social/validate.clj`; tests: `test/futon3c/social/validate_test.clj`; integration: `test/futon3c/evidence/integration_test.clj:373` |
| Evidence store | Wired | `src/futon3c/evidence/store.clj`; tests: `test/futon3c/evidence/store_test.clj`; integration: `test/futon3c/evidence/integration_test.clj:37` |
| Thread projection | Wired | `src/futon3c/evidence/threads.clj`; tests: `test/futon3c/evidence/threads_test.clj`; invariants: `test/futon3c/evidence/integration_test.clj:421` |
| Peripheral: explore | Wired (spec) | Spec in `resources/peripherals.edn` (`:explore`); loaded/validated in `src/futon3c/social/peripheral.clj`; tests: `test/futon3c/social/peripheral_test.clj` |
| Peripheral: edit | Wired (spec) | Spec in `resources/peripherals.edn` (`:edit`); hop tests: `test/futon3c/social/mode_integration_test.clj:128` |
| Peripheral: test | Wired (spec) | Spec in `resources/peripherals.edn` (`:test`); hop chain tests: `test/futon3c/social/mode_integration_test.clj:153` |
| Peripheral: deploy | Wired (spec) | Spec in `resources/peripherals.edn` (`:deploy`); invalid path tests: `test/futon3c/social/mode_integration_test.clj:181` |
| Peripheral: reflect | Wired (spec) | Spec in `resources/peripherals.edn` (`:reflect`); “from-any” behavior tests: `test/futon3c/social/mode_integration_test.clj:195` |
| Hop protocol | Wired | `src/futon3c/social/peripheral.clj`; tests: `test/futon3c/social/peripheral_test.clj`; integration: `test/futon3c/social/mode_integration_test.clj:128` |
| HTTP transport | Referenced | Mentioned in `holes/missions/M-agency-refactor.md` and `holes/missions/M-social-exotype.md`; no `src/futon3c/*http*` transport module present |
| WebSocket transport | Referenced | Mentioned in missions; no WS server/client implementation present |
| Disk persistence | Referenced | Store/persist layers are in-memory atoms (`src/futon3c/evidence/store.clj`, `src/futon3c/social/persist.clj`); no disk/XTDB backend implemented |
| L1 observer | Referenced | Mentioned as glacial reader in `holes/missions/M-social-exotype.md`; no implementation present |

