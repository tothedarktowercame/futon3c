# PUR: Single-routing-authority — boundary namespace landed

pattern (re-confirmed):
  - `futon3/library/agency/single-routing-authority` — primary.
  - `futon3/library/agency/loud-failure` — accompanying (every failure mode prints VIOLATION + returns structured violation).
  - `futon3/library/agency/delivery-receipt` — accompanying (return shape is `{:ok bool ...}` always).
  - `futon3/library/storage/durability-first` — accompanying (verify-persisted before declaring success).

actions taken:
  - Created `src/futon3c/evidence/boundary.clj` (~210 lines) with `append!`, `append-default!`, the canonical statement `I-single-boundary`, and the coercion helpers `coerce-keyword`, `coerce-tags`, `coerce-subject`, `coerce-evidence-type`, `coerce-claim-type`, `coerce-pattern-id`, `coerce-entry-fields`, `coerce-args-fields`, `un-namespace-evidence-keys`, `coerce-input`.
  - Created `test/futon3c/evidence/boundary_test.clj` (~180 lines) with 10 deftests across round-trip / coercion / loud-failure / receipt-shape / canonical-statement scenarios.
  - Created PSR at `holes/labs/M-invariant-queue-unstuck/psr/2026-04-29__derive__single-routing-authority.md`.
  - No existing call sites of `store/append*` modified. The boundary is purely additive at this stage. Caller migration is INSTANTIATE-2.

outcome:
  Full pass — 10 tests / 31 assertions for the boundary tests; full evidence-test suite (boundary + invariant + store) 33 tests / 83 assertions, all green. The boundary correctly:
    - Coerces string-shaped fields (`:tags`, `:subject :ref/type`, `:type`, `:claim-type`, `:pattern-id`) to keywords before validation.
    - Surfaces shape violations as structured `:ok false :error/code :invalid-entry :invariant/violation {:kind :shape ...}` receipts, with VIOLATION line on stderr.
    - Surfaces coercion exceptions as `:ok false :error/code :exception` with structured violation context.
    - On success, calls `invariant/verify-persisted` and returns `:ok true :evidence/id <id> :entry <full-entry>`.

prediction errors:
  - PSR did not anticipate that fully-namespaced input *without* `:evidence/id` and `:evidence/at` would fall into store/append*'s "args-map" branch (which destructures unqualified keys, finds nothing, and produces an entry with nil fields that fails `ensure-entry`). Discovered when two tests for namespaced-input-with-string-tags failed. Resolved by adding `un-namespace-evidence-keys` to normalize all input to the unqualified args-map form before passing to `store/append*`. The unqualified form is what `store/append*` auto-completes (`:evidence/id` via `gen-id`, `:evidence/at` via `now-str`). Minor design extension; the un-namespace is idempotent against already-unqualified input. PSR updated mentally; documented in this PUR.
  - PSR predicted "high confidence" — confirmed. The pattern is well-trodden in the futon stack (futon1a's `pipeline.clj/run-write!` is the canonical analog). The only surprise was the namespacing-completion edge case, which was an artefact of `store/append*`'s dual-input shape, not of the pattern itself.

invariants verified:
  - `I-single-boundary` canonical statement is a non-empty string containing its own name (grep-verifiable from CI).
  - Round-trip via XTDB: well-shaped entries persist and read back with stable `:evidence/id`.
  - Loud-failure: shape rejections return structured violations; no entry is persisted; `query*` after a rejection returns 0 entries.
  - `I-evidence-per-turn` (existing): `verify-persisted` is called on every successful append; integrated with the boundary's success branch.

connections:
  - **INSTANTIATE-2 next:** migrate the ~30 `estore/append*` direct callers in `peripheral/common.clj` (the existing `maybe-append-evidence!`), `transport/{http,ws}.clj`, `social/{bells,whistles,dispatch}.clj`, `agents/tickle*.clj`, `agents/{apm,arse}_work_queue.clj`, `peripheral/{mentor,real_backend,common}.clj`, `blackboard.clj`, `portfolio/core.clj`, `dev/futon3c/dev/invoke.clj`. Each becomes a small commit. The `peripheral/common/maybe-append-evidence!` becomes a thin alias delegating to `boundary/append!`.
  - **INSTANTIATE-3 next:** the hourly + on-demand + autoshutter live-state probe.
  - **INSTANTIATE-4 next:** taps for the multi-source projection wave (M-live-geometric-stack tests, the three operational core.logic layers, VSATARCS, War Machine AIF).
  - **Library feedback:** the `agency/single-routing-authority` flexiarg's COMPOSITIONS section already covers the case applied here; no library revision needed. Confidence in the pattern as written increased by one application.
  - **Mission cross-link:** `M-invariant-violations` (sibling, MAP) tracks individual violations; this boundary makes the boundary-level invariants surfaceable in that ledger going forward.
