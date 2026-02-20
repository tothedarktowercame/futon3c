# Mission Surfaces Contract

Date: 2026-02-20

This contract covers the operational surfaces that were previously easy to
treat as aspirational: Mission Peripheral, Mission Control, and War Room
workflows.

## Scope

- Mission Peripheral (`:mission`) as a cycle-machine runtime surface
- Mission Control (`:mission-control`) as portfolio observation/control surface
- War Room semantics as evidence-backed operations (`:mc-bulletin`, tickle loop)

## Machine-Readable Claims + Evidence

- `docs/mission-claims.edn`
- `docs/mission-evidence.edn`

Validation:

- `test/futon3c/architecture/mission_evidence_test.clj`

## Canonical Runtime Files

- `src/futon3c/peripheral/mission.clj`
- `src/futon3c/peripheral/mission_backend.clj`
- `src/futon3c/peripheral/mission_control.clj`
- `src/futon3c/peripheral/mission_control_backend.clj`
- `src/futon3c/mission_control/service.clj`
- `src/futon3c/agents/tickle.clj`

## Change Rule

Any change to these surfaces must update:

1. `docs/mission-contract.md`
2. `docs/mission-claims.edn`
3. `docs/mission-evidence.edn`
4. relevant tests/scripts/artifacts referenced by the evidence entry

Green tests without updated claim/evidence records are not accepted as
mission-surface convergence.
