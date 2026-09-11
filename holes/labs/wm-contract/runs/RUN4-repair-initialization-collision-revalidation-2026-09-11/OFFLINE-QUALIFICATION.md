# Initialization collision: executed offline qualification

2026-09-11, Codex-17. Plan 730061ac reviewed at the actual two-cohort
runner and writer/T3 regressions. Canonical qualification producer executed
its two exact commands in separate processes into the new offline-evidence root.

Receipt SHA256:
`02821ef4033146884d9ea4993d9bfd5c3a26170762aad0fcfd1d2e8c4babbbbe`.

- Runner: 130 tests / 617 assertions; exit zero, not timed out.
- Tripwire: 33 tests / 71 assertions; exit zero, not timed out.
- Audit rechecked strict single forms, plan SHA, finding identity, five current
  sources, exact ordered command population/timeouts, captured output hashes.
- Futon2 HEAD stayed `16c215c5bf615741715dc92bce47817fbd96e9e1`.
- Audit lint 0 errors/warnings and check-parens passed.

Receipt still says independent review not performed and repair admitted false.
It belongs only to the initialization-b076e0f8 finding; no057/058 receipt or
live finding was changed. Independent executed review of this exact receipt is
next. This is not an admission, repair discharge, cohort allocation or trial.
