# Bounded repair acceptance — 2026-09-11

Accepted for the two reproduced faults: futon2 5a1979e4/7d0215f4 and futon3c b3577dd2. This does not accept or close the failed live run, authorize a retry, or implement the reconciliation proposal.

## Independently executed evidence

- Original qualified-tripwire reproduction: local-only lookup still reports missing-durable-stop-line; qualified lookup passes. This is expected: a local-only observation cannot identify the cross-cohort repair. The corrected producer carries both IDs; the real-writer/real-T3 regression uses external-attempt-id with local attempt-id retained. Tripwire suite: 33 tests / 71 assertions, zero failures/errors.
- Original marker-race reproduction: HTTP 500, reason existing-start-disappeared-or-changed, zero click calls. Added permanent regression `run4_lifecycle_inspection_test.clj`, also proving no reservation was created: 1 test / 4 assertions, zero failures/errors.
- Existing controller suite: 12 tests / 53 assertions, zero failures/errors.
- Existing series service suite: 10 tests / 60 assertions, zero failures/errors. This retains the exhausted-existing and exhausted-fresh distinction.
- New regression lint: zero errors/warnings; parentheses and diff checks pass.
- Every retained live source hash in TERMINAL-SOURCE-PINS-2026-09-11.json still matches. No live reload, step call, close, reset, retry, or acceptance write occurred during this review.

## Correspondence to the Lean draft

Mathlib4 00eb0c045d states separate qualified identity and lifecycle properties. The phase-context external-attempt-id supplies the missing identity component to the T3 join, while the cohort checkpoint retains its local identifier. This is tested runtime correspondence, not a proof of the string codec in Lean.

The controller checks the server-owned inspection-only marker under its existing lock before the fresh-reservation branch. If no valid started lifecycle remains, it refuses. Present started/terminal records still pass the existing schema, identity and admission checks. Thus the captured operation cannot become fresh dispatch after a disappearing marker. The code does not pin the exact earlier started-file bytes; acceptance here covers disappearance refusal plus the controller's existing identity checks, not a new claim of byte-for-byte historical comparison.

## Reconciliation proposal review

The distinct infrastructure-reconciliation-required state is a reasonable proposed representation of an infrastructure outcome with unknown task result. It must stop dispatch without pretending the trial succeeded, failed as a task, or was a busy not-attempted case.

The draft's requirement to prove no dispatch cannot be discharged by missing files alone. Admission needs the exact identity-bound checkpoint prefix, positive producer evidence of where execution stopped, and a cause record for the closure exception; missing or contradictory evidence must remain unknown. The local cohort identity, global auxiliary identity and wrapper-returned initialization identity must be recorded separately, with explicit relations rather than substitution.

Do not implement automatic appending of agent-unavailable solely because the existing six sorries have that label. Validate their producer meaning and the observed repair-role availability before proposing the close. A reconciliation record should preserve the incomplete binding and explain the mismatch, never fabricate the absent run record/projection. These are remaining design/implementation obligations; the immutable historical failure itself must stay visible after reconciliation.
