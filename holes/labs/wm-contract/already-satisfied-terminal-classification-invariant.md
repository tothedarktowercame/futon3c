# Producer-qualified already-satisfied terminal classification

Status: bounded state-machine invariant for the terminal-classifier repair. This is not a new outcome-admission rule, a learning-success rule, or authority to rewrite an injury ledger.

## Invariant

Let `r` be a retained terminal run record. `r` is a producer-qualified already-satisfied close only when the runner-retained producer fields agree that:

1. `[:failure :kind]`, `[:failure :detail :failure-kind]`, `[:failure :detail :outcome]`, and `[:run-output :outcome]` are all `:already-satisfied`;
2. `[:failure :stage]` and `[:failure :detail :failure-stage]` are both `:author-wait`, the boundary where the producer runs the existing-artifact refusal guard before the fresh-artifact gate;
3. `[:failure :detail :grounded-commit]` contains a nonblank repository name and a 40–64 lowercase-hex commit SHA.

Then terminal classification is `:non-injury` with the distinct reason `:producer-qualified-already-satisfied`.

The classifier trusts these retained fields only as the output of the existing PROOF-2b producer boundary. It does not re-run Git, infer evidence from author prose, search arbitrary metadata, or accept `:already-satisfied` from one field alone. Commit existence was checked upstream by `author-refusal-classification`; this packet does not create a second admission authority.

Unsupported author claims remain `:invalid-author-refusal`, and a DONE claim at the base HEAD remains `:artifact-binding-mismatch`. Any missing, malformed, or contradictory retained qualifier fails closed as an injury.

## Retry closing law

Producer-qualified already-satisfied is a valid no-change terminal answer, but it does not establish that an earlier repair corrected its original failure boundary. Therefore:

- with no attributable enacted retry, integration appends the ordinary non-injury observation;
- with an attributable enacted retry, integration appends `:retry-inconclusive` and leaves the injury enacted;
- it is never translated to `:successful-task-outcome` and never discharges an injury.

Record deduplication remains by the existing immutable record pin.

## Warrant and limitation

The producer warrant is `full_loop_runner.clj`'s PROOF-2b contract: a feature card saying the selected construction already holds plus an artifact reference that resolves to an existing commit produces `:already-satisfied` with that commit as `:grounded-commit`; `throw-if-author-refused!` closes at author-wait before the fresh-artifact gate; close-core suppresses a repair finding for that outcome. `author_already_satisfied_test.clj` fixes its positive, nonexistent/absent-commit, and DONE-at-base controls.

The state-machine warrant is `DarkTower/WarMachine/SelfHealRoute.lean`: `RetryClickOutcome.closing` and `closing_dischargesOnlyOnSuccess` say only `successfulTaskOutcome` discharges, while every other constructor is retry-inconclusive; the source-alignment note for `integrate` likewise says every non-injury reason other than `:successful-task-outcome` appends `:retry-inconclusive` and leaves the injury enacted.

No existing Lean constructor or theorem names producer-qualified already-satisfied. This packet uses the established catch-all side of that closing law; it does not claim a new Lean theorem. Adding the named constructor and an explicit theorem would be a separate formal-alignment packet if required.
