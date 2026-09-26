# Downstream versioned identity review

The accepted successor artifact still passes the actual production admission reader via check-admission-input.clj. Execution log: /tmp/run4-postauthority-admission-check.out. This does not establish post-execution compatibility.

Source trace at 8788443d/2185297c exposes remaining incompatibilities:

- The actual runner historical branch now sends the cohort-qualified execution identity to historical-verification-execute-fn; its local runner attempt remains attempt-001.
- run4_historical_projection/read-bundle! still requires runner-attempt/id to equal execution-attempt.id. That is valid for legacy local identity but false for new authority-qualified identity. The current projection retains only cohort ID/SHA and must join versioned identity to canonical cohort evidence rather than relax equality blindly.
- repair_obligation/commit-historical-resolution! still requires verification-attempt.id to equal verification-execution.attempt-id and constructs validation-attempt.id as cohort-id--local-attempt. Those predicates reject new ea1 identities. The transition needs validated versioned associations on both sides and must retain legacy evidence explicitly.

These are code-traced incompatibilities, not a claimed successful composed reproduction. The existing packet cannot currently replay its old qualification producer against changed runtime pins, so a new stable-source composed gate is required after corrections. Do not hide the mismatch by changing raw local attempt IDs or accepting unchecked caller identity maps.

Correction job 4b0eb3f0 is running for nil authority and symlink replay. This downstream work must join that reviewed change before packet installation. No live state or receipt was changed.
