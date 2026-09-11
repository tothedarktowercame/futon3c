# Execution-authority packet gate

The packet passed its 30-assertion disposable lifecycle at the original
Futon2 source boundary. Concurrent execution-authority work then changed the
three pinned Futon2 runtime sources. The packet provenance has been refreshed
to Futon2 `2185297c` and Futon3c `9a22025d`; the old success is not reused.

The first rerun correctly refused the retained historical qualification with
`:source-drift`. A separate disposable qualification for the current authority
source then reached the actual historical action and found a new boundary:
the versioned identity for cohort
`:run4-successor-v2-selection-admission-20260911-v1` exceeds the repair store's
128-character safe-ID limit. `commit-historical-verification!` rejects it as
`Historical verification execution identity invalid`; the outcome is
incomplete and historical projection refuses. This is not a packet admission,
repair resolution, or successor result.

Codex10 was notified through Agency job
`invoke-1789144794201-20209-6dce8378`. The exact lifecycle must pass again
against the corrected final source bytes before independent packet acceptance.

Futon2 `810be2a9` corrected this boundary by using the bounded identity
`ea1-<authority-digest>--attempt-001`; the digest continues to bind the cohort
ID, preregistration bytes, and canonical data root. The packet was repinned to
that commit for a fresh lifecycle gate.
