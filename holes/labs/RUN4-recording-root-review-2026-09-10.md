# RUN4 recording root review — 2026-09-10

Independent 5bb5ae87 tests: recording 3/9, acceptance 6/26 pass. Acceptance
now separately reads a persisted record; missing recording is no longer
replaced by route-battery checks.

Boundary repro /tmp/run4-recording-root-review.clj creates two disposable
directories and an attempt-1.edn symlink from the configured root to a valid
record in the other directory. Real read-bundle-recording! accepts the external
file; persist-bundle! returns its external canonical path. Both operations
must enforce the configured canonical root and reject escaped/nonregular
artifacts. Cleanup removed the temporary files. No production data touched.

The writer delegates to realized-recording/persist!, which opens CREATE_NEW
and writes directly, without the fsync/atomic publication used by RUN4 admission.
Its immutable semantics must be retained, but durable acknowledgement needs the
same tested publication guarantees before controller advancement. A partial
artifact must remain a corruption/refusal, never be silently overwritten.

Existing findings ef6fff4a and 8663e900 remain: casting author is still emitted
as policy and trial ID as tick; rerunning the nil-attestation reader repro still
returns succeeded/safe. Resolve these explicit cases before calling the chain
accepted. Lifecycle extraction remains separate unfinished implementation.
