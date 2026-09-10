# RUN4 effective attestation reader review — 2026-09-10

Independent series service suite at 41373fc7: 6 tests / 41 assertions pass,
including the isolated asynchronous record-writing path. Producer pre-click
and selector checks are present; the latter result is carried into the record.

Durable admission remains incomplete: read-run-record! merely adds
:run4/effective-environment-attestation to the exact key set. It does not
validate the value, flags, hierarchy, or pinned declaration correspondence.
The committed positive reader fixture even uses :flags [].

Executed /tmp/run4-attestation-reader-review.clj against the disposable test
fixture. Replace the record's attestation with nil, recompute the record digest
and both projection-reference digests, then call the real terminal reader.
Result: {:task-result :succeeded, :infrastructure :safe}. Hash integrity does
not establish semantic attestation validity. No live data was read or written.

Before acceptance, require exact attestation schema and complete unique flag
population, true effective booleans, matching required/observed values, correct
consumer references and hierarchy, and correspondence to the actual pinned
serving declaration through the durable provenance. Validate recorded values,
not today's environment during historical readback. Add nil/false/empty,
missing/duplicate/foreign flags, wrong values and declaration-mismatch controls.
Keep the recording non-attestation boundary. Do not weaken fresh producer
checks or infer a successful declaration solely from the field's presence.

This finding belongs to existing integration job df40b60a, not a parallel
implementation. No deployment or launch approval is inferred.
