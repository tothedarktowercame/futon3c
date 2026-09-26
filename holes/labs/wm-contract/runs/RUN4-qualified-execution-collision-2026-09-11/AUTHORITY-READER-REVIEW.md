# Authority-reader review findings

Reviewed Futon2 8788443d/2185297c. Cohort suite independently passes 13 tests/54 assertions. The snapshot-sharing correction is appropriate, but two retained real-reader controls still admit invalid authority.

Run from Futon2: `clojure -M:test ../futon3c/holes/labs/wm-contract/runs/RUN4-qualified-execution-collision-2026-09-11/authority-reader-review-repro.clj`.

1. A real activated/start/checkpoint/close lifecycle with a present `:execution-authority nil` time-step field returns identity-version 0. Legacy absence must be distinguished from malformed presence; otherwise corrupted new provenance downgrades to legacy interpretation. Refuse malformed present authority. Deleting all version evidence is a broader provenance issue and must not be represented as solved by this check alone.
2. Publish a real system finding, move identical bytes to an external disposable file and replace the finding with a symlink. The new identical replay branch accepts it. Strict replay must validate canonical root, parent directories, regular non-symlink file and strict bytes before acknowledgment. No external artifact can authorize a finding in the configured store.

Both reproductions touched disposable roots only. No live repair, capacity, history or runtime changed. Authority repair is not yet accepted; deployment packet repinning remains pending the corrected implementation and independent gates.

## Corrected candidate

Futon2 `3fb144a4` distinguishes key absence with `contains?`: a genuinely
legacy time-step with no authority key remains version 0, while present nil,
false, malformed, or foreign authority refuses as
`:closed-execution-unavailable`. This does not claim that erasing every trace
of provenance from old-schema evidence is detectable.

System-finding replay now requires a canonical nonsymlink root and findings
directory, a regular nonsymlink target, exact bytes, and serialized JVM/OS
publication locking. New publication uses `CREATE_NEW`, forces the file and
parent directory, and never replaces a conflict. Root, parent-directory, and
target symlink controls refuse; two concurrent identical publishers both
return the one immutable record.

The retained reproduction now reports both original cases as typed refusals.
Focused results: cohort 14/59, repair store 11/43, runner 131/625, tripwire
33/71, and clj-kondo 0/0. Independent review is still required before live
loading or packet repinning.
