# Qualified execution collision review

Independently ran retained `repro.clj` against real cohort activation/start and real repair publication in disposable storage. Two distinct canonical cohort roots with the same preregistration both generated local attempt-001 and external run4-u88-20260911-v1--attempt-001. The second writer refused FileAlreadyExistsException; the outer initialization finding was written only in that disposable repair store. Output: `/tmp/run4-qualified-collision-review.out`.

This proves cohort ID plus local ordinal does not distinguish separate execution authorities. It does not prove whether the historical disposable executions were concurrent, sequential or recovery attempts. The historical production attempt is not associated with those deleted test roots by this evidence.

Current `run4-series-service-test` already rebinds repair/default-root and tripwire/default-trip-root inside the full-core fixture and supplies repair-system-record-fn. Do not claim its current source still lacks that isolation. Source inspection is not a comprehensive hermeticity proof; a structural fix should add real-writer containment controls as well as execution-identity tests.

Required invariant: all durable auxiliary IDs and their readers must use one validated execution authority; distinct authorities cannot alias merely because declared cohort IDs and local ordinals match. Same-authority replay must distinguish byte-equivalent evidence from conflicting content without weakening CREATE_NEW/no-overwrite behavior. Existing historical IDs and receipts remain readable and immutable. Avoid injecting randomness into finding names to evade identity joins.

Successor-v2 offline verification is already complete at dc0aceeb; independent artifact review remains job invoke-1789142588808-20194-c97a7581. This later collision is a distinct repair obligation, not a reason to relabel or rewrite that qualification.

## Structural repair candidate

Futon2 `8788443d` adds `:wm/cohort-execution-authority-v1`. Its authority ID
is deterministic over the declared cohort ID, pinned preregistration SHA-256,
and SHA-256 of the canonical cohort data-root path. The final execution ID also
binds the local attempt ID. New runner time-step judgments retain the authority
map, and strict `closed-execution` recomputes it from its supplied server-owned
binding. Ledgers without that field retain the legacy `cohort--attempt`
identity as explicit version 0; no historical bytes changed.

The updated reproducer retains the old concatenation as a negative control and
uses the public authority/identity producer for the repaired arm. Its two
distinct activated roots both allocate `attempt-001`: the historical IDs alias
and produce typed `:repair-finding-conflict`, while the versioned IDs differ and
both real repair findings publish. System-finding replay is acknowledged only
when the complete serialized bytes match; callers must provide stable
`:opened-at` to request that narrow replay behavior.

Focused gates at this commit:

- full-loop cohort: 13 tests / 54 assertions
- full-loop runner: 131 tests / 625 assertions
- repair obligation: 10 tests / 39 assertions
- tripwire: 33 tests / 71 assertions
- Futon3c series service: 14 tests / 81 assertions
- Futon3c terminal evidence: 10 tests / 54 assertions
- clj-kondo over all six changed Futon2 files: 0 errors / 0 warnings

This source change is deliberately separate from the already executed
successor-v2 qualification and verifier artifact. Those artifacts remain
truthful for their pinned pre-change Futon2 HEAD, but they do not qualify or
review `8788443d`; any future packet claiming this collision repair must run a
fresh qualification and independent review rather than silently repinning.
