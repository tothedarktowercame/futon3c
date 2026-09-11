# Qualified execution collision review

Independently ran retained `repro.clj` against real cohort activation/start and real repair publication in disposable storage. Two distinct canonical cohort roots with the same preregistration both generated local attempt-001 and external run4-u88-20260911-v1--attempt-001. The second writer refused FileAlreadyExistsException; the outer initialization finding was written only in that disposable repair store. Output: `/tmp/run4-qualified-collision-review.out`.

This proves cohort ID plus local ordinal does not distinguish separate execution authorities. It does not prove whether the historical disposable executions were concurrent, sequential or recovery attempts. The historical production attempt is not associated with those deleted test roots by this evidence.

Current `run4-series-service-test` already rebinds repair/default-root and tripwire/default-trip-root inside the full-core fixture and supplies repair-system-record-fn. Do not claim its current source still lacks that isolation. Source inspection is not a comprehensive hermeticity proof; a structural fix should add real-writer containment controls as well as execution-identity tests.

Required invariant: all durable auxiliary IDs and their readers must use one validated execution authority; distinct authorities cannot alias merely because declared cohort IDs and local ordinals match. Same-authority replay must distinguish byte-equivalent evidence from conflicting content without weakening CREATE_NEW/no-overwrite behavior. Existing historical IDs and receipts remain readable and immutable. Avoid injecting randomness into finding names to evade identity joins.

Successor-v2 offline verification is already complete at dc0aceeb; independent artifact review remains job invoke-1789142588808-20194-c97a7581. This later collision is a distinct repair obligation, not a reason to relabel or rewrite that qualification.
