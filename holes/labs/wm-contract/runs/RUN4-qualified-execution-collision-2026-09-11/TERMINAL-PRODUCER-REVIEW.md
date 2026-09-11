# Remaining terminal producer/consumer gap

Independently ran terminal evidence 10/54 and historical projection 2/7; both pass. Inspected 0af4708a/4cca2542 and Futon2 844f9189/2380bd27. No combined new-history/new-successor acceptance yet.

Actual disposable terminal-reader reproduction in new-record-shape-review.clj adds the two fields emitted by the current real runner (`runner-execution/identity`, `runner-execution/provenance`), derived with real cohort authority APIs, to the existing terminal fixture and recomputes source/projection/binding hashes. read-terminal-evidence-bundle refuses `:run-record-binding-mismatch`: read-run-record!'s exact key set still only allows the old record shape.

This fixture isolates producer-shape compatibility; it does not claim an actual new-authority cohort was executed in that fixture. The full real-producer composed gate remains required.

Additionally, resolve-from-durable! derives the successor identity from the configured cohort and compares only cohort ID/SHA/local attempt to the run record. It does not join the run record's new execution identity/provenance to that closed execution. Merely adding the new keys to the allowlist is insufficient: distinct canonical roots may share declared ID, preregistration and local ordinal. Require exact versioned producer-to-closed-execution agreement and reject foreign-root substitution, missing/malformed/partial provenance. Legacy absence needs an explicit branch, not truthiness or ignored fields.

Retain historical v1/v2 semantics and complete a newly generated historical-admission-to-successor materialized gate without replacing strict readers. No live state changed.
