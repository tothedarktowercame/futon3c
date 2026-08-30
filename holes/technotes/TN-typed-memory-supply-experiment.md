# Typed memory supply: observation before policy

The F36–F57 review distinguishes `:substitutive-content` from
`:regulative/process`, but does not establish that one class should receive a
larger experimental supply. Production snapshot ordering therefore remains
unchanged.

`futon3c.apm.memory-snapshot/order-candidates` now records `:kind-counts` and
`:kind-stratification :observed-only` in every snapshot ordering record.
Absent and legacy kinds are reported as `:unknown`; they are not inferred from
prose. `stratify-candidates` provides a deterministic, cardinality-preserving
supply order, and callers may request it explicitly with
`:kind-stratification :substitutive-first`. That opt-in is recorded in both the
ordering mode and signal, so an experimental count cannot change invisibly.

The next experiment should preregister a cap and compare two otherwise equal
arms:

1. the historical relevance order;
2. explicit `:substitutive-first` stratification.

For each arm, retain total offered/used counts, kind counts, unknown counts,
cross-problem provenance, and the class-appropriate close-frame audit verdict.
Do not enable the stratified arm globally until it shows a benefit independent
of total memory supply. In particular, this interface prevents regulative
items from crowding out explicitly substitutive items when the experimental
arm is selected, but makes no claim that such prioritization improves proofs.
