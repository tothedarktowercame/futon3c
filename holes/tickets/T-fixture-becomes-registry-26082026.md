# T-fixture-becomes-registry-26082026 — a test fixture became a production domain in 110 minutes

**Status (triaged 2026-09-01): PARTIAL -- anti-pattern repaired, data gap by design.** futon2 4a65616: renamed to sampled-candidate-cleans with an explicit no-census docstring, sample test, :domain-mismatch regime, inhabited/bound dial. Still exactly four missions; grounded realization is sample-only. Remaining item is a product decision: bind CLeans for WM-selected missions or record sample-only as the contract.

Parent: `E-R8-red-ring-fill.md` slice 5. Found 2026-08-26 by claude-13 while
diagnosing why R8 has been red since July. Status: open — the fix is a design
call, not a patch.

## The finding

`futon2/src/futon2/aif/actuator_a3.clj:372`, `reviewed-candidate-cleans`, is a
hardcoded map of **four** missions → CLean paths. Its docstring says, twice,
what it is: *"for the A3 live-test suite … Not a formal proof — a grounded,
re-runnable check."*

`futon2/src/futon2/aif/fold_realized.clj:113` reads it as the set of missions
the system can ground. The mission the WM actually selects,
`futon6-d/mission/bayesian-structure-learning`, is not in it — so `bound = 0`
and `realized-score` is `nil` for every tick.

## Provenance, to the minute (2026-07-08)

    08:06  6ceadc4  fixture created, 4 entries ("the three A3 live tests")
    09:56  723cacf  production producer points at it
    10:11  6261d74  two more missions bound — never added to the map
    16:53  b624242  armed default ON, "live-CAPABLE, latent"

**110 minutes** from fixture to domain. The capability armed at 16:53 was four
missions; the message says "live-CAPABLE" without qualification.

## The pattern

> **A fixture becomes a registry.** A list enumerated for testing is read by
> production as though it enumerated the domain. Nothing fails: the consumer
> returns a well-typed empty answer outside the fixture, and the caller cannot
> distinguish *no data* from *out of domain*.

Forces: a fixture is the only curated list available when you need one; adding
to a *test* list reads as test maintenance, so it is skipped when new cases
bind; and "not in the map" and "nothing to report" are the same value.

## Proposed requirement

**A producer declares its domain, and a substitution may not shrink it
silently.** `realized-outcome-of` has domain *any enacted decision*;
`realized-outcome-grounded` has domain *four missions*. That contraction is
recorded in no commit message, docstring, or excursion.

This is the second clause of the module-1 formal property (see
`p4ng/empirics-futon/NOTE-modular-formalisation-order.md`) and it is the clause
that would have rejected `d36086f`.

## Cross-references

- `futon2/holes/NOTE-the-whitelist-provenance.md` — full provenance.
- `futon2/holes/NOTE-slice4-slice5-understood.md` — why slices 4 and 5 are in series.
- Candidate for `p4ng/plop-2026.tex` as a named anti-pattern.
