# Cohort-qualified successor review: changes required

Reviewed Futon2 02aa9e8a and Futon3c bee38ba5. The run-record cohort metadata
addition is useful, but the resolution boundary does not yet establish distinct
executions. It qualifies only the successor identity and compares it with the
unqualified immutable historical identity.

Retained reproduction: review-qualified-identity.clj uses the actual strict
terminal fixture, real verification store, actual terminal reader, successor
adapter and resolution store. Both local execution IDs are internal-1.
No activated historical or successor cohort authority is supplied. The fixture
run record claims :successor-cohort and digest d repeated 64 times.

Observed:
- resolved? true
- verification execution: {:kind :runner-execution :id "internal-1"}
- successor execution: {:kind :runner-execution :id "successor-cohort--internal-1"}
- cohort authority roots present? false

The final assertion correctly fails. This is not proof that two real executions
are identical: it proves resolution is accepted without evidence establishing
whether they are distinct. Changing the spelling of only one identity cannot
establish that distinction.

The terminal reader currently allows execution-cohort but does not validate
its preregistration digest against canonical bytes, activation, or the exact
closed attempt. The store checks a structured map's syntax and concatenated
string, not those cohort joins. It also compares that qualified successor with
an unqualified historical verification identity. Production repair admission
must remain immutable; its cohort association must be derived from the existing
strict historical bundle, which already binds the actual store transition and
activated closed cohort.

Required next slice: validate both cohort/local execution associations through
their durable readers; compare like-grained identities, preserving stored
historical identity separately. Same physical execution must refuse even with
different controller IDs. Two real distinct cohorts with attempt-001 must pass
only after both preregistration/activation/closed-attempt joins pass. Fabricated,
missing, stale or mismatched authority must refuse. Do not rewrite historical
evidence, pad capacity or rename caller inputs.

Independent gates:
- terminal evidence: 10 tests / 39 assertions pass.
- series service: 11 tests / 63 assertions pass.
- retained new repro: resolves, then fails the intended rejecting assertion.
- lint zero errors/warnings; parens OK; diff check clean.

No code was loaded live and no successor capacity or admission was created.
Joe's direct trial authorization remains active; this is a concrete repair
before spending that capacity, not an invented permission requirement.
