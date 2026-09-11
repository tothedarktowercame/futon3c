# Applicability review and existing-start observation

Reviewed 1cf69c80. New-admission mismatch refusal is correct at its tested seam.
The same predicate was also applied to existing-start inspection. After historical
admission, its finding is awaiting-validation and no longer an open action. A
retained regression reproduced two failed assertions: preparation refused and
called current-action applicability for an existing inspection.

The correction limits fresh-action applicability to new-capacity preparation.
Existing-start inspection still authenticates and validates its sources, config,
cohort activation and identity; the existing controller inspection-only capability
requires the exact durable started/admission lifecycle under lock. No request key
can enable this mode, and it does not authorize a new reservation or dispatch.

Verification: trusted entry 9 tests / 50 assertions, service 11 / 63; all pass.
Lint and parentheses pass. This includes zero remaining capacity for inspection
and retained refusal before capacity preflight for a new inapplicable action.

The old historical packet composed test was attempted with Futon2 test paths;
it refuses source-drift during offline verifier setup before reaching service.
That is not a passing composed replay gate. Do not alter historical production
qualification to repair a fixture. A fresh isolated actual-selection/replay gate
is still required for the new applicability boundary. The new real-store test
uses the actual candidate reader but does not itself invoke runner selection.

058's retained finding has its own code-commit discharge contract. Current
first-open selection is 058, while 057 is awaiting-validation. Neither removing
historical-action configuration nor reusing 057's verification discharges 058.
No live reload, record, capacity or attempt changed in this review.
