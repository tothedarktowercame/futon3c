# P13a rule records

`futon3c.agency.rule-record` is the validated writer for `:rule/record`
hyperedges. It uses the existing P6b mint route and explicit XTDB valid time;
no futon1b schema change or shared JVM reload is needed. It never supplies an
existing id or a retract operation. An optional durable delivery key makes the
same reviewed write idempotent; a conflicting payload is refused by P6b.

The schema requires internal, input-output and accomplishment descriptions,
a refused-input example, an observable accomplishment, a world assumption,
and a HOWEVER. Known failures require both a failure mode and an observable
signal. Explicit unknown failures require nonblank text and an ISO review date
or instant. Temporary measures require a typed incident reference and a
withdrawal condition. Typed refusals use `:reason :invalid-rule-record` and
`:field`; validation happens before any HTTP request. The generic hyperedge API
is unchanged; rule callers use this writer's validation contract.

```
clojure -M -m futon3c.agency.rule-record holes/labs/M-象-2000/P13a-requisition-rule.edn
clojure -M -m futon3c.agency.rule-record --write holes/labs/M-象-2000/P13a-requisition-rule.edn
```

The first command only validates. The second mints, then verifies exact properties
and endpoints through GET hyperedges at the explicit valid time. Retrying uses
the same delivery key. Store errors and readback mismatches are errors, not
successful receipts.

The requisition instance is reconstructed from 80428193, 5146606d, d5e3147e,
MAP-Q5 and the actual gate-fails-loudly signature. Its incident ref names Joe's
original 15:48 evidence record. That ownership is explicit in the owner's P13a
commission, not inferred from simultaneous events. Accomplishment, assumption
and withdrawal condition are marked retrospective, not attributed as words
adopted in September 24's conversation. The known HOWEVER names the observed
42 repeated notices. The earlier clock-default implementation is distinguished
from the final explicit requisition rule.

The record is valid from its preparation time on September 27; this is not a
claim the old runtime gate remains active. P13b will record adoption, commit and
runtime activation separately. This write does not clear the incident, withdraw
a measure or change any existing record.

Gates: clj-kondo zero errors/warnings; check-parens OK for source, test and EDN;
`futon3c.agency.rule-record-test`: 5 tests, 36 assertions, zero failures/errors.

Live receipt (2026-09-27): `act:8f827fa4-b3de-46a3-a9cb-f3262f7a94ff`.
GET `/api/alpha/hyperedges?type=rule/record` at valid-as-of
`2026-09-27T20:11:00.759554Z` returned the minted record with all three layers,
world assumption, known HOWEVER and original incident evidence reference.
Exact readback passed; repeating the reviewed write returned the same id,
`:no-op? true`, `:verified? true`. Original incident evidence and the existing
5146606d commit hyperedge were compared before/after and are unchanged.
Raw local readback: `/tmp/p13a-live-readback.json`. No server reload occurred.
