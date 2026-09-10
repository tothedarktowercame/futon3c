# U88 deployment template review — 2026-09-10

Independent preflight test 1/7 passes; running the documented command gives
sources current, template disabled, DRAFT eligibility refusal, credential
unprovisioned, six missing roots, supported declaration, consumer unknown,
eligible-to-launch false. These are preparation facts, not verified activation.

The file is an intermediate template, not the actual service config: it uses
:authority-root/:stores/:serving while run4-series-service consumes nested
:run4/:series and trusted-entry expects its own server-owned ports. No
materializer is present in this slice. Likewise no serving boundary invokes
report-durable. The README's operator-invoked function is not an authenticated,
server-prepared report caller. Both remain commissioned implementation work.

Preflight uses first manifest trial, unions source and pin allowlists, echoes
the route and casting is not validated. This suffices for its narrowly stated
source-read facts on the current one-trial packet, not full service authority
validation. The next slice must preserve the real port separation and validate
all trial identities/casting with the production consumers. Mission eligibility
should come from the actual parser/resolver, not a DRAFT substring heuristic.

Required next unit: pure strict template-to-disabled-service-config materializer
plus authenticated read-only report caller using server-owned roots and current
prepared identities. Show the actual async isolated roundtrip using the U88
configuration through recording/readback, route battery and visibility. No
credential generation, production stores, mission activation, live namespace
loads, acceptance or launch occurs as part of that work.
