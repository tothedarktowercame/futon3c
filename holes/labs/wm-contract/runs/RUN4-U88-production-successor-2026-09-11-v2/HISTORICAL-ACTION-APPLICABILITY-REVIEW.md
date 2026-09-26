# Historical action applicability correction

The live successor click `wm-click-9526b317-e016-4865-aed1-9a0812c38db3`
is retained as an incomplete attempt.  This correction performs no live write,
reset, admission, or synthetic finalization.

## Defect and correction

The installed configuration pins historical action 057, but the runner's
actual stop-line rule selects the first obligation whose status is `:open` and
whose class is not `:environmental-hold`.  Repair 057 is already
`:awaiting-validation`; repair 058 is the first matching open obligation.
Consequently the action for 057 was inapplicable, but this was discovered only
after reservation, click creation, and cohort start.

Trusted preparation now applies that same runner predicate and validates the
pinned historical candidate before cohort preflight.  A mismatch returns
`:run4-historical-action-not-applicable`; capacity and click dispatch are not
reached.  The test uses the real repair store and historical candidate reader
with 057 awaiting validation and 058 open.  A separate boundary test proves
cohort preflight is not called after refusal.

Historical successor resolution is independent of historical-action
execution.  A later configuration may therefore omit `:historical-action`
while retaining the exact `:historical-successor` link.  Its repair authority
comes from the successor link's pinned historical-evidence roots.  This does
not resolve 057: resolution still requires a distinct successful grounded
successor and the strict durable evidence reader.

## Current evidence boundary

Repair 058 is a separate open machine-failure obligation from a selection
`HttpTimeoutException`.  Its contract requires a distinct repair commit,
independent review, grounded repair, and distinct production-shaped successor.
The qualification and verification for repair 057 are identity- and
source-bound and do not qualify repair 058.  Thus the smallest coherent next
work is to investigate and satisfy repair 058's own contract before expecting
the ordinary U88 action to be selected.

The failed successor click has no run record or terminal projection, and its
binding is invalid.  Strict terminal observation is therefore unavailable and
must remain unknown/refused; no task or historical terminal can be inferred.

## Reproduction

```sh
clojure -M:test:test-all \
  -n futon3c.wm.run4-historical-verification-test \
  -n futon3c.wm.run4-trusted-entry-test \
  -n futon3c.wm.run4-deployment-config-test \
  -n futon3c.wm.run4-series-service-test \
  -n futon3c.wm.run4-boot-test
```

Expected: all selected tests pass; no production paths are written.
