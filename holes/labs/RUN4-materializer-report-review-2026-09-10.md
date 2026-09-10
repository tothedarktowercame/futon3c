# RUN4 materializer/report integration review — 2026-09-10

Independent tests: deployment 2/7 and report 1/2 pass at 43dbe03e/5cf0856e.
These tests do not establish composition or authentication.

Concrete mismatch: materialize sets :run4 :acceptance to :reserved-unwired,
whose committed value has only :battery and :reason. report! needs
:control-map-root, :control-map-ref and :control-map-sha256. The actual
materialized U88 configuration therefore cannot support the report caller.
Fix the template and materializer together, with exact source-pinned control
map authority; do not require a caller to invent extra configuration later.

Authentication is only inside mapv over manifest trials. The committed report
test uses :trials [] and empty headers and obtains an incomplete report without
any authentication. Authenticate independently before evidence reads, reject
empty/malformed manifests through the same validator, and test wrong/missing
credentials against a real nonempty U88-shaped manifest. Reusing prepare may
also impose current deployment/misson/consumer requirements on historical
reporting; distinguish that operational restriction explicitly from historical
attestation validity.

report! parses the manifest from a second read after capturing manifest-text,
and does not verify its configured allowlist/pinned digest before traversal.
Use one captured validated snapshot and the existing authority validators.
Current source freshness alone is not a substitute for the pinned manifest.

The required combined fixture must start with the actual template materializer
and U88 configuration, then traverse actual trusted/async/recording/lifecycle/
visibility/report functions with only task actuation stubbed and disposable
OPEN mission/root/credential fixtures. It should expose these mismatches, not
fill missing fields with a separate hand-built test config. No production
credential, mission, directory, namespace, service or run was changed here.
