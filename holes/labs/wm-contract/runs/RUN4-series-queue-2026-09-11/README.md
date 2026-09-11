# RUN4 frozen-series queue (disabled)

This component is an in-process, server-owned control plane for a finite list
of already frozen RUN4 series. It does not add an HTTP endpoint and it does not
provide a second executor: every tick calls `run4-series-service/step!` with the
entry's fixed server configuration, authentication headers, and manifest
request.

Each entry binds an exact manifest digest and series identity to the canonical
manifest authority already accepted by the series materializer, plus the exact
execution-cohort identity and preregistration digest. The runtime configuration
is deliberately not serialised because it contains the credential-derived
header and server-owned ports. Durable state contains only the queue/config
digest, cursor, click identity, state, and hold reason.

The only installation seam is private JVM administration after constructing
the entries from reviewed server configuration:

```clojure
(require '[futon3c.wm.run4-series-queue :as run4-queue])
(run4-queue/start! server-owned-queue-config)
```

`recover!` restores scheduling only when durable state says `:running`.
`:held` and `:stopped` remain passive. `resume!` is the explicit operator action
needed after a held result. `stop!` cancels scheduling and serialises the stop
behind any current tick; it never calls the series service itself.

No production queue, root, manifest, cohort, credential, or capacity was
created or activated by this change.

## Current eligibility boundary

The read-only repair-store audit on 2026-09-11 shows the first open
non-environmental obligation after the awaiting-validation records is
`repair-initialization-b076e0f8-dbc2-4368-80a0-073d243951a0-initialization-failed`.
The later
`repair-run4-u88-production-successor-20260911-v2--attempt-001-untyped-failure`
is also open. Consequently an ordinary U88 queue entry is not currently the
next eligible action: the existing runner's stop-line precedence must first
select and satisfy the initialization repair contract. A queue encountering
either incomplete evidence or a historical awaiting-validation observation
holds; it cannot infer task success, reset, or retry.
