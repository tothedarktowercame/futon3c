# Durable per-click acceptance — Packet 6

Base: `e27d3bf93ce4435f8614b3c89b4c78b33b568f2f`.

`POST /api/alpha/wm/click` now persists a `:wm/click-state-v1` queued record
and its `:wm/click-idempotency-v1` binding before returning. Ordinary ration
consumption occurs once at this acceptance boundary. The worker then moves the
same record through running to completed/failed. GET with `click-id` reads only
that durable record; it never consults the mutable global `last-result`.

Same key plus same canonical payload returns the original click. Same key plus
a different payload refuses with `:wm-click-idempotency-conflict`. Single
flight remains authoritative for distinct concurrent requests. Explicit cast
resolution and preflight remain ahead of acceptance at the HTTP boundary.

Hermetic controls cover slow execution beyond the caller wait, lost response
retry, conflicting reuse, one ration charge, queued/running/completed states,
and a stale unrelated global result. No live click or production data was
used.

Untested production boundaries: recovery of execution itself after a serving
JVM crash (the accepted identity remains durable, but the in-process worker is
not a durable queue consumer), real reverse-proxy response loss, and live
ordinary-budget contention across separate serving JVMs.
