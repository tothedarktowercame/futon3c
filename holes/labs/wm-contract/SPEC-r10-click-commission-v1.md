# R10 click commission v1

Status: proposed prerequisite contract; no production binding or commission is
created by this packet.

An authority file is exactly one EDN map with keys `:schema`,
`:commission/id`, `:commission/issuer`, `:commission/source-pin`, and
`:commission/scope`. Its schema is `:wm/r10-click-commission-v1`. The server
configuration, not an HTTP request, supplies its absolute path and exact SHA-256.
The source pin and issuer are nonblank authority facts; request fields cannot
replace them. The production adoption packet must commit and independently
review the real file and binding.

The server-issued click id is the declared dispatch id. A dispatch receipt must
echo `:node :R10`, the pre-existing commission id, and equal nonblank
`:click/id`/`:dispatch/id` values.

Before invoking a dispatch function, the mechanism creates
`<reservation-root>/<commission-id>.edn` with CREATE_NEW. The caller must supply
an explicit durable store root; there is no default atom or path. Existing files
refuse as `:r10/duplicate-commission`, including after restart.

Persisted states are `:reserved -> :dispatched -> :recorded`. The reserving
caller alone creates `:reserved`; the receipt-validating caller advances to
`:dispatched`; the evidence-recording caller advances to `:recorded`. Reading a
remaining `:reserved` intent reports recovery state `:dangling`: it is not
reusable and is not silently replayed. Completion is deliberately non-idempotent
unless a later integration proves an exact matching completion record.

Typed refusals include `:r10/invalid-commission`,
`:r10/duplicate-commission`, `:r10/unlinked-dispatch-receipt`,
`:r10/recording-failed`, `:r10/reservation-missing`,
`:r10/reservation-invalid`, and `:r10/invalid-transition`. A malformed receipt
is necessarily an after-dispatch refusal and carries `:dispatch/occurred true`;
reservation/store failures occur before dispatch.
