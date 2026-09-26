# Independent reconciliation review — changes required

Reviewed futon3c `3361253cbb038cb27f828cda23a2c67a8bfd0c55`.
Existing focused test passes: 1 test, 7 assertions. Independent executable
reproduction `review-reproductions/reconciliation-publication.clj` returns
`{:escaped-publication? true, :stale-evidence-published? true}`.

The reproduction uses disposable test evidence. The existing test corrupts
its last checkpoint after constructing its record. Public `publish!` then
accepts that stale record, with an altered controller attempt ID `../escaped`,
and writes outside the specified output directory. The low-level recording
append function does not supply the path validation its normal caller performs.
Require validated safe identity, canonical output containment, and evidence
recapture at the publication boundary; arbitrary caller maps cannot establish
an admitted reconciliation. Preserve immutable replay and conflict refusal.

Additional source-review issue: the six cohort cells only join each other by
caller-supplied cohort/local identity. No validated source binds that prefix
to the controller click/wrapper failure. Schema and internal identity checks
for reservation/click/binding are also absent. Tests currently author reduced
maps rather than exercising retained production evidence. Require a real-case
frozen fixture and explicit derivation of each cross-store join; a missing
historical join must remain unknown, not be supplied as an asserted identity.

All 13 retained live evidence hashes rechecked unchanged. No live append,
close, dispatch, capacity change, or reload performed. This review does not
accept the implementation for live use.

Stop-line inspection remains useful: actual repair API reports selection
 timeout finding 057 open with no implementation/resolution. Its distinct
repair/successor discharge contract still applies. Historical repair-reviewer
is the coordinator codex-17; the recorded invoking state explains why merely
reusing ordinary author/reviewer availability is insufficient. No disposition
or future availability is inferred.
