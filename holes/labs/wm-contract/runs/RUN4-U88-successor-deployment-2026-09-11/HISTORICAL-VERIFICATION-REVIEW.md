# Historical verifier review — 2026-09-11

Reviewed 2c21cdc0. Its focused suite passes 1 test/6 assertions but uses invented
qualification rows, not actual producer receipts. Independent reproduction
admits an executed job belonging to the author as awaiting-validation. Retained
repro: review-reproductions/historical-verification-actor.clj. It must refuse
a wrong/missing reviewer identity before publication, using the actual Agency
job identity contract and trusted reader, not a caller's identity assertion.

A second reproduced defect admitted an entirely unexecuted author job because
only EDN filenames reached the non-code review gate. Fixed independently in
the public Futon2 helper: executed evidence is now mandatory for all artifact
types. Full runner tests pass 128/606; lint 0/0, parens clean.

Further required checks from source review:
- Qualification schema, pinned manifest, argv/timeout population, source list
  and stdout/stderr bytes/digests are never validated. Exact ID coverage alone
  does not establish actual producer output. Consume a real producer receipt
  against its reviewed manifest and source authority, with current source
  checks. Finding schema/status/class must be supported, not merely an ID.
- Ancestry only establishes last-commit precedes an asserted source-head.
  It does not bind the current source bytes or executed checks to that head.
- The supplied reviewer is never joined to the fetched job, and a substring
  marker is not an exact review binding. Require an unambiguous marker and
  authority-backed job/attempt/source association; freshness must be evidence,
  not the word fresh in a variable.
- Output publication lacks canonical target and no-symlink validation before
  the low-level append helper. Reuse the producer's explicit root guards.

Do not enable the action or publish live admission before these corrections.
The producer remains accepted within its narrower documented boundary. The
new canonical packet is preparation only until actual action integration.
No live records, services, capacity or attempts were changed.
