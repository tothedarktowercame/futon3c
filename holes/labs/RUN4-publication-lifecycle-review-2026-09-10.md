# RUN4 publication/lifecycle review — 2026-09-10

Reviewed bd592699 and a8de005c. Independent tests pass: recording 9/30,
controller 12/53, visibility 7/20. Publication now serializes cooperating
publishers under normalized JVM and OS locks before checking the target and
moving it; replay uses strict single-form EDN. This fixes the demonstrated
check/move race between these publishers, without claiming exclusion of
arbitrary external filesystem writers or a subprocess/power-loss experiment.

The serving adapter obtains lifecycle from controller/read-lifecycle!, with
its server-prepared trial map. The reader reuses event/admission/start/terminal
checks and validates the predecessor chain. Visibility represents validated
busy and prior-unsafe not-attempted markers as blocked; the legacy path still
refuses unjoined markers. No success or operator acceptance is inferred.

Selected-policy absence was independently accepted at c85ce4e1 (7/25);
historical required-value validation includes follow-up 72a4c364 (6/29).
U88 packet and loader review remains 32a94134 / 52a28cd9. The next work is a
concrete disabled deployment configuration and read-only preflight, not another
launch. Mission activation, credentials and process changes have not occurred.
