# Applicability and replay independent review

## Review of 652ba19f

The existing-start exception is correctly server-owned: the request schema has
no inspection flag, and the service derives it only from a durable started
file.  Trusted preparation still validates authentication, frozen sources,
configuration, cohort identity, and the zero-capacity read mode.  The
controller verifies the exact started/admission lifecycle while holding its
JVM and OS advancement locks.  A missing or changed start cannot authorize a
fresh click.

Review found and corrected one related race: the service previously ran a
standalone preflight/reconciliation before `controller/step!`, causing
`prepare-trial` to execute twice.  A started file removed between those calls
could turn inspection into fresh admission.  The redundant pre-lock pass is
removed.  Persisted-terminal reconciliation already runs via
`before-terminal-advance` under the controller lock; newly written terminals
retain the required post-step reconciliation.  The retained disappearing-start
test now refuses with `:existing-start-disappeared-or-changed` and records no
second click.

## Actual selection and successor replay

An isolated real repair store contains the admitted 057 finding in
`:awaiting-validation` and a separate open 058 finding.  The actual
`full-loop-runner/run-opportunity!` selection consumes that store and passes
058 to the real historical candidate reader.  The pinned 057 candidate refuses
with `Historical candidate targets another stop-line`; the dispatch port is
never called.

The paired successor test now materializes a successor-only configuration:
`:historical-successor` remains pinned while stale `:historical-action` is
absent.  It exercises the real historical bundle and successor resolution
readers.  This is valid only in its isolated store, where 057 is the sole
awaiting-validation obligation.  In production, open 058 retains precedence,
so removing the stale action cannot cause ordinary U88 selection.

An existing admitted start at zero cohort capacity remains observable through
the service/controller path without rerunning fresh historical applicability
and without another click.  If that start disappears during preparation, the
locked controller refuses rather than reserving or dispatching.

## Repair 058 and live evidence boundary

The retained finding is `repair-attempt-058-untyped-failure`, opened after a
selection-stage `java.net.http.HttpTimeoutException` (`request timed out`).  It
records selection/construction/dispatch/build as not reached and requires a
distinct repair commit, independent review, grounded repair, and a distinct
production-shaped successor.  Repair 057's qualification is not evidence for
058.

The smallest honest next worker task is therefore: reproduce 058's selection
timeout against the current bounded retry implementation, determine whether
the already-reviewed timeout classification/retry changes cover this exact
failure, and, if so, qualify those immutable commits for 058 through its own
historical revalidation contract; otherwise implement and independently review
the remaining selector repair.  Only then may a separate production-shaped
successor validate and resolve 058.

The failed live successor click still has no run record or projection.  Its
strict terminal observation remains unavailable; this correction does not
manufacture a terminal or alter live evidence.
