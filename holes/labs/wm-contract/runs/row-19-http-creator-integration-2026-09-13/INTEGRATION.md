# Invoke creator ingress integration

Status: source integration only; not loaded, activated, or deployed.

`futon3c.transport.http/create-invoke-job!` is the common boundary used by all
seven lexical call sites: automatic bellback, parked resume, direct invoke,
bell, announce, whistle stream, and whistle asynchronous handling (the two
whistle modes provide the eighth surface). The wrapper calls
`begin-creation!` before entering `invoke-jobs-writer-lock`, releases the
controller lock before waiting for that writer lock, and calls
`creation-finished!` exactly once in `finally`. A job ID is supplied only
after `update-invoke-jobs-ledger!` has completed its durable publication; a
stable active-ID retry is supplied again and the controller set deduplicates
it. A failed publication supplies nil and removes only the entrant.
If the ledger reports that its atomic rename committed but directory-force
confirmation failed, the thrown exception carries the retained job identity;
the error is preserved while the controller conservatively retains that job
as accepted work. A pre-rename failure has no retained identity.

The central running transition advances an accepted job to execution only
when the ledger itself changes from queued/activating to running. The first
terminal ledger transition moves either queued cancellation/submission failure
or executing work to final delivery, but it does not release the executing
identity. The first durable delivery receipt clears final delivery. Only the
actual worker wrapper's `finally`, after unregistering the worker, releases
execution. Thus timeout/cancel notification cannot report drain while an
interrupt-resistant worker remains alive. Duplicate creation, running,
terminal, and delivery calls preserve the existing lifecycle set and do not
advance state twice. A terminal job lacking a safe delivery receipt remains
in final delivery and therefore remains non-drained.

An HTTP lifecycle-order monitor surrounds each ledger transition and its
controller notification. Its ordering is lifecycle monitor, ledger writer,
then controller; `begin-creation!` releases the controller before entering the
monitor. No path holds controller and then waits for the ledger writer.

Loading the namespace leaves `!invoke-ingress-controller-config` exactly
inactive, preserving ordinary service behavior. Activation requires the
non-HTTP `configure-invoke-ingress-controller!` service API with schema
`:agency/invoke-ingress-http-v1` and a non-test controller holding a durable
deferred store. Partial/unknown configuration refuses. Its installed record
always states `:restart-authorized? false` and `:lifecycle-wiring
:creation-only`; request maps cannot select or replace the controller.

## Remaining serving seams

This packet does not make the process restart-ready. The same controller still
must be joined to durable parked/deadline resume replay. Startup must
reconcile already accepted jobs against the ledger, queues, execution and
delivery records before intake can open. The controller cannot protect its
own first installation, so an independently reviewed external first-install
fence and service lifecycle configuration remain required. No verification
listener or control endpoint is installed here.
