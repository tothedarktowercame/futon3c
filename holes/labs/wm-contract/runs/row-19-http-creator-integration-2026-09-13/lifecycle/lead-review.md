# Lifecycle review — changes required

Reviewed 835c39cd through f5a3cf11 and receipts236ab773. Four source pins match (lead-pins.json); raw HTTP8tests54, controller11tests38, kondo zero errors/warnings (one info), parens and deliberate failure match receipts. Passing suites were not rerun.

Post-rename creation identity propagation is accepted narrowly by source inspection; live deployment is absent. Two new isolated executed controls both exit0:

1. lead-duplicate-control.clj: create, mark running, create same requested ID produces both executing1 and accepted-queued1. Repeated mark-running test does not test repeated creation of an executing job.
2. lead-worker-control.clj: an explicitly alive isolated worker remains blocked while actual HTTP timeout finalization plus delivery receipt produce drained=true/executing0. This exercises finalization/accounting with a synthetic worker, not a live service timeout. Source finalize-job-at-ceiling! calls finalization before interrupt-job-worker! and does not await actual exit. Terminal ledger state cannot be used as worker-exit evidence.

Callbacks also run after releasing the ledger writer lock, so lifecycle notification order requires protection under concurrent finalization/start/delivery. This is a source risk to cover in the repair, not an executed race claim.

Next actual-serving packet must distinguish worker lifetime, terminal ledger state and delivery; preserve worker count until actual wrapper finally/unwind; deduplicate creation against the correct lifecycle state; test timeout/cancel and ordering. Row19 still open. Scoreboard: zero rows closed, zero new claims admitted (35 total). No storage-substrate expansion or live mutation.
