# APM coordinator-tick substrate-read audit, 2026-09-12

## Scope and method

This is a static audit of futon3c commit
`52856d8fe7530dfe0c38d06525bf8b9a1f49aee6` (`master`).  Every source
location below is therefore read as
`52856d8fe7530dfe0c38d06525bf8b9a1f49aee6:<file>:<line>`.  I followed the
durable JIT reconciliation entry through the queue and active-frame adapters,
then checked every reachable APM use of `futon3c.substrate.client`, the
futon1b evidence backend, and the conductor's direct HTTP reader.  I also used
clj-kondo var-usage analysis as a cross-check.  No live JVM or HTTP service was
called.

The requested binary classification needs one precision.  `read-health`
observes latency but does not catch a failed read
(`src/futon3c/substrate/read_health.clj:17-31`), and `store-read-hold/with-frame!`
only binds the observer (`src/futon3c/apm/store_read_hold.clj:34-45`).  The hold
is constructed later, after a frame has returned terminal and retirement runs
(`src/futon3c/apm/queued_frame_adapter.clj:969-998`).  Therefore an observed
30-second timeout cannot reach frame-end hold construction: it is still RAW
under the question's definition.  “Observed RAW” below means warning-capable
inside the frame binding, but the exception still escapes.  “Contained” marks
a third behavior which the requested binary taxonomy does not describe: the
site converts or suppresses the exception and does not create a repair hold.
There are **no GUARDED sites for a thrown timeout at this audited SHA**.

## Common call chain and exception route

All table rows begin with:

`durable-coordinator/coordinator-tick` (`durable_coordinator.clj:368-411`)
→ the JIT adapter `:reconcile-fn` (`jit_queue_coordinator.clj:112-159`)
→ `countdown-control/autonomous-problem-list-step!` (`countdown_control.clj:2505-2516`)
→ `set-alight-problem-list!` (`countdown_control.clj:2341-2490`)
→ `set-alight-problem-queue!` (`countdown_control.clj:1953-2089`)
→ `problem-queue-supervisor/tick!` (`problem_queue_supervisor.clj:743-818`).

For every RAW row not otherwise qualified, no queue or durable-coordinator
catch intervenes.  `live-regulator/tick!` catches the throwable at
`live_regulator.clj:104-144`, turns it into `:live-regulator-tick-threw`, marks
the regulator failed and durably retains the pending intent
(`live_regulator.clj:145-164`).  The semantic watchdog observes that pending
intent as `:awaiting-job` (`durable_coordinator.clj:616-654`); after the intent
deadline plus grace it returns `:external-job-deadline-exceeded` and calls the
durable `stop!` path (`semantic_progress_watchdog.clj:180-200`).  That disables
the registry entry and leaves the campaign durably stopped.

## Reachable read sites

| Call site(s), pinned to audited SHA | Tick-local call chain after `problem-queue-supervisor/tick!` | Classification and exception route | PROPOSED treatment |
|---|---|---|---|
| `promotion_candidate_store.clj:225,250` evidence GET; `:232,263` hyperedge GET during `persist!` | `frame-tick-fn` → `countdown-control/set-alight!` → `drive-live-learning-phase!` (`countdown_control.clj:1399-1448`) → `live-promotion/run-live!` (`live_promotion.clj:630-657`) → `candidate-store/persist!` | **Observed RAW.** Evidence and edge transport exceptions are not caught in `persist!`; common route above. | **PROPOSED:** catch only typed transport failures at the promotion transaction boundary and return a durable frame store-read hold. Do not retry writes blindly; a read-back may follow a committed write. |
| `promotion_candidate_store.clj:284-294` evidence/review-evidence GET and `:285` hyperedge GET during `visible?` | same through `live-promotion/run-live!` → `drive!`'s `:candidate-visible-fn` (`live_promotion.clj:636`) | **Observed RAW.** No catch; common route. | **PROPOSED:** one bounded read-only retry, then the same durable frame hold with memory id and operation identity. |
| `promotion_candidate_store.clj:325` evidence GET during `review-inputs` | same through `live-promotion/run-live!`; review/recovery arms at `live_promotion.clj:539,595` → `candidate-store/review-inputs` | **Contained for timeout; RAW for connection/other transport error.** `fetch-review-entry` catches timeout and returns typed `:promotion-review-candidate-evidence-timeout` (`promotion_candidate_store.clj:23-36`), which returns normally from the tick and is handled by promotion retry state. Non-timeout failures are rethrown at line 36 and take the common route. This is the **F218 site**: the timed-out id `e-apm-promotion-2588028e88b64f282e2b31e7ed5313b0` was being freshly assembled into reviewer inputs. The current timeout containment means the incident either executed code predating this catch or a stale loaded namespace; the incident record alone cannot distinguish them. | **PROPOSED:** extend the typed containment to connection failures and, after the existing bounded promotion retry is exhausted, create the frame store-read hold rather than a generic promotion/frame failure. Add a regression that drives this exact chain. |
| `promotion_review_store.clj:188,195,199-200` evidence GETs and `:214-221,237-239` lifecycle hyperedge/evidence reads during review persistence | same through `live-promotion/run-live!` → `review-store/persist!` (`live_promotion.clj:635`) | **Observed RAW** before `review-attachment!`. Inside `review-attachment!`, `ExceptionInfo` is caught and converted to `:promotion-review-projection-invalid` (`promotion_review_store.clj:212-225`), but the fallback `observed-attachment-status` at lines 237-239 can itself throw. All uncaught cases take the common route. | **PROPOSED:** make the entire read-only projection/read-back portion one bounded, typed transaction; on transport failure retain its state and emit the frame hold. |
| `memory_snapshot.clj:320,324-325` memory projection plus memory and review-evidence GETs for `candidate-visible?`; `:376` evidence-text GET for candidates lacking inline text | `drive-live-learning-phase!` → one of `publish-promotion!`, `publish-zai-scribe-promotion!`, or `publish-guide-promotion!` (`countdown_control.clj:1172-1397`) → `memory-snapshot/publish!` | Visibility reads are **Contained**: `observe-visibility` catches all throwables (`memory_snapshot.clj:83-115`) and publication returns typed `:memory-snapshot-visibility-not-obtained` (`:354-391`). Text enrichment is also **Contained** by `order-candidates`' fetch catch (`memory_snapshot.clj:202-218`), recording `:fetch-failed?`. Neither path creates a hold. | **PROPOSED:** visibility transport failure should become the same durable frame hold after its existing one-miss retry; keep optional text enrichment non-fatal but retain a warning when it is slow/unavailable. |
| `conductor.clj:516` hyperedges-by-end, `:525` semantic-why relations, `:536` pattern entity, `:541` evidence body in memory-cascade readers | active `frame-tick-fn` → `set-alight!` → `live-learning-phase-inputs` → `live-learning-phases/build-request` → supplied `cascade-fn` (`countdown_control.clj:882-904`) → `conductor/run-observed-memory-cascade` → `expand-memory-cascade` | These calls bypass `read-health` entirely. Hyperedge, relation, and transport-failed pattern reads are **RAW** after the conductor's bounded admission/IO retry (`conductor.clj:352-452`); `run-observed-memory-cascade` records a durable failed operation and rethrows (`conductor.clj:109-150`), then the common route applies. Missing non-transport pattern reads are intentionally `nil` (`:532-540`), and evidence-body enrichment catches every throwable and degrades to no enrichment (`:699-706`): those two are **Contained**, without a hold. | **PROPOSED:** preserve the conductor's operation deadline and bounded retry, but convert exhausted transport failure into the shared frame store-read hold. Feed its direct HTTP timings into `read-health`; do not silently suppress transport failure for evidence enrichment. |

The evidence backend and substrate-client plumbing used by the promotion and
snapshot rows both call `read-health/observe!`
(`evidence/futon1b_backend.clj:262-268`, `substrate/client.clj:62-81`).  Inside
the wrapped frame effects this produces the same warning files seen in F225,
but a warning file alone is not a hold.  The hold gate is reached only after a
successful frame completion and retirement, so F225's 6-second successful
reads were held while F218's thrown 30-second read could stop the tick.

## Conclusion

The missing behavior is not one isolated uninstrumented GET.  The current
frame wrapper observes many reads, while the exception-to-hold transition does
not exist.  In addition, the conductor owns a separate HTTP reader outside
`read-health`, and several visibility/enrichment sites convert failures into
typed results or absence without entering the hold protocol.  A structural
repair should establish one frame-scoped substrate-read boundary with three
explicit outcomes—value, durable retry, or durable repair hold—and require all
of these readers to use it.  Raising timeouts or adding site-specific catches
would leave the inconsistent outcomes intact.
