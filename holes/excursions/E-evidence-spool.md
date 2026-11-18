# E-evidence-spool: survey of append callers and side-door writers

Date: 2026-11-18

Proposed: commit refused evidence appends to `~/storage/futon3c/evidence-spool.db`, then return `{:ok true :spooled true ...}`.
A single FIFO drainer replays them; while pending rows exist, new appends queue behind them so replies/forks cannot overtake parents.

Survey of master at `6b472ab5ebe7ef9332a9b7d45c11baca62555ceb`, including the working source tree. No live requests, source edits, reloads or test writes were made. Unrelated dirty Giraffe files were left alone. Paths below are repo-relative; line numbers are this survey's snapshot.

**Finding B0: the boundary already reads back every append.** `evidence/boundary.clj:330,385` calls `store/append*`, then `invariant/verify-persisted` against the backend. A locally spooled entry absent from `-get` yields `:not-persisted` (or a caught transport exception), not success. The success receipt at414 does not forward `:spooled`. Thus adding a spool only to `futon1b_backend/-append` does not implement the proposed public success contract. `boundary/append!:451–460` also schedules turn-finalisation post listeners on success, so changing that contract starts downstream work earlier than remote persistence.

Classes below are transitive: **a** fire-and-forget; **b** returned receipt/entry only; **c** reads through store; **d** caller-dependent/unclear. Every boundary call is **c (B0)** even where the local emitter ignores its receipt; its local behaviour is recorded separately. This is not a claim that every emitter has an additional independent read. No read overlay currently covers get/query/count/forks; queued durability alone does not make those operations succeed during an outage.

## 1. Evidence append calls

85 resolved call sites in `src`, `dev`, and operational `scripts`, each listed once, including four plumbing calls and the file-only AtomBackend control. Test fixture calls are excluded from this operational inventory. Receipt-returning generic wrappers are explicitly not certified fire-and-forget for arbitrary injected callers.

| # | Call site / enclosing function | Class | Result use and readback consequence |
|---|---|---|---|
| 1 | `dev/futon3c/dev.clj:944` — `emit-context-evidence!` | c (B0) | Checks ok; observes returned entry in pattern-card provider; returns ID. Local b. |
| 2 | `dev/futon3c/dev.clj:1902` — `start-tickle!` | c (B0) | Ignores receipt; continues notification/restart. Local a. |
| 3 | `dev/futon3c/dev/invoke.clj:79` — `emit-invoke-evidence!` | c (B0) | Returns receipt to invoke lifecycle. Local b; downstream session queries can lag. |
| 4 | `dev/retry_acceptance.clj:23` — `-main` | b | Prints ok/error/attempt count; no readback. Retry acceptance would no longer measure remote delivery. |
| 5 | `scripts/deposit_runner_gate_memory.clj:133` — `-main` | c (B0) | Throws on failure, then reviews attachment using new review ID: remote lookup can fail. |
| 6 | `scripts/wire_math_memory_patterns.clj:314` — `ensure-description-evidence!` | c (B0) | Explicit get-entry immediately after append; throws if invisible/mismatched. |
| 7 | `scripts/wire_math_memory_patterns.clj:432` — `ensure-review-evidence!` | c (B0) | Checks ok; later attachment processing needs this review remotely. |
| 8 | `scripts/xiang2000_p2c_driver.clj:33` — `FileBackend/-append` | b | Delegates to AtomBackend then atomic-spit! on success. File-only, cannot reach futon1b. |
| 9 | `src/futon3c/agency/clock_decision.clj:524` — `replay-outbox!` | c (B0) | Deletes existing clock outbox row on ok or duplicate. Durable ownership transfers to spool, not remote. |
| 10 | `src/futon3c/agency/clock_decision.clj:600` — `record-durably!` | c (B0) | Checks existing/duplicate by ID; restores session clock from store; updates RAM and writes lineage separately. Restart before drain can restore stale/missing clock. |
| 11 | `src/futon3c/agency/history_constraints.clj:157` — `run!` | c (B0) | Reads existing/duplicate IDs; advances constraint-processing cursor when all receipts ok. Lag can omit acts from subsequent history scans. |
| 12 | `src/futon3c/agency/promise_history.clj:127` — `record!` | c (B0) | Async: increments written, then outcome/evaluate! queries history. Lag can miss newly recorded observation. |
| 13 | `src/futon3c/agency/promise_history.clj:192` — `shadow-write!` | c (B0) | Immediately get-entry and compares body; returns shadow-mismatch/failed if absent. |
| 14 | `src/futon3c/agency/promise_outcome.clj:167` — `write-check!` | c (B0) | Counts success; duplicate requires get-entry equality. Later checks query history. |
| 15 | `src/futon3c/agency/promise_outcome.clj:242` — `evaluate-observation!` | c (B0) | Counts success; duplicate requires get-entry equality. Later evaluation queries observations/checks. |
| 16 | `src/futon3c/agency/refusal_reset.clj:46` — `reset!*` | c (B0) | Ignores result and swallows exception after reset. Local a. |
| 17 | `src/futon3c/agency/work_orders.clj:141` — `*append-act!*` | c (B0) | Async append-default; catches/logs exception. Local a. |
| 18 | `src/futon3c/agents/apm_work_queue.clj:886` — `emit-apm-evidence!` | c (B0) | Returns boundary receipt from event emitter; no additional immediate readback found in emitter. Local b; later history consumers are not a remote-delivery guarantee. |
| 19 | `src/futon3c/agents/arse_work_queue.clj:159` — `emit-arse-evidence!` | c (B0) | Returns boundary receipt from event emitter; no additional immediate readback found in emitter. Local b; later history consumers are not a remote-delivery guarantee. |
| 20 | `src/futon3c/agents/tickle.clj:157` — `emit-scan-evidence!` | c (B0) | Returns boundary receipt from event emitter; no additional immediate readback found in emitter. Local b; later history consumers are not a remote-delivery guarantee. |
| 21 | `src/futon3c/agents/tickle.clj:174` — `emit-page-evidence!` | c (B0) | Returns boundary receipt from event emitter; no additional immediate readback found in emitter. Local b; later history consumers are not a remote-delivery guarantee. |
| 22 | `src/futon3c/agents/tickle.clj:190` — `emit-escalation-evidence!` | c (B0) | Returns boundary receipt from event emitter; no additional immediate readback found in emitter. Local b; later history consumers are not a remote-delivery guarantee. |
| 23 | `src/futon3c/agents/tickle.clj:235` — `emit-availability-evidence!` | c (B0) | Returns boundary receipt from event emitter; no additional immediate readback found in emitter. Local b; later history consumers are not a remote-delivery guarantee. |
| 24 | `src/futon3c/agents/tickle_orchestrate.clj:142` — `emit!` | c (B0) | Returns boundary receipt from event emitter; no additional immediate readback found in emitter. Local b; later history consumers are not a remote-delivery guarantee. |
| 25 | `src/futon3c/agents/tickle_orchestrate.clj:197` — `report-status!` | c (B0) | Throws on error/component; no immediate caller-specific read shown. |
| 26 | `src/futon3c/agents/tickle_orchestrate.clj:830` — `fm-conduct-cycle!` | c (B0) | Returns boundary receipt from event emitter; no additional immediate readback found in emitter. Local b; later history consumers are not a remote-delivery guarantee. |
| 27 | `src/futon3c/agents/tickle_work_queue.clj:271` — `emit-ct-evidence!` | c (B0) | Returns boundary receipt from event emitter; no additional immediate readback found in emitter. Local b; later history consumers are not a remote-delivery guarantee. |
| 28 | `src/futon3c/agents/zai_api.clj:1260` — `persist-transcript-entry!` | c (B0) | Requires ok, updates persistence health counters, rethrows failure; later transcript/session reads need delivery. |
| 29 | `src/futon3c/agents/zaif_controller.clj:310` — `persist-decision!` | c (B0) | Requires ok, updates decision persistence counters, rethrows failure. |
| 30 | `src/futon3c/apm/memory_caption_store.clj:235` — `default-ports` | c (B0) | Injected append port; append-and-read! at217 fetches before/after, falls back to returned entry. Later caption queries still need remote visibility. |
| 31 | `src/futon3c/apm/promotion_review_store.clj:145` — `persist!` | c (B0) | Injected append port; fetch before/after with returned-entry fallback; then materializes remote projection. Local durability is not projection authority. |
| 32 | `src/futon3c/blackboard.clj:1496` — `emit-blackboard-evidence!` | c (B0) | Explicit fire-and-forget; catches Throwable. Local a. |
| 33 | `src/futon3c/evidence/boundary.clj:330` — `append-unhooked!` | c (B0) | B0: verify-persisted at385 reads ID back; rejects queued-only success; does not carry spooled flag. |
| 34 | `src/futon3c/evidence/boundary.clj:449` — `append-default!` | c (B0) | Returns boundary receipt through append-default!; B0 inherited. |
| 35 | `src/futon3c/evidence/store.clj:115` — `append*` | d | Validates and returns backend receipt unchanged; no own readback. Caller-dependent. |
| 36 | `src/futon3c/evidence/store.clj:123` — `append!` | d | Returns append* result on default store; no own readback. Caller-dependent. |
| 37 | `src/futon3c/logic/archaeology.clj:327` — `emit-load-time-fired!` | c (B0) | Returns boundary receipt from event emitter; no additional immediate readback found in emitter. Local b; later history consumers are not a remote-delivery guarantee. |
| 38 | `src/futon3c/logic/locus.clj:484` — `emit-load-time-fired!` | c (B0) | Returns boundary receipt from event emitter; no additional immediate readback found in emitter. Local b; later history consumers are not a remote-delivery guarantee. |
| 39 | `src/futon3c/logic/metabolic_balance.clj:307` — `emit-load-time-fired!` | c (B0) | Returns boundary receipt from event emitter; no additional immediate readback found in emitter. Local b; later history consumers are not a remote-delivery guarantee. |
| 40 | `src/futon3c/logic/probe.clj:169` — `emit-family-fired!` | c (B0) | Returns boundary receipt from event emitter; no additional immediate readback found in emitter. Local b; later history consumers are not a remote-delivery guarantee. |
| 41 | `src/futon3c/logic/probe_taps.clj:165` — `emit-agency-backtrace!` | c (B0) | Returns boundary receipt from event emitter; no additional immediate readback found in emitter. Local b; later history consumers are not a remote-delivery guarantee. |
| 42 | `src/futon3c/logic/ratchet.clj:145` — `emit-demotion-event!` | c (B0) | Returns boundary receipt from event emitter; no additional immediate readback found in emitter. Local b; later history consumers are not a remote-delivery guarantee. |
| 43 | `src/futon3c/logic/ratchet.clj:325` — `emit-load-time-fired!` | c (B0) | Returns boundary receipt from event emitter; no additional immediate readback found in emitter. Local b; later history consumers are not a remote-delivery guarantee. |
| 44 | `src/futon3c/logic/snapshot.clj:224` — `emit-snapshot!` | c (B0) | Returns boundary receipt from event emitter; no additional immediate readback found in emitter. Local b; later history consumers are not a remote-delivery guarantee. |
| 45 | `src/futon3c/logic/tracer.clj:68` — `emit-tracer!` | c (B0) | Returns boundary receipt from event emitter; no additional immediate readback found in emitter. Local b; later history consumers are not a remote-delivery guarantee. |
| 46 | `src/futon3c/logic/tracer.clj:105` — `emit-tracer-closed!` | c (B0) | Returns boundary receipt from event emitter; no additional immediate readback found in emitter. Local b; later history consumers are not a remote-delivery guarantee. |
| 47 | `src/futon3c/nlp/classical_pipeline.clj:341` — `append-row!` | c (B0) | Checks for existing row before append (deduplication); returns receipt. |
| 48 | `src/futon3c/pattern_lifecycle/action_rpc.clj:19` — `append-required!` | c (B0) | Throws pattern-action-evidence-not-persisted on not-ok; required evidence gates action. |
| 49 | `src/futon3c/peripheral/common.clj:95` — `maybe-append-evidence!` | c (B0) | Converts failed receipt to E-store social error; caller may refuse peripheral transition. |
| 50 | `src/futon3c/peripheral/memory_lifecycle.clj:69` — `append-challenge!` | c (B0) | Checks ok, returns challenge ID or error; later review reads challenge. |
| 51 | `src/futon3c/peripheral/memory_lifecycle.clj:108` — `record-decision-keyed-external-check!` | d | Returns store/append* receipt directly through external-check API. No own readback; remote consumer/witness lookup contract needs decision. |
| 52 | `src/futon3c/peripheral/memory_lifecycle.clj:274` — `promote-memory-attachment!` | c (B0) | Injected append callback; after attachment hyperedge, writes review and calls review path with fetch-entry. Missing review blocks validation. |
| 53 | `src/futon3c/peripheral/memory_write.clj:380` — `record-memory-validated!` | c (B0) | Checks evidence ok then writes memory assertion hyperedge separately; can partially succeed while remote evidence absent. |
| 54 | `src/futon3c/peripheral/mentor.clj:138` — `checkpoint!` | c (B0) | Ignores receipt, catches exceptions; checkpoint continues. Local a. |
| 55 | `src/futon3c/peripheral/pull_receipts.clj:47` — `record-pull-offer!` | c (B0) | Returns boundary receipt from event emitter; no additional immediate readback found in emitter. Local b; later history consumers are not a remote-delivery guarantee. |
| 56 | `src/futon3c/peripheral/pull_receipts.clj:99` — `record-pull-uses!` | c (B0) | Returns boundary receipt from event emitter; no additional immediate readback found in emitter. Local b; later history consumers are not a remote-delivery guarantee. |
| 57 | `src/futon3c/peripheral/real_backend.clj:601` — `persist-proof-path!` | c (B0) | Warns on error; returns original proof-path result. Local a. |
| 58 | `src/futon3c/peripheral/real_backend.clj:906` — `persist-memory-use!` | c (B0) | Throws if receipt not ok; returns it; later memory-use observation scans can lag. |
| 59 | `src/futon3c/portfolio/core.clj:48` — `emit-evidence!` | c (B0) | Returns boundary receipt from event emitter; no additional immediate readback found in emitter. Local b; later history consumers are not a remote-delivery guarantee. |
| 60 | `src/futon3c/process_watchdog.clj:218` — `emit-evidence!` | c (B0) | Returns boundary receipt from event emitter; no additional immediate readback found in emitter. Local b; later history consumers are not a remote-delivery guarantee. |
| 61 | `src/futon3c/social/bells.clj:112` — `ring-standup!` | c (B0) | Writes bell event before invocation; does not require remote confirmation beyond boundary. |
| 62 | `src/futon3c/social/bells.clj:127` — `ring-standup!` | c (B0) | Writes follow-up coordination event; parent/reply ordering needed across writes. |
| 63 | `src/futon3c/social/bells.clj:171` — `ring-test-bell!` | c (B0) | Writes test bell event; ignores receipt. Local a. |
| 64 | `src/futon3c/social/coordination_ledger.clj:115` — `record-invoke-edge!` | c (B0) | Returns invoke-edge receipt; ledger queries need edges visible. |
| 65 | `src/futon3c/social/coordination_ledger.clj:164` — `run-scheduled-dispatch!` | c (B0) | Requires ok after scheduled dispatch; otherwise recording-failed; dispatch already happened. |
| 66 | `src/futon3c/social/dispatch.clj:171` — `emit-dispatch-evidence` | c (B0) | On ok returns the submitted evidence ID. Local b; shared B0 remains. |
| 67 | `src/futon3c/social/whistles.clj:140` — `whistle!` | c (B0) | Writes bell result, then returns invoke/whistle response; evidence receipt not used. Local a. |
| 68 | `src/futon3c/test_registry.clj:694` — `append-record!` | c (B0) | Requires ok or registration-failed; subsequent registry reads query stored registrations. |
| 69 | `src/futon3c/transport/http.clj:4058` — `handle-evidence-create` | c (B0) | Returns HTTP201 with entry/id on ok, mapped error otherwise; immediately-following client GET may miss spooled ID. |
| 70 | `src/futon3c/transport/http.clj:4154` — `arse-ask!` | c (B0) | Returns evidence-ok; later ArSE answer looks up question. |
| 71 | `src/futon3c/transport/http.clj:4196` — `arse-answer!` | c (B0) | Returns evidence-ok; later session/questions query may omit answer. |
| 72 | `src/futon3c/transport/http.clj:4438` — `handle-psr` | c (B0) | Updates disk backpack regardless; exposes evidence-ok. Later pattern retrieval sees delayed PSR. |
| 73 | `src/futon3c/transport/http.clj:4488` — `handle-pur` | c (B0) | Clears disk backpack regardless; exposes evidence-ok. Later outcome query sees delayed PUR. |
| 74 | `src/futon3c/transport/http.clj:4541` — `handle-par` | c (B0) | Exposes evidence-ok; later PAR/session reads lag. |
| 75 | `src/futon3c/transport/http.clj:5406` — `emit-review-snapshot!` | c (B0) | Best-effort snapshot append; catches exception. Local a. |
| 76 | `src/futon3c/transport/http.clj:8824` — `handle-todo` | c (B0) | Returns created ID; same handler GET queries goals/conclusions. Newly created todo invisible until drain. |
| 77 | `src/futon3c/transport/http.clj:8879` — `handle-todo` | c (B0) | Returns completion success; GET still shows outstanding todo if conclusion not drained. |
| 78 | `src/futon3c/transport/http.clj:8982` — `handle-mc-backfill` | c (B0) | Counts successful receipts as created; census/count queries can disagree. |
| 79 | `src/futon3c/transport/http.clj:9032` — `handle-mc-sync-mission` | c (B0) | Reports created on ok, duplicate separately; mission-control projection/query can lag. |
| 80 | `src/futon3c/transport/http.clj:12440` — `xiang-turn-service` | c (B0) | Returns ok/id or typed failure to turn service; settlement hooks and downstream queries can run before drain. |
| 81 | `src/futon3c/transport/irc.clj:50` — `emit-error-evidence!` | c (B0) | Best-effort error evidence; catch ignores exception. Local a. |
| 82 | `src/futon3c/transport/irc.clj:183` — `make-irc-callbacks` | c (B0) | Writes IRC lifecycle event; locally ignores receipt. |
| 83 | `src/futon3c/transport/irc.clj:460` — `make-relay-bridge` | c (B0) | Writes IRC chat turn before relay invocation; locally ignores receipt; history retrieval can miss input. |
| 84 | `src/futon3c/transport/irc.clj:606` — `start-irc-server!` | c (B0) | Writes outgoing IRC turn; continues relay; exceptions logged. |
| 85 | `src/futon3c/transport/ws.clj:446` — `make-ws-callbacks` | c (B0) | On error sends error frame; on success sends acknowledgement/entry. Subsequent client query may miss accepted event. |

## 2. Side-door writers (not covered by this spool)

Transport leaves are enumerated below; multiple payload sites sharing a transport are grouped, with their locations. A configured endpoint is not assumed to be port 7073 merely because it is named FUTON1A. In particular `evidence/futon1b_backend.clj` defaults to **7074**, while `substrate/client.clj` and memory clients commonly default to **7073**. Deployment configuration must be checked before promising outage coverage. `:throw false` handles HTTP status errors, **not** connection exceptions. “No retry” below means no refused-connection retry in the listed writer, not that a higher-level scheduled job can never run again.

| Writer / locations | Writes and route | Connection-refused behaviour today / boundary implication |
|---|---|---|
| `src/futon3c/arxana/store_http.clj:86,93,137,141` | Conditional command and append-batch to `/api/alpha/arxana/command` | Catches exception into `:arxana.store/http-unavailable`; recovery suggests retry but does not perform it. Not evidence backend. |
| Arxana command callers: `arxana/service.clj:130`, `arxana/write.clj:143` (under `src/futon3c/`) | Revision ingestion and generic typed write operations; includes workplan service operations | Same store-http failure receipt; no evidence spool. In-memory store implementations are not remote writers. |
| Arxana batch callers under `src/futon3c/`: `arxana/xiang_interpretations.clj:102`, `giraffe_needs.clj:283`, `turn_mention_writer.clj:137`, `workplan_transfer.clj:32`, `pattern_occurrences.clj:67`, `pattern_proposals.clj:63`, `dog_memory.clj:132,199,267`, `item_inventory.clj:165,168,196`; `diagramprover/publish_findings.clj:97`; `circus/hou_store.clj:146,174` | Interpretation, needs, mentions, transfer, occurrences/proposals, dog records/use/burial, inventory, findings, hou attempts/focus | All call append-batch above; failed command is not queued by evidence spool. Publication additionally reads back. |
| `src/futon3c/substrate/client.clj:104,112`; caller `agency/artifact_activation.clj:192` | `/api/alpha/derived` activation | Transport exception propagates; no refused retry. Activation immediately reads derived projection. |
| `src/futon3c/agency/rule_record.clj:155,179` | EDN request helper; rule hyperedge | Java send throws; no retry; successful write followed by readback. |
| Same helper callers under `src/futon3c/agency/`: `grant_record.clj:293`, `attestation_record.clj:125`, `incident_clearance.clj:101`, `agreement_record_cli.clj:32`, `offer_record_cli.clj:81`, `blocker_escalation_record_cli.clj:50`, `disclosure_record_cli.clj:53,110,153`, `pattern_card_record_cli.clj:191,204` | Act hyperedges; disclosure endpoint at153 | No transport retry in helper; exceptions propagate or are converted by CLI/caller to failure. Several verify by subsequent read. |
| `src/futon3c/agency/promise_log.clj:66`, `promise_reconcile.clj:25` | Promise log/state hyperedges; reconciliation batch `/hyperedges/batch` | Same throwing helper; no refused retry at write leaf. Reconciliation pacing is not durable queuing. |
| `src/futon3c/transport/http.clj:11447,11886` | Direct evidence POST: negation interpretation; agreement-route record | Negation throws and verifies readback; agreement catches into `:recorded false` (409 checks existing). Both bypass backend FIFO even though endpoint is evidence. |
| `src/futon3c/transport/http.clj:12378,12392` | Xiang scope-edge hyperedge | Java send throws; turn service treats failure as non-blocking settlement. No spool. |
| `src/futon3c/xiang/lu_memory.clj:213,215` | Evidence plus lu occurrence hyperedge, memory base default7073 | Outer catch returns failure; two separate requests, no transaction/retry spool. |
| `src/futon3c/agency/dog_provider.clj:1239–1252` (`bury-one!`) | Memory evidence and version hyperedge through `post-edn!` | Java send throws; no retry in helper. Memory file can already exist before remote ingestion fails. |
| `src/futon3c/peripheral/memory_write.clj:244,286` | Hyperedge; atomic `/api/alpha/memory/assert` evidence+edge | HTTP-kit error/catch becomes failed receipt; no refused retry. The atomic endpoint deliberately bypasses separate evidence append. |
| `src/futon3c/agency/clock_lineage.clj:116` | Clock lineage hyperedge additions/retractions | Catches connection exception as status -1; outer persistence catches/returns failure. RAM decision and evidence can advance without lineage. |
| `src/futon3c/dispatch_with_recall.clj:1495` | Direct evidence receipt POST to substrate-base (default7073) | No refused retry at writer; failure throws. The default explicitly bypasses Agency; no backend FIFO. |
| `src/futon3c/evidence/http_backend.clj:118` | JSON evidence POST to configured base | Returns transport failure, not futon1b-backend retry. Normally an Agency forwarding backend; direct futon1b configuration bypasses proposed spool. |
| `src/futon3c/watcher/file_ingest.clj:127,185,318,339,361,386` | Hyperedges (including mission errors), entities, relations, relation/entity batches | Catches transport failure into failed/status -1 result. No durable refused-connection queue here. |
| `src/futon3c/watcher/commit_ingest.clj:116,171` | Commit graph hyperedges and batches | Failed receipt/status -1; failed batches **not** replayed as single writes. Unsupported route fallback is different from connection retry. |
| `src/futon3c/watcher/multi.clj:206,778` | Replacement/repoint hyperedges; `/documents/retract` | Failed receipt for edge; replacement helper637 retries **503 only**, not refused connection. Retraction throws outcome-unknown and requires verification. Watcher tick retry does not make a single write durable. |
| `src/futon3c/scripts/mission_scope_ingest.clj:71,183,209,226,235,283` | Entity/hyperedge singles and batches, document retractions | Java request throws; explicit status checking. 404/405 retraction fallback only, not refused retry. |
| `scripts/wire_math_memory_patterns.clj:202`, `scripts/deposit_runner_gate_memory.clj:18` | Pattern/memory graph writes via POST helper | Failure returned/raised and checked by workflow; no durable refused retry. Separate from their evidence calls in table1. |
| `scripts/o5_honest_holes.py:392` | `cascade/hole-target` hyperedges | Fails closed; retry covers admission shedding, not refused connection. |
| `scripts/memory_outcome_sweeper.py:756,758` | Outcome-check evidence | urllib transport exception; no refused retry; verifies by GET at773. Also invokes lifecycle tooling for memory attachments. |
| `scripts/xiang2000_p6o3.py:205` | Evidence through `api.request('POST', ...)`, base default7073 | Direct HTTP result, outside backend; no SQLite queue. |
| `scripts/m40_store_copy.py:61,154,259,309` | Entity POST; evidence and hyperedge batches **inside futon1b through Drawbridge** | POST catches HTTPError but transport error escapes; batch evaluation failures stop copy. Pacing/checkpoints are not the proposed spool. |
| `scripts/o4_land.bb:13`, `held_work_ledger_land.bb:57`, `d4_feeder_b_load.bb:96` | Entity/hyperedge landers to7073 | o4 converts exception to failed status/count; other HTTP helpers propagate transport exceptions. No refused retry. |
| `scripts/mission_pattern_scopes_load.bb:39`, `mission_pattern_refresh.bb:39` | Historical/on-demand mission-pattern hyperedges | Legacy **7071** target, not automatically live7073; transport failure throws, no spool. Refresh also retracts old edges. |
| `scripts/archivist_migrate_mission_docs.bb:27`, `archivist_cleanup.bb:29` | Drawbridge evaluated entity migration / gated erasure | Legacy embedded substrate via `futon3c.dev/!f1-sys`; remote eval failure throws/exits. Not covered; do not assume these old tools address current futon1b. |
| `scripts/prune_evidence_event.py:132–144`; `src/futon3c/evidence/futon1b_backend.clj` `-delete!` | Drawbridge invokes backend deletion, not append | Current futon1b -delete! is a logged no-op, not a remote write; retained here as an administrative exclusion. Other backend implementations can delete. A future delete must define ordering against pending insertion. |

Excluded after checking targets: `portfolio/heartbeat.clj:121,132` writes futon5 nonstarter, not futon1b; `xiang/ma_reading.clj:316`, `peripheral/problem.clj:1239`, agent invocation clients and bell/park/followup clients target Agency. Arxana `/query`, substrate structured query, catalog census and arxana-clock Drawbridge calls are reads even when HTTP POST. Standalone Arxana demos with private/in-memory stores are not live7073 writers.

Agency-facing evidence HTTP producers are **indirect callers of table1 row69**, not separate backend call expressions: `scripts/ma_observe.py:408`, `backfill_claude_jsonl_chat_turns.py` (base default7070), `ngircd_bridge.py` evidence URL, `codex_ws_invoke_bridge.clj:362`, `claude_ws_invoke_bridge.clj:260`. Configuring their base directly to futon1b bypasses the boundary. Their responses/next GETs need the same queued-vs-delivered distinction. This includes other external clients not in this repository; static xref cannot enumerate them.

## 3. Failure as a signal

| Signal consumer | Current meaning/action | Effect of locally durable success / remaining outage behaviour |
|---|---|---|
| `evidence/boundary.clj:362–426`, invariant `verify-persisted` | Failed append/readback logs invariant violation and returns error | Backend-only change still fails B0. Read-overlay or explicit durable-local contract needed; cannot simply delete verification without redefining invariant. |
| `evidence/boundary.clj:451–460`, turn-events pre/post listeners | Post listeners queued only after successful persistence | A successful spool acknowledgement triggers settlement-derived work while remote store remains unavailable. Need delivery phase or spool-aware readers. |
| `agency/clock_decision.clj:183,524,584–631` | Restore reads authoritative clock; busy write drives local outbox/error; successful replay removes outbox file | Success may clear old outbox and advance RAM before remote restore works. Cold-start restoration still fails; lineage writes remain unspooled. Do not interpret “append accepted” as “clock store healthy”. |
| `transport/http.clj:4058–4070` | Evidence ingress returns201 on ok; failure mapped to HTTP status; clock failures503 | If boundary updated, outage can become201 but following GET/count fails or omits record. Receipt currently omits spooled field. |
| `transport/ws.clj:446–449`, `peripheral/common.clj:95–102` | Error frame / E-store social error blocks transition | Durable local receipt may allow transition; remote history still absent. |
| `agents/zai_api.clj:1260–1280`, `agents/zaif_controller.clj:310–330` | Persistence failure counters and exceptions | Queued work counted as successful persistence; need separate pending/delivered health, not blanket green. |
| `agency/promise_history.clj:127,192`, `promise_outcome.clj:167,242`, `history_constraints.clj:157` | Written counters, shadow verification, duplicate equality, history cursor | Metrics/cursors may advance while queries omit rows; direct shadow reread reports mismatch even after local acceptance. |
| `pattern_lifecycle/action_rpc.clj:19`, `test_registry.clj:694`, `social/coordination_ledger.clj:164` | Fail closed on missing required evidence / registration / dispatch receipt | Success grants permission or claims recording earlier; external consumers may not see the authority yet. |
| `apm/promotion_review_store.clj:184–245`, `memory_caption_store.clj:217`, `peripheral/memory_lifecycle.clj:274`, `memory_write.clj:380` | Evidence readability supports review/materialisation; graph write follows | Returned entry fallback is not remote authority. Hyperedge/atomic-memory side doors still fail; partial local/remote state must be explicit. |
| `transport/http.clj:4154,4196,4438,4488,4541,8824,8879,8982,9032` | evidence-ok, created counts, todo success; backpack effects independent | Can report success while later questions/todos/mission/count queries disagree; some already proceed after failure. |
| Best-effort emitters (blackboard, mentor, bells, IRC, logic probes, watchdog) in table1 | Ignore/log errors, return normal operation result | Better durability, but no signal to caller that drain failed permanently. Keep operator-visible backlog/failure state. |
| `evidence/futon1b_backend.clj` `health`, query-cache and read methods | Independent shallow health; reads fail on transport error; cache can outlive new write | Spool success must not assert remote health. Draining must invalidate cached queries; failed reads must not become empty successful histories. |

## Decisions before implementation

- **All c/B0 cases:** define whether get/query/count/forks include the durable queue, or retain delivery-required APIs separately. A point-read overlay alone does not solve session queries, restore, counts, or joins.
- **Clock, promise/history and turn hooks (c):** decide when cursors/outbox deletion/post listeners advance. Preserve a recoverable chain across JVM restart and distinguish local durability from remote delivery.
- **Promotion/memory/act writers (c):** evidence FIFO does not serialize independent graph/Arxana writes. Decide whether dependent operations wait for drain, queue in the same causal system, or return explicitly pending instead of claiming materialisation.
- **Generic store/append* and append! plus external-check wrapper (d):** document the new receipt contract for injected/external callers. No caller-wide readback guarantee can be inferred from their forwarding functions alone.
- **HTTP/WS and configurable direct clients (c/d):** propagate spooled status end-to-end; define whether201 means queued or visible. Confirm deployed7073/7074 bases. Do not promise safety for side doors merely because evidence append works.
- **Drain failure/deletion:** specify permanent validation/409 handling, observability and delete ordering; FIFO can otherwise stall indefinitely or replay a deleted record. This survey makes no implementation choice.

## Reproducing the inventory

Commands run from the repository root (no services contacted):

```sh
clj-kondo --lint src dev scripts --skip-lint \
  --config '{:output {:format :json} :analysis {:var-usages true :var-definitions true}}' \
  > /home/joe/notes/codex-12/evidence-spool/xref.json
rg -n '/append[*!]?|/-append' src dev scripts
rg -n 'requiring-resolve.*append|resolve.*append' src dev scripts
rg -n 'http/(post|put|patch|delete)|\.DELETE|:method :(post|put|delete)' src dev scripts
rg -n '\.POST|setRequestMethod|"POST"|"DELETE"' src dev scripts
rg -n '7073|FUTON1B|FUTON1A|api/alpha/(evidence|hyperedge|entity|relation|memory|derived|documents)' src dev scripts
```

The xref filter is alias-aware (including `evidence/append!` bound to boundary):

```python
import json
x = json.load(open('/home/joe/notes/codex-12/evidence-spool/xref.json'))
want = {'futon3c.evidence.boundary': {'append!', 'append-default!'},
        'futon3c.evidence.store': {'append*', 'append!'},
        'futon3c.evidence.backend': {'-append'}}
sites = {(u['filename'], u['row'], u['col']): u
         for u in x['analysis']['var-usages']
         if u.get('name') in want.get(u.get('to'), set())}
assert len(sites) == 85
for key in sorted(sites):
    u = sites[key]
    print(*key, u.get('from-var'), u['to'], u['name'])
```

Read enclosing definitions, their read/query/error paths, and transport helpers; inspected .bb and Python scripts separately because Clojure xref does not cover those languages. `sites.json`, `contexts.txt`, and search output are retained under `/home/joe/notes/codex-12/evidence-spool/`. Static coverage is of checked-in operational call expressions and transports, not arbitrary forms submitted through a REPL or external repositories. No live health/deep-health probes were used. This is a source survey, not an outage acceptance test.
