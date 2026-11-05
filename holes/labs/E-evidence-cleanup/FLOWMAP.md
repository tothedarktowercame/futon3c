# Evidence Landscape R0 flow map

Window: `2026-11-04T05:45:00Z` (inclusive) through `2026-11-05T05:45:00Z`
(exclusive). Counts and sizes are from a read-only query of
`futon1b/migration-store-21/fts5-evidence.db`, joining `ev_attr` to `ev_doc` and
using the indexed `at`, `author`, and `type` attributes. MB is UTF-8 document
bytes / 1048576. The R0 fields deliberately distinguish semantic tag/author
streams which happen to share the broad `:coordination` type.

## Writers and their readers

| writer site | author | type / claim / tags | copies text? | records/day | MB/day | filtered readers |
|---|---|---|---:|---:|---:|---|
| `futon3c/src/futon3c/agency/artifact_activation.clj:190-215,224-248` | `futon3c/artifact-activation` | `:artifact/weak-activation`, `:observation`, `:artifact-retrieval :weak-activation` | yes, source artifact/prompt `:text` | 595 | 6.716 | none but `:generic-search` |
| `futon3c/src/futon3c/agents/zai_api.clj:1251,1280-1328,1684` | computed `agent-id` (`zai-1`, `wm-author`, etc.) | `:coordination`, `:step`, `:transcript <event> <profile>` | yes, full prompt at `:turn-start`; reply/tool text at rounds | `zai-1` 348; `wm-author` 962 | 1.176; 2.826 | transcript bug scan `zai_api.clj:1697-1705`; 馬 reads `zai-1` |
| `futon3c/src/futon3c/agents/zai_api.clj:1251,1297-1328` (Kimi profile through the same transcript sink) | computed `agent-id` (`象-1`) | `:coordination`, `:step`, `:transcript` | yes, full Kimi prompt and round text | 541 | 2.245 | none but `:generic-search` |
| `futon3c/scripts/claude_ws_invoke_bridge.clj:243-262,287-310` | computed CLI `agent-id` (`claude-N`) | `:coordination`, `:step`, `:invoke :ws-bridge <agent>` | yes, 200-char prompt and result previews | all `claude-*` coordination: 3,065 | 5.055 | none but `:generic-search` |
| `futon3c/src/futon3c/xiang/turn_service.clj:379-394,498-517,535-572` | `xiang-turn-service` | `:memory`, `:assert`, `:xiang-turn` | yes, `record` retains source text and `reading` summary | 78 | 0.896 | `scripts/xiangma_port_export.py:82-108` (`tag=xiang-turn`) |
| `futon3c/src/futon3c/xiang/turn_service.clj:398-419,516-517` | `xiang-turn-service` | `:coordination`, `:proposal`, `:xiang-relationship-review :relationship-review-proposed` | yes, quoted operator text in proposal evidence | included with low-volume author/type tail (0 in window query) | 0 | none but `:generic-search` |
| `futon3c/scripts/ma_observe.py:363-380,385-399` | `馬` | `:coordination`, `:observation`, `:ma-card` | yes, derived summary of a cited transcript turn | 104 | 0.716 | `scripts/xiang_turn_check.py:121`; `scripts/xiangma_port_export.py:82-108` |
| `futon3c/src/futon3c/agency/promise_history.clj:86-125,179` | computed promise agent (notably `claude-14`) | `:promise/*`, `:step`, `:promise-history` | yes, serializes the full scheduler record and changes | `claude-14`: 1,414 lifecycle rows | 25.727 | `agency/obligations_reader.clj:42-54,98`; `transport/http.clj:11170`; `agency/promise_outcome.clj:128-143,196-218` |
| `futon3c/src/futon3c/agency/promise_outcome.clj:128-143,196-218` | computed promise agent | `:promise/fulfilled`, `:promise/lapsed`, `:promise/fulfilment-check` | yes, carries checked promise facts | 0 for `claude-14` in window | 0 | `agency/obligations_reader.clj:98`; `agency/answer_population.clj:85-86`; `agency/obligations.clj:53-66,196` |
| `futon3c/src/futon3c/transport/http.clj:3940-3971` | request field (validated/stamped at ingress) | arbitrary; operator captures are `:coordination` with turn tags | yes, exactly the submitted body | `joe` 87 | 0.124 | futon2 operator focus `src/futon2/aif/operator_focus.clj:228-248`; 象 turn service via its source record |
| `futon3c/src/futon3c/social/coordination_ledger.clj:115,164`; `social/dispatch.clj:171`; `social/bells.clj:112-171`; `social/whistles.clj:140` | computed actor | mostly `:coordination` | sometimes message/reply text | included in per-author totals | — | `social/coordination_ledger.clj` mesh queries; otherwise generic search |
| `futon3c/src/futon3c/blackboard.clj:1491`; `process_watchdog.clj:218`; `test_registry.clj:694`; `nlp/classical_pipeline.clj:341` | constructor-specific literal/computed author | constructor-specific | sometimes | not separable without semantic author | — | tag-specific test-registry/audit readers; otherwise generic search |
| `futon3c/src/futon3c/agents/tickle.clj:157,174,190,235`; `agents/tickle_orchestrate.clj:141,196,827`; `agents/tickle_work_queue.clj:271`; `agents/arse_work_queue.clj:159`; `agents/apm_work_queue.clj:886`; `agents/zaif_controller.clj:310` | computed agent/worker | coordination and work-queue records | sometimes prompt/result text | included in agent totals | — | none found beyond generic search |
| `futon3c/src/futon3c/peripheral/mentor.clj:138`; `peripheral/real_backend.clj:586,891`; `peripheral/memory_write.clj:380`; `peripheral/memory_lifecycle.clj:69,274`; `peripheral/pull_receipts.clj:47,99`; `peripheral/common.clj:95` | constructor-specific | memory/receipt types | sometimes source memory | not separable | — | memory recall/lifecycle readers; generic search |
| `futon3c/src/futon3c/logic/probe.clj:169`; `logic/archaeology.clj:327`; `logic/tracer.clj:68,105`; `logic/snapshot.clj:224`; `logic/locus.clj:484`; `logic/metabolic_balance.clj:307`; `logic/ratchet.clj:145,325`; `logic/probe_taps.clj:165` | constructor-specific | logic observation types | no/derived | not separable | — | logic-specific readers not found; generic search |
| `futon3c/src/futon3c/transport/ws.clj:446`; `transport/irc.clj:50,183,460,606`; `transport/http.clj:3971,4067,4109,4366,4416,4469,5334,8650,8705,8808,8858,12227` | request/transport actor | arbitrary plus PSR/PUR/PAR | submitted text | not separable | — | filtered HTTP query API; generic search |
| `futon3c/scripts/backfill_claude_jsonl_chat_turns.py:336`; `deposit_runner_gate_memory.clj:133`; `wire_math_memory_patterns.clj:269,314,432`; `memory_outcome_sweeper.py:758`; `codex_ws_invoke_bridge.clj:339-390` | script-specific/computed | backfill, memory, outcome, coordination | often source text | not separable | — | script-specific filtered readers; generic search |
| `futon2/src/futon2/aif/evidence_emit.clj:34-69` (POST boundary) | call-supplied | call-supplied | call-supplied | included above by stamped author | — | no single reader; type-specific readers |
| `futon1b/futon1b_evidence.clj:1390-1450` (durable append implementation) | submitted/stamped author | arbitrary | stores submitted body | transport, not an additional semantic record | — | all indexed readers below |

The broad rows above are static append-call inventories, not claims that all
their constructors write the same schema. Sites whose constructor is dynamic
are retained as typed unknowns below rather than assigned guessed fields.

## Filtered readers

* `futon2/src/futon2/aif/operator_focus.clj:228-248`: author `joe`, tag
  `turn`, then post-validates operator origin and user role.
* `futon3c/src/futon3c/agents/zai_api.clj:1697-1705`: author/session plus
  `transcript` tag.
* `futon3c/src/futon3c/agency/obligations_reader.clj:42-54,98`: tag
  `promise-history`, promise id, and outcome types.
* `futon3c/src/futon3c/dispatch_with_recall.clj:719-728`: type
  `pattern-outcome` and author.
* `futon3c/src/futon3c/agency/dispatch_graph.clj:126-139`: composed filtered
  evidence query for dispatch graph inputs.
* `futon3c/src/futon3c/inbox_zero/infer_adapters.clj:90-99`: session id.
* `futon3c/scripts/xiangma_port_export.py:82-108`: tags `ma-observation` and
  `xiang-turn`; `scripts/xiang_turn_check.py:121` reads tag `ma-card`.
* `futon3c/scripts/ways-of-working-probe.py:80,154,584`,
  `lu_situations.py:86`, `turn_frames.py:72,97`, and
  `scope_view_data.py:118,258`: bounded author/tag/time filters.
* `futon3c/emacs/evidence-landscape-search.el` and the forwarded endpoint
  `futon3c/src/futon3c/transport/http.clj:3775-3786` are the single
  `:generic-search` reader. It is intentionally not wired to every semantic
  field: generic search is not a purpose for retaining a derived type.

## DiagramProver findings (verbatim)

```clojure
{:written-never-read
 [{:finding :written-never-read, :field :evidence/agent-step,
   :writers [:writer/claude-ws-step]}
  {:finding :written-never-read, :field :evidence/artifact-weak-activation,
   :writers [:writer/artifact-activation]}
  {:finding :written-never-read, :field :evidence/relationship-review,
   :writers [:writer/xiang-turn-service]}
  {:finding :written-never-read, :field [:evidence/transcript "象-1"],
   :writers [:writer/kimi-transcript-xiang-1]}],
 :read-never-written
 [{:finding :read-never-written, :field :evidence/dispatch-graph,
   :readers [:reader/dispatch-graph]}
  {:finding :read-never-written, :field :evidence/generic-search,
   :readers [:reader/generic-search]}
  {:finding :read-never-written, :field :evidence/obligation,
   :readers [:reader/obligations-api]}
  {:finding :read-never-written, :field :evidence/pattern-outcome,
   :readers [:reader/dispatch-pattern-outcomes]}
  {:finding :read-never-written, :field [:evidence/operator-turn "joe"],
   :readers [:reader/operator-focus]}]}
```

The scoped `joe` finding exposes a deliberate R0 limitation: HTTP ingress is
modelled as the unscoped writer because its author is request data. The runtime
records do establish that the 87 rows were authored by `joe`; the spec does not
guess a single producer site for them.

## Typed unknowns

* `{:status :unknown :field :author :sites ["blackboard.clj:1491"
  "process_watchdog.clj:218" "logic/*.clj sites above"] :reason
  :author-is-constructor-or-call-data}`.
* `{:status :unknown :field :volume-by-site :reason
  :fts-index-has-author-and-type-not-writer-source}`. Shared author/type streams
  cannot be apportioned among append call sites without reading every body and
  relying on optional origin metadata.
* `{:status :unknown :field :kimi-writer-implementation :reason
  :same-zai-transcript-boundary-selected-by-profile}`. The durable rows prove
  author `象-1`; the common constructor is `agents/zai_api.clj:1297-1328`, not a
  separate Kimi-only append call.
