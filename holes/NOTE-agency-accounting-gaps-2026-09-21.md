# Agency accounting gaps — discovery and design, 2026-09-21

Requested by claude-5 for Joe; author codex-17. Discovery only: no application
code changed, no reloads, no eval, no service writes. Builds on
`futon0/analysis/audits/SOURCES-work-records-2026-09-21.md` (db11a65), especially
§1, §4 and Source comparison. Source baseline: futon3c
`4d8a84a74b143fac5ec1d58742e8de3386f56863`; line references below are relative
to futon3c. A checked-in source path is not proof that every form is live-loaded.

## 1. Mesh edges: storage, schema, coverage

**The current mesh endpoint is not evidence of durable mesh history.**
`social/coordination_ledger.clj:49` constructs EvidenceEntry records;
`:78` writes through `evidence.boundary/append!`, but defaults to
`estore/!store`. `evidence/store.clj:25,45` defines that default as an atom and
wraps an atom in AtomBackend. In contrast, boot constructs a Futon1bBackend
and puts it in dev's separate `!evidence-store`
(`dev/futon3c/dev/bootstrap.clj:384`; `dev/futon3c/dev/invoke.clj:32`).
The job writer does not pass that backend (`transport/http.clj:1660`).
The mesh reader likewise queries the default store (`coordination_ledger.clj:178`).
This is a concrete source-level split, consistent with the GET observations below;
I did not inspect live vars through eval.

The record carries:

- Envelope: `evidence/id`, agent subject naming recipient, type `coordination`,
  claim-type `step`, author=caller, timestamp, tags `[coordination mesh-edge]`,
  `evidence/session-id` equal to **edge ID**, not a provider session ID.
- Body: `edge/id`, `edge/kind` (`invoke` or `invoke-result` only), `edge/from`,
  `edge/to`, `edge/surface`, `edge/at`, optional `edge/ok?`, `edge/error`.
- HTTP job creation uses `edge/id=job-id`. Direct wrapped registry calls use a
  fresh `mesh-edge-UUID`, shared by their invoke/result pair
  (`coordination_ledger.clj:138`). They need not have a retained job.
- No provider session, parent edge, `in-reply-to`, park ID, lifecycle sequence,
  or token usage is serialized. The HTTP caller supplies warrant fields, but
  the constructor does not consume them: its comment promises more than it stores.

`agency_send.py:23,76` makes `--from` the caller; `http.clj:1595,1660,4348`
uses it for the job, edge and prompt header. Despite the CLI warning's wording,
current HTTP source attempts an edge even for `http-caller`; the absent real
caller still prevents correct agent attribution/reply routing.

Completeness defects: `create-invoke-job!` catches thrown edge errors and ignores
`:ok false` receipts (`http.clj:1656`). It emits again on stable-ID job reuse,
with a new evidence UUID, so counting rows need not count distinct calls.
Only the direct wrapper emits invoke-result; HTTP job creation emits invoke
alone. Auto-bellbacks retain caller=`auto-bellback` for control semantics
(`http.clj:1220`), not the original callee's agent identity. Bell versus whistle
is a surface; semantic `bell-type` is a different dimension.

### Read-only sample

GET snapshots captured September 21 at about **17:14:36 UTC**. Exact seven-day
window: **[2026-09-14T17:14:36Z, 2026-09-21T17:14:36Z)**. Stores were read
sequentially, not in a cross-store transaction.

| Observation | Count / range |
|---|---|
| `/api/alpha/invoke/jobs?limit=10000` | 2,307 retained jobs overall |
| Retained jobs created inside seven-day window | 2,199 |
| Window surfaces | 1,320 bell; 756 auto-bellback; 38 whistle; 85 other |
| `/api/alpha/coordination/edges?limit=10000` | 20 rows, all distinct job IDs and all matching window jobs |
| Returned edge timestamps | Sep 21 16:13:36.500993908Z .. 17:13:58.688014416Z |
| Returned mesh rows / retained window jobs | 20 / 2,199 = 0.91% |
| Durable `:7073 /api/alpha/evidence/count?tags=mesh-edge` | 0, all time |
| Same durable count with exact window `since` and `before` | 0 |

Example: `invoke-1790010837682-23002-502e9a75`, claude-5 → codex-17,
surface bell, kind invoke; evidence `e-2d92e52f-b6d6-4e93-b5ed-5087fdd62e45`.
The coordination endpoint returns it; GET of that evidence ID through :7070's
configured evidence route returns 404. This corroborates the split above.

Do not call 0.91% a historical delivery success rate. Jobs themselves are
retention-limited; edge query metadata can also be lost by `mapv` in the mesh
reader if its backend ever supplies partial results. The present atom reader
has no 20-row cap in source. There is no explicit mesh TTL in this code, but
atom history lasts only as long as that process/store instance. Earliest
returned edge establishes about one hour of observed coverage, not the date
edge recording began. Durable historical coverage was zero in these queries.
Job detail lasts 24h; unreferenced terminal tombstones seven days, with active
and parked exceptions (`http.clj:414–640`). Backups do not close the gap.

Reproduction (GET only; `tags`, plural, is essential on :7073):

```sh
curl -s 'http://localhost:7070/api/alpha/coordination/edges?limit=10000'
curl -s 'http://localhost:7070/api/alpha/invoke/jobs?limit=10000'
curl -s 'http://localhost:7073/api/alpha/evidence/count?tags=mesh-edge'
curl -s 'http://localhost:7073/api/alpha/evidence/count?tags=mesh-edge&since=2026-09-14T17:14:36Z&before=2026-09-21T17:14:36Z'
```

A diagnostic `tag=mesh-edge` singular count returned 284,853 (unfiltered), so
it must not be used. The audit used plural `tags` correctly. An optional
`:7073 evidence?tags=mesh-edge&limit=2` request timed out; no completeness
claim rests on that request. Scratch JSON snapshots are
`/tmp/c17-accounting-{jobs,edges,agents}.json`; they are not durable artifacts.

## 2. Smallest coherent append-only lifecycle journal

**Extend the existing evidence boundary and coordination schema; do not add a
second best-effort JSONL logger.** Canonical immutable lifecycle entries should
be written via `evidence.boundary/append!` to the boot-configured Futon1bBackend,
served on :7073. The checked-in service specifies
`/home/joe/code/futon1b/migration-store-21`
(`scripts/systemd/units/futon1b-server.service:9`). No direct writes into its
files, new serving JVM, or TTL on accounting entries. Export can be JSONL later.

Merely adding append calls after each state mutation leaves a crash gap. The
smallest coherent addition is a **durable pending-event outbox in each existing
state authority**, committed atomically with the transition. Publish pending
entries through the one evidence boundary; clear pending entries only after
verified read-back. Retry uses stable event identity and verifies identical
content, refusing ID/content conflicts. The outbox is delivery state; evidence
is the historical journal. It must not be bounded/pruned while unacknowledged.
An API acceptance receipt means state plus pending accounting is durable;
projection lag must be explicit, not presented as a complete graph.

Job transitions already have a serialized atomic snapshot writer
(`http.clj:646,810,825`): add pending entries in that same committed map, under
the same writer discipline. Do not perform side effects inside a retryable
swap function or treat two independent writes as a transaction. Preserve
post-rename committed-error handling and ingress reconciliation. Existing
`append-job-event` (:750) supplies sequencing, but its transcript is compacted;
copy selected lifecycle events into the durable pending set before compaction.

**Park persistence is a prerequisite conflict:** `parked_on.clj:90` currently
spits and catches persistence failures; `note-completion!` (:305) removes
released records before invoking resume. Simply adding evidence calls cannot
prove that every acknowledged park/release survives a crash. First give park
state serialized atomic durable commits and retained delivery intents, with
loud failure. Until that change passes recovery tests, do not claim a complete
park journal. This is a structural correction, not an exception to durability.

Proposed versioned event body:

```clojure
{:schema :agency/lifecycle-v1
 :event/id "<entity-id>/<monotone-sequence>"
 :event/type :accepted                 ; running, done, failed, timeout, cancelled,
                                      ; park, release, wake-attempt, wake-ack, retract
 :event/at "<UTC instant>" :event/sequence 1
 :job/id "<job or nil>" :park/id "<park or nil>"
 :edge/id "<stable call/continuation ID>"
 :edge/type :bell                     ; whistle, auto-bellback, park, wake, other
 :edge/from "<caller>" :edge/to "<callee>"
 :edge/in-reply-to nil :edge/caused-by nil
 :transport/caller "<unaltered routing caller>"
 :surface "bell" :bell/type :request
 :caller/session-id nil :callee/session-id nil
 :provider/invocation nil}
```

Null means unknown; do not infer identities from today's roster for old work.
Keep observed transition time separate from ingestion time. `in-reply-to` is
an explicit edge-to-edge relation, not a replacement for the bell/whistle type;
a graph can materialize it as a typed relation. Preserve caller→callee agent
endpoints and separately record harness origin. Auto-bellback's graph sender
comes from the referenced original job's callee, while its control caller
remains `auto-bellback`. Missing original identity stays unknown. `ref` (ArSE
semantic referent) must not be conflated with job reply identity.

Park relates the waiting agent to Agency plus explicit awaited job IDs; release
means dependencies/timer allow resumption; wake means a delivery attempt to the
same agent/session; wake-ack confirms receipt, not necessarily paid model work.
Record lease retries with attempt identities, deadline/budget retractions and
coalescing separately. Headless resumes need a `park/id`→new `job/id` link;
buffer delivery needs the existing park ID→lease→ACK link. Otherwise park
latency and wake overhead remain incomparable. Preserve manual reply
`bellback-of` independently of whether automatic routing is enabled.

### Cost at observed volume

2,199 retained jobs / seven days ≈ **314 jobs/day** (lower bound if jobs were
removed early; not a measured request-rate peak). Three main lifecycle events
per job gives ~942 events/day. At an explicitly assumed **1 KiB serialized
metadata/event**, that is **0.92 MiB/day, 27.6 MiB/30 days, 336 MiB/year** before
store indexes, replication and backups. A planning envelope of eight events
per job gives ~2,513/day and 2.45 MiB/day (~0.87 GiB/year). Actual park/wake
volume is unknown; add `events/day × bytes/event`, not a fabricated multiplier.
No prompts, tool streams or result bodies are needed in these journal entries.

Publication adds an append and verification read per event, plus durable outbox
updates. Existing full-snapshot rewrite amplification may dominate these byte
estimates. No latency/fsync/substrate amplification benchmark was run; measure
it in the follow-up before selecting batching. Batching must retain durable
pending intents, not acknowledge volatile buffers. Recording metadata has no
model-token cost; an actual resumed model turn does.

## 3. Job → session → usage: what joins today

There **is** a retained job-to-provider-session join today. It is not an exact
job-to-token-cost join. `http.clj:1887–1935` writes final `session-id` and the
independent harness `trace-id` when finalizing; new/running jobs often have nil
session. The roster is a current routing view, not a historical identity table.

| Source | IDs carried | Join now / addition required |
|---|---|---|
| Job | job-id (also HTTP edge ID), agent-id, caller, final session-id, optional trace-id; disk bellback-of | Join final session to local logs now; trace-id is not a provider request ID |
| Claude | JSONL sessionId; message.id; requestId; timestamp; message.usage | Session-level candidate usage now. Exact per-job usage needs provider invocation/request linkage covering all requests/retries in a job |
| Codex | rollout session_meta.payload.id/session_id, filename UUID; turn context IDs; token_count cumulative/last usage | Session UUID matches job session-id now. Exact job attribution needs provider turn ID(s) and usage boundaries; token_count rows need not carry a turn ID |
| Mesh evidence | evidence/session-id = edge ID | Cannot use this column as a provider session join |

In the seven-day sample: 637 Claude-target jobs, **625 with session IDs**, all
625 matched local `.claude/projects/**/*.jsonl` filenames (25 distinct sessions).
864 Codex-target jobs, **842 with session IDs**, all 842 matched local
`.codex/sessions/**/*.jsonl` filenames (42 distinct sessions). These are file
matches, not proof that every matched file retains all usage. Overall 2,069 of
2,199 jobs have session IDs, including other agent types.

Checked content examples:

- Claude job `invoke-1790010466477-22998-34c5fa10` → claude-3 →
  `355a71b9-163b-4f96-bebe-0497607deff0`; matching local file contains that
  sessionId and usage-bearing message/request IDs (e.g.
  `msg_011CfGnSc2n1R44hXdX27Jzq`, `req_011CfGnSbCRK3KMQiNLN1Gu8`).
- Codex job `invoke-1790009472891-22996-45f5ee00` → codex-14 →
  `01a0c489-1ec2-7553-9e83-52d0f2bea66e`; matching rollout's session_meta ID
  agrees and it contains token_count events. `agents/codex_cli.clj:185,393`
  accepts provider thread_id/session_id as the returned Agency session ID.

One added **structured field**, `provider/invocation`, can carry provider,
actual session, native turn/request IDs and attempt numbers (a vector when a
job retries or crosses sessions). This is sufficient as a join envelope only
if adapters capture the real provider identifiers and usage boundaries; a new
string named job-id in Agency alone accomplishes nothing. Capture identity
when execution starts, and append updates on session change, not only at finish.
Claude often makes multiple API requests per turn, so one requestId scalar
would undercount. Caller session is separately needed to account for dispatch
work; callee usage alone is not the total cost of a call chain.

Historical exact attribution is not generally recoverable from the retained
fields. Prompt `Edge:` markers can support case-specific reconstruction, but
missing transcripts, compaction, mixed operator turns, retries and multiple
jobs per session prevent a complete retrospective join. Time overlap is an
estimate, not a key. Deduplicate Claude message usage updates; difference Codex
cumulative counters within a verified attempt/turn, handle resets, and never
sum cumulative snapshots. Tokens also need model/rate information for money.

## 4. Existing pieces to extend

| File:line | Existing behavior / limitation |
|---|---|
| `src/futon3c/social/coordination_ledger.clj:49,78,138,178` | Mesh schema, append boundary, paired direct invokes, edge reader; fix backend wiring and extend |
| `src/futon3c/transport/http.clj:1559,1634,1845,1887` | Creation, accepted edge, running transition, finalization; job lifecycle hooks |
| `src/futon3c/transport/http.clj:459,564,606,646,750,810` | Compaction, immutable commission archives, atomic snapshot commit, bounded event sequence; archives preserve requests, not lifecycle history |
| `src/futon3c/evidence/boundary.clj:1,217,440` | Single append authority and receipt/read-back checks; default-store convenience is not boot-backend injection |
| `src/futon3c/evidence/futon1b_backend.clj:270,324,437` | Durable backend, bounded cursor queries and partial-result metadata; reuse, expose completeness |
| `src/futon3c/agency/parked_on.clj:90,115,138,167,272,305,332,401,443` | Snapshot, inbox/lease/ACK, release/retract, park and recovery/deadline paths; no historical journal |
| `src/futon3c/transport/http.clj:1325,5228,5251` | Headless vs buffer resume routing, lease and ACK endpoints |
| `src/futon3c/agency/bell_router.clj:16` | Explicit bellback-of conversation graph; avoids guessing reply pairing |
| `src/futon3c/transport/http.clj:5403,5490,6284` | Accepts in-reply-to, conditionally persists bellback-of, thread graph endpoint |
| `src/futon3c/agency/mesh_qa.clj:121` | Deduplicates job/mesh twins for QA |
| `scripts/mesh_trace.py:1,60` | Existing graph observer; chronological reverse-pair reply heuristic is not reliable explicit reply provenance |
| `dev/futon3c/dev/invoke.clj:64` | Existing lifecycle evidence emitter using an explicit store; reuse event conventions |
| `dev/futon3c/dev.clj:3791,3894,4525,4683` | Provider session propagation and separate generated invoke trace IDs |
| `src/futon3c/agency/invoke_lifecycle_snapshot.clj:29,71,92` | Serialized multi-provider snapshot boundary; not historical events |
| `src/futon3c/agency/invoke_lifecycle_reconciliation.clj:82` | Snapshot consistency/recovery checker; extend its coverage for pending journal intents |
| `src/futon3c/agency/turn_queue.clj:152` | Durable bounded queue writer, not unbounded history |
| `src/futon3c/agency/clock_lineage.clj:133` | Durable clock attribution edges; complementary mission context, not dispatch history |
| `src/futon3c/test_registry/ledger.clj:65` | Write-only content-addressed artifact store; useful durability precedent, not an ordered event log |
| `holes/C251-invoke-ledger-durability-discovery.md` | Earlier framed-log/SQLite analysis; its old non-atomic job-writer finding is superseded by current :646 writer |

Search included `journal`, `ledger`, `mesh`, `edge`, `trace` across src/dev and
scripts. Other journal hits (APM rearm, memory service, peripheral drive) are
domain-specific; none examined supplies the requested general lifecycle journal.

## Proposed small follow-up handoffs (review each before the next)

1. **Make mesh reads/writes use the configured durable backend.** One behavior:
   identical backend authority at both endpoints. Acceptance: isolated real
   Futon1bBackend integration (slow test) writes an edge, retrieves it through
   mesh and evidence readers after client reconstruction; an AtomBackend passed
   as production configuration is explicitly refused. No stub-only durability proof.
2. **Define immutable accounting event identity.** One behavior: same ID/same
   content retries deduplicate, same ID/different content refuses. Acceptance:
   exercise both against real backend, retain typed surface, semantic bell type,
   reply and provider fields; verify no warrant metadata silently disappears.
3. **Atomically retain job lifecycle publication intents.** One behavior:
   accepted/running/terminal transitions and pending events commit together.
   Acceptance: inject failure before and after snapshot rename; reconstruct
   state and show no acknowledged transition lacks its event intent. Reused job
   IDs and racing terminal outcomes produce one event per actual transition.
4. **Publish pending events reliably through the evidence boundary.** One
   behavior: retry until verified durable, then clear pending. Acceptance:
   actual unavailable backend leaves intent visible; restart between append
   and clearing deduplicates; conflicting receipt blocks with a typed error.
   Unpublished entries survive compaction. Report publication lag and backlog.
5. **Repair park snapshot commit durability.** One behavior: failed persistence
   cannot acknowledge/remove a park transition. Acceptance: concurrent writers,
   disk error and process-death recovery with real temporary files; preserve
   exact committed state and refuse corrupt input. No journal completeness
   promise before this passes.
6. **Retain park/release accounting plus continuation intent atomically.** One
   behavior: removal of a released park cannot lose its event or continuation.
   Acceptance: completion/recovery race, coalesced park, exhausted budget and
   timer expiry each produce their correct single transition and causal links.
7. **Record wake delivery attempts and acknowledgement.** One behavior: park ID
   joins every lease/retry/ACK or headless resumed job. Acceptance: lost ACK
   causes distinct attempts but one acknowledged wake; lost WS poke does not
   lose the durable ready item; wake-ack is not misreported as model execution.
8. **Preserve explicit graph reply provenance.** One behavior: persist
   in-reply-to and original agent endpoints independently of routing flags.
   Acceptance: crossed bells, auto-bellback and manual reply reconstruct correct
   directions; unknown parent stays unknown; park dependency joins remain typed.
9. **Capture provider invocation identity for Claude.** One behavior: append
   actual session/request IDs per job attempt. Acceptance: two jobs in one real
   recorded session and a retry join disjoint usage messages without counting
   duplicated updates. Missing provider IDs are explicitly unjoinable.
10. **Capture provider invocation identity for Codex.** One behavior: join
    native turn/attempt IDs and usage boundaries. Acceptance: real rollout
    fixture with adjacent turns, repeated counters and reset; totals belong to
    the correct job and never sum cumulative snapshots.
11. **Expose a paginated typed graph/journal read view.** One behavior: consumer
    can exhaust a declared time window with explicit partial/lag status.
    Acceptance: sparse pages and exhausted request budget cannot masquerade as
    complete; job compaction leaves historical graph unchanged. Reuse bell-router
    and mesh QA, replacing mesh_trace's reply heuristic with explicit identity.

Each coding handoff runs clj-kondo, check-parens for changed Lisp/Clojure and
only its relevant test namespace. Real backend/crash integration tests should
be tagged slow; run in isolated test processes, never mutate the shared JVM.
The sequence above is proposed work, not implemented or validated behavior.
