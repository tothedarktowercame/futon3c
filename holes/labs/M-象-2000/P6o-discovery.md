# P6o discovery — origin at the write boundary

2026-09-27, codex-4 for claude-17. Discovery only: no runtime edits, store writes,
backfill, Emacs reload or JVM reload. Inspected futon3c master at
`5a32e3b55bd5cf7210e9a3a1798c04fde5a85cb9` plus the shared working tree. Locations
below are relative to futon3c unless prefixed with another repository; historical
locations explicitly name their revision. Line numbers describe this inspection.

The stack has no single answer to “who wrote this turn.” Emacs records the login
name for user-role text, transcript instrumentation records the receiving agent,
and the HTTP header classifies the supplied caller string. None alone proves
that Joe typed the text. A write-time origin must travel with the turn through
queues and delivery, alongside the attributed author and an exact source ID.

Scope: primary conversational records, their ingress/delivery paths, and derived
operator-turn copies. This is an inventory of the located writers in those paths,
not a claim that every evidence producer in every Futon repository was audited.
The patterns read were `futon3/library/象/名分有据.flexiarg` and
`futon3/library/象/释义非授.flexiarg`; the plan is BUILD-PLAN-象2000.md:121 and
MAP exit item 1 is in holes/missions/M-象-2000.md:332. Origin does not establish
permission, delegation scope or institutional effect.

## Sites and what they know

| Write or ingress site | Present attribution / information available | Proposed write-time treatment |
|---|---|---|
| `emacs/agent-chat.el:3294` (`agent-chat-emit-turn-evidence!`), author at :3314, body at :3323 | User role → `$USER`/login/joe, claim `question`; assistant → supplied assistant author. Role cannot distinguish operator, agent message and generated notice. Dictation marker only adds a surface tag. | Accept a structured origin argument from turn context; never derive it from role or `$USER`. Assistant model text has agent origin. |
| `emacs/claude-repl.el:796`, :819; `emacs/codex-repl.el:3347`, :3370 | Wrappers call the shared emitter for user and assistant turns. Claude request caller at :1028 is also `$USER`. | Propagate the same origin context to evidence and invoke request. Changing only the evidence helper leaves misleading invocation headers. |
| `emacs/agent-chat.el:2451`, :2464, :2508, :2576 | Already retains operator vs unsolicited, speaker, and queued turn context. Unsolicited is broader than harness: an agent's message may also be unsolicited. | Capture operator origin at genuine interactive send; retain an immutable per-turn context through queue/drain/callback. Split unsolicited sources explicitly; do not equate all unsolicited text with harness. |
| `src/futon3c/transport/http.clj:4502`, :4524 | `resolve-origin` and `wrap-surface-header` produce Surface/From/To/Origin/Edge/Caller. Joe/joe-repl → operator; a small caller allowlist or auto-bellback surface → harness; everything else → agent. | Preserve structured origin and source edge separately from display header. Today these are caller-name assertions, not authentication. `parked-resume` is absent from the harness set at :4499 and therefore classified agent. |
| `src/futon3c/transport/http.clj:1682`; `src/futon3c/social/coordination_ledger.clj:112` | Invoke edge ledger knows endpoints, surface and job/edge ID. | Record origin at ingress and carry this edge ID into all derived turn records; avoid nearest-time joins. |
| `dev/futon3c/dev/invoke.clj:64`; invoke-start callers `dev/futon3c/dev.clj:3623`, :3895 | Lifecycle emitter sets author to receiving agent. Invoke-start body has prompt preview, truncated to 200 characters (dev.clj:3616), which may contain an Origin header. | Lifecycle event itself is harness-generated; separately preserve the input's origin, caller and edge. Do not parse a truncated prompt to populate the authoritative stamp. |
| `src/futon3c/agents/zai_api.clj:1108`, :1123, :1090 | `transcript-entry` assigns agent author. `persist-turn-start!` preserves exact prompt, requisition and dispatch ID; safe persistence does not decide origin. | Turn-start input needs propagated input origin; transcript envelope writer is harness. Dispatch ID already gives an exact join. |
| `src/futon3c/agents/zai_api.clj:1392`, :1411 | Compaction bookkeeping and model turn-round records share that transcript builder. | Distinguish harness compaction operation from agent-produced summary/round text. One agent author default cannot describe both. |
| `src/futon3c/transport/irc.clj:180`, :457, :603 (author assignments :188, :465, :611) | IRC conversational evidence uses sender nick (`from`/`from-nick`); connection events use irc-server (:55). | Preserve sender/session provenance; identify trusted operator/agent sessions at ingress. A nick alone cannot prove operator origin. Connection notices are harness. |
| `scripts/backfill_claude_jsonl_chat_turns.py:94`, :140, :294, :336 | Another actual chat-turn writer: imports local Claude JSONL; parsed header caller becomes user author, assistant agent is contextual/defaulted. It excludes some continuation/local-command boilerplate. | Imported source provenance must remain historical, explicitly distinct from write-time observation. Stamp importer execution as harness and carry source actor as a separate supported claim. |
| `emacs/session-turn-analysis.el:117`, :606, :659; `scripts/operator_turn_capture.py:79` | Local JSON operator-turn copies: interactive advice requires operator origin and expected speaker; external capture accepts text selected from author=joe evidence and calls `session-mode-record-external-turn`. | Carry evidence ID and source origin into copies and analyses. External capture cannot prove operator origin from joe attribution. Analysis is interpretation, not new authorization. |
| `src/futon3c/agency/promise_history.clj:91` | Promise transition envelope author is park/followup agent although transition is generated by machinery; format-3 payload preserves record. | Stamp transition execution harness, retain agent as promise participant and exact predecessor/source IDs. |
| `src/futon3c/transport/http.clj:4450`, :4484 | Generic chat-envelope emitter is disabled; review snapshot emitter does write with supplied author or mission-control. | Do not count disabled emitter as a live chat writer. Review computation is harness execution with initiating actor separately retained. |
| `src/futon3c/evidence/boundary.clj:256`, `src/futon3c/evidence/store.clj:84`; `futon1b/futon1b_evidence.clj:43` | Boundary/store and server accept supplied author; futon1b requires nonblank author and constructs a document. They cannot recover whether a human typed it. | Validate and preserve provenance, not invent it. Extend explicit document/serialization projections and schemas before relying on a new envelope field. |

## Generated text delivered as user input

| Generator / delivery | What is known there |
|---|---|
| `transport/http.clj:1325` assemble-resume-prompt; :1345 parked-resume! | Generated wake/deadline text, park ID, original payload and dependency reports are available. Stamp the new delivery harness with source park ID; preserve original input and agent report provenance as components. Headless caller is parked-resume (:1298, :1370). |
| `emacs/agent-repl-park.el:89`, :117 | Ready park ID is known before unsolicited continuation delivery. Carry its origin through agent-chat rather than merely changing displayed speaker. |
| `agency/followup_queue.clj:72`; `transport/http.clj:5348`; `emacs/agent-repl-park.el:270`, :306 | Queue stores type, metadata, prompt, ID and session; Emacs delivers with speaker followup. This transport knows the item, not necessarily its original author. Set provenance at producer, preserve through enqueue/lease/ACK and into the turn. |
| `inbox_zero/sweeper.clj:252`, :263; `inbox_zero/turn_promotion.clj:191` | Notice/proposal generators have explicit inbox-zero type and producer metadata. Stamp harness here; keep recipient distinct from author. |
| `apm/store_read_hold.clj:89` | Repair followup has apm-store-repair type. Another generated followup requiring harness stamp, beyond the three historical filters. |
| `agents/zai_api.clj` at `626df9fa^`:1272–1288, :1291–1310 | Kimi caller-followup generator knew caller, session, notice type, dedupe key and metadata. Both clock reminder and refusal-copy were system delivery. Stamp harness, refer to originating invocation; caller is recipient, not text author. |
| Current `agents/zai_api.clj:1239` | Kimi caller followups were retired on 09-25 (`626df9fa`); function remains a no-op for existing closure references. Do not revive it to implement provenance. Historical notices still need attribution findings. |

## Proposed contract for a later implementation

Use an envelope `:evidence/origin` map containing `:kind` in
`#{:operator :agent :harness}`, `:actor` (actual source actor), `:writer`
(component committing the record), `:source-id` (edge/park/followup/message ID),
`:surface`, `:recorded-at`, and `:basis :write-time`. Keep `:evidence/author` as
existing attribution for compatibility. For instrumentation about another act,
store that act's provenance separately as `:input-origin`; a harness-written
invoke-start must not masquerade as an operator act just because its prompt was
operator-authored. For mixed resume prompts retain component source references.

Set origin when an interactive turn, agent message or generated delivery is
created. Propagate it through request → job/edge → queue → Emacs callback →
transcript/chat evidence. Validate at the common boundary and direct futon1b
append endpoint. Preserve fields through in-memory store, backend wire encoding,
futon1b normalization, reads and snapshots. A boundary default cannot supply
truth that producers discarded. Historical/legacy unknowns must stay explicitly
unresolved; do not guess operator or quietly call an unknown program an agent.
A staged migration needs visible missing-origin reporting before making the
three-way stamp mandatory for new writes.

Caller strings and Origin text are not credentials. A forged `caller=joe` or
quoted header must not gain operator provenance. Bind trusted producer context
at ingress; distinguish asserted from verified identity during migration.
Authorization refs (scope, validity time, delegation chain) are separate from
origin and require their own packets. Neither a provenance classifier nor an
intent interpretation creates a grant or changes a promise.

## Historical filter and backfill findings

The mission documents 436 park/wake, 27 inbox-zero and 42 Kimi exclusions at
M-象-2000.md:163. **I did not locate a saved script implementing that exact
three-category window export.** Searches covered futon3c/scripts, Python/Markdown
under storage/operator-turns and top-level /tmp Python scripts. The two files in
window-0922 contain no filter manifest. This is an evidence gap, not a reason to
claim a nearby script reproduces the export.

Located rules:

- `scripts/operator_turn_capture.py:54–61,90`: skips text containing
  `--- resumed:` or `so your clock says what you are doing`. This is a live
  evidence-to-Emacs capture filter, not the full window filter: no inbox-zero
  rule, and it misses the older Kimi wording and refusal notices.
- `scripts/operator_turn_lexical.py:21–42`: strips structural-analysis,
  system-reminder, pasted-content, resumed suffix and code blocks for lexical
  statistics. Stripping part of mixed text is not classifying the entire record.
- `scripts/filter_agent_register.py:16–35`: k=6 clustering, drops clusters
  1/2/5/6 by default. Not an authoritative origin filter or the window export.
- `scripts/harvest_turn_origins.py:46–82,96–137`: harvests invoke-start headers,
  then joins nearest timestamp within 180 seconds **by session**. Despite its
  introductory agent-and-time description, implementation uses session. This
  remains a heuristic; it can confuse adjacent/queued turns. Pagination uses
  before timestamp only, so ties can be skipped; headers may be truncated.

Read-only reproduction against the files on disk (2026-09-27): raw file has
17,290 rows, not only the named window; joe file has 485. Restricting raw `at` to
`2026-09-22 <= at < 2026-09-27` gives 961 rows. Removing IDs in the joe file gives
476: 406 containing `--- resumed:`, 22 remaining starting `inbox-zero`, 42
remaining starting `You requisitioned` or `You can't use a Kimi`, and six other
rows (including short replies). Across all 961, substring inbox-zero matches 35,
including operator discussion. Thus the 42 Kimi count reproduces, but **436/27
are not reproduced by these candidate rules**. Recover the original extraction
script/input pin before asserting exact replay of those counts.

A one-time evidence-store backfill is feasible as a new, versioned attribution
layer; none of these scripts is safe to run unchanged as a store migration.
The capture script writes local Emacs interpretation files, the lexical/cluster
scripts transform corpora, and the harvest writes local JSONL. Proposed steps:

1. Pin system-as-of and source file hashes; page evidence with `(at,id)` cursors,
   limit <=1000, checking completeness. Decode real EDN/JSON body representation.
2. Prefer exact source edge/dispatch/park/followup references. For historical
   templates, anchor full producer wording and exclude quoted/operator discussion;
   label result inferred, retain rule/version/source IDs and matched span. Mixed
   operator-plus-resume records need component attribution, not wholesale erasure.
3. Emit a dry-run manifest of IDs, classes, ambiguous/unmatched cases and counts;
   compare the 485-ID corpus and 42 Kimi witnesses. Report the 436/27 discrepancy.
4. After review, append idempotent attribution records referring to originals,
   with deterministic `(source evidence ID, classifier version)` identity. Keep
   original author/text/timestamps and original unknown write-time origin intact.
   The backfill's own origin is harness; its claim is retrospective interpretation,
   never `:basis :write-time`. No overwriting old evidence to simulate foreknowledge.
5. Downstream views may opt into reviewed attribution with explicit precedence;
   uncertain rows stay unresolved. This backfill grants no authority and triggers
   no promise transition. Its event and insertion times must remain distinct.

## Risks and proposed acceptance for implementation

Emacs changes require Joe's Emacs reload on every sending machine. A JVM-only fix
cannot repair the user-role author collapse. Queued turns and async callbacks
must retain their own context rather than a mutable buffer's next-turn metadata.
Shared futon3c/futon1b JVM reloads require tested additive changes from canonical
master checkouts, with old invoke closures considered; no restart from this agent.

Tests should exercise interactive operator input, agent bell, zero-budget wake,
inbox-zero delivery and repair followup through real serialization boundaries;
assert distinct actor/recipient/writer, preserved source IDs and queued-context
isolation. Add spoofed caller/header and mixed resumed-text bad cases. Verify
store roundtrip and old unstamped reads, and replay an identical backfill twice
without duplicate attribution records. Discovery changed Markdown only, so no
Clojure/Lisp runtime gates or live mutation were appropriate for this packet.

## Reviewer addendum (claude-17, 2026-09-27)

Spot-checked three citations (operator_turn_capture.py:54 and :79,
session-turn-analysis.el:117); all match. A narrower grep for writers that put Joe's
name on a record found three not covered above, all to be included in the
implementation packet:
- `emacs/session-mode.el:983` — `(author . "joe")` set outright;
- `emacs/kimi-repl.el:258` and `emacs/zai-repl.el:411` — `:caller` from `$USER`,
  the same form as claude-repl.el:1028 and codex-repl.el:3903.
`src/futon3c/nlp/classical_pipeline.clj:223-258` also hard-code `"joe"`, but those are
test fixtures (`fixture-turns`), not writers. The broad grep (`:author|evidence/author`,
128 files) is mostly agents and services stamping their own ids; they need the
`:agent`/`:harness` value, not a rewrite.
