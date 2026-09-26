# Job A discovery: memory captions and applicability observations

2026-09-10, Codex-15, for claude-5 and Job H (claude-1).
**Discovery only.** This note proposes a contract, not implemented functions or
routes. No source, tests, store entries, campaign inputs or runtime were changed.
Read in full: `TN-memory-caption-upgrade-dispatch-2026-09-10.md` (83f08382),
`TN-APM-memory-descriptive-captions-2026-09-10.md` (cc62da46), and
`TN-APM-memory-current-audit-2026-09-10.md` (330315fe), in this directory.
Citations below are workspace-relative paths, followed by starting line numbers;
`src/` and `holes/` without a repository prefix mean futon3c. They describe the
source checkout observed today, not a claim about which versions the JVM loaded.
The three declared concurrent source edits were not changed. Other agents also
have untracked historical pattern-library drafts; those were left alone.

Recommendation: store versioned captions, applicability observations, and their
review/lifecycle decisions as separate append-only **reflection evidence** about
the original memory, with a derived current-caption view and an explicit search
join back to that memory. Do not add captions to signed memory bodies or create
new `:memory/assert` edges for them. Lab files remain H's draft interchange.

## 1. Write path and immutable identities

| Hop | Existing consumer and shape | Effect of a new caption field |
|---|---|---|
| Frozen role card | `src/futon3c/apm/queued_frame_adapter.clj:354` selects `codex-scribe-v2.md`, `zai-scribe-v2.md`, and promotion-proctor-v4. Codex card `holes/labs/M-apm-demonstration/role-cards/codex-scribe-v2.md:35` mines certified head/base diff; line 53 asks for a transferable positive move. Zai card `holes/labs/M-apm-demonstration/role-cards/zai-scribe-v2.md:58` specifies six arc fields. | Neither is a caption admission contract. Do not assume the older `scribe-v3.md` alone is the current card. Change future pins, not an active frame's frozen card. |
| Typed submission | `src/futon3c/apm/typed_role_submission.clj:151` requires `:receipt` for Scribe promote-solver; `src/futon3c/apm/live_promotion.clj:365` explicitly consumes the typed `:receipt` EDN map/JSON object, not conversational prose. `src/futon3c/apm/typed_role_submission.clj:297` checks required keys, disallows supplied authority, and checks durable search/pattern accounting. `:submit!` at line 392 binds registered job/token and hashes authority plus payload. | Extra nested candidate keys are not generally rejected by this required-key validator; they can survive submission without being admitted or persisted as memory content. The Student's nested `:memory-use` is different: lines 325–359 explicitly reject keys outside the generated role-authored set. Putting observations there today is invalid. |
| Candidate validation | `src/futon3c/apm/promotion_pipeline.clj:27` declares name/hook/body/pattern-ids for Guide candidates; `src/futon3c/apm/promotion_candidate_store.clj:85` requires nonblank name/hook/body and nonempty vector of patterns. `persist!` at line 178 derives depositor from request and requires controller source attempts. | Unknown caption keys do not satisfy missing mandatory memory fields. These are validators for mathematical/process memories, not caption records. |
| Canonicalization | `src/futon3c/apm/promotion_candidate_store.clj:54` overwrites kind, admission schema, source attempts, memory id and digest with controller values; reported values remain diagnostic fields. | A caption may remain on the in-memory candidate map, but is not in its persisted body. |
| Body and identity | `src/futon3c/apm/promotion_candidate_store.clj:38` selects exactly name, hook, kind, body, why, how-to-apply, admission/schema. Lines 71–83 hash that body, then dispatch id + ordinal + depositor + content digest to produce `e-apm-promotion-…`. | **Confirmed drop before both digest and write.** Merely extending this select-keys list changes new memory identity and invalidates old content digests if used to rewrite old bodies. Do not do that for captions. |
| Write | `src/futon3c/apm/promotion_candidate_store.clj:95` builds EvidenceEntry `:memory/:assert`; line 108 builds a proposed mathematics `:memory/assert` edge. `src/futon3c/peripheral/memory_write.clj:279` sends the pair to `/api/alpha/memory/assert`. Candidate store lines 204–255 handle exact replay, pair/edge write and fresh read-back. | Evidence body is selected again; edge props also enumerate fields and omit captions. A sibling caption cannot pass as a memory without a real memory edge, which the commission forbids fabricating. |
| Lower storage boundary | `src/futon3c/social/shapes.clj:312` has a closed enum of evidence types, including `:reflection`, but no `:caption`; EvidenceEntry at line 318 accepts any body. `futon1b/futon1b_evidence.clj:35` builds an explicit evidence envelope and retains the body at line 58. `write-evidence!` at line 166 rejects duplicate ids, verifies existence and queues indexing. `src/futon3c/evidence/boundary.clj:314` runs coercion, validated append and persistence verification. | A new top-level evidence type needs a schema change. Arbitrary top-level envelope keys are not a portable extension (futon1b enumerates its fields). Put typed metadata inside a reflection body, with its own admission validator. Generic body permissiveness is not semantic approval. |
| Read-back | Candidate store `exact-entry?` line 130 compares complete entry except first-write time; `materialization-witness` line 152 records content/persisted/read-back digests and persistence id. `visible?` line 260 verifies content and edge. `review-inputs` line 295 freshly fetches complete entries; absent/nonblank-body failures and mismatched digest fail closed. | Updating an old body is a conflict, not enrichment. Caption side records need separate exact read-back receipts. |
| Review dispatch | `src/futon3c/apm/live_promotion.clj:250` puts candidates, full persisted candidate evidence, role and candidate-set digest in reviewer authority; line 398 refuses dispatch when read-back fails. Line 220 checks returned set digest, pinned base blob and residual shape and derives reviewer from the expected request. | Even a caption key excluded from the memory body can perturb the **whole candidate-set digest** if retained in the candidate map. Sidecar drafts must be separated before the frozen memory candidate vector; independently digest their review input. |
| Proctor and persistence | `holes/labs/M-apm-demonstration/role-cards/promotion-proctor-v4.md:7` requires one attributed verdict/reason/residual per candidate and typed use-kind for approved typed memories. `src/futon3c/apm/promotion_pipeline.clj:182` validates set, attribution, patterns, reasons and kind. `src/futon3c/apm/live_promotion.clj:955` validates then persists. `src/futon3c/apm/promotion_review_store.clj:24` derives review identity; line 75 enumerates review-body fields, and line 137 persists then calls lifecycle. | Existing memory review does not independently approve an extra caption. Unknown review fields are not included in the persisted review-body selection. A distinct caption-review receipt is required even if the same independent proctor evaluates both in one job. |
| Publication/snapshot | `src/futon3c/apm/promotion_review_store.clj:214` applies `memory-lifecycle/review-attachment!`; `src/futon3c/apm/promotion_pipeline.clj:357` validates persisted reviews and merges approved fields into candidates. `src/futon3c/apm/live_promotion.clj:1011` invokes completed-pass publication. `src/futon3c/apm/countdown_control.clj:1182` attaches campaign provenance and calls `memory-snapshot/publish-cumulative!`. `src/futon3c/apm/memory_snapshot.clj:230`, line 302 and line 338 validate, freshly check review/edge authority, hash, atomically write and read back snapshots. | Snapshot body stores ordered candidate maps (line 245), not a caption projection. Adding keys can change snapshot digest even when unused in retrieval. Existing snapshots must remain byte-stable; future caption supply should be separately pinned. |

Identity computations to preserve: `campaign-machine/ledger-digest`
(`src/futon3c/apm/campaign_machine.clj:27`) is SHA-256 over recursively canonical
EDN encoded as UTF-8. Besides the memory id/body and materialization digests,
`promotion-pipeline/candidate-key` (`src/futon3c/apm/promotion_pipeline.clj:6`) deduplicates by
content digest + sorted patterns; reviewer authority hashes the whole candidate
vector (`src/futon3c/apm/live_promotion.clj:257`); review identity hashes job, memory id, verdict,
reason, residual, patterns and derived status/kind (`src/futon3c/apm/promotion_review_store.clj:51`).
Snapshot ids/digests hash the entire snapshot body (`src/futon3c/apm/memory_snapshot.clj:374`).
Typed submission ids hash authority plus payload (`src/futon3c/apm/typed_role_submission.clj:406`).
A *new* submission may legitimately have a new hash; no previous submission,
review, body, snapshot or receipt is to be amended to insert a caption.

## 2. Read path and exact caption-to-original join

The path is not a single pipeline ending in snapshot creation. Search and frozen
shelf publication are separate supply channels that meet in Student accounting.

1. `src/futon3c/apm/role_memory_search.clj:77` authenticates a registered job/token,
   permits only student/scribe/zai-scribe/promotion-proctor (line 16), caps results
   at 10, and queries `recall/propose-patterns-by-query` with domain mathematics.
2. `src/futon3c/peripheral/memory_recall.clj:500` performs bounded FTS overfetch
   (3 times requested limit, maximum 100); if no admitted primary match exists,
   lines 542–549 try an OR query over at most four selected tokens. Thus AND is
   the index default, not the only end-to-end retrieval behavior.
3. `futon1b/futon1b_text.clj:113` specifies FTS5 unicode61 (no stemming).
   `body-text` at line 155 indexes strings verbatim and structured bodies via
   pr-str. `index-batch!` at line 163 indexes all evidence body text under the
   evidence id. `match-string` at line 371 quotes tokens, preserves explicit
   AND/OR and otherwise uses conjunction. `search` at line 627 rechecks candidates
   against the store, including content, then hydrates full evidence entries.
4. `src/futon3c/peripheral/memory_recall.clj:326` retains only `:memory` entries or the narrowly typed
   reflection pattern-description at line 318. A caption reflection currently
   drops here. `proposals-from-rows` at line 350 uses each hit's **evidence id**
   as recall endpoint, then requires its projected `:memory/id` to equal it.
   An arbitrary caption id therefore cannot masquerade as its parent memory.
5. `project-components` (`src/futon3c/peripheral/memory_recall.clj:30`) enforces explicit domain,
   reviewed attachment, non-retracted/non-superseded state and entry presence.
   `futon2/src/futon2/aif/memory_contract.clj:76` checks memory evidence type,
   assert-edge type, equal entry id and endpoint membership, then provenance
   and typed kind. These remain checks on the original entry and edge.
6. `src/futon3c/apm/role_memory_search.clj:62` calls `memory-access-gate/enforce-carrier` on
   content matches AND pattern candidates before returning them. The gate at
   `src/futon3c/apm/memory_access_gate.clj:64` handles depositor holdout, not role/domain policy:
   unknown holdout and unverifiable provenance fail closed; same-problem source
   and explicitly withheld ids are refused; supporting memories are recursively
   checked. No holdout returns allowed at line 75. Role policy is step 1; domain
   and attachment policy are step 5. Do not conflate these authorities.
7. `src/futon3c/apm/memory_snapshot.clj:512` verifies the frozen snapshot digest, frame, problem
   and exact original-memory accessible-id set. `src/futon3c/apm/live_learning_phases.clj:368`
   derives surfaced ids from shelf plus current/repair search receipts; line 391
   additionally considers cascade offers for allowed-use validation. Search can
   legitimately return reviewed mathematics memories outside the frozen shelf;
   do not require every search hit to already belong to that snapshot.

**Required new join, proposed:** recognize a versioned caption reflection as a
lexical seed; fetch its exact record, publication decision and current-head state;
verify its parent memory id and body digest; fetch the ORIGINAL evidence entry
and current reviewed edge; run the existing domain/visibility/contract checks on
those originals; deduplicate by original memory id; run depositor holdout on
original provenance, including every pattern-support record. Only then return
an original `:memory/id` plus separate caption id/revision/review-id metadata.
Never replace `:memory/body` with the caption record. Search `:result-ids`, use ids
and snapshot accessible ids remain original ids. Receipt match metadata records
which caption revision supplied the lexical match, preserving replay even after
supersession. A broken/missing/retracted parent or missing caption approval is a
refusal, not permission to return a naked caption hit.

**Existing source-level provenance mismatch to test, not conceal:** compact-memory
currently emits `:memory/provenance` with author/session (`futon2/src/futon2/aif/memory_contract.clj:68`,
line 111), while holdout `valid-provenance?` expects unqualified `:provenance`
and `:depositor` (`src/futon3c/apm/memory_access_gate.clj:21`, line 33); direct content-match
assembly at `src/futon3c/peripheral/memory_recall.clj:374` does not add those unqualified keys. Pattern
supports at line 402 copy those keys from compact memory. This trace does not
establish a successful non-shelf held-out search in the live JVM: source suggests
such records fail closed. The caption join must derive verified provenance from
the ORIGINAL evidence author/subject, never the caption author's task. A focused
fixture must settle the ordinary-path behavior before claiming gate preservation;
do not weaken the gate or invent campaign provenance to make it pass.

## 3. What an observation can bind to today

| Evidence | Identity, location, content | What it warrants |
|---|---|---|
| Authenticated search | `src/futon3c/apm/role_memory_search.clj:14`, line 19: `data/apm-role-memory-searches/receipts/<receipt-id>.edn`. At lines 93–128: trace digest from job/query/limit; receipt digest from full body; job, dispatch, agent, frame, problem, phase, role, query, index-as-of, holdout decisions, result ids, full content matches and candidates. Atomic write and exact replay/conflict check. | Results were recorded for that authenticated job, including returned original body when a content match. Not reading, understanding or use. No standalone retrieval revision field today. |
| Valid receipt lookup | `src/futon3c/apm/role_memory_search.clj:225` reads receipts by job and verifies content address; line 249 validates authority for explicit receipt claims. `receipt-surfaced-ids` at line 158 includes patterns, subjects and other ids as well as memories. | An observation must additionally resolve its selected id to a genuine original memory. Membership in this broad id set alone is insufficient. Repair predecessor receipts are re-gated at line 187. |
| Shelf exposure | `src/futon3c/apm/memory_snapshot.clj:245`, line 512: immutable snapshot id/digest plus accessible memory set. `src/futon3c/apm/live_learning_phases.clj:797` archives exact delivered packet as `<phase>-packet.txt` beside phase state; line 801 writes it atomically. | Availability and recorded delivery. Shelf id inclusion is not proof that a body was opened. Archive writes can fail and report failure; do not assume all historical packets exist. |
| Cascade exposure | `src/futon3c/apm/live_learning_phases.clj:593` retains offers and used-via-cascade in Student receipt; line 1099 prints request into prompt. | Offer delivery (typically compact references), not full-body inspection. Preserve route and original id, not invented use. |
| Student terminal/use | `src/futon3c/apm/live_learning_phases.clj:570` builds receipt with job/session/frame/problem, ordinal, outcome, failure-account, memory-use, snapshot binding and cascade; line 681 hashes it. `run-live!` at line 1249 uses state-path and at line 1303 persists through `live-preflight-runtime/atomic-persist!` (`src/futon3c/apm/live_preflight_runtime.clj:181`). `src/futon3c/apm/live_job_driver.clj:1351` stores certificate as `:receipt`. | Attributed used-ids checked against controller supply, not causal proof. Archive source at `src/futon3c/apm/live_learning_phases.clj:793` supports later artifact checks. Missing observation receipts (line 684) explicitly distinguish collection failure from Student reporting. |

The Student currently authors only used-ids in memory-use
(`src/futon3c/apm/live_learning_phases.clj:1116`; `src/futon3c/apm/typed_role_submission.clj:355`). Existing
contracts neither require nor admit an authoritative “considered these memories”
list there. The proposed optional observation vector belongs in separately
declared typed evidence, and the apparatus resolves receipt links from job
records. Require a specific served result or pinned exposure reference, an
attributed consideration statement, and an evidence pointer to the actual
comparison/use artifact when claiming more. A zero-result search can explain a
miss but cannot establish consideration of a particular unseen memory.

Today we can prove recorded supply plus authenticated claims. We cannot prove
internal attention, comprehension, actual execution from exposure alone, or
successful transfer merely from used-ids. Do not backfill fake consideration
from all shelf members. Historical H observations may use source/review artifacts
without a Student receipt, explicitly marked historical-source context; they
must not be presented as Student search/use observations.

## 4. Storage choice and indexing consequences

| Option | Authority/history | Indexing and cost | Decision |
|---|---|---|---|
| Separate reflection evidence entries, keyed by memory + revision | Existing EvidenceEntry envelope supports attributed arbitrary body and memory subject; duplicate-id refusal and read-back exist (`src/futon3c/social/shapes.clj:318`; `futon1b/futon1b_evidence.clj:166`). New semantic admission/review/lifecycle fold still needed. | All bodies indexed today, including unreviewed logs. Explicit typed caption branch and original-memory join required. Raw observations must not become ordinary recall seeds; bounded prefilter/typed query and dedupe are needed so logs/stale revisions cannot monopolize the overfetch cap. | **Recommend**, for both durable captions and log. Preserve evidence authority and keep metadata distinct from mathematical assertion. |
| futon1b entities with properties | `futon1b/futon1b_graph.clj:313` builds name/type/props; line 335 does verified entity put. That is not the evidence append/review contract. Version ids and separate approval/history would have to be added. | Current `futon1b/futon1b_text.clj:163` indexes evidence documents, not entity props. Requires a new index/projection plus the same join and access checks. | Loses on duplicated persistence, review and indexing machinery; no reason to create mathematical assertions for this either. |
| Lab EDN/JSON files | Git can pin drafts and independent review, but a file alone is not store publication or role-authorized evidence. Existing snapshot immutable-file writer is precedent, not automatic admission for arbitrary files (`src/futon3c/apm/memory_snapshot.clj:338`). | No ordinary futon1b search discovery. Requires importer, versioned index and explicit join; filesystem presence must never confer publication. | Good H interchange now, weaker permanent runtime authority. Import reviewed drafts through the new apparatus, retaining original file hash. |
| Caption inside original memory body | Changes existing body/id/read-back digests; revises what the independent review actually signed. | Easy text match, but violates commission. | Rejected, even if implemented as an “optional field.” |

Use existing `:reflection` evidence type with **new proposed body event tags**
for caption, applicability observation, review and lifecycle decision. Do not
reuse `:pattern-description` or put captions through candidate pattern admission.
No `:memory/assert` duplication is required by any acceptable option. With the
recommended option, semantic authorization must be implemented above generic
append; generic evidence acceptance is not enough to serve a caption.

The entire log is retained, but only a bounded approved caption projection enters
ordinary role recall. Generic evidence search remains an audit tool over history,
not proof of current admissibility. Merely filtering *after* the existing 3x
FTS overfetch is insufficient if logs crowd out all caption/memory seeds.

## 5. Review authority

Current enforcement is executable, not just a card instruction:
`src/futon3c/apm/promotion_pipeline.clj:194` and line 364 refuse reviewer=depositor;
`src/futon3c/apm/promotion_review_store.clj:154` independently refuses the same attribution;
`src/futon3c/peripheral/memory_lifecycle.clj:124` derives reviewer from persisted
review evidence, compares against original author, and at line 137 checks acting
identity; `src/futon3c/apm/memory_snapshot.clj:331` compares original and review authors again.
`src/futon3c/apm/live_promotion.clj:203` derives expected reviewer rather than trusting a report.

For captions, bind proposal author and contributing observation authors to
registered jobs or independently verified historical import provenance; bind the
reviewer to a different authenticated principal. Proposed policy: the approving
reviewer differs from caption proposer **and all authors of observations it
compresses**. This is stronger than comparing just a compression bot name and
prevents a Student having its own observations rubber-stamped through that bot.
Keep original-memory depositor/reviewer unchanged; caption review cannot approve
or repair the original mathematics. A reviewer may approve a faithfully labelled
suggestion without certifying its application. New independent review needs a
separate exact caption digest, source-set digest and verdict receipt. Existing
memory review is not transferable authorization for later caption versions.

## 6. Compatibility inventory

Within the traced APM producer/search/shelf/use pipeline, these are the affected
consumers. This is not an assertion that all generic clients across all Futons
were audited.

| Consumer | Memory WITH added caption | Memory WITHOUT caption |
|---|---|---|
| Candidate typed submission/required shape | Extra candidate field survives generic required-key validation, but Student memory-use extras reject; new explicit observation schema required. | Existing memory remains valid; do not make historical captions mandatory. |
| evidence-body, memory-entry, edge builder | Drop caption; expanding body selection perturbs identity. Side records avoid this. | Existing behavior unchanged. |
| Candidate-set review and exact read-back | Whole candidate map digest can change; body mutation fails digest/exact comparison. Separate review bundle required. | Keep existing review path. |
| Review normalization/persistence/lifecycle | Enumerated review body does not approve captions. Passing a caption as a memory demands patterns/assert edge and fails genuine identity tests. | Existing independent memory review remains sufficient for the memory. |
| futon1b FTS + recall row selector | Body field is indexed if persisted; reflection caption is dropped by row selector today; pretending it is a memory still fails edge/id equality. New join is essential. | Original FTS remains available, with identical ids and gate checks. |
| Compact recall and pattern-support maps | Explicit compact projection omits caption. Carry caption metadata alongside verified original projection. Do not substitute author/subject or add an authored pattern edge. | No mandatory caption keys in current compact contract. |
| Access gate, repair receipt inheritance | Caption id or caption author would misidentify withheld memory; unqualified original provenance must be verified. Old receipt hashes must remain valid. | No-caption is not an access denial. Keep unknown-provenance refusals. |
| Snapshot ordering/publication/access | Extra candidate keys change hash; candidate text ordering considers name/hook/body (`src/futon3c/apm/memory_snapshot.clj:114`), not new caption fields. Do not mutate or re-rank active snapshots. | Snapshot validation has no caption requirement; preserve exact access set. |
| Student prompt, use/close receipts | Existing original ids must remain stable; raw growing log must not be embedded. New optional separately pinned caption supply and receipt match metadata need explicit consumers. | Empty observation vector is valid; no fabricated use, no mandatory search or observation. |
| H drafts / generic evidence API | New body events are representable but not automatically admissible for recall. H must wait for reviewed importer. Unknown extra envelope fields are not portable. | Explicit missing-evidence/review-pending dispositions are honest outputs. |

A companion record that no consumer reads causes no old parser failure but also
provides no improvement. Disconnected-consumer acceptance tests must detect that
case. Do not count successful append as successful delivery.

## 7. Budget and proposed schema

Recommend **1,024 UTF-8 bytes** maximum for the complete *searchable caption
projection*, including all rendered qualifications, suggested-context labels,
conditions and scope limits. Measure actual encoded output, not source map size,
word count or characters. This is deterministic across clients and handles Lean
identifiers, Unicode and punctuation without a model-tokenizer dependency. It
is a replacement proposal for 120 words, not a mathematically equivalent limit.

FTS indexes full pr-str bodies (`futon1b/futon1b_text.clj:155`) and hydrated search can
return full bodies (`src/futon3c/peripheral/memory_recall.clj:377`); prompts print request maps
(`src/futon3c/apm/live_learning_phases.clj:1099`). Role search caps results at 10, so the caption
text increment is at most 10 KiB before envelope/EDN escaping, not a claim that
the entire packet fits 10 KiB or a fixed token count. Do not put the observation
log into that projection. Suggested additional admission limits: at most three
Student observations per terminal, each canonical body <=8 KiB; <=8 conditions,
<=3 suggested contexts per record. References/review packets need their own
bounded batching (e.g. <=16 source records per compression review); never drop
contradictions just to meet a batch cap. If complete qualifications do not fit,
refuse/revise the projection and retain the old version; no string truncation.

The following are **proposed data fields**, not existing function/API names.
All persisted records use existing EvidenceEntry envelope fields: evidence/id,
subject `{:ref/type :memory :ref/id ORIGINAL}`, type `:reflection`, claim-type
`:observation`, author, at, session-id, tags and body. New event names live in
body. IDs below are schematic prefixes followed by full SHA-256; no literal
example ids are for publication.

### Caption revision body (proposed v1)

```edn
{:schema :apm-memory-caption-v1
 :event :memory-caption
 :memory-id "ORIGINAL"
 :memory-content-digest "SHA256-of-original-body"
 :memory-review-evidence-id "ORIGINAL-REVIEW"
 :revision 1
 :previous-caption-id nil
 :useful-when "Useful when comparing continuous trajectories ..."
 :epistemic-status :supported
 :basis [{:source-ref "IMMUTABLE-REF" :source-digest "SHA256"
          :locator "section/line/receipt field" :claim "what was checked"}]
 :conditions [{:condition-id "continuity"
               :condition "Continuity at the initial endpoint"
               :status :established
               :context-ref "SOURCE-CONTEXT"
               :basis [{:source-ref "REF" :source-digest "SHA256"
                        :locator "proof location" :claim "checked statement"}]}]
 :suggested-contexts []
 :scope-limit "Does not establish existence of solutions."
 :observation-ids ["OBSERVATION-ID"]
 :contradiction-refs []
 :compression {:trigger :review-event :policy-version 1
               :input-caption-id nil :input-observation-ids ["OBSERVATION-ID"]}
 :proposal-context {:kind :historical-source :source-manifest-ref "PINNED-MANIFEST"}}
```

The caption record id is `e-apm-caption-` plus digest of schema, original id/body
digest, revision, previous-caption-id, authored content, exact source references,
authenticated author/job and controller-stamped creation time. Controller stamps
once at intake; retries replay that immutable intake, not a new clock value.
Revision increments from a verified predecessor; parallel drafts may share a
proposed revision but only one may be published against the same expected head.
No last-writer-wins. Initial caption is revision 1 with nil predecessor; absence
is nil, not a fake revision 0 caption. Original memory “revision” is its existing
immutable id plus body digest: do not invent an editable original revision number.

A separate derived display/index projection renders useful-when, each condition,
status qualifiers, suggested contexts and scope limit, with record references
outside reusable prose. Persist its exact text/digest and byte count in a
publication decision, so H drafts cannot smuggle unreviewed extra search text.
Under today's index the entire record body is indexed; a future dedicated caption
projection/filter must ensure source ids and raw observation prose do not become
unbounded ordinary recall vocabulary. Held-out tests must exercise this seam.

### Applicability observation body (proposed v1)

```edn
{:schema :apm-memory-applicability-v1
 :event :memory-applicability-observation
 :memory-id "ORIGINAL"
 :memory-content-digest "SHA256"
 :caption-id nil
 :caption-revision nil
 :context {:kind :student-attempt :campaign-id "CAMPAIGN" :frame-id "FRAME"
           :problem-id "PROBLEM" :task-ref "PINNED-TASK"
           :attempt-job-id "JOB" :attempt-receipt-id "RECEIPT"}
 :exposure {:kind :search :receipt-id "SEARCH-RECEIPT"
            :match-memory-id "ORIGINAL" :caption-id nil :caption-revision nil}
 :useful-when "Useful when continuity is available at the endpoint."
 :epistemic-status :suggested
 :conditions [{:condition-id "continuity" :condition "Endpoint continuity"
               :status :unchecked :context-ref "PINNED-TASK" :basis []}]
 :task-observation {:disposition :considered
                    :reason "I did not establish this prerequisite in this attempt."
                    :evidence-refs ["IMMUTABLE-ATTEMPT-LOCATION"]}
 :basis []
 :suggested-contexts []
 :scope-limit "No claim that continuity is absent or that this solves the task."
 :supersedes-observation-id nil
 :contradiction-refs []}
```

Observation id: `e-apm-applicability-` plus canonical digest of its body and
controller-bound author/job/intake time. It is an append-only event, not an
in-place editable row. Corrections name supersedes-observation-id and preserve
the predecessor and contrary evidence. A future observation after a caption
search must bind the served caption id/revision from the actual receipt, not
whichever revision is current when terminal collection happens.

For shelf/cascade exposure, use tagged variants with snapshot id/digest plus
packet reference/digest, or dispatch/offer reference plus packet reference/digest.
Do not invent a search receipt for those channels. Observation admission verifies
membership and original-memory identity from those sources. A historical-source
variant replaces Student context/exposure with source artifact and independent
memory-review references; no claim of Student consideration follows. Missing
references are explicit H draft dispositions and cannot be approved as supported.
If terminal receipt identity is not yet minted, intake may bind the immutable
job/submission/exposure first, then append a linking event to the eventual
terminal receipt; do not mutate the earlier observation or create a digest cycle.

### Status semantics and lifecycle records

`epistemic-status` is exactly supported/suggested/unknown. Supported requires
specific checked basis; suggested preserves proposed application status even
through repeated compression; unknown states missing evidence explicitly.
Each suggested-context has its own text, status and basis. Conditions are always
qualified by context: observed = reported observation, established = checked
artifact/derivation, absent = explicit evidence of absence, unchecked = not
established either way. Observed success does not turn a condition into a
necessary or sufficient theorem. A caption may aggregate multiple contextual
condition reports without erasing disagreement; it must not universalize one
attempt's absent prerequisite. `task-observation.disposition` is used/considered/
not-used/unresolved, independent of epistemic status.

Proposed review/lifecycle reflection bodies carry schema/event, exact target
caption id/digest, source-set digest, reviewer job, verdict (approve/reject/
cannot-judge), reason, qualifications, previous publication/head, and evidence
references. Publication carries derived projection text/digest/byte count and
activation boundary. Retraction/challenge/supersession are new events with target,
reason, authority and predecessor, not overwrites. Derive current status as
review-pending/approved/superseded/contested/retracted. Pending and failed reviews
are not searchable captions; contested/retracted revisions cannot serve as
current supported text. Default proposal: withhold contested captions pending
review, while original memory recall still follows its own independent authority.
A successor approval must explicitly account for contradictory observations;
absence of approval cannot fall back silently to a retracted caption.

### Fields Job H authors versus fields the apparatus derives

H drafts MUST populate: draft schema; original memory id; pinned original body
and review references/digests (or explicit missing-evidence disposition); proposed
useful-when, epistemic status, individually contextualized conditions, basis and
scope-limit; suggested-contexts (empty allowed); source problem/artifact/locator;
contradiction refs (empty only if no conflict found); predecessor caption reference
if updating. Supply source manifest/draft file reference and claimed author for
import verification. Record coverage disposition separately (drafted/adequate/
missing-evidence/conflict/review-pending); it is not caption approval. Historical
observations populate historical-source context, never invented Student receipt
ids. A draft without source evidence may be retained as unknown, not supported.

Apparatus MUST derive/verify: authoritative original id/body digest/review and
current admissibility; author/job/time; accepted source hashes; evidence ids;
revision/predecessor consistency; receipt ownership and exposed-memory membership;
actual served caption revision; review authority and verdict identity; exact
rendered caption and byte count; publication/current-head decision and activation
boundary. H may suggest these values but cannot authorize them. H does not assign
searchable status, forge materialization receipts, or publish to the store directly.

## Slices: one behavior per handoff, with named acceptance cases

Names below are proposed acceptance-case labels, not claims that tests exist.
Each implementation slice requires namespace tests, clj-kondo and check-parens;
futon1b changes additionally follow its ownership/instructions. Activate only at
an agreed future-frame boundary. No slice author self-reviews its publication.

| Slice / single behavior | Named acceptance cases |
|---|---|
| A1 Admit one structured observation | `positive-condition-absent-vs-unchecked`, `supported-requires-checked-basis`, `unknown-source-stays-unknown`, `provenance-complete`, `historical-is-not-student-use`. Cover commission positive wording and source/provenance controls. |
| A2 Bind an observation to actual controller supply | `search-job-and-memory-membership`, `pattern-id-is-not-memory`, `zero-result-cannot-witness-consideration`, `shelf-and-cascade-exposure-not-use`, `repair-receipt-regated`, `unseen-observation-refused`. |
| A3 Persist one immutable observation with read-back | `exact-idempotent-replay`, `conflicting-id-refused`, `readback-mismatch-refused`, `correction-retains-predecessor`, `terminal-link-no-digest-cycle`. |
| A4 Persist one caption revision without changing memory | `original-body-id-review-snapshot-unchanged`, `caption-revision-readback`, `missing-parent-refused`, `concurrent-predecessor-conflict`. |
| A5 Admit an independent caption review | `author-cannot-approve`, `observation-author-cannot-launder-through-compressor`, `wrong-target-digest-refused`, `suggestion-approved-as-suggestion`, `unreviewed-not-current`. |
| A6 Derive current caption from lifecycle decisions | `supersession-retains-history`, `retraction-not-current`, `contradiction-withheld-until-reviewed`, `ambiguous-head-refused`. |
| A7 Resolve a caption lexical hit to its original memory | `caption-original-id-join`, `parent-digest-mismatch-refused`, `domain-visibility-review-preserved`, `same-problem-depositor-holdout`, `pattern-support-holdout`, `ordinary-search-provenance-fixture`, `no-caption-original-path`. Test current source provenance mismatch before claiming parity. |
| A8 Serve only bounded approved caption search projection | `utf8-byte-limit-boundary`, `over-budget-refused-not-truncated`, `log-and-stale-revision-crowding-control`, `relevant-task-query`, `negative-query-controls`, `suggested-hit-labelled`. Use actual primary AND and fallback OR behavior and both captioned/original arms on same population. |
| A9 Record exact caption consumption in retrieval receipt | `served-revision-retained-after-supersession`, `receipt-original-result-ids`, `old-receipt-hash-compatible`, `disconnected-consumer-fails`. Append/read-back alone must fail this slice's success criterion. |
| A10 Accept optional forward-role proposals | `scribe-draft-reaches-sidecar-admission`, `student-zero-observations-valid`, `student-max-three`, `student-cannot-author-authority`, `old-no-caption-completion-compatible`, `future-pin-only`. Changes to generated submission contract and role cards must agree. |
| A11 Produce one bounded compression proposal | `suggestion-never-promoted-by-compression`, `absent-not-inferred-from-unchecked`, `all-source-and-contradiction-links-retained`, `declared-trigger-only`, `oversize-revision-refused`. Output still awaits A5 review, never auto-publishes. |
| A12 Import one independently reviewed H draft | `historical-manifest-source-binding`, `missing-evidence-disposition-retained`, `draft-not-publication`, `import-next-query-and-outcome-link`. Demonstrate approved update -> ordinary query -> exact revision receipt -> attributed downstream observation/outcome. |

This covers every commission item-6 case explicitly: positive condition distinction
A1/A11; suggestion retention A5/A11; provenance A1/A2; author/reviewer A5; history
A3/A4/A6; size A8/A11; original join A7; holdout/domain/visibility A7; unknown source
A1/A12; disconnected consumer A9; absent caption A7/A10; relevant and negative
query controls A8. Historical held-out relevance evaluation remains H's separate
population/query protocol, not the tuned development query promoted to a result.

## Genuine decisions for owners

1. **Reflection side records vs dedicated evidence types.** Recommend existing
   reflection envelope with strictly validated body events. New top-level types
   require cross-repository schema agreement; either way no memory/assert edges.
2. **Dedicated caption index projection vs typed evidence FTS lane.** Recommend
   dedicated bounded projection for predictable vocabulary and exclusion of raw
   logs/provenance. A typed evidence FTS lane is smaller initially but needs a
   demonstrated crowding bound and must account for whole-body provenance hits.
   This is a futon1b ownership decision, not permission to bypass store recheck.
3. **All contributor-author separation vs proposer-only separation.** Recommend
   reviewer distinct from caption proposer and every contributing observation
   author. Proposer-only is cheaper but needs an explicit policy preventing a
   Student from approving its own input through a different compressor identity.
4. **Review-event compression vs threshold-triggered compression.** Recommend
   explicit review-event trigger for first delivery; alternative is a declared
   count of new admissible observations. Neither means rewriting on every query.
5. **Future-frame pinned caption basis vs live-current revision lookup.** Recommend
   pin eligible caption publication basis per future frame, with live retraction/
   admissibility checks still able to refuse it; live-current lookup permits
   mid-frame supply changes and needs separate campaign authorization.
6. **Withhold contested caption vs serve a labelled contested description.**
   Recommend withholding caption enrichment pending independent review; either
   choice must retain original memory authority and explicit contrary evidence.
7. **1,024-byte projection budget vs 120-word budget.** Recommend bytes for exact
   cross-client enforcement; this choice and the three-observation intake cap
   should be agreed with H before draft validation is treated as admission.

No source changes, live API writes, loads or restarts were necessary for this
trace. Implementation, runtime efficacy and the identified provenance behavior
remain unvalidated; the required next evidence is the named bounded acceptance
work, not a claim that this discovery document has shipped captions.
