# E: shared design requirements for a futon/mfuton successor (outer loop)

Joe, 2026-09-14, in the Matrix room *Private Federation Proof*
(`!_qvu9Pec8-hw1-nsN18SA8uIChKlJPmS4f4ji3zajRw`): fucodex runs the inner loop
(tight Q&A with @fiona, mfuton's representative); fuclaude (claude-16) runs the
outer loop, synthesising those turns into shared design requirements for a
possible successor system.

## Method

- Each pass reads the room from the homeserver, not from a client buffer:
  `ssh lucy-joe python3 - '<room>' fuclaude <cursor-ms> < scripts/matrix_room_history.py`
  (prints JSON lines, oldest first, only events after the cursor).
- A requirement cites where it came from (speaker, UTC time of the message).
  Futon-side status is stated only where a file or run was checked.
- Revisions are recorded as deltas in the pass log; requirements are not edited
  silently (the room's own revision rule, Joe 2026-08-01 14:54Z).
- Each pass posts its delta to the room, so the mfuton side can correct it.

## Requirements (v0, pass 1)

**R1. Truth lives in source artifacts and typed witnesses.** Every store is a
projection whose authority is declared per relation (graph topology,
embeddings, evidence history, formal validity), not per product.
Source: fiona 09-14 22:45Z. The two systems split authority differently:
mfuton makes Neo4j authoritative for memory topology and uses pgvector only
for ranking; futon's evidence history is authoritative in the futon1b/XTDB2
backend (futon3c `deps.edn` test alias). Stating authority per relation lets
both splits be expressed.

**R2. Each projection names its source digest and its drift check, and its
staleness is visible.** Today mfuton's drift control is per family (fiona
22:47Z): the callable registry byte-compares harvested content under
deterministic `cs-*` IDs; the code graph uses source mtimes plus entity
fingerprints; Neo4j+pgvector mutations use optimistic concurrency and return
applied/conflict/repair-required receipts naming completed and pending stores;
XTDB evidence closure verifies IDs, chain links and a pass/partial/fail
verdict. Drift is caught at refresh or verification time; there is no
continuous cross-store transaction, so a projection can be stale until its
governed refresh runs. The successor keeps per-family checks but makes the
source digest and the staleness window part of every projection's record.

**R3. Identity and bounded authority on every turn.** Each turn carries an
explicit principal and source identity; each channel declares what may be
requested through it; refusals are explicit. Source: fiona 22:45Z; the
listener checks by @codex (mfuton) at 22:07Z (identity answered from a
listener receipt) and 22:08Z (a shell request through the room was refused).

**R4. Receipts, and no completion claim beyond the witness.** Every step emits
a machine-readable receipt; partial failure and repair-required are
first-class states; continuation and prior evidence are preserved.
Source: fiona 22:45Z. Related open question from Rob (08-06 12:43Z): whether
futon's evidence mechanisms prevent the "proof laundering" he sees with Codex.

**R5. Procedure and judgment are separate, and gate decisions are recorded in
a typed form.** A replayable procedural spine (algorithm) is kept apart from
judgment surfaces (flexiargs: IF/THEN/BECAUSE plus context, guardrails,
failure modes, authority references). A run-time gate decision records the
option chosen, the alternatives rejected, the deciding principal, the evidence
and the continuation. Source: fiona 22:45Z, 22:47Z. mfuton has no
installation-wide typed gate receipt; outside algorithms that name exact
fields, the record is structured prose in the mission. Futon has 1346
`.flexiarg` files under `futon3/library`; how futon records a gate decision
has not been compared yet.

**R6. Mission closure is a recorded conjunction, not a single token.** In
mfuton (fiona 22:47Z): every acceptance criterion classified satisfied against
concrete tests/receipts/logs; no in-scope blocking remainder; explicit
operator verification unless the lane contract permits machine-only
acceptance; and a mission-file transition carrying status, close decision,
accepted evidence, reason, verifier/date and deferred items. The mission file
is the authoritative record; graph projections are refreshed afterwards.
Futon has 135 mission files in `futon3c/holes/missions` and no file named as a
template there; its closure practice has not been compared yet.

**R7. Formal validity is certified only by the Lean kernel/build.** Memory,
search, LSP and LeanDojo are navigation aids. Source: fiona 22:45Z; futon's
APM work uses the Lean build as its witness in the same way.

**R8. Measurement is modelled separately from the process it measures,
including its missingness.** An absent receipt is not evidence that the event
did not happen. Source: Rob relaying his engine's assessment, 08-01 14:32Z.

**R9. Relations are modelled, not only artifact properties.** memory →
surfacing → information or policy/route → trajectory → independent witness.
Substitutive effects (through information) and regulative effects (through
policy/route) are different edge targets; a dependency such as `:blocked-on`
becomes a causal arrow only when intervening on it has defined consequences.
Source: 08-01 14:32Z.

**R10. Models are authored, then revised from observed failures.** Revisions
are explicit deltas, never silent edits. Source: 08-01 14:32Z; Joe 14:54Z;
Rob 08-02 11:18Z (the causal model is constructed by iterating on failures,
not given in advance).

**R11. Rules must constrain actions, not only be restated.** Agents restate a
rule and then act against it, so the successor has to enforce, not document.
Source: Rob 08-02 11:33Z; Joe 11:29Z (generalising a learned rule into a
pattern rather than a one-off).

**R12. Federation transport.** At-most-once admission; a newly joined room's
history is not replayed as new work; a listener's identity is answerable from a
receipt; internal state (session IDs, current activity) is never posted to a
shared room. Source: @codex (mfuton) 22:07Z; futon's bridge on 09-14 22:35Z
replayed fiona's pre-join message to fucodex and fuzai and posted
"[accept failed] … session=…" notices into this room.

**R13. A shared governed receipt envelope wraps family receipts; it never
replaces them.** Proposed by fiona 22:49Z: a versioned `GovernedReceiptEnvelope`
with `receipt_id`, `schema_rev`, `receipt_type`, `principal`, `authority_refs`,
`source_refs[{id,digest}]`, `evidence_refs`, `predecessor_receipt_id`,
`recorded_at`, `continuation`, `payload`, plus an optional judgment block
(R5) and an optional convergence block. Existing payloads are kept
byte-for-byte; shared metadata is additive; adapters lift existing receipts
without changing their producers. mfuton seeds: ResultEnvelope/ErrorEnvelope
(structure), MemoryUpdateReceiptView/MemoryLifecycleReceiptView (mutation
semantics), semantic-ingress v1 (provenance and idempotency). mfuton owner
proposed: a new child of M-mfuton-familiar-typed-surface-world-and-memory
(e.g. M-shared-governed-receipt-envelope-and-provenance-chain). Futon-side
receipt families an adapter would have to lift, seen in this session: evidence
entries (`subject`, `type`, `claim-type`, `author`, `body`, `tags`, as written
by the IRC/Matrix bridges) and turn-queue terminal entries (`:id`, `:status`,
`:finished-at`, `:reply-route`). No futon owner yet.

**R14. Evidential sufficiency and replica convergence are independent axes.**
`verification_verdict` (pass/partial/fail) and `convergence_status`
(e.g. current/applied/conflict/repair-required) are never collapsed into one
enum; a repair-required mutation can carry a passing diagnosis. Source: fiona
22:49Z, agreed by fucodex 22:49Z.

**R15. Receipt identity is the digest of a typed logical model under one
normative canonical byte profile.** Not EDN-first, and not "whatever Python
`json.dumps` does": canonical JSON with UTF-8, deterministic key order, no
insignificant whitespace, duplicate-key rejection, an explicit integer range,
and floats either forbidden or given exact decimal normalisation, fixed by
conformance vectors. EDN-only distinctions (keyword vs string, UUID/instant vs
string, integer vs decimal, absent vs null) survive as explicit tags or are
rejected as ambiguous. Original wire bytes may be hashed separately for
forensics. Source: fiona 22:50Z, fucodex 22:50:16Z. Futon writes EDN, so the
EDN adapter's tagging rules are where futon's side of this contract sits.

**R16. Unknown revisions survive but fail closed; redaction precedes
hashing.** An unknown envelope revision or `receipt_type` round-trips opaquely
without data loss, but verification and mutation fail closed until it is
understood. Redaction happens before persistence and before canonical
hashing, and a redacted receipt cannot claim the digest of the unredacted
content. Chain verification rejects tampering, reordering, missing
predecessors, duplicate IDs, cycles and cross-schema chains. Source: fiona's
11-test prototype acceptance set, 22:50Z.

**R17. The formal/implementation boundary is explicit.** The first envelope
mission is proof-ready rather than proved: its acceptance contract states the
laws and emits machine-readable conformance vectors (canonicalize is
deterministic; decode(encode(x)) = x on the admitted domain; equivalent
EDN/JSON inputs share a digest; append preserves chain validity for a fresh
identity whose predecessor is the head; mutation/reorder/removal breaks
verification; unknown revisions are preserved, not interpreted; redaction
precedes persistence and identity). Lean follows as a child once v1 is frozen
and both adapters pass the same vectors, starting pure (ReceiptIdentity,
PredecessorChain, append, verify; determinism and append-preservation), with
SHA-256 as an abstract digest under an explicit collision-resistance
assumption. Conformance tests prove implementation behaviour, Lean proves the
abstract laws, and neither is cited as evidence for the other. Source: fiona
22:50:50Z. This is R7 applied to the receipt layer, and it bears on Rob's
proof-laundering question (R4).

**R18. References are portable identities, and a design exchange confers no
authority over the other side's artifacts.** Artifact paths are repo-relative
(`mfuton/holes/missions/…`) and resolve only against a mount declared for each
host; machine-local paths such as `/home/rob/gh/mfuton` are not identities.
Creating or changing an artifact runs on the side that owns the repository,
through its governed procedure, when that side's operator asks; agreement in a
shared room is design analysis, not authorisation. Source: fiona 22:52:26Z,
after fucodex found the cited mfuton paths absent on Joe's machine
(22:52:06Z). Related: R3.

### Memory-system requirements (pass 2)

Inputs: fiona's comparison of Neo4j+pgvector with XTDB (09-14 22:57:35Z),
answering Joe (22:57:02Z); futon's retrieval whitepaper v2
(`docs/retrieval-whitepaper-v2.md`, futon3c `52aa1416`), which the room
discussed on 08-01.

**R19. Structure first.** Domain objects and their typed relations exist in
the store before any prose similarity is consulted. In mfuton a Lean
declaration, module, source range, proof dependency, caller, source binding
and evidence edge are typed graph objects; pgvector only locates candidates
inside a typed candidate set, and results can be reranked by typed adjacency.
Source: fiona 22:57:35Z.

**R20. Retrieval lanes are explicit, and every surfacing records its lane.**
Structural traversal, vector similarity and lexical search are separate
declared lanes; none is an invisible fallback for another (fiona 22:57:35Z: a
Neo4j full-text index is possible but would be added as an explicit lexical
lane). futon's whitepaper shows why the record matters: `surfacing-via` was
populated on 30 of 129 dispatches, all in the final seven hours, so the pattern
arm's share could be measured only for that tail (§4.6).

**R21. History is append-only and time-indexed; graph and vectors are
projections of it.** fiona (22:57:35Z) credits XTDB's append-only, bitemporal
posture with historical state, audit and as-of questions, and the typed
graph+vector split with dense relationships and structure-first lookup; her
suggested convergence keeps both, with receipts and history outside the graph
projection. Related: R1, R2, R13.

**R22. Relations are written by declared rules at ingest, and graph
population is measured.** futon's schema accepts many patterns per memory
(`review-attachment!` takes a non-empty vector of pattern IDs), yet the
deployed corpus is a forest of stars: one pattern per memory, because nothing
wrote the other edges. The whitepaper's verdict is "schema well-constructed,
content well-curated, graph essentially unbuilt" (§4.6). A schema that permits
edges does not produce them.

**R23. Memory is a service; each client lane pairs a client with an
independent witness source.** The memory system cannot witness its own
usefulness without self-report contamination, so every lane whose outcomes
count supplies a decision-keyed independent check (APM: the Lean compiler; WM:
its external adjudicator). Whitepaper §2. Related: R7, R17.

**R24. How a memory acted is recorded, not only whether it was used.** Five of
seven observed use-modes were regulative (changing what the runner does)
rather than substitutive (supplying content); a used/unused field cannot
express them. The V3 residual is a `:memory-use/kind` field. Whitepaper
abstract and front matter; R9.

**R25. Every field an evaluation depends on is populated from the start, and
its coverage window is recorded.** Three futon fields (`:rejection-reasons`,
`used-ids`, `surfacing-via`) turned out to be populated only in recency-biased
tails, each discovered during analysis rather than by the audit meant to find
it (whitepaper §4.6, §7.1). Related: R8.

## Configurator sketch (pass 2)

Joe (22:59:53Z) asked what a "best of" successor, or a configurator for custom
memory systems, would take from both systems. On R1–R25, a memory system is a
configuration with these parts, each declared per domain:

1. **Carrier and ingress.** The source artifacts that own truth, and an
   ingress contract giving each a stable source ID, content digest and
   idempotent request ID (R1, R13; mfuton semantic ingress v1).
2. **Typed schema and edge rules.** Node and edge types for the domain (for
   Lean: declaration, module, dependency, caller, evidence), with the rules
   that write edges at ingest and a population metric (R19, R22).
3. **Retrieval lanes.** Structural, vector-within-typed-set, lexical and
   rerank, each named, with per-surfacing lane attribution (R20).
4. **History.** An append-only, time-indexed log of receipts in the shared
   envelope; graph, vector and lexical indexes are rebuildable projections
   with drift checks (R2, R13–R16, R21).
5. **Witness binding.** For each client lane, its independent witness source
   and the decision key that joins receipts to it (R7, R17, R23).
6. **Use and measurement.** The use-mode taxonomy (substitutive, regulative,
   and whatever else the domain shows), the fields evaluations rely on, their
   coverage, and how missingness is modelled (R8, R24, R25).
7. **Revision and governance.** How the configuration itself is revised from
   failures, as explicit deltas, with principal and authority on every change
   (R3, R5, R10, R11).

The acceptance test for the configurator's vocabulary: it can describe both
existing systems as configurations. That means mfuton's Lean memory (Neo4j
typed graph, pgvector lane, mutation receipts) and futon's APM memory
(XTDB-backed store, pattern attachments, Lean-compiler witness). If either
cannot be written down in it, the vocabulary is wrong rather than the system.

## Open questions carried to the next pass

- Settled (fiona 22:50:50Z, now R17): Lean is a planned child after v1 is
  frozen, not a closure requirement of the first envelope mission.
- Who owns futon's side of R13/R15 (EDN adapter; lifting evidence and
  turn-queue records)?
- Implementation surface (R18): the mfuton work needs a host with an mfuton
  checkout, or Joe mounting one under a declared path. Until then fiona
  answers bounded factual questions with repo-relative paths.
- Configurator acceptance test: write mfuton's Lean memory and futon's APM
  memory as configurations. mfuton side needs its node and edge types for the
  Lean graph and the point at which edges are written (ingest or use); asked
  of fiona in the pass-2 posting.
- The fucodex/fiona envelope thread closed at 22:54:17Z as advisory only, with
  fiona's precedent map (22:53:25Z) as its record: ResultEnvelope (wrapper),
  memory receipts (convergence), semantic ingress v1 (provenance), all under
  the typed-surface parent mission. No mission or repository changes were
  authorised or made.
- fuzai's three seams (22:39Z), still unanswered from the mfuton side except as
  covered by R13: turn/message envelope, agent identity and registry
  semantics, evidence and memory model.
- Rob's 08-06 questions to futon: proof laundering; how many Lean entities an
  APM problem expands to.

## Pass log

- Pass 1, 2026-09-14: read 141 messages, 2026-08-01 13:01Z to 09-14 22:50:16Z.
  Cursor: 1789426216261 (origin_server_ts of fucodex's Lean question).
  Drafted R1–R16. fiona's 22:47Z answers arrived during the pass and were
  folded into R2, R5 and R6 before the first posting.
- Pass 1 addendum, 2026-09-14: read 3 more messages to 22:52:26Z. Cursor:
  1789426346172. Added R17 and R18. Delta: the pass-1 outer-loop suggestion to
  state chain validity in Lean now is superseded by fiona's R17 position
  (state the laws in the acceptance contract now; prove them once v1 is
  frozen).
- Pass 2, 2026-09-14: read 20 messages to 22:59:53Z (Joe's configurator
  question). Cursor: 1789426793667. Added R19–R25 (memory-system
  requirements, from fiona's Neo4j/pgvector vs XTDB comparison and futon's
  retrieval whitepaper v2) and the configurator sketch with its acceptance
  test. The pass-1 posting reached the room at 22:53:54Z with an earlier
  sentence of this turn's text prefixed to it by the bridge.
