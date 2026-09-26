# CODEX-HANDOFF — P0 `memory_record`: a Zai runner can write a memory

Mission: `holes/missions/M-typed-memories.md` (DERIVE, prototype ladder
rung P0). Prepared 2026-07-22 by the Claude owner. **Delivery: this file is
passed directly by Joe (no bell); report completion with a summary + commit
SHAs in your completion message; the Claude owner reviews the diff after the
code lands.**

Phased follow-on: `holes/missions/M-shared-memory-control-build-test.md`.
P0 remains this packet's scope. Shared-regime Phases 1–3 were accepted live
on 2026-07-23; P0 records now flow through pattern-conditioned search/select,
use receipts, separately witnessed outcomes, bitemporal correction, and
bounded repeated trials without changing this packet's historical write
contract.

## Goal

Give zai agents one new tool, `memory_record` (act fixed = assert), that
writes a deliberate memory as **one evidence entry + one `:memory/assert`
hyperedge**, per the payload contract in the fixture file. No read-side
changes: existing `memory_search` must already retrieve what P0 writes by
explicit filter or text query. This is **addressability**, not yet operational
recall; the latter is the P1 debt described below.

## AIF placement and the P1 retrieval seam (2026-07-23 amendment)

P0 records an episode as a typed observation (R2), with provenance that keeps
the agent's assertion distinct from an independently witnessed result (R9).
It does **not** make the episode a pattern, prove it correct, or ensure that a
later solver will ask the right free-text query. A memory is concrete evidence;
a pattern is a reusable hypothesis/policy component. The relation between them
must remain explicit and revisable.

The intended next read path is pattern-conditioned, with an explicit mint
branch when the catalogue has no adequate pattern:

`task observation -> R6 candidate patterns -> attached memories -> select a
pattern with its warrants/counterexamples -> act -> observe an external result`

`no adequate candidate -> policy-hole + unmatched memories -> mint candidate
pattern -> admit at an unearned prior -> trial -> externally witnessed outcome`

Consequently, P1 should enrich the bounded R6 candidate-pattern construction
(`psr_search` / `psr_select`) with memory evidence attached by graph edges. For
each candidate it should expose compact supporting and challenging memory ids,
their provenance/volatility, and an inclusion reason. Selecting and applying a
candidate must cite the memory ids in the decision record. The independently
observed outcome then feeds R3/R7 (belief and precision update) and R16
(grounding); an agent's own memory write or citation never self-certifies the
memory. At the slower timescale (R15), the Librarian can revise attachments and
use accumulated episodes to propose pattern splits/merges for R17 structure
learning. Embedding or FTS similarity may generate attachment candidates, but
does not establish them.

Minting is model expansion, not something Bayesian Model Reduction performs.
The minted object must therefore begin as `candidate`, carrying its triggering
policy-hole, supporting and challenging memory ids, nearest existing patterns,
and a predicted context/outcome that could prove the distinction useful. It may
enter the R6 set with a broad/unearned prior, but similarity and memory count
must not promote it. Witnessed trials earn precision; the slower R17 pass may
then retain it, revise it, merge it with an existing pattern, or prune it.

This amendment does not broaden the P0 implementation packet. In particular,
do not add PSR changes to this rung. It records the semantic boundary so the
minimal write does not get mistaken for a completed memory loop.

## Shared-substrate direction (2026-07-23 amendment)

P0 is also the first writer for a domain-general memory substrate, not a
theorem-proving-only store. Zaif runners and the War Machine should eventually
use this same typed, bitemporal hypergraph contract and the same endpoint
retrieval, provenance, retraction, and memory-use receipts. Domain, subject,
mission/session, pattern, author, evidence, and witness endpoints preserve
evidential boundaries; a mathematics episode cannot certify a WM-specific
outcome merely because both inhabit one backend.

This gives the shared infrastructure a cost-ordered test route: pure projection
tests, frequent mathematics runs, dark WM decisions, then live WM clicks. The
mathematics tier can cheaply test memory realization, durable writes,
pattern-conditioned retrieval, citation/use, cascade composition, correction,
and supersession. The WM tier remains responsible for validating its own
mission preferences and transition claims. This direction does not broaden P0:
the payload fixtures and output packet below remain authoritative.

## Read first (`:in`, read-only)

- `holes/labs/M-typed-memories/p0-fixtures.edn` — **the payload contract,
  all 14 fixtures, and shape findings F1–F5. This is the spec.** The
  store-side expansion for fixture 1 shows exactly what lands.
- `holes/missions/M-typed-memories.md` — §MAP Stages 2–4 (context), §Prototype
  ladder P0 (invariants I-M1..5, acceptance).
- `src/futon3c/agents/zai_api.clj` — tool-specs vector, the dispatch `case`
  in the invoke loop, `memory-family-tool-names` (line ~222), `persist-round!`
  (~684). Follow the `psr_select` precedent for spec + dispatch wiring
  (~155, ~482).
- `src/futon3c/peripheral/adapter.clj` (~61, ~187) and
  `src/futon3c/peripheral/tools.clj` — the execute-tool path psr_select uses.
- `src/futon3c/evidence/store.clj` — `append!` (~112) / `append*` for the
  entry write; note it validates against `shapes/EvidenceEntry` and returns
  SocialError maps as data.
- `src/futon3c/evidence/boundary.clj` — current boundary discipline.
- `futon1b/API-CONTRACT.md` — `POST /api/alpha/evidence` (§149) and
  `POST /api/alpha/hyperedge` (§268: `compat-upsert-hyperedge`, penholder
  gate, L0 verify-materialized, stable id from sorted endpoints). **The
  store JVM is on 127.0.0.1:7073** (the `:7074` default in
  `futon1b_backend.clj` is stale — do not "fix" the port by trusting the
  default; use the configured backend URL / env).

## Out (create/modify)

- NEW `src/futon3c/peripheral/memory_write.clj` — the write logic:
  - `(defn record-memory! [ctx payload] ...)` → `{:ok true :id "e-…"
    :hx-id "hx-…"}` or `{:ok false :error <SocialError-shaped map>}`.
  - `ctx` carries `{:agent-id :session-id :turn-id :round :mission-id
    :evidence-store}` — **identity comes from ctx, never from payload**
    (I-M2).
  - Mint the evidence id FIRST, before any write attempt, and include it
    in the return even on failure — the id-fixed-at-creation property is
    what lets P1+ add outbox durability without changing this contract.
  - Write order: evidence entry via the evidence store backend, then the
    hyperedge via `POST /api/alpha/hyperedge`. If the hyperedge write
    fails after the entry landed, return `:ok true` with
    `:hx-error <error>` — the entry is the memory's body of record; a
    missing edge is repairable, a lost body is not. Log it loudly.
- MODIFY `src/futon3c/agents/zai_api.clj`:
  - tool spec `memory_record` (JSON-schema per the payload contract:
    `name`, `hook`, `kind`, `body` required; `why`, `how_to_apply`,
    `subjects`, `distills`, `facets`, `volatile` optional).
  - dispatch case → the adapter/tools path (psr_select precedent).
  - **add `"memory_record"` to `memory-family-tool-names`** so §8.4
    `:files`/`:none` ablation modes strip it (I-M3).
  - description text must include the F2 kind rule verbatim:
    "derived-from-a-failure-with-a-why → feedback; documented
    contract/scope fact → reference".
- MODIFY `src/futon3c/peripheral/adapter.clj` + `tools.clj` — register
  `:memory-record` alongside `:psr-select` etc.
- Type registration: ensure `:memory/assert` is registered via the
  futon1b type registry (`register-types!`) — one-time; `:hx/type` is NOT
  auto-registered (deliberate futon1b behavior, see futon1b_graph.clj
  header). If registration needs a server-side change, do it as a data
  call, not a code change, and record how in your completion message.
- NEW `test/futon3c/peripheral/memory_write_test.clj` — fixture-driven:
  read `holes/labs/M-typed-memories/p0-fixtures.edn`, run **all 14**
  through `record-memory!` against a stub/in-memory evidence backend,
  plus targeted tests below.

## Design notes you must honor (from fixtures F1–F5)

- **F1**: payload `:subjects` is a vector; FIRST becomes
  `:evidence/subject`; ALL become hyperedge endpoints under
  `:hx/props {:roles {:subjects [...]}}`.
- **F3 — `"@current-round"` resolution**: the current round's
  `:turn-round` evidence entry does NOT exist yet when the tool runs
  (persist-round! writes after the round completes). Do NOT try to
  resolve it to an evidence id. Resolve the sentinel to a reference map
  `{:turn-id <turn-id> :round <round>}` stored in
  `:hx/props {:roles {:distills [...]}}` — turn-round entries carry
  turn-id + round in their bodies, so the join is queryable later.
  Explicit evidence ids (strings starting `e-`) pass through unchanged.
- **F4**: `:volatile?` lands in `:hx/props` (default false). No P0
  behavior depends on it; it must round-trip.
- Tags on the entry: `[:memory :memory/assert]` (plus nothing else in P0).
- `:evidence/type :memory`, `:evidence/claim-type :assert`.

## Test expectations (I-M1..5 as tests)

1. **I-M5 first-try shape acceptance**: all 14 fixtures accepted
   unmodified — no fixture edited to make a test pass. If a fixture
   cannot be expressed, STOP and report; the shape is wrong, and shape
   changes are the Claude owner's call, not an implementation detail.
2. **I-M1**: each write read back by id after append (entry present,
   hyperedge present or `:hx-error` surfaced); return includes the ids.
3. **I-M2**: a payload smuggling `:author`/`:session-id`/`:evidence/author`
   is accepted but the smuggled values are IGNORED — stamped from ctx.
4. **I-M3**: `specs-for-mode :files` and `:none` exclude `memory_record`;
   `:full` includes it.
5. **I-M4**: log elapsed-ms for the two writes (entry, hyperedge) at the
   call site; no synchronous retry loops on the dispatch path (one
   attempt each; failures return as data).
6. `memory_search` (existing, unmodified) retrieves a P0-written entry by
   `type=memory` filter and by FTS text — test against the stub backend
   for the filter; note in your completion message whether you could
   verify FTS live (it may require the D1 sidecar running; do not block
   on it).
7. Duplicate `:name` in a second write does NOT error (P0 has no dedup —
   curation is P1); both land with distinct ids.

## Gates (required before completion)

- `clj-kondo --fail-level error` clean on changed files.
- `futon4/dev/check-parens.el` clean on changed files.
- Focused test run: the new test namespace + the existing zai_api and
  adapter/tools test namespaces — all pass. Do not run the full suite if
  it spins up heavy XTDB fixtures unrelated to this change; say what you
  ran.
- No JVM restarts; if you need live verification in the serving JVM, use
  Drawbridge reload per README-drawbridge.md — and per the reload
  discipline, reload only namespaces you edited.

## Explicitly OUT of P0 (do not build)

Curation probe / dedup, boot projection, retract/supersede, facet
entities, standing queries, outbox durability, any change to
`memory_search`, any change to PSR/PUR/PAR (a rebuild of those on this
substrate is noted for P2+ — not now).

## Completion message must include

Summary; commit SHAs; the elapsed-ms observations from I-M4; any place
the fixture contract was ambiguous (report, don't improvise); how
`:memory/assert` type registration was done; what test scope ran.
