# GCC × Giraffe store — read-only design review

kimi-1 for claude-13 and Joe, 2026-11-12. M-repl-circus §DERIVE ("rather than
just dump-and-load … use the material to help us finish our design").
Read-only: nothing was written to the timer, queue, corpus or run
directories; no process was signalled; futon1b was not probed for this
review (the :7073/:7094 facts below are cited from claude-6's inventory and
my GCC-xtql note of the same day). Sources: claude-6's
`GIRAFFE-store-2026-11-12.md` (`8ff0c1925`), E-gcc-schema (futon4, Parts
A–B + §16), M-repl-circus §DERIVE, `GCC-use-cases-2026-11-12.md`
(`c93c00d84`) and `GCC-xtql-2026-11-12.md`, and the code in
`futon3c/src/futon3c/giraffe/` as of today. Anything not verified is marked
**unconfirmed**.

## 1. Mapping: each kind of thing in the store → E-gcc-schema type, or a gap

Schema references are (section, name). "—" = the schema lacks it; proposed
wording for Joe is in §1b.

| Thing in the Giraffe store | Shape today | E-gcc-schema type | Verdict |
|---|---|---|---|
| An item and its state group | `{:id "M-…/E-…/T-…" :state-group (:backlog\|:unstarted\|:started\|:completed\|:cancelled)}` (pass.clj docstring; census from `holes/**/*.md` in 14 repos) | (B3, Pending item) `[:workplan/item 1]`; state group is *attestation content* on the item, as B8 already does for entity `status`/`live`/`tier` (futon3c `dfbde7e95`) | **Fits**, with one gap: B8's attestation vocabulary is the lexicon's (exists/partly/…), not Giraffe's five state groups. Wording needed (§1b i). |
| A reading and its source digest | `batch-*.jsonl` line: `{id, source{path,sha256}, status_as_read, purpose, edges[]}`, plus checker result and record pin (`readings.clj:29-48`); 645 records | —. B3 has no reading node; the nearest devices are the B1 external reference (artifact revision: path + sha256 + digest) and the B1 attestation (basis `model`, producer = reader seat) | **Missing.** A reading is an agent judgement pinned to a document revision — data of record, not derivable. Proposed `[:giraffe/reading 1]` (§1b ii). |
| A `needs` edge, class closure/artifact/alternative, source recorded/author-stated/prose-reading | pass edge `{:from :to :kind (:item\|:external) :class :source :quote? :file? :line?}`, converted from a checked reading by `readings.clj:51 convert-edge` | (B4a) `[:needs 1]`, roles `:needing`/`:supplier`, content `class` — declared with **no writer**; "met is computed, never stored" already matches pass.clj | **Fits.** The edge's *source* maps onto attestation basis: recorded→`exact` (checker-verified quote), author-stated→`parser` (declared in the document's header), prose-reading→`model`. Two gaps: external targets (`:kind :external`, e.g. "operator decision") have no endpoint kind in `[:needs 1]` (**unconfirmed** whether the type admits an external-reference endpoint — likely yes via B1, worth stating); the `part-of`/`relates-to` links readings also carry are *not* needs and land in B4(b)'s reduced vocabulary (`part-of`, `related`) as `[:workplan/referent 1]`-style candidates instead. |
| A re-read request and its states | SQLite `reread_requests(target_id, source_sha, state ∈ pending/claimed/landed/failed/superseded, claimed_by, lease_until_ms, superseded_reason, …)` (`reread_queue.clj:44-192`); 593 rows | —. Closest: (B7) a want with a horizon — "a reading of (item, sha D) exists eventually" — with the queue states as observation status (`not-run`/`failed` from B1's receipt vocabulary) | **Missing, and a design choice for Joe** (§1b iii): either a `[:giraffe/reread-request 1]` record, or the first concrete instance of the B7 want record. Note the queue conflates "withdrawn by operator" with "failed" (claude-6 §4; 145 rows), which rule 7 (absence is typed) says must be split before either modelling works. |
| A pass row with rank and readiness | `pass.edn` per run directory; readiness, cones, cycles, `:order` all computed (pass.clj: "computed, never written back into the input") | —. By rule 3 (a link a rule can compute is kept as the rule) pass rows should **not** be stored records at all; what is stored is one receipt, exactly as §16 item 5 does for cascades with `[:cascade/reading 1]` | **Missing as a receipt only.** Proposed `[:giraffe/pass 1]` (§1b iv): content = input-sha256, as-of, counts; readiness/rank recomputed on read. The 188 historical pass.edn files are recomputable output, not records. |
| A priority row with band | `priority.edn` rows `{:target :pass-rank :relation :priority-band (:focus\|:associated\|:elsewhere\|:unknown) :cutoff-status}` (priority.clj) | —. Same rule-3 verdict: a sort of the pass through a focus relation; store the receipt, recompute the rows. The **focus relation itself** is 猴's output and has no type: B9 lists 猴 as **[open]**, and M-repl-circus reserves it for schema 1.1 on purpose | **Missing** (deliberately, per Joe's extensibility point). Proposed `[:focus/relation 1]` (§1b v). |
| A work cluster | `work-clusters.json` per run: `{id "work-cluster/<digest>", members[], edges[]}` | — | **Not a record.** Derived grouping with no code reader (claude-6 §2: one grep pass found none). Keep as a computed view over `[:needs 1]` + referents, or drop. No schema line proposed. |
| A shadow demand | `giraffe-shadow-demands.jsonl` lines: debounced "recompute Giraffe input" with freshness probe result | — | **Not a GCC record.** Operational telemetry of the watcher's debounce. In the event-driven design (claude-6 §4a) the demand *becomes* the re-read request upsert; the ledger stays a file log, or is dropped once the real sink exists. |

Also in the store, for completeness: the 41 hand-reviewed edges in
`giraffe-mission-edges.json` (source, but unused by the pass since the mint
takes reading edges — claude-6 §1.4) map to `[:needs 1]` / referent
candidates with basis `human`; and relationship-review resolutions
(`e-xiang-relationship-resolution-*`; zero resolved today, **unconfirmed**
count of proposals) are operator verdicts → assessments on candidate
`[:needs 1]` 'clusions, per B9's "Giraffe: a reviewer's verdict as an
assessment".

### 1b. Proposed schema lines (for Joe to rule on; E-gcc-schema not edited)

i. **State group on an item (extends B8).** "A pending item's Giraffe state
   group (`:backlog|:unstarted|:started|:completed|:cancelled`) is
   attestation content on its `[:workplan/item 1]`, basis `parser` (read
   from the document's declared status), producer = the census or watcher,
   at a source sha. It corrects without changing the item's id."

ii. **Reading.** "`[:giraffe/reading 1]`: roles `:item` (a
   `[:workplan/item 1]`) and `:source` (an artifact revision: path +
   sha256). Content: `status_as_read`, `purpose`, checker id and
   certificate hash. One 'clusion per (item, source sha); a re-read of a
   changed document is a new 'clusion, and `[:supersedes 1]` links it to
   the reading of the same item's earlier revision. Each edge quote the
   checker verified is an attestation on the reading, basis `model` with an
   `exact` quote check; the reader seat is the producer."

iii. **Re-read request (choice).** Either: "`[:giraffe/reread-request 1]`:
   roles `:item`, `:source` (the digest to be read); content: state,
   claimant, lease; states are the B1 receipt vocabulary extended with
   `withdrawn`" — or it is the first **B7 want record**: "content is the
   query (a `[:giraffe/reading 1]` for this item and sha) and the horizon
   B1; pending/claimed/landed/failed/superseded are computed on read from
   the queue, never stored" — which needs Joe's ruling on whether
   operational queues enter the GCC at all. My recommendation: the want
   reading, with the SQLite row staying the operational authority
   (DERIVE-core §14: indexed, never mirrored).

iv. **Pass receipt.** "`[:giraffe/pass 1]`: no roles (or role `:input`
   naming the items-and-needs digest); content: `input-sha256`, `as-of`,
   counts of open/ready/cycles, method/version of pass.clj. Rank and
   readiness per item are computed on read (rule 3), as `met` already is."

v. **Focus (schema 1.1, 猴).** "`[:focus/relation 1]`: roles `:operator`
   and `:target` (an item); content: `relation ∈
   {:focus,:associated,:useful-elsewhere}`, with valid-time. Written by 猴
   per settled turn. The priority projection is a sort of the pass receipt
   through the newest focus relation; rows are never stored."

## 2. Source of truth, writer, and which history matters

| Thing | Kind | Writer of record | History that matters |
|---|---|---|---|
| Item documents | **Source** (the things themselves) | humans/agents editing `holes/**/*.md`; the *record* of an item is written by the watcher per changed document (replaces the per-cycle census) | Git is the history; the GCC needs only the current revision + valid-time on the record |
| Readings | **Agent readings — data of record.** An accepted reading is a seat's judgement, pinned to a source sha, carrying checker provenance; it cannot be recomputed (the same seat re-reading the same text may read it differently) | reader seats (zai-2, kimi-1) via `reread_worker.clj:249` → corpus; in the design: `clusion/assert` on the Arxana route | **Supersession matters**: a reading superseded by a later reading of the same item (new sha) must stay queryable (`[:supersedes 1]` + valid-time) — that lineage is the audit trail for why a `needs` edge existed. The 441 full-copy corpus *generations* (594 MB) are not that history; they are an artefact of copy-on-publish and are redundant once readings are versioned records |
| `needs` edges | Mixed: the edge asserted by an accepted reading is part of that reading (data of record); an edge from an operator's relationship-review resolution is a **source** (operator verdict); `met` is derivable, never stored | readings (`readings.clj:51`); operator via the resolve route (`turn_service.clj:645`) | The asserting reading's id must be pinned on the edge (an attestation source-ref); when a new reading lands, its replacement of the item's edges is the event that matters |
| Queue rows, mint-batch state | **Operational** | the timer cycle / the event sink | None, beyond the failed/withdrawn split needed for the invariant check |
| Pass, priorities, work clusters | **Derivable on demand** — recompute, never load | pass.clj / priority.clj over current records | None. 188 run directories are 188 evaluations of the same function; keep the newest as the War Machine export until futon2 reads the pass record |
| Shadow demands | **Operational telemetry** | watcher debounce | None |
| Relationship-review proposals | Derived proposals (basis `model`); resolutions are **source** (operator) | 象/`turn_service.clj:449`; Joe via the resolve route | Resolutions matter (they are rulings, rule 9); unresolved proposals are candidates that count until rejected |

## 3. Overlap with the workplan stack on :7094

The :7094 stack holds 323 `[:workplan/item 1]` 'clusions with attestations
(WORKPLAN-INSTANTIATE, futon3c `9abe433b5`); Giraffe's corpus holds 826
members and the newest run 503 open items. **Same id space**: both are keyed
by the item id (`M-…`, `E-…`, `T-…`) parsed from `holes/**/*.md`, and
Giraffe's id is the document's declared id while the workplan loader keys
the same documents. Whether the 323 is a strict subset of the 826 (the
workplan loader may cover fewer repos or only missions — **unconfirmed**;
claude-6 is reviewing that side separately, so I did not re-derive it), the
ids that coincide denote the same item.

**One item record must live once.** Proposal: the item node is
`[:workplan/item 1]`, loaded once, by whichever loader runs first (the ids
are content-digested, B1, so two loaders asserting the same item produce
the same 'clusion id and the second is a no-op or an added attestation).
Giraffe then contributes *attestations and links on that node*, never a
parallel item table: the state group (§1b i), the readings (§1b ii, role
`:item` pointing at the workplan item), and the `[:needs 1]` edges. The
lexicon refers to the same node through `[:workplan/names 1]`. This is
exactly B1's "the id is a digest of the type revision, the ordered
role-labelled endpoints": identity is free; provenance is attestations.

The reverse hazard to flag: `members.json` pins (path, sha256) per item and
my own re-read work shows those pins lag the live documents (five
consecutive requisitions pinned stale shas this week). In a record world
that pin is the `:source` endpoint of the reading, corrected by
supersession — the lag becomes visible as drift between the item's current
revision and its newest reading, which is precisely invariant clause 1 of
claude-6 §4.3.

## 4. Recommended load

**Load once into the GCC (on the Arxana route; today that means the private
stack, per B10):**

1. **Items** — the union of :7094's 323 and Giraffe's 826 members, as one
   `[:workplan/item 1]` each, with a state-group attestation at the current
   source sha (§1b i). The 324 members no longer open get their state group
   too (`:completed`/`:cancelled`); they close cones.
2. **Readings** — the current 645 records as `[:giraffe/reading 1]`, one
   per (item, source sha), each with its checker attestation. Where one
   item has several readings (re-reads), keep them all and link
   `[:supersedes 1]` — that is the history that matters (§2).
3. **Needs edges** — every edge of every accepted reading as `[:needs 1]`
   with class, attested by its reading (basis by source, §1); plus the 41
   hand-reviewed edges from `giraffe-mission-edges.json` with basis
   `human`, standing `verified` (they were reviewed); plus future
   relationship-review resolutions as assessments. Edges of superseded
   readings stay, marked via their reading's supersession.
4. **Nothing else.** No pass rows, no priority rows, no clusters, no queue
   history, no shadow demands, no run directories.

**Recompute, on demand or per event:** the pass and (when a focus relation
exists) the priority projection, in the futon3c JVM over the records, with
one `[:giraffe/pass 1]` receipt written per evaluation (earlier receipts
stay in XTDB history). Work clusters become a view. Run directories remain
only as the War Machine's export format until futon2 reads the record.

**Keep operational, off the GCC:** the SQLite queue (indexed, not mirrored
— §14), mint-batch state, the shadow ledger (until the real sink lands).

**Drop / reclaim afterwards (sizes only; nothing deleted by this review):**
- 187 of 188 run directories: ≈ 1.3 GB − 8.2 MB ≈ **1.29 GB**
- 440 of 441 corpus generations: ≈ **593 MB** (594 MB − ~1.3 MB)
- the queue's three `.bak` copies: ≈ 1 MB; second-reading outcomes 68 KB
  (keep or drop, immaterial)
- Total reclaimable: ≈ **1.9 GB**, against which the loaded records are
  tiny: 826 items + 645 readings + a few thousand edges ≈ low single-digit
  MB.

**Unconfirmed in this review:** whether `[:needs 1]`'s `:supplier` role
admits external (non-item) endpoints; whether the 323 workplan items are a
strict subset of Giraffe's 826 members; the count of relationship-review
proposals at :7073 (claude-6's filter miss, unrechecked); pass run time in
isolation (bears on whether per-event whole-pass recompute needs §4.3's B2
bound or an incremental pass).
