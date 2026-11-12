# GCC workplan review — :7094 stack and lexicon against E-gcc-schema

kimi-2, 2026-11-12, for Joe (requisition from claude-13, rerouted from
claude-6 who generated the slice). Read-only design review. Decides what of
the :7094 workplan/lexicon prototype to load into the live GCC, and how, per
Joe 2026-11-12: not dump-and-load; use the material to finish the design and
load it in ways that are meaningful.

**Method.** Read E-gcc-schema (futon4 `holes/excursions/E-gcc-schema.md`,
Parts A–C and §1–§16), M-repl-circus §DERIVE, claude-6's generator
(`~/notes/cascade-live-outline/arxana/make_all.py`), the loaders
(futon3c `src/futon3c/arxana/lexicon_loader.clj`, `lexicon_views.clj`),
WORKPLAN-INSTANTIATE (futon4 `holes/labs/M-jvm-arxana/`), and the
lexicon-graph generator (`~/notes/claude-13/lexicon-graph/build.py`,
`template.html`). Queried :7094 read-only through `/arxana/v1`
(`graph/query`, `entity/get`) and read its derived SQLite index
(`~/.local/share/futon4a/workplan-demo/arxana-index.sqlite`, watermark 7352)
for the census. No commands, no writes, no signals.

**Census of :7094 (index, 2026-11-12):** 3,604 edge rows; distinct
'clusions by type: `workplan/item` 323, `benefit-line` 75, `lexicon/entity`
251+ (254 rows, 3 duplicate slugs), `workplan/names` 454, `workplan/serves`
416, `workplan/mention` 327, `workplan/referent` 593, `workplan/introduction`
3, `workplan/entity-match-rule` 6, `workplan/entity-rule-member` 6.
Attestations 3,034; assessments 3; attempts 0.

---

## 1. Every record kind vs the schema

| Prototype record kind | E-gcc-schema place | Verdict |
|---|---|---|
| `[:workplan/item 1]` 'clusion (`item/id`) | B3 "Pending item"; §2 row 2 | has a place |
| `[:benefit-line 1]` 'clusion (`benefit/line`) | B3 "Benefit line"; B8 | has a place; the trunk/harm-line/entry taxonomy is **not** in the schema (§2 below) |
| `[:lexicon/entity 1]` 'clusion (`entity/slug`) | B3 "Feature (entity)" | has a place |
| `[:workplan/names 1]` (item→entity) | B4(a) row 1 | has a place |
| `[:workplan/serves 1]` (item→benefit-line) | B4(a); B8 "benefit of a pending item" | has a place; name clashes with lexicon verb `serves` (B4(b), already flagged) |
| `[:workplan/mention 1]`, role `filed` (item→git external-ref) | B4(a) "item mentioned in turn or commit" | has a place |
| `[:workplan/mention 1]`, evidence roles (`context`, `first-statement`, `operator-request`, `cause-found`) | B4(a) same row | place exists; the role vocabulary is **not** declared (§2) |
| `[:workplan/introduction 1]` (entity→turn external-ref) | B4(a); B6 | has a place |
| `[:workplan/referent 1]` (entity→target, `referent/role`) | B4(a); B6 | has a place; role vocabulary is the unreduced 114 verbs (B4(b) **[open]**) |
| `[:workplan/entity-match-rule 1]` | B2 rule 3; B4(a) "turn names entity" row | has a place |
| `[:workplan/entity-rule-member 1]` (rule↔catalog) | **no place** — B4(a) does not name it | schema gap (§2) |
| Attestation, basis `exact`: git provenance (`git log --diff-filter=A`), evidence-fetch with codepoint range and quote sha256 | B1 attestation | has a place |
| Attestation, basis `parser`: names matches (markup.py forms); `sessionista/same-turn-referent` | B1 | has a place |
| Attestation, basis `model`: pending level (P0–P6 draft), three serves producers (trunk filing, project-language placement, user-story pass), user story (`story/*`), lexicon entity status/live/tier, lexicon relation candidates | B1; B8 for status/live/tier | has a place |
| Assessment: `joe` accepted, policy `{id workplan/closed, revision 1}` (E-R14) | B1 assessment | has a place |
| Assessment: `claude-13` rejected ×2, policy string `sessionista/referent-review` (self-referents) | B1 assessment | has a place; policy field shape is inconsistent (string vs `{id, revision}`) — feeds the open "named policy" item (B1, B14 #2) |
| Review records among the post-load commands | B1 assessment | the only review records on :7094 are the 3 assessments above; zai-1's stepper test ruling lives on :7294's data-root, not :7094 |

Nothing on :7094 is shapeless against the model: every record is a B1
'clusion, attestation or assessment. The kinds with **no schema home** are
only `entity-rule-member` and the two vocabularies (mention roles, pending
levels).

## 2. Proposed schema lines (for Joe to rule on; E-gcc-schema not edited)

1. **B4(a), new row:** "rule is member of catalog —
   `[:workplan/entity-rule-member 1]` (`:rule`, `:catalog`) — written by the
   rules materialiser; the catalog is what sessionista fetches."
2. **B4(a), mention row, add:** "`mention/role` is one of `filed`,
   `context`, `first-statement`, `operator-request`, `cause-found`; the
   vocabulary is declared in the type, so a new role is a type revision."
3. **B3, pending-item row, add:** "readiness of an item is `pending/level`
   as attestation content (draft P0–P6 scale, claude-6 2026-11-09), so Joe's
   re-grading is a new attestation, not a new item."
4. **B8, benefit lines, add:** "a benefit line has a level — `trunk`,
   `harm-line`, or language-table `entry` — and, for entries, the
   project-language pattern and a `hole?` flag, as attestation content."
5. **B6, add:** "an introduction's attestation carries the codepoint range
   of the `[[X]]` mark and the turn's evidence time; a hand-entered time is
   recorded as basis `human` with its own source-ref." (Occasion: the
   switchover re-record of Joe's `[[Arxana]]` introduction is 54 s before
   the turn's evidence time, entered by hand — WORKPLAN-INSTANTIATE,
   packet 3.)
6. **B4(b), note:** the store holds 593 referent 'clusions against the
   loader's reported 589. One of the four extra is confirmed post-load
   (§5); three are unresolved in the time available — flagged, unconfirmed.

## 3. Meaningful load — where each kind's truth comes from, and the live writer

| Kind | Truth of record | Live writer (per M-repl-circus §DERIVE) | Snapshot finding |
|---|---|---|---|
| item 'clusions | the item's `holes/**/*.md` document in git | document watcher upserts on digest change | all-items.edn is a one-day snapshot; 323 items frozen at 2026-11-09 |
| `filed` mentions | git history (`--diff-filter=A`) | git loader, at commit time | derivable continuously; snapshot acceptable but stale |
| evidence mentions | operator/agent turns | **turn projector** at `:settled` | the 4 T-pattern-search mentions were hand-curated in `make_all.py` (`MENTS`); this is a hand-made snapshot of exactly what the projector must produce — the clearest finding of this review |
| names links | item text × lexicon forms (`markup.py`) | recomputed by the watcher when the doc or the forms change; the parser is the rule (B2 rule 3) | snapshot |
| serves links | agents' trunk filings and language placements (`~/notes/cascade-live-outline/` TSVs) | the reading seats, as `model/unverified` attestations re-derived when the tables change | truth lives in `~/notes`, **outside any versioned repo** — finding: those TSVs are inputs of record and should be filed or the attestations point at moving files |
| pending levels | `pending_index.py` verdict (draft scale) | model attestation; Joe's re-grades are assessments | draft; Joe has not ruled on the scale |
| user stories | codex-22/23 reading of item docs | model attestation, re-derived per doc digest | snapshot |
| lexicon entities, status/live/tier | the lexicon `@entity` blocks (`~/notes/.../lexicon/`) | document watcher over the lexicon; Joe's corrections are assessments | snapshot; Joe's corrections to date are **not** records anywhere (M-giraffe: 0 of 589 reviewed) |
| candidate referents | lexicon relations | watcher/loader, `model/unverified`; reduction to 14 verbs is a view rule (B4(b)), not a rewrite | snapshot |
| introductions, Joe's referents, rulings, word-rule switches | **Joe's acts in Emacs** | Joe, through sessionista/entity pages | data of record — not derivable; must survive any reload (Part A disagreement 1) |
| claude-13's 2 rejections | reviewer verdicts | the reviewer, through the assessment route | data of record |

The load split follows: **everything in the upper rows is a derived
projection** and should be produced continuously by the watcher, git and the
turn projector; **the bottom two rows are inputs of record** and are the
only part that needs migrating as data. A snapshot copy of the derivable
rows into the live GCC is a finding, not a plan.

## 4. The lexicon graph page as a Mojira view

**Today** (`wip/formal-arxana/lexicon-graph.html`; generated, not committed —
the wip path does not exist in any repo; generator and template in
`~/notes/claude-13/lexicon-graph/`): a static force-directed page over
`lexicon/merge/all.json`. 251 entity nodes coloured by status; edges either
in the readers' original 114 verbs or the reduced 14
(`LEXICON-VERBS.tsv`); per-node measures — degree, unfinished features
within two hops, pending items naming it, turns mentioning it; a regions
sidebar; click a node for its out/in relations with the raw lexicon text.
All data is baked in at build time; nothing reflects a ruling Joe has made
since.

**A GCC-backed Mojira view would query** (all indexed reads, B1):
- nodes: `[:lexicon/entity 1]` with the newest attestation for
  status/live/tier under a **named** assessment policy (B1 open item);
- edges: `[:workplan/referent 1]` with their assessments — accepted solid,
  unverified faint (the "dashed" convention Joe approved for proposed
  patterns, §16 item 14), rejected hidden;
- the reduced-verb view as a read-time mapping (B4(b)) over the stored raw
  roles, so Joe's correction of the 14-verb table changes the view, not the
  data;
- per-entity counts (items naming it via `[:workplan/names 1]`, backlinks
  from the derived rule index, open wants B7) as materialised aggregates
  with a watermark (B1 reads);
- the §14 ranking measure itself as a versioned aggregate, so "the measure
  is on trial" is checkable against the priorities Joe names next.

**What it needs from the schema:** the named assessment policy (B14 #2);
the verb reduction settled (B14 #6); want records if "no pending item names
this feature" is to be shown as a want rather than an absence (B14 #9);
and entity pages/rulings to remain the write path so the view is never a
writer.

## 5. The post-load records on :7094 (data of record)

M-repl-circus §DERIVE counts "24 post-load commands held nowhere else". The
observable post-load record set, by kind (ids abbreviated to 12):

- **Introductions (3)** — all Joe's acts:
  - `e25a8ee8f042` — arxana ← evidence `emacs-60640fc2…` (the switchover
    re-record of Joe's `[[Arxana]]`, hand-timed 54 s early);
  - `2b33e0b9b785` — evidence-flow ← `emacs-a124507f…` (2026-11-11);
  - `d25de8c7e0f9` — gcc ← `emacs-fbccae60…`.
- **Word rules (6)** `entity-match-rule`: `3afa0cffa9d0` kangaroo-pouch,
  `82d743235bff` arxana, `3b99c15ce735` giraffe, `052178e63545` agency
  (seeded trial), plus `984c3824d8a3` evidence-flow and `78064147a769` gcc
  (created by Joe's two introductions). 6 `entity-rule-member` 'clusions
  accompany them (ids not enumerated — unconfirmed detail).
- **Candidate referent (1, confirmed post-load):** `c1039643832c` —
  evidence-flow → agency, attestation `30d4634498d6` (producer joe, method
  `sessionista/same-turn-referent`, basis parser, unverified): the turn that
  introduced evidence-flow also named Agency.
- **Assessments (3):** `0c62ec7c560b` — **Joe's verdict**: E-R14 accepted,
  policy `workplan/closed` (this one is inside the 3,881-record load);
  `2fefd13009ae` and `9d821dae0a27` — claude-13's two rejections of the
  self-referents (arxana part-of arxana; holistic-argument replaced-by
  holistic-argument), policy `sessionista/referent-review`.
- **Introduction attestations** `47a59d2a8ee8`, `03a2c0cbbe10` (the two
  post-load introductions), plus forms attestations on the two new rules
  (ids not enumerated).

**Which must survive:** Joe's three introductions, Joe's E-R14 acceptance,
the two rules and forms his introductions created, and claude-13's two
rejections (review verdicts of record). The single same-turn candidate
referent is reproducible from the turn, but it is also a Joe-turn artifact
and cheap to carry. Everything else on :7094 (the 3,881-record load, the
1,590-record lexicon load, the 589 candidates) is re-derivable from its
inputs and should be regenerated by the live writers in §3, not copied.

Unconfirmed: the exact decomposition into "24 commands" (the figure is from
the DERIVE note; the observable records above are consistent with it but I
did not replay the command log), and three of the four referents by which
593 exceeds 589.

## Notes on rules observed

- :7094 was queried only through `/arxana/v1` reads (`graph/query`,
  `entity/get`) and its on-disk derived index; no commands, no restarts.
- :7073 was not touched. futon3c-public discipline: this file names ids,
  counts, kinds and code locations only.
