# GCC use cases — who asks what, how often, how fast (kimi-1, 2026-11-12)

DERIVE survey for M-repl-circus, at claude-13's requisition (2026-11-12).
Purpose: decide where the GCC lives (SQLite sidecar; graphqlite; Datascript
in-JVM mirrored to XTDB; XTDB/XTQL alone) by surveying how it would be
*used*. Sources are cited per row; anything unchecked is marked
**[unconfirmed]**.

Latency budgets, from the requisition: *Prompt-line* = inside a Prompt-line
render, hundreds of ms; *in-turn* = inside one turn's processing, seconds;
*offline* = batch, nightly, or on Joe's demand.

## 1. The use cases

| # | Who asks | The question, in words | Query shape | How often | Latency budget | Writes? | Source |
|---|----------|------------------------|-------------|-----------|----------------|---------|--------|
| 1 | GCC projector | (write) extend the GCC with one settled turn's records; does the incremental result equal a full rebuild? | set equality over the whole graph | per turn at `:settled`; rebuild on demand | in-turn; rebuild offline | **yes** | M-repl-circus §DERIVE "The composition" (holes/missions/M-repl-circus.md:317) |
| 2 | 猴 | where does this settled turn's cascade sit in the GCC: which earlier occurrences share its elements, which connected component does it join? | neighbourhood + pattern match over a subgraph (shared-element gluing); possibly graph embeddings | per turn at `:settled` | in-turn | yes (focus record, schema 1.1) | M-repl-circus "Pattern occurrence moves from 馬 to 猴" (:144); DERIVE "猴's place follows" (:344); E-gcc-schema §16 items 4, 6, 9 |
| 3 | Giraffe (pass) | which work-pool items are ready: per `needs` edge is the need met (closure / artifact / alternative), what is each ready item's transitive cone and one-step unblocks, are there cycles? | aggregation + reachability over the `needs` subgraph; cycle detection | per GCC update (per settled turn) or debounced | in-turn to offline | no (readiness computed, never stored — pass.clj:33) | futon3c src/futon3c/giraffe/pass.clj:1-46; M-repl-circus DERIVE "Giraffe priorities, kept current" (:333) |
| 4 | Giraffe (priority projection) | band the pass order by 猴's focus relation: focus / associated / elsewhere per target | point lookup per target + join + sort | per pass run | in-turn | no | futon3c src/futon3c/giraffe/priority.clj:1-40 |
| 5 | Coverage check | does every operator chat turn at :7073 have a record that reached `:settled` in the interval, each animal's field present or typed-absent, and its GCC records present? | aggregation / set difference over turns × animals × records | on demand; nightly plausible | offline | no | M-repl-circus §"What VERIFY checks" (:350) |
| 6 | Agent through CTRL | is a linked to c? name / cite / ask / wait / hand back / mark; "a links to c *eventually*" as a want with a horizon | point lookup (edge existence); want = open query + horizon, witnessed by a later link | per agent action | Prompt-line to in-turn | yes (wants, witnesses, marks) | M-repl-circus §"The control layer" (:224-248); E-gcc-schema B7 (:334-354) |
| 7 | War Machine (wants with locators) | is this want observed? locator classes C3–C8 (git objects at a pinned sha, test registry) and T1 (a later act quotes the want's words) | point lookup + external check (git/test registry); T1 = text match over evidence | per WM run | offline to in-turn | yes (observation status) | E-gcc-schema §16 items 11, 13; B11 (:425) |
| 8 | 馬 / 猴 / War Machine (`[:cascade/aims-at 1]`) | what does this turn's cascade aim at; where does an operator intent match a reply, or a pattern cascade match a META item's stated materials? | pattern match: join occurrence THEN-elements to wants | per turn; per WM task | in-turn | yes (aims-at links) | E-gcc-schema §16 item 12 |
| 9 | Joe through Mojira | draw one turn's cascade: its occurrences, elements, glue, partials | bounded neighbourhood (one situation's subgraph) | per Joe page view | Prompt-line (page render) | no | futon4 dev/mojira/cascade-view.js:1-9; cascade-reader.js:4 ("the one module to swap for a GCC query later") |
| 10 | Joe through Emacs (cascade s-exp) | show one turn's cascade as lisp-data with `#n=` joins | bounded neighbourhood, serialised | per Joe action | Prompt-line | no | E-gcc-schema §16 item 10; futon3c scripts/cascade_sexp.py |
| 11 | Joe / an agent (象 stepper) | assemble one session's frames: turn, parse, patterns, alignment, reply joins | neighbourhood around a session's turns; many point lookups | per Joe action | Prompt-line today is ~1 s per named entity for the REVIEW section — the stated occasion for a fast datalog layer | no | futon3c scripts/turn_frames.py:1-40; E-gcc-schema §15 (:1013-1032) |
| 12 | 狗 | which memories are relevant to this turn (recall at `:recorded`); was one used (at `:settled`); bury | today: text search over minus40 (:7093 `/api/alpha/evidence/text-search`, 100 ms budget, dog_provider.clj:11-20). As GCC reads: point lookup by memory slug + one-ring `[[links]]` neighbourhood | per turn | Prompt-line (100 ms stated) | yes (bury) | futon3c src/futon3c/agency/dog_provider.clj:1-44; M-repl-circus "Spreading activation" (:170) |
| 13 | 驴 / agent following a line item | the next ring out from a line item: a memory's `[[links]]`, a pattern's cascade | one-ring neighbourhood from a handle | per agent tool call | Prompt-line | no | M-repl-circus §"Spreading activation" (:164-171) |
| 14 | futon7 questions | which question does this turn bear on; questions as records linked to turns and citations | path / reachability between turn and question records | per turn (linking); per Joe query (reading) | in-turn; Prompt-line for reading | yes (question links) | futon7 holes/E-futon-questions.md item 5 (:46-51) |
| 15 | War Machine (selection) | read the current Giraffe priorities to select work | table read of the projection output | per WM tick / selection | in-turn | no | M-repl-circus DERIVE "猴's place follows" (:344; "Who reads the priorities" open item :374) |
| 16 | Anyone (as-of) | what did the map hold on a date; was X used before it was named? | as-of-time over valid-time records | occasional | offline acceptable | no | E-gcc-schema B6 (:319-332) |

Seed-list items all verified and extended (rows 1–15 correspond; row 16 added
from B6; the 狗 row splits recall vs bury per the code).

## 2. Query shapes by frequency and latency

**Dominant shapes.** Point lookups and bounded neighbourhoods dominate by
count: rows 4, 6, 9, 10, 11, 12, 13 — every Prompt-line reader is one or two
hops from a known id. Reachability over a modest subgraph (`needs` cones,
glued components, turn→question paths) is the second class: rows 2, 3, 8, 14.
Whole-graph aggregation is rare and offline: rows 1 (equality check), 5
(coverage). As-of-time is occasional (16).

**Must be fast (Prompt-line, hundreds of ms or less):** the reads Joe or an
agent waits on — Mojira/Emacs cascade views (9, 10), the stepper (11), 狗
recall (12, stated 100 ms), line-item following (13), CTRL edge existence
(6). All are point lookups or one-to-two-hop neighbourhoods. None needs
whole-graph traversal.

**In-turn (seconds):** the projector's incremental write (1), 猴's cascade
location (2), Giraffe pass + projection (3, 4), aims-at joins (8). These run
once per settled turn; a second or two is tolerable.

**Offline:** full rebuild + equality (1), coverage (5), as-of archaeology
(16), want observation runs (7).

**Writes** are all per-turn and small: one turn's records (occurrences,
elements, gluing is *computed* per §16 item 4), 猴's focus, wants/witnesses,
bury rulings, question links. No write needs transactionality beyond
single-writer append; the whole store is rebuildable from the turn records
(M-repl-circus DERIVE: "its store needs speed more than durability").

**Scale check:** the corpus is hundreds of missions/tickets, ~800-member
read corpora, tens of thousands of evidence turns, and per-turn records in
the low tens. The whole GCC is comfortably a few hundred thousand edges —
small by any of the candidates' standards. **[unconfirmed: no count of
settled turn records exists yet; the projector does not exist.]**

## 3. The four candidates

### SQLite sidecar (recursive SQL)

- **Serves well:** everything point-lookup and neighbourhood (rows 4, 6,
  9–13) — indexed joins, the established sidecar pattern (pattern index,
  warrant index, entity columns; M-repl-circus DERIVE :354). Reachability
  (3, 14) via recursive CTEs is standard SQL:1999 and fine at this scale.
  Aggregations (1, 5) trivially. Single file, rebuildable, disposable — the
  exact shape of "a projection, not a seventh writer". Python and JVM both
  read it today (xerial sqlite-jdbc is already on the futon3c classpath for
  the other sidecars **[unconfirmed for the GCC path specifically]**).
- **Serves badly:** subgraph *pattern match* (rows 2, 8 — "occurrences
  sharing elements", "THEN-elements joined to wants") has to be spelled out
  as hand-written joins per record type; datalog-style *rules* (gluing as a
  rule, readiness as derived) have no home, so they live in Clojure over SQL
  results, as pass.clj already does. As-of (16) needs explicit valid-time
  columns and discipline.
- **Not at all:** nothing, at this scale; the cost is ergonomics, not
  capability.

### graphqlite (GitHub: colliery-io/graphqlite)

What it is **[checked 2026-11-12 against the GitHub API and README]**:
a **SQLite extension** (written in C) that adds graph storage and the
**Cypher** query language, with built-in algorithms (PageRank, Louvain,
Dijkstra, BFS/DFS, connected components). MIT licence. Created 2025-07-19,
actively developed (last push 2026-11-10), **v1.0.0 released 2026-11-10** —
two days ago. ~530 stars, 33 forks. Self-reported openCypher TCK
conformance 98.5% (**[unconfirmed: project's own numbers]**). Bindings:
Python (`pip install graphqlite`, a `Graph` wrapper), Rust, raw SQL;
Homebrew for the native lib.

- **Serves well:** the two shapes plain SQL serves badly — subgraph pattern
  match and variable-length reachability are Cypher's native ground
  (`MATCH (o1:Occurrence)-[:HAS_ELEMENT]->(e)<-[:HAS_ELEMENT]-(o2)` is the
  gluing query; `[:needs*]` is the cone). Keeps SQLite's single-file,
  rebuildable, zero-server properties. Connected components is built in —
  exactly row 2's "which component does it join".
- **Serves badly:** as-of/valid-time (16) still manual; aggregations fine
  but no advantage; datalog rules absent (Cypher is a query language, not a
  rule engine).
- **Risks:** (a) *maturity* — 16 months old, v1.0.0 two days old, one
  organisation; adopting it bets the GCC's query layer on a young native
  dependency. (b) *JVM loading*: it is a native SQLite extension, so from
  xerial sqlite-jdbc it needs `load_extension` enabled and a per-platform
  shared library on the deploy path **[unconfirmed: xerial exposes
  `SQLiteConfig`/`enable_load_extension` pathways but I did not test
  loading; no JVM loads per the rules]**. From Python, `pip install
  graphqlite` ships its own wrapper **[unconfirmed whether it can attach to
  an arbitrary existing sqlite3 connection or only its own]**. (c) *language
  fit*: nobody on the stack writes Cypher; the team's graph idiom is
  datalog.

### Datascript in the JVM, mirrored to XTDB (futon1 `apps/graph-memory/src/app/store.clj`)

The precedent is real: Datascript working copy, append-only journal +
snapshots, `sync-to-xtdb!` / `hydrate-from-xtdb!` (store.clj:213, 372).

- **Serves well:** the *hard* rows — pattern match, rules, reachability
  (2, 3, 8) are datalog's home; pull-syntax neighbourhoods (9–13) are
  one-liners; in-memory speed meets every Prompt-line budget; EDN in/out
  suits the Emacs s-exp view (10).
- **Serves badly:** operational cost, exactly as Joe warned (E-gcc-schema
  §15: "futon1 became complex and had to be rewritten twice!"): heap in the
  shared futon3c JVM, reload hazards for in-memory state, a second copy to
  keep in step with the mirror, and hydration-on-start cost. As-of queries
  against the XTDB mirror are fine, against the Datascript copy not.
- **Not at all:** nothing, given the mirror — but the mirror is what you
  pay for the speed.

### XTDB / XTQL alone (futon1b :7073)

- **Serves well:** durability and bitemporal as-of (16) natively; the
  already-running store; the mission's own goal text says "in XTDB … read
  … through XTQL" (:23-25).
- **Serves badly:** the hot path. The recorded evidence is the stepper's
  REVIEW section at ~1 s per named entity (E-gcc-schema §15), and Joe on
  11-12: "it's also slow". The :7073 heap is already near full
  (M-repl-circus DERIVE :369). Fails every Prompt-line row.
- **Not at all:** interactive cascade views at hundreds of ms
  **[unconfirmed as a hard bound; the 1 s/entity figure is one anecdote,
  not a benchmark]**.

## 4. Recommendation

**SQLite sidecar as the GCC's home, XTDB as its eventual durable mirror,
graphqlite held as the named fallback for the pattern-match rows.**

Reasons, tied to the survey:

1. The dominant shapes are point lookups and small neighbourhoods under a
   hundreds-of-ms budget; indexed SQL meets all of them with technology the
   stack already runs (three existing sidecars).
2. The one whole-graph reachability job (Giraffe's pass over `needs`) is
   already computed in Clojure over a plain map (pass.clj) and can stay so,
   reading its edges from the sidecar — readiness was never a query the
   store had to answer by itself.
3. The store is a rebuildable projection; durability is worth little, so
   XTDB's one decisive advantage (bitemporal durability) buys little, and
   its cost (slow reads, a full heap) lands on the hottest rows. Write the
   mirror later, when a durable copy is wanted to save rebuild time or to
   answer as-of queries — the projector's full-rebuild equality check (row
   1) is exactly the harness that makes adding a second writer safe.
4. Datascript-in-JVM is the better *query-language* fit but the worse
   *operational* fit, and Joe has already named that failure mode once
   (futon1, rewritten twice). Keep it as the spike's comparison arm in a
   separate process, per the mission's own spike design (:370-375).

**What would change it:**

- If the 猴/Giraffe/aims-at rows (2, 3, 8) turn out to want datalog *rules*
  (gluing as a rule, readiness as derived relations) rather than per-type
  SQL joins — i.e. if the hand-written recursive SQL is the part that hurts
  in the spike — then graphqlite is the cheapest step up (same file, same
  embedding, Cypher for the hard queries) and Datascript-in-a-side-process
  the more expressive one.
- If as-of queries become a first-class reader (Joe asking "what did the
  map hold on 11-04" routinely), XTDB stops being optional.
- If the graph outgrows the single-file assumptions (orders of magnitude
  more edges than projected), re-open.

**Measurements the spike should take** (extending the mission's three
queries with the shapes above):

1. Build the same GCC (all settled turn records, pattern library, missions)
   in SQLite and in Datascript in a separate process; record: on-disk /
   in-memory size, full-rebuild time, one turn's incremental write time.
2. Time the three named reader queries on both: 猴 locate-a-cascade
   (shared-element gluing + connected component), Giraffe readiness over
   `needs` (recursive CTE vs datalog rule — record the SQL's length and
   readability, not just its speed), the coverage check.
3. Time one Prompt-line row end-to-end (a Mojira cascade fetch) against the
   SQLite store.
4. If the recursive SQL for query 2 is judged painful, add the graphqlite
   arm for queries 2–3 only: same data, Cypher, and record whether it loads
   cleanly from Python and from sqlite-jdbc on this machine (both currently
   unconfirmed).
5. Record edge/row counts so the scale assumption in §2 is replaced by
   numbers.
