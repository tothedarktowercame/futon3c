# MAP Q2 — XTDB (as deployed in futon1b) vs. the requirements of Elephant 2000

Mission: `futon3c/holes/missions/M-象-2000.md` (IDENTIFY). Facts, not a design.
Written 2026-09-26. Uncommitted.

Sources and what I checked:
- Paper: `storage/references/mccarthy-elephant-2000/elephant.tex` (line numbers below
  are tex lines; the working notes after `\end{document}` at l.1621 are cited the same way).
- futon1b at `059bb79` (clean tree). futon3c writers at `e42ee032`.
- Live store: read-only GETs against `:7073` (evidence windows around 09-24 16:00–19:05,
  `hyperedges?end=<sha>&type=code/v05/commit` with and without `valid-as-of`/`system-as-of`).
  No writes, no `/health?deep=true`.
- XTDB docs opened (2.x only; the stack runs 2.1.0, so 1.x docs were **not** opened and
  nothing below is claimed about 1.x):
  - D1 https://docs.xtdb.com/about/time-in-xtdb.html
  - D2 https://docs.xtdb.com/reference/main/xtql/queries.html
  - D3 https://docs.xtdb.com/reference/main/xtql/txs.html
  - D4 https://docs.xtdb.com/reference/main/sql/txs.html
  - D5 https://docs.xtdb.com/reference/main/sql/queries.html
  - D6 https://docs.xtdb.com/reference/main/stdlib/temporal.html
  - D7 https://docs.xtdb.com/concepts/key-concepts.html

---

## 0. What futon1b is, temporally (the facts section 2 relies on)

| fact | where |
|---|---|
| XTDB **2.1.0** (`xtdb-core`, `xtdb-api`) | `futon1b/deps.edn:7-8` |
| **Evidence** writes are `put-docs` with **no valid-from**, so XTDB valid-from = the write's system time. The event time is a separate, client-supplied string column `:evidence/at` (defaults to now). | `futon1b_evidence.clj:51`, `:165-168` → `migration/ingest.clj:97-107` (4-arity call, `valid-from` nil) |
| Evidence is **append-only**: a duplicate id is refused 409; `/documents/retract` only accepts `#{:entities :hyperedges :relations}` | `futon1b_evidence.clj:158-161`; `futon1b_graph.clj:319-339` |
| Evidence query filters: type, claim-type, author, session-id, fork-of, subject, pattern-id, tags, `since`/`before` — `since`/`before` compare `:evidence/at` **lexicographically as strings**. No `valid-as-of`, no `system-as-of`. | `futon1b_evidence.clj:208-215`, `:522-523`, `:528-548`; route `futon1b_server.clj:674-718` |
| Evidence has a reply chain: `:evidence/in-reply-to` must exist at write (409 otherwise); `GET /evidence/{id}/chain` walks it to the root | `futon1b_evidence.clj:142-148`, `:635-647` |
| **Hyperedge** writes accept `hx/valid-time` → `put-docs {:into :hyperedges :valid-from T}`; `hx/op "retract"` → `delete-docs {:from :hyperedges :valid-from T}` (ends validity from T on) | `futon1b_server.clj:125-186` (valid-from `:130`, delete `:137-140`); `migration/ingest.clj:101-103` |
| Hyperedge ids are stable from type+endpoints, so re-asserting "the same fact" hits the same XTDB id and its history | `futon1b_server.clj:67`, `:102-104` |
| `GET /hyperedges?type=|end=` accepts `valid-as-of` (alias `as-of`) and `system-as-of`; both become XTQL `(at T)` only | `futon1b_server.clj:860-887`; `futon1b_graph.clj:918-928` |
| `POST /memory/projection` accepts the same two instants | `futon1b_server.clj:951-973`; `futon1b_graph.clj:1586-1660` |
| `GET /hyperedge/{id}` is **not** temporal over HTTP (the 3-arity exists but the route calls the 2-arity) | `futon1b_server.clj:667`; `futon1b_graph.clj:846-855` |
| In an as-of memory projection the joined evidence row is read at **current** time, not at the requested basis | `futon1b_graph.clj:1329-1333` |
| Tests pin "made at t1, retracted at t2: present as-of t1+1s, absent now" — McCarthy's `exists(t, commitment x)` on hyperedges | `futon1b/test_temporal.clj:116-129`; also `:99-114`, `:264-311` |
| No `erase-docs` in the serving code (only in the phase-2 probe `p2_queries.clj:80-85`) | grep |
| Writers that stamp valid time today: commit ingest (valid-time = git `%at`, author date), file ingest, clock lineage (retract-then-put at now), memory lifecycle | `futon3c/src/futon3c/watcher/commit_ingest.clj:48-56`, `:465-468`, `:550`; `clock_lineage.clj:150-164`; `peripheral/memory_lifecycle.clj:541-656` |
| An arbitrary-query escape hatch exists (`/eval` in the serving JVM, :6769) but it is an nREPL bridge, not a query API | `futon1b_drawbridge.clj:1-19` |

Live confirmation (09-26): commit `5146606d` (`:prop/timestamp 1790267648` = 09-24T16:34:08Z)
is absent at `valid-as-of=2026-09-24T16:00:00Z`, present at `…T17:00:00Z` and at
`system-as-of=…T17:00:00Z`. Commits `2ef7a010`, `626df9fa`, `97021ac2` are also present as
`code/v05/commit` hyperedges.

## 1. Requirements extracted from the paper

| # | requirement | tex line |
|---|---|---|
| R1 | Program state is (virtually) a single history list of events — inputs and the program's own actions; "in principle, this is all that is required" | 195, 436-438, 677-685 |
| R2 | In deciding a response the program may inspect the entire past of inputs and responses | 518-519 |
| R3 | Functions of the past: value of a parameter at a time; time of an event; first/last time an event occurred or a proposition held; time-valued functions of the whole past | 452-470 |
| R4 | "Made and not subsequently cancelled": `exists(t, commitment x) ≡ ∃t'(t'<t ∧ arises(t',x)) ∧ ∀t''(t'<t''<t → ¬revoke(t'',x))`; pseudo-Prolog `hasreservation … finalseg(history, X.U), ismakrev(X…), not ∃Y (member(Y,U), cancelsrev(Y,X))` | 471-473, 1397-1398, 1666-1668, 1707-1710 |
| R5 | Inputs/outputs are typed speech acts: requests, questions, offers, acceptances, permissions, answers, assertions, promises, commitments | 155-160, 2119 |
| R6 | Performatives create obligations; a promise's utterance creates an obligation to fulfil it; correctness = fulfilled requests, truthful answers, kept commitments | 165-170, 213-217 |
| R7 | Abstract performatives: internal commitments, not necessarily expressed in output; `make`/`cancel`/`exists` apply to commitments independent of the object | 228-233, 412-415, 543-549 |
| R8 | Answers must be truthful **and responsive** (questioner will *know* the answer) | 368-383, 838-891 |
| R9 | Promises and future-directed commitments: "committed future actions that are not performed in response to an input"; Leora's note "Don't you often commit to a particular time in the future?" (the Promises subsection, l.2159, is a stub) | 1801-1802, 1998-2001, 2159-2163 |
| R10 | The program is a sentence of logic; history-based form uses `arises`, `outputs`, `revoke` with **circumscription** of those predicates (only asserted events occur) | 1304-1416 |
| R11 | Authorization: the program does only what it is authorized to do; authority to act | 169, 1784-1789 |
| R12 | Illocutionary vs perlocutionary, including on inputs: "hearing that" vs "learning that" | 265-270, 810-836 |
| R13 | Parsing the past: pattern matching over the history as a string; quantification over sets/lists of times (matching parentheses; sums over intervals) | 488-496, 1678-1695, 2044-2083 |
| R14 | Reservations/commitments are abstract objects governed by nonmonotonic axioms ("valid unless there is specific reason not to honor it") | 654-668, 766-769, 1768-1777 |
| R15 | Set comprehension over the past (e.g. `full flt ≡ card{psgr | exists commitment…} = capacity`) | 610, 1791-1793 |

## 2. Requirement → XTDB feature → exposed in futon1b → gap

| req | XTDB 2 feature (generic) | exposed in futon1b today? | gap |
|---|---|---|---|
| R1 history list | Immutable system-time log: "Users (rightly) have no control over system-time" (D1); every row carries `_system_from/_system_to/_valid_from/_valid_to` (D7) | Partly. Evidence is an append-only event table (`evidence.clj:158-161`, `graph.clj:339`); but agent/operator turns, commits and xlate analyses are split: turns in `:evidence`, commits as `code/v05/commit` hyperedges, the typed analyses only in files under `storage/operator-turns/` (no writer to :7073 in `session_turn_analysis.py`/`xlate.py`) | No single history; the typed acts are not in the store at all |
| R2 inspect whole past | Unrestricted query at current basis | Yes, bounded: evidence filters + pagination (`evidence.clj:528-548`) | No join/subquery over HTTP; one table per route |
| R3 value at a time | `FOR VALID_TIME AS OF` / XTQL `(at T)` (D2) | `valid-as-of`/`system-as-of` on `/hyperedges` and `/memory/projection` only (`server.clj:875-880`, `:961-965`) | Not on `/evidence`, not on `/hyperedge/{id}` (`server.clj:667`), not on entities/relations |
| R3 time of event; first/last time | `:all-time` / `FOR VALID_TIME ALL` + `_valid_from` columns (D2, D7); `in`/`from`/`to` period filters (D2) | No — only `(at T)` is ever generated (`graph.clj:918-920`) | "When did R come into force?" needs bisection over `valid-as-of`, or `:evidence/at` for append-only events |
| R4 made-and-not-cancelled | **Direct**: put `valid-from t'` + delete `valid-from t''` ⇒ as-of `t` returns the row iff `t' ≤ t < t''` (D2 `at` semantics `row-from <= ts < row-to`; D4 defaults "now … end-of-time") | Yes for hyperedges: write `hx/valid-time` + `hx/op retract`, read `valid-as-of` (`server.clj:130-140`; test `test_temporal.clj:116-129`; in use by `clock_lineage.clj:160-164`) | Encodes the *state*, not the acts: the cancelling act's identity is not recorded by the delete. Evidence (the acts) cannot be retracted or valid-timed |
| R5 typed speech acts | none (schemaless documents) | `:evidence/claim-type` exists, but every Joe turn observed 09-24 is `:question`, including constraints and the harness-injected requisition notices (live GETs above) | Typing lives in xlate files (19 intents); store typing is uninformative |
| R6/R7 obligations, commitments | none | none | No commitment object, no fulfilment check |
| R8 truthful + responsive | none | reply linkage only (`evidence.clj:142-148`, `:635-647`) | Pairing question→answer exists; truth/responsiveness does not |
| R9 promises / future | Valid time may be in the future: `at` covers rows that "were/will be visible" (D2) | `valid-as-of` accepts any instant (`server.clj:386-399`) | A future-valid row is a scheduled fact, not an open obligation with a deadline and beneficiary; no "due and unfulfilled" query |
| R10 logic + circumscription | Closed-world query answers (absent row ⇒ not found) | Implicitly | The closed world is only as good as ingestion: silent `safe-q` empties are documented (`graph.clj:959-962`) |
| R11 authorization | none at DB level | Penholder allow-list on writes (`futon1b_gates.clj:49-52`, `server.clj:459-464`) | Authorship (`:evidence/author`) is a payload field unrelated to penholder: harness notices are stored as author `joe` |
| R12 hearing vs learning | **System time vs valid time** is exactly this split: system-time "captures the time that information entered the system", valid time is user-managed for "out-of-order updates and backfilling" (D7) | Only on hyperedges | Evidence has one XTDB time (write time) plus a string `:evidence/at`; backfilled evidence can't be read "as known at S" over HTTP |
| R13 parsing the past | SQL/XTQL over rows; Allen period predicates `OVERLAPS/CONTAINS/PRECEDES/…` with strictly/immediately variants, `PERIOD(from,to)` (D6) | None exposed | Pattern matching / bracket matching over the history is not a query form |
| R14 nonmonotonic defaults | none | none | — |
| R15 set comprehension | SQL aggregates (D5) | `count` endpoints only (`server.clj` `evidence/count`, `census`) | — |

## 3. "In force as of T" — directly in XTDB, and over futon1b HTTP

**Which time the acceptance case needs.** The three checkpoints (09-24T16:00 absent,
T17:00 present, 09-25T21:00 followup withdrawn) ask *when things happened*: **valid
time**, read with the latest knowledge (system time = now). System time answers a
different question — what the record said at S — e.g. acts on 09-24 typed by the
overnight mining on 09-26 are invisible at `system-as-of` 09-25 but should be visible at
`valid-as-of` 09-24. That second question belongs to M-the-perfect-crime, not to this
acceptance case. Where it matters in practice: live-captured turns have
valid-from ≈ system-from ≈ `:evidence/at` (written within ms), so they agree; anything
**backfilled or mined** diverges, and the evidence write path cannot record the event's
valid time (`ingest.clj:97-107` called without valid-from from `evidence.clj:167`).

**Directly in XTDB — yes, two ways.**
1. *State encoding* (McCarthy's `exists`): one document per rule R; the creating act is
   a put at `valid-from = act time`, the withdrawing act a delete at
   `valid-from = withdrawal time`; then `FROM rules FOR VALID_TIME AS OF T` is R4
   exactly (D2 `at`, D4). The derivation is lost unless the put carries the act ids.
2. *Event encoding* (the `hasreservation` form): acts stay append-only rows; in force
   at T = ∃ create-act a with `a.at < T` and ¬∃ withdraw-act c for the same R with
   `a.at < c.at < T` — one SQL query with `NOT EXISTS` on an event-time column (D5), no
   temporal feature needed. Keeps derivation; needs a rule key on each act.

**Over futon1b HTTP as it stands — no.**
- Encoding 1 is possible in mechanism (`hx/valid-time` + `hx/op retract` + `valid-as-of`
  on `/hyperedges`, tested at `test_temporal.clj:116-129`), but there is no rule
  hyperedge: the store has commit *events* (`code/v05/commit`, valid from commit time,
  never retracted), so every commit is "in force" forever after it lands. As a sanity
  check the commit spine does answer "did `5146606d` exist as of T" correctly (live, §0).
- Encoding 2 is not expressible: `/evidence` filters by author/session/type/time window
  only, has no rule/act-type key, no negation, no join. It must be done client-side
  from a bounded time window (316 evidence rows in 09-24 15:40–16:40 alone).
- The acts are not typed in the store (§2 R5), and the 42 notices are stored as
  `author "joe"`, `role "user"`, the same as Joe's turns — the IDENTIFY filter lives in
  `storage/operator-turns/window-0922/operator-turns-joe.jsonl`, not in futon1b.

**Derivation of R.** Pieces exist: `in-reply-to` chains agent turns to the operator
turn they answer (`/evidence/{id}/chain`), commits carry `valid-time` and repo, sessions
group turns. Nothing links a commit to the turn(s) or session that produced it; the join
would be by time + agent + session, done outside the store.

## 4. What XTDB does not give at all

- Speech-act typing, and any semantics per type (request vs promise vs constraint).
- Cancellation *semantics*: which act withdraws which rule. XTDB deletes a row; it does
  not know that a 09-25 remark made a 09-24 proposal lapse, nor record the delete's cause.
- Obligations and fulfilment: open promises, deadlines, beneficiaries, "due and not
  kept". Future valid time is scheduling, not commitment (R9).
- Truthfulness/responsiveness of answers (R8), authorization of acts (R11), nonmonotonic
  defaults (R14).
- History-as-string pattern matching / bracket matching (R13).
- Git history. It is in futon1b only because `commit_ingest.clj` copies it in, stamped
  with the **author** date (`%at`, `commit_ingest.clj:465-468`); a rebased commit's
  author date predates its landing.
- Protection from its own erasure primitive: `erase-docs` removes a document "including
  through system time, for all valid-time" (D3; "even queries as of a previous
  system-time no longer return the erased data", D4). Not used in serving code today.

## 5. Surprises

- McCarthy's `exists` formula (l.1397-1398) binds `t'` inside the first conjunct's `∃`
  and uses it in the second conjunct; as typeset the `∀t''` clause has a free `t'`. The
  intended reading (one `∃t'` over both) is the one the notes' version at l.1666-1668 has.
- The paper's "Promises" subsection (l.2159) is a stub: "Searle" and one Searle
  condition. Future-directed commitment gets Leora's two-line objection (l.1801-1802)
  and one sentence (l.1998-2001) — the paper does not treat it.
- McCarthy's own input distinction "hearing that" vs "learning that" (l.269) maps onto
  XTDB's system time vs valid time more closely than anything else in the paper.
- XTDB's `at` is already R4: futon1b's clock lineage uses retract-then-put with valid
  time (`clock_lineage.clj:160-164`) — a working "made and not later cancelled" store for
  clocks, nowhere near the speech acts.
- Every stored Joe turn is `claim-type :question`, so the store's only act-type field is
  flat; and harness text sent under Joe's name is indistinguishable in the store.
- The one temporal read path that joins edges to evidence reads the evidence at *current*
  time (`graph.clj:1329-1333`); harmless while evidence is append-only, wrong once it
  is not.
- XTDB permits setting a transaction's system time (`BEGIN … SYSTEM_TIME`, "must not be
  earlier than any other transaction", D4), so system time is monotone but not strictly
  "when we learned it" if a writer sets it.
- `hyperedges?…&fields=id` returned `[{}]` rows with `:count 1` — an unknown field name
  yields empty rows rather than an error.
- The commit vertex for `5146606d` carries `repo "futon3c-d"`, not `futon3c`.

---
Reviewer check (claude-14, 2026-09-26): read futon1b_server.clj:128-141
(hyperedge retract = delete-docs with :valid-from), :875-880 (valid-as-of /
system-as-of query params), test_temporal.clj:116-129 (a retract at t2 hides
the edge now but not as of t1+1s), and elephant.tex:1395-1398 (exists(t,
commitment x) iff it arose at some t' < t and was not revoked between t' and
t). All four say what the report says.
