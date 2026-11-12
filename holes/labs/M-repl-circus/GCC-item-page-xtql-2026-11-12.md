# The item page as XTQL, and what each query lets us infer (claude-13, 2026-11-12)

Joe, on the readable item page in Mojira (demo 1, round 2): "Those look fine
to me but let's see what inferences they support, i.e., what queries would
these correspond to in XTQL? (We don't need to run it yet b/c we haven't
integrated the stores yet.)"

## The assumption every query below rests on

E-gcc-schema B1: GCC records are Arxana records. In futon1b they live in
`arxana_clusions`, `arxana_evidence` and `arxana_assessments`, with the record
inside `record/data` (futon1b `futon1b_arxana*.clj`). XTQL cannot filter
inside nested maps (GCC-xtql note §3), so today every read goes through the
Arxana index (B1 "Reads"). For the queries below to run as XTQL, **the writer
stores each record's type and role endpoints as flat columns** beside
`record/data`, as futon1b already does for hyperedges (`prop/mission`,
`prop/repo`): `clusion/type`, `role/<role-name>`, `subject/clusion-id`,
`attest/basis`, `attest/at`, `assess/verdict`, `assess/actor`. That is a
design requirement this exercise surfaced; it is not built.

All queries are parameterised (`futon1b-xt/pq`), bounded, and use XTDB's
valid time, so "as of" a date is the same query with `valid-as-of`.

## 1. Header: the item and its state

```clojure
;; the item node
(-> (from :arxana_clusions [xt/id clusion/type role/item record/data])
    (where (= clusion/type :workplan/item) (= role/item $item)))
;; its state group: the newest state-group attestation on the item
(-> (from :arxana_evidence [subject/clusion-id attest/type attest/at record/data])
    (where (= subject/clusion-id $item-clusion) (= attest/type :giraffe/state-group))
    (order-by {:val attest/at :dir :desc}) (limit 1))
```
**Infers:** what state the item is in, and since when (valid time), so
"what changed state this week" is the same query over a time window.

## 2. What Giraffe read: the current reading and its lineage

```clojure
;; readings of this item, newest first, with the document version each read
(-> (from :arxana_clusions [xt/id clusion/type role/item role/source-sha record/data])
    (where (= clusion/type :giraffe/reading) (= role/item $item))
    (order-by {:val xt/valid-from :dir :desc}) (limit 50))
;; the item's current document version
(-> (from :arxana_evidence [subject/clusion-id attest/type attest/source-sha attest/at])
    (where (= subject/clusion-id $item-clusion) (= attest/type :git/doc-version))
    (order-by {:val attest/at :dir :desc}) (limit 1))
```
**Infers:** whether the latest reading is of the current version. Over all
items (one query per type, joined in Clojure): **which items Giraffe has not
read since they last changed**, the "Giraffe is always up to date" invariant
(M-repl-circus §DERIVE) and the War Machine's WM-1. The supersession chain
(`[:supersedes 1]`) shows how an item's purpose moved between versions.

## 3. Waiting on: needs, and whether they are met

```clojure
;; what this item needs
(-> (from :arxana_clusions [xt/id clusion/type role/needing role/supplier record/data])
    (where (= clusion/type :needs) (= role/needing $item-clusion)))
;; then each supplier's state group (query 1, per supplier)
```
`met` is computed, never stored (B4): a closure need is met when the
supplier's state group is `completed`; an artifact need when it is
`started` or `completed`.
**Infers:** **is this item ready** (all needs met), the Definition of Ready
and WM-2. Following `role/needing` → `role/supplier` repeatedly (bounded, in
Clojure): **what an item is transitively blocked by**, and the cycles.
Reversed (`(= role/supplier $item-clusion)`): **what this item unblocks**,
which says how much finishing it is worth.

## 4. Related

```clojure
(-> (from :arxana_clusions [xt/id clusion/type role/from role/to])
    (where (= clusion/type :relates-to) (= role/from $item-clusion)))
```
**Infers:** the item's neighbourhood; with section 3, the connected
component an item sits in (B4(b) links never enter readiness).

## 5. Why it matters: benefit lines and the question

```clojure
;; benefit lines this item serves
(-> (from :arxana_clusions [xt/id clusion/type role/item role/benefit-line])
    (where (= clusion/type :workplan/serves) (= role/item $item-clusion)))
;; the question it serves (no such link exists yet: the IDENTIFY gap)
(-> (from :arxana_clusions [xt/id clusion/type role/item role/question])
    (where (= clusion/type :question/served-by) (= role/item $item-clusion)))
```
**Infers:** reversed, **which items serve a given question or benefit
line**, and over all items **which items serve no question** (L-1: a finding)
and **which questions no item serves** (a gap in the work, or a question
nobody is working on). With section 3: **the ready items that serve a
question**, which is a candidate "next" list with its reason attached (WM-4,
L-2's "safe next").

## 6. Names

```clojure
(-> (from :arxana_clusions [xt/id clusion/type role/item role/entity])
    (where (= clusion/type :workplan/names) (= role/item $item-clusion)))
```
**Infers:** reversed, **every item that names an entity** (e.g. all work
touching `minus40`), the lexicon's view of the backlog; the entity's
newest status assessment comes from `arxana_assessments` under the named
choosing policy (B1, still open).

## 7. Mentioned

```clojure
(-> (from :arxana_clusions [xt/id clusion/type role/item role/turn attest/at attest/who record/data])
    (where (= clusion/type :workplan/mention) (= role/item $item-clusion))
    (order-by {:val attest/at :dir :desc}) (limit 100))
```
**Infers:** how recently and by whom an item is discussed. With section 1:
**items discussed but not moving** (mentioned this week, state unchanged)
and **items moving but not discussed**. With the turn records: **which
items a turn bears on**, the link the turn projector would write at
`:settled` and the missing local-to-global step in M-repl-circus IDENTIFY.

## Inferences that need more than one section

| Question | Sections | Who asks |
|---|---|---|
| What is ready and worth doing, with its reason? | 3 + 5 (+ 2 for freshness) | War Machine (WM-2, WM-4), LOI L-2 |
| What has Giraffe not read since it changed? | 1 + 2 | Giraffe's invariant, WM-1 |
| What is blocked, by what, and what would unblock most? | 3 (both directions) | Joe, War Machine |
| Which items serve no question; which questions have no items? | 5 over all items | L-1, IDENTIFY |
| What is talked about but stuck? | 7 + 1 + 3 | Joe, 猴's operator focus |

## What XTQL does not do here, and who does it

Following chains (transitive blockers, components, cycles) and computing
`met` happen in Clojure over what these queries fetch, as Giraffe's pass
already does (GCC-xtql note). Counts over all items are kept as derived
records with a watermark (B1 "Aggregates are materialised"), not computed
per page.
