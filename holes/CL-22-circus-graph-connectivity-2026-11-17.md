# CL-22 — Circus graph connectivity survey (2026-11-17)

## Result

**No: the live Circus records surveyed do not yet form one connected
component containing Giraffe, and port 7073 does not expose an XTQL query
surface.**  The storage representation is ready for the joins: endpoint kinds
are flattened to the same scalar endpoint ID.  In particular, 象's
manifestation-scope joins the same turn ID used as an external reference by
驴, 馬, 狗 and 猴.  Likewise, Giraffe's external-reference item endpoints carry
the exact `[:workplan/item 1]` clusion ID used by 猴 and `[:supersedes 1]`.
Those are different endpoint *kinds*, not parallel identities.

In the five most recent turn files with result fields for all four inference
writers, every 猴 header joined the turn component, but none had a workplan
mention or a 猴 focus-relation.  Therefore no item node, and hence no Giraffe
edge, was reachable from those turn components.

## Node identity table

The examples below are invented shapes.  No live turn, item, pattern, memory,
mission, or operator text appears in this report.

| Writer | Edge type | Endpoint role | Endpoint kind | ID scheme (invented shape) | Identity note |
|---|---|---|---|---|---|
| 驴 | `[:pattern/proposal 1]` | `:turn` | `:external-ref` | `arxana:record:sha256:TURN…` | Same turn reference ID as 馬/狗/猴 and 象's revision ID |
| 驴 | `[:pattern/proposal 1]` | `:pattern` | `:external-ref` | `arxana:record:sha256:PATTERN…` | Same pattern reference scheme as 馬 |
| 馬 | `[:pattern/occurrence 1]` | `:pattern` | `:external-ref` | `arxana:record:sha256:PATTERN…` | Joins 驴 by pattern |
| 馬 | `[:pattern/occurrence 1]` | `:turn` | `:external-ref` | `arxana:record:sha256:TURN…` | Joins turn component |
| 狗 | `[:memory/recall 1]`, `[:memory/use 1]`, `[:memory/burial 1]`, `[:memory/repetition 1]` | `:turn` | `:external-ref` | `arxana:record:sha256:TURN…` | Joins turn component |
| 狗 | same four types | `:memory` | `:external-ref` | `arxana:record:sha256:MEMORY…` | Memory identity |
| 象 | `[:interpretation 1]` | `:source` | `:manifestation-scope` | `artifact/revision-id = arxana:record:sha256:TURN…` | Different kind/key, deliberately the same scalar turn ID |
| 象 | `[:interpretation 1]` | `:intent` | `:entity` | `arxana:entity:sha256:INTENT…` | Intent entity |
| 象 | `[:interpretation 1]` | `:target` | `:external-ref` | `arxana:record:sha256:TARGET…` | Target reference |
| 猴 | `[:hou/focus 1]` | `:evidence` | `:external-ref` | `arxana:record:sha256:TURN…` | Directly joins the turn component |
| 猴 | `[:hou/focus-relation 1]` | `:focus` | `:clusion` | `arxana:clusion:sha256:FOCUS…` | Joins the 猴 header by its clusion ID |
| 猴 | `[:hou/focus-relation 1]` | `:item` | `:clusion` | `arxana:clusion:sha256:ITEM…` | Exact deterministic `[:workplan/item 1]` clusion ID |
| 猴 mention | `[:workplan/mention 2]` | `:evidence` | `:external-ref` | `arxana:record:sha256:TURN…` | Alternative turn-to-item bridge |
| 猴 mention | `[:workplan/mention 2]` | `:item` | `:clusion` | `arxana:clusion:sha256:ITEM…` | Same item clusion ID |
| Giraffe | `[:needs 1]` | `:needing`, `:supplier` | `:external-ref` | `ref/id = arxana:clusion:sha256:ITEM…` | Different kind/key, deliberately the same scalar item ID |
| Giraffe/inventory | `[:supersedes 1]` | `:successor`, `:predecessor` | `:clusion` | `arxana:clusion:sha256:ITEM…` | Same item clusion ID |

The writers establish these identities at
`src/futon3c/arxana/pattern_proposals.clj:25,32-35`,
`src/futon3c/arxana/pattern_occurrences.clj:34-39`,
`src/futon3c/arxana/dog_memory.clj:67-115`,
`src/futon3c/arxana/xiang_interpretations.clj:58-68`,
`src/futon3c/arxana/turn_mentions.clj:65-70`,
`src/futon3c/circus/hou.clj:14-16,61-68,122-128`,
`src/futon3c/arxana/giraffe_needs.clj:74-84`, and
`src/futon3c/arxana/item_inventory.clj:18-20,110-115`.

futon1b confirms that the kind distinction does not block a join.  Both the
sidecar and XTDB-derived columns choose the first of `entity/id`, `ref/id`,
`artifact/revision-id`, and `clusion/id`, returning one scalar string
(`/home/joe/code/futon1b/futon1b_arxana_index.clj:39-44` and
`/home/joe/code/futon1b/futon1b_arxana.clj:211-227`).

## Bounded live walk

I selected the five most recent turn records which contained all four result
fields at survey time and called them T1–T5.  Starting at each derived turn
reference ID, bounded `:role-reverse` reads (`limit 200`) produced:

| Turn | 驴 proposal | 馬 occurrence | 狗 recall | 象 interpretation | 猴 focus | workplan mention | Reached component |
|---|---:|---:|---:|---:|---:|---:|---|
| T1 | 3 | 6 | 3 | 2 | 1 | 0 | `{驴 馬 狗 象 猴}` |
| T2 | 1 | 0 | 0 | 1 | 1 | 0 | `{驴 象 猴}` |
| T3 | 3 | 0 | 3 | 4 | 1 | 0 | `{驴 狗 象 猴}` |
| T4 | 3 | 6 | 3 | 3 | 1 | 0 | `{驴 馬 狗 象 猴}` |
| T5 | 3 | 0 | 3 | 2 | 1 | 0 | `{驴 狗 象 猴}` |

For T1 and T4, 狗 also had three `[:memory/use 1]` edges each and zero burial
or repetition edges.  A second hop from each 猴 header found zero
`[:hou/focus-relation 1]` edges.  Consequently each observed component ended
at the turn/header layer.  `{Giraffe}` remained in the item graph and was not
joined to any of T1–T5.  The zero 馬/狗 cells agree with writer result failures
or empty results in those turn records; they are missing records, not failed
ID joins.

This used 62 read-only HTTP reads in total, including two rejected API-shape
probes and repeated evidence reads while checking the walk.  It made no
command calls, store writes, rebuild request, deep
health request, JVM reload, or process signal.

## XTQL surface

There is no arbitrary XTQL endpoint on port 7073.  The authenticated
`POST /api/alpha/arxana/query` route exposes only `:role-reverse`,
`:attestations`, `:assessments`, `:attempts`, `:pattern-score`,
`:record-by-id`, `:artifact-head`, `:revision-by-id`, and
`:clusion-endpoints` for reads (plus the mutating `:rebuild`, which this survey
did not call): `/home/joe/code/futon1b/futon1b_server.clj:1097-1138`.  The
server registers the command and query routes, but no XTQL route, at
`futon1b_server.clj:1773-1775`.  The sidecar's role-reverse query is a bounded,
single-edge exact lookup (`futon1b_arxana_index.clj:189-202`).

The intended three-animal XTQL query would be a join over the already-flat
columns, equivalent to:

```clojure
(unify
  (from :arxana_clusions [{:xt/id lu, :clusion/type :pattern/proposal,
                            :role/turn turn, :role/pattern pattern}])
  (from :arxana_clusions [{:xt/id ma, :clusion/type :pattern/occurrence,
                            :role/turn turn, :role/pattern pattern}])
  (from :arxana_clusions [{:xt/id dog, :clusion/type :memory/recall,
                            :role/turn turn, :role/memory memory}]))
```

It asks for turns where 驴 proposed pattern P, 馬 recorded P on the same turn,
and 狗 recalled a memory on that turn.  It was **not run**, so there is no row
count or execution time: exposing or locating a server-side query runner would
be required.  futon1b already contains a parameterized XTQL helper for one
type/role lookup, but explicitly labels it “not wired to a route yet”
(`/home/joe/code/futon1b/futon1b_arxana.clj:257-276`).

## Smallest gaps, ordered by packet

1. **猴 packet — emit the existing bridge when focus is non-empty.** No new
   type or ID is required.  A `[:workplan/mention 2]` or
   `[:hou/focus-relation 1]` joins the turn/header component to the deterministic
   item clusion.  The surveyed turns simply had neither.
2. **Giraffe packet — no ID unification needed.** Preserve the current rule
   that `[:needs 1]` external refs contain the exact item clusion ID.  Once 猴
   reaches an item, `:needs` and `:supersedes` are joinable on that scalar ID.
3. **futon1b query packet — expose a bounded, read-only prepared XTQL join.**
   The flat `clusion/type` and `role/*` columns already exist
   (`futon1b_arxana.clj:229-243`).  Add an allow-listed prepared operation (not
   arbitrary submitted code), with limit and timeout, that can join three or
   more edge aliases.  This is the missing inference surface.
4. **Writer completeness packet — surface partial animal writes as such.**
   Three of five recent records lacked 馬 edges and one also lacked 狗 recall.
   Connectedness cannot be guaranteed merely by compatible identity schemes;
   packet completion must either write every required edge or expose the
   partial component explicitly.

