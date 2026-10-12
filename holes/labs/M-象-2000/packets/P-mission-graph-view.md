# Packet: a dependency-graph view of one mission's acts (KnowTeX-style)

From claude-16 (owner, M-象-2000), 2026-10-12. Mode: work. Python 3.12, no
third-party packages, no graphviz on this host (`dot` is absent): the layout
is yours to compute. Work under `holes/labs/M-象-2000/graph/` only. Read-only
everywhere else; never touch `~/.emacs-graph` records or any store.

## Why

Joe asked (2026-10-12) for front-end work on 象 inspired by KnowTeX
(arXiv:2601.15294): a tool that draws definitions, theorems and proofs as a
dependency graph from LaTeX, with **manual** edges (`\uses{}`) and **inferred**
edges (deterministic D-rules, heuristic H-rules), solid / dashed / dotted edge
styles by edge kind, cycles found by Tarjan and drawn red, optional transitive
reduction, and an **edge review table** (source, target, type, location,
rule) where the author deletes or adds edges and exports the corrected
annotations back. 象 has the same two sources of edges over acts: the
labelled incidences from the Zai campaign (manual) and the kernel's port
closes (inferred). Joe has no way yet to say which edges are right; the
review table is how that measurement starts.

## Inputs (all exist; read them first)

- Records: `~/.emacs-graph/session-turn-analysis/turn-*.json` (skip names
  with a second `.json`, e.g. `.analysis.json`). Fields used: `mission_id`,
  `session_id`, `created_at`, `agent_id`, `turn_id`, `acts` (list of
  `{id, kind, text, author?}`, text is a 120-char prefix),
  `ports.closed_this_turn` (list of `{act, kind, text}`),
  `ports.still_open` (same plus `since`), `ports_status.rules_version`.
  Mission join: reuse `brief/resume_brief.py:load_records(directory, mission,
  seats, since, join, excursions)` by import (`--join seats` picks the seats
  that carry the mission); do not copy it.
- Labelled sidecars: `data/moves-*/shard-*/turn-*.moves.json` and
  `data/moves-pilot/*.moves.json`, schema `moves/1`
  (`moves/moves.schema.md`): per act `move`, `quote`, `answers[] {act,
  quote, effect closes|keeps|unclear}`, `scope {opens, closes, within}`,
  `confidence`; top-level `labeller`, `at`. Full act texts are in the input
  beside each sidecar (`input/<record>.input.json`: `fragments`,
  `reply_acts`, `previous_reply_acts`, `open_ports`).

## Build

`graph/build_mission_graph.py --mission M --join auto|mission|seats
--out PATH.html [--reduce] [--since ISO]`

Nodes: every act of the mission's records (operator fragments `-f-`, reply
paragraphs `-r-`) plus any act an edge points at (an earlier opener). Label:
kind, first 60 chars of text, author (operator | agent id), record id, time.

Edges, each with a provenance:
- `answers/closes` (labelled): sidecar answer with effect closes. Solid.
- `answers/keeps` (labelled): effect keeps. Dashed.
- `answers/unclear` (labelled): dotted grey.
- `scope/within` (labelled): governed act → opener. Dotted.
- `scope/closes` (labelled): closing act → opener. Dotted, marked as a close.
- `port/close` (kernel): for each `ports.closed_this_turn` entry on a record,
  an edge from the record's reply node (use the record's first `-r-` act when
  it has one, else a per-record node `turn-<id>/reply`) to the closed act.
  Solid, a second colour. Provenance: `kernel`, `rules_version`.
Edge id: a short hash of (type, source, target), stable across runs. Edge
title (hover): provenance (labeller and `at`, or kernel rules version), the
verbatim quote when labelled, the record id.

Where a labelled edge and a kernel edge coincide (same source record, same
target, both a close), draw one edge and mark it `agreed` in the table.

Cycles: Tarjan SCCs over the answers+scope+port digraph; edges inside an SCC
of size > 1 red. `--reduce`: transitive reduction of the `scope/within`
edges only (answer edges are evidence, not a partial order).

Layout (pure Python SVG, no external libraries, no CDN): one column per
session, columns ordered by first record time; within a column nodes in time
order, operator acts flush left, agent acts indented; edges as cubic paths;
cross-column edges allowed. Inline a minimal pan/zoom (drag + wheel on the
SVG viewBox) in a few lines of JS. Legend for the edge styles and colours.

Edge review table (below the SVG): one row per edge: id, type, source (id +
text), target (id + text), provenance, record, quote, and a three-way verdict
control (`right` / `wrong` / unsure, default unsure); a short form to **add**
an edge (source id, target id, type); a button **Export decisions** that
downloads `decisions-<mission>-<ISO>.json`:
`{"mission", "generated_at", "graph_built_at", "verdicts": [{"edge_id",
"type", "source", "target", "provenance", "verdict"}], "added": [{"source",
"target", "type"}]}`. Document this format in `graph/README.md` so a later
script can score kernel vs labelled edges against Joe's verdicts.

## Tests (`graph/test_build_mission_graph.py`, run `python3 -I`)

- One live pin: record `turn-07igHH` closes port `claude-17-turn-48-f-s1-0`
  (verify by reading the record first; use whatever the record actually says,
  verbatim) → exactly one `port/close` edge with that target.
- One sidecar-derived edge from a real sidecar (pick one, quote its ids).
- Tarjan: a three-node cycle plus a tail → exactly the three cycle edges red.
- Transitive reduction: a→b→c with a→c removed when `--reduce`, kept without.
- The HTML contains as many table rows as edges.

## Acceptance

`python3 -I graph/build_mission_graph.py --mission M-象-2000 --join seats
--out graph/out/M-象-2000.html` writes the file; report edge counts by type
and provenance, agreed edges, SCC count, and the time it took. Add
`graph/out/` to a `.gitignore` in `graph/`; commit the script, test, README
and .gitignore only (touched files only, never `-a`). If this runs past 60
minutes, ship the graph and table without the add-edge form and say so.
Bell claude-16 back with the sha and the counts.
