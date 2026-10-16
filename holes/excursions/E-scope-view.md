# E-scope-view: one view of segments, spans and edges, with four data sources

Date: 2026-10-15. Owner: claude-16. Status: building a demo. Joe
(2026-10-15, later): no need to ask Rob first, he has a good model of what is
needed; make the demo from what we know. Adapter one is written
(`scripts/scope_view_data.py`), fixture at
`web/scopes/fixtures/claude-16-turn-162.json`; the view module is out to
Codex.

## What this is

Issue 7 in mfuton-share asks for "A pluggable typescript view of scopes that
could be used in the developing VS Code Plugin and the Augmented Element view
of a room." Joe (2026-10-15) wants the same module to be a drop-in
replacement for the mark7 margin reading
(`wip/mark7-0708.2185-margin.html`) and for the 象 speech-act scope overlay
now drawn in Emacs (`futon3c/emacs/xiang-scope-overlay.el`), and to serve
whatever Rob has in mind for compiler scopes.

The four readers already in hand draw the same picture from different data:

| reader | text | spans | edges | notes beside the text |
|---|---|---|---|---|
| `xiang-scope-overlay.el` (Emacs, live REPL) | operator turn and reply, as the buffer shows them | 象 acts (`start`, `end`, `kind`) | `within` / `opens`, with a rule label (R3, OPEN) | help-echo: kind, rule, governing act's first words |
| `scripts/turn_margin_html.py` (turn margin page) | `source_text` of the turn record | display cues of each analysed fragment (not the fragment: it covers the sentence) | none drawn; `relations` listed in the note | intent, target, rationale, pattern id |
| mark7 margin page (LaTeXML output plus 29 KB of JS) | the rendered paper, each word placed at its source offset | S1 marks `[start, end, kind, grounded, tip]`; S3 node quotes `at`; S4 scope `lines` | proof edges `premises -> conclusion` with a warrant | per-region scopes, per-proof nodes; glossary defines every label |
| Rob's VS Code plugin / Element room view | a source file; a room's messages | compiler or notation scopes; whatever a message carries | parent / shadowing; reply-to | to be said by Rob |

What the four share is the view. What they do not share, and what makes
each heavy, is getting their text and offsets to agree:

- Of the Emacs module's 415 lines, about 250 relocate spans into a buffer
  whose text is not the record's (dictation prefix, tool lines, streaming).
  All six commits to it on 2026-10-15 fixed relocation, not scope logic.
- Of the mark7 page's painting script, the first half places every rendered
  word and formula at an offset in the run's source (`findWord`, `nextMath`,
  `blockStart`), because LaTeXML's `data-sourcepos` is per block and a
  formula's column is only approximate.
- The turn margin page has no relocation at all, because it renders the
  record's own text. That is why it is 300 lines, and why its principle
  ("most of a turn stays unmarked") came for free.

So the proposal is: the view owns the painting and the margin, and never
relocates. Relocation, where a host needs it, is an adapter's job, done
before the data reaches the view.

## The data the view takes

```json
{
  "segments": [
    {"id": "claude-16-turn-162", "role": "operator", "label": "joe · 23:23Z",
     "text": "BTW my previous turn came through all underlined, ..."}
  ],
  "layers": [
    {"id": "acts", "name": "speech acts", "ink": "c"}
  ],
  "spans": [
    {"id": "claude-16-turn-162-f-s1-0", "seg": "claude-16-turn-162",
     "start": 0, "end": 141, "layer": "acts",
     "kind": "report-problem", "mark": "㊩", "weight": 1.0,
     "note": "Joe reports that something the agent produced does not fit the world"}
  ],
  "edges": [
    {"from": "claude-16-turn-162-f-s1-1", "to": "claude-16-turn-162-f-s1-0",
     "relation": "within", "label": "R3"}
  ],
  "legend": {"report-problem": "...", "R3": "..."}
}
```

- `segments` are the texts. Offsets in `spans` are Unicode code points into
  one segment's `text`, zero-based, end exclusive (the unit the turn records
  already declare as `offset_unit`).
- `layers` let several readings of one text show at once. mark7's cyan /
  magenta / yellow plates are three layers; the ink mixing where they overlap
  is the view's, not the page's.
- `spans` carry a `kind` and an optional `mark`, a `weight` in [0, 1] (mark7
  inks an ungrounded S1 mark at half strength), and a `note` for the margin.
- `edges` are drawn between spans; `relation` and `label` are shown, not
  interpreted. A `to` that names a span outside `spans` is drawn as a stub
  with its id (the Emacs module already does this for a parent in an earlier
  turn).
- `legend` defines every `kind`, `relation` and `label` that appears. A label
  with no entry is shown as undefined, not hidden (mark7's rule).

Two ways to host it:

1. **Owned text.** The view renders `segments[].text` itself. Spans are
   exact. This is the turn page, the Element room view, and the 象 overlay
   when it moves to a page.
2. **Anchored text.** The host hands the view one element per segment, with
   the segment's coordinate range, and the view inks the elements it is
   given. This is the mark7 page and a VS Code editor (where the decoration
   API, not the DOM, does the inking; the same spans feed
   `createTextEditorDecorationType`). Placing words inside a block is the
   adapter's problem: mark7's `units` list, `{e, a, b}` per word, is exactly
   the anchored segment list, already computed.

What the view does with the data, in every host: ink spans by layer with
mixing; set a mark before a span that has one (what the Emacs overlay now
does for operator fragments, since underlining tiled fragments underlines
the turn); a margin note per span with a note, lit on hover or click of the
span and vice versa; a jump along an edge to its parent; a layer toggle and
legend. What it does not do: fetch, relocate, compute run statistics, or
know what 象, Lean or LaTeXML are.

## A fourth source: what was retrieved to support the agent's turn

Joe (2026-10-15): "Where this may start to become interesting is if we're
able to demonstrate some inference, e.g., if we had scopes that also tapped
into m40 memories that were retrieved to support the agent's turns."

Two retrievals already happen around every operator turn, and both leave a
record the adapter can join on:

- **Patterns.** Each turn's prompt line carries the patterns futon3a
  retrieved for it (`Prompt: pattern ~musn/intent-restatement (retrieved,
  score 0.4249; also ...)`). The record is a `context-retrieval` evidence
  entry in futon1b (author = agent, session id, body with `query` = the
  operator text's first 100 characters and `results` with id, title, score,
  rank). The adapter joins on the query prefix. In the view: one segment per
  pattern (title and conclusion line from its flexiarg), a `query` span over
  the operator text's first 100 characters, and `retrieved-for` edges
  labelled with the score.
- **Memory (minus40).** Nothing yet records which m40 facts supported a
  given turn: the SessionStart brief (`~/code/data/minus40/briefs/`) lists
  facts per session, not per turn, and the Claude Code memory index is loaded
  whole. So the adapter computes it after the fact: each reply paragraph
  (each 象 act on the agent side) is embedded with the same MiniLM model and
  sidecar `m40_near` uses, and its k nearest facts become segments, with
  `near` edges labelled score and the fact's standing at the turn's time
  (`in-force`, `retracted-since`, `not-in-force`, `not-yet-said`), computed
  from minus40's said / unsaid / said-again dates.

What the first fixture shows (turn 162, k = 2): the three pattern scores are
0.42, 0.32, 0.32; the twelve memory scores run 0.33 to 0.46 and the matches
are weak (a paragraph about the data shape lands nearest a memory about
keyboard firmware). The view inks a span by its weight, so these show faint,
which is the honest reading. The inference Joe wants needs a better signal
than post-hoc nearest neighbour over fact sentences: the per-turn retrieval
should be recorded when it happens (as the pattern retrieval already is),
and the m40 memories put in front of the agent at turn time, not the nearest
afterwards. That is the next adapter change, and it belongs on the minus40
side (a per-turn `context-retrieval`-shaped record for memory), not in the
view.

## What "drop-in" covers for the mark7 page

Covered by the view: the three plates and their toggles, ink mixing, the
per-word hover that reads every layer at a point, margin notes for regions
and proofs, the glossary rule.

Kept in the page, as its own adapter and banner: the run-level numbers in
the banner, the mock-verdict mode (it re-selects which spans exist, so it is
a second span set, switched by the adapter), the S1 group select (three
sub-layers of one plate: model as three layers), the defined-term classes
(a fourth layer), and word placement.

## Order of work (Joe agreed to the shape on 2026-10-15; Rob gate dropped later that day)

1. ~~Propose the data shape on issue 7 first.~~ Joe: not needed; build the
   demo. The draft comment below stays as a description for Rob once there
   is something to look at.
2. One framework-free TypeScript module, `renderScopes(data, host)`, owned
   text first, anchored second. No build step for the page use: compiled
   once to a single `scopes.js`.
3. Adapter one (done, `scripts/scope_view_data.py`): 象 acts plus the two
   retrieval sources above. `GET /api/alpha/xiang/scopes?session&turn` gives
   spans and edges but no text of its own. The operator text is the
   record's `source_text`, served by `GET /api/alpha/xiang/turns/<record-id>`
   (keyed by the record id, `turn-ecOiC4`, not the turn id,
   `claude-16-turn-162`; the adapter has to map one to the other), and the
   reply text comes from the reply record.
4. Adapter two: the turn record's analysis, replacing the hand-rolled
   markup in `turn_margin_html.py`.
5. Adapter three: mark7, replacing the second half of its painting script;
   the first half becomes the anchored-segment adapter.
6. Freeze `xiang-scope-overlay.el`: marks and jump-to-parent only, no more
   relocation fixes; retire it when the page is up in the REPL's place.

## Draft comment for issue 7

> Would this cover what you have in mind? We have three readers that draw
> the same picture from different data: 象 speech-act scopes over an
> operator turn (live, in Emacs), the mark7 margin reading of a paper
> (S1 marks, proof graphs, expository scopes as three ink plates), and a
> turn-analysis margin page. In each, the heavy part is relocating spans into
> text the view does not own; the view itself is the same: segments of text,
> spans into them with a kind, a mark and a note, edges between spans, a
> margin, hover and jump.
>
> Proposal: one framework-free TypeScript module that takes
> `{segments, layers, spans, edges, legend}` (offsets are code points into
> a segment's text) and renders into a host element, either owning the text
> or inking elements the host anchors to segments. It knows nothing about
> 象, Lean or LaTeXML; each of those is an adapter. The same spans would
> feed VS Code decorations, and an Element room is segments = messages.
>
> Two questions: does "scopes" in the issue mean compiler/notation scopes
> (the operator_scope port), speech-act scopes, or both? And what does the
> Augmented Element view need from a room that a plain message list does
> not have? Full shape and the readers compared:
> `futon3c/holes/excursions/E-scope-view.md`.

## The memory layer shows what reached the turn, not what resembles it (2026-10-16)

The first fixture's yellow layer drew minus40 facts nearest each reply
paragraph by embedding, with an "in-force" label from the fact's dates.
Neither says the agent saw the fact: for claude-16-turn-162 the nearest
facts were the Matrix bridge, a keyboard's drive file and a no-home-paths
rule at cosine 0.33 to 0.46, and the three most recent claude-16
transcripts contain no read of a memory file. Joe agreed on 2026-10-16
that the layer should show what reached the context, with resemblance
kept only as an opt-in that says what it is.

What `scripts/scope_view_data.py` draws now:

- **reached**: a minus40 tool call the agent made during the turn, from the
  Zai harness round record (`zai_api.clj` `persist-round!`, one
  `:turn-round` entry per round with `:calls`), and the memories whose
  names the call's result lists. The join is 象 record turn id = Agency
  job id = the harness `:turn-start` entry's `:dispatch-id`, whose
  `:turn-id` the rounds carry. Since futon3c c7408261b the round record
  keeps the full result text of minus40 reads (`:result-text`, capped);
  before it the 240-char preview kept two or three names of ten.
- **echoed**: a run of five or more words, or the memory's name, that the
  reply shares with a memory it reached. String match, no model. Whether
  the reply followed the memory in substance is a judgement; per the
  2026-10-15 rule it goes to a blind judge and an aggregate, not a drawn edge.
- A turn with no recorded call gets an empty layer whose name says so.
  Claude seats have the same tools but no instruction to use them, and the
  Claude Code transcript is not in the evidence store, so their layer is
  empty until a reader of that transcript is written.
- `--near` adds the old layer under the name "resembling the reply
  (resemblance, not use)".

First evidence, two Zai sessions belled from claude-16 with small real
checks (packets in `web/scopes/packets/`), fixtures
`invoke-1792122022238-15098-437a4a50` (zai-1) and
`invoke-1792122022748-15099-8635690e` (zai-2):

| seat | calls | memories reached | echoed in the reply | named in the reply |
|---|---|---|---|---|
| zai-1 (/tmp checkouts) | 3 | 4 | 0 | 0; the reply says none applied |
| zai-2 (park protocol) | 3 | 17 | 3 | 3, by name |

zai-2's reply used three memories (verify-running-before-parking,
park-deadlines-budget-the-acceptance-run, seat-bellbacks-look-like-mine)
and reported the record's disagreements with README-park.md. zai-1's two
`m40_find` calls (["tmp" "worktrees"], ["zone-health"]) returned no
memory: the store found hits, none of them memories. The /tmp rule lives
in CLAUDE.md and the zone-health check, not in any memory; the four
memories `m40_state` named for "tmp" were adjacent facts, and the reply
said so rather than citing them. The live check found 1654 checkouts
under /tmp, nearly all throwaway fixture repos.

Limits seen: `m40_state` for a common term returned 12k characters and the
round record cut it at 8192; 象's analysis of a Zai reply has no proforma
marks, so the reply has no act spans and echoes anchor on raw offsets; Zai
seats write no context-retrieval evidence, so the pattern layer is empty
for them.

## The memory layer over a session (2026-10-16, futon3c 8c5d549f2)

`scripts/scope_view_over_time.py` lays the same layer out across a
session: one row per 象 turn record, one column per memory any recorded
call ever named, and in each cell the strongest thing the record shows.
A cell is **retrieved-only** when the name (with up to three matching
lines) came back from `m40_find` and nothing read the statements;
**reached** when the statements came back (`m40_state`, `m40_asof`,
`m40_history`, `m40_near`); **echoed** when the reply shares a run of five
words or the name with it. The per-turn view did not make the first
distinction: it drew every named memory as reached. Each cell also says
whether the memory was in force at the reply, and each column carries the
memory's said / unsaid / said-again events from the store (the data
`m40_history` renders), so a memory withdrawn mid-session shows where
along the column it stopped. A turn with no recorded call is a row of
blanks, not a missing row: how often the seat consulted memory is part of
what the view shows.

Grids after two packets per seat (`web/scopes/fixtures/over-time-zai-{1,2}.json`;
`--md` prints them as a table; `web/scopes/index.html?session=zai-2` draws
one, see below):

| session | turns | turns with calls | memories named | echoed | reached | retrieved-only |
|---|---|---|---|---|---|---|
| zai-1 (25 turns since 10-09) | 25 | 2 | 13 | 7 | 5 | 1 |
| zai-2 (23 turns since 10-09) | 23 | 2 | 26 | 9 | 8 | 11 |

Per row:

| row | packet | named | echoed | reached | retrieved-only |
|---|---|---|---|---|---|
| zai-1 437a4a50 03:40Z | hook trailers (task 1) | 4 | 0 | 4 | 0 |
| zai-1 6f64d143 03:53Z | amend rule in shared trees | 9 | 7 | 1 | 1 |
| zai-2 8635690e 03:40Z | park / accepted (task 2) | 17 | 3 | 8 | 6 |
| zai-2 c99086c6 03:53Z | Kimi seats, record vs script | 11 | 6 | 0 | 5 |

The populated rows are the four turns belled on 2026-10-16 with an
instruction to use the tools; the 44 earlier turns in the two sessions
made no minus40 call. zai-2's six retrieved-only memories on the first row
are those `m40_find` listed for ["parking" "accepted"] and the like that
`m40_state` did not then surface; the three echoed are the three the reply
named. Two of zai-1's first four memories have unsaid facts
(blueprint-parallel-lemmas, 2 of 18; vsat-upstream-write-access, 5 of 17)
but were still in force at the reply, so no cell is marked not-in-force
yet. (The count of 17 for zai-2's first row corrects an 18 first written;
the per-turn fixture has 22 segments: two texts, three calls, 17 memories.)

What the second rows show. The two seats were given packets chosen to
overlap the first in memory neighbourhood for zai-2 and not for zai-1, and
the grids come out that way: zai-1's two rows share no column, zai-2's
share two. Those two are the point of the view. proof2-dispatch-pool was
retrieved-only on zai-2's first row (listed by `m40_find`, statements not
read) and echoed on the second (read by `m40_state` and quoted: "HTTP 403
on every seat"); mfuton-lean-compiler-project went the other way, reached
on the first row and only listed on the second. On zai-1's second row the
reply named seven of the nine memories it was given, and the two it did
not name differ in kind: futon1b-504-storm-0930 was reached (its text came
back from `m40_state`) and not used, proof2-dispatch-pool was only listed.
The Kimi row (zai-2, second) has no reached-but-unechoed cell at all: the
reply quoted a run of words from every memory whose statements it read.

A false echo found and removed. proof2-dispatch-pool first came out
**echoed** on zai-1's second row with the evidence
"never-git-stash-in-shared-futon-trees": its text links to that memory as
`[[never-git-stash-in-shared-futon-trees]]`, the reply named that memory,
and the seven hyphen-split words matched as a five-word run. A link names
another memory; it is not this memory's own words. `echoes` in
`scope_view_data.py` now drops `[[...]]` before matching (the displayed
text keeps its links). The same fix removed two echo spans from the
per-turn fixture for zai-2's first turn, where verify-running-before-parking
had read as echoed through its link to seat-bellbacks-look-like-mine.
Both are in the regenerated fixtures.

The grid on the page. `web/scopes/grid.js` draws a fixture as a table
(`renderGrid`), with the four cell marks of the `--md` table, a yellow cell
for a memory not in force at the reply, a column key by `cN` on the side,
and a click on a column header or key entry listing that memory's said /
unsaid / said-again events. Rows with calls link to the per-turn view of
that turn (`?turn=<id>`), so the grid and the per-turn view read on one
page: `index.html?session=zai-2` for the grid, a row click for the turn.
`auditGridData` and `tallyGrid` check a fixture (every cell names a row
and a column, the legend defines every level and status, a row's memory
count is the sum of its cells, a row with no call has no memory);
`test_scopes.mjs` runs them over both grids beside the per-turn audits.
The renderer was exercised against a minimal DOM shim in node (no browser
on this host): 13 and 26 column headers, 325 and 598 cells, two row links
per grid, a column click lights its header.
