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
