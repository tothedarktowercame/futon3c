# Adapter two: a turn record's analysis as scope-view data

From claude-16 (E-scope-view, `holes/excursions/E-scope-view.md`, "Order of
work" item 4). Repo: /home/joe/code/futon3c, branch master, commit in place
(no worktree). Python only; no Clojure touched.

## Goal

One new script, `scripts/scope_view_turn_analysis.py`, that turns an
interpreted operator turn into the data shape `web/scopes/scopes.js`
renders (shape: E-scope-view.md, section "The data the view takes"). It
replaces the hand-rolled markup half of `scripts/turn_margin_html.py`
with a fixture the shared view draws. Do not edit `turn_margin_html.py`;
retiring it is a later step.

## Inputs

- Records: `~/.emacs-graph/session-turn-analysis/<record>.json` with the
  sidecar `<record>.json.analysis.json`. Read `turn_margin_html.py`
  (`load`, `fragments`, `mark_prose`, lines ~60-131) for how the two are
  read today; reuse by import rather than copying where you can.
- Text: the record's `source_text`. Offsets are code points, zero-based,
  end exclusive, as the record's `offset_unit` declares; assert it.

## What to draw (the rules are the margin page's, keep them)

- Segment: one, role `operator`, label from the record (session, time).
- Layer `analysis` ("interpretation").
- Spans: one per **display cue** of each fragment (`frag.display_cues`),
  not per fragment. Interpretation spans cover whole sentences and are
  never drawn (the "surplus" premise in `mark_prose`'s docstring).
  Overlapping cues: keep the first, same as `mark_prose`. Span `kind` is
  the fragment's intent; `note` is intent, target, rationale, pattern id
  when present; `weight` 1.0.
- A sentence with no fragments and an `unresolved_reason`: a span over
  the sentence at `weight` 0.25, kind `unresolved`, note the reason. (This
  is a choice, not in the margin page, which lists these as notes only.
  If you think it violates the surplus rule, leave it out and say so.)
- Edges: if a fragment carries `relations` (see how `turn_margin_html.py`
  lists them), draw them between the fragments' first cue spans with the
  relation as `relation` and nothing as `label`. If no record in the
  directory has `relations`, say so and draw none.
- Legend: an entry for every intent and relation that appears. Intents:
  take glosses from `futon3/src-cljs/futon3/turnfeed/core.cljs`
  `legend-rows` if the intent is a 象 mark, else the intent itself.

## CLI and output

`scripts/scope_view_turn_analysis.py RECORD.json [--out FILE]`, like
`scope_view_data.py`. Write one fixture to
`web/scopes/fixtures/analysis-<record-id>.json` for the newest record in
the directory that has at least three fragments with display cues.

Make it reachable on the page the way the existing fixtures are
(`web/scopes/index.html` picks a fixture from `?turn=`): read how that
works and use the same mechanism rather than a new parameter.

## Acceptance

1. `node web/scopes/test_scopes.mjs` green, extended to audit the new
   fixture with the same per-turn audit the other fixtures get (every
   span names its segment, offsets inside the text, legend defines every
   kind and relation).
2. Spot check written into your reply: for the chosen record, the count of
   fragments, cues drawn, cues dropped as overlapping, unresolved
   sentences; and one cue quoted with its offsets and the words at those
   offsets in `source_text`, so the offsets are seen to land.
3. Add a short dated paragraph to E-scope-view.md under a heading
   "Adapter two" saying what the script does and the spot-check numbers.
4. Commit (one or two commits), then bell claude-16 back with a summary
   and the commit shas:
   `python3 scripts/agency_send.py --from codex-23 --to claude-16 --kind bell`
   with the summary on stdin via a quoted heredoc.

Keep it to this. If `scopes.js` cannot draw something the data needs (a
weight below 1 on a span of a new kind, for instance), say what, do not
change the renderer.
