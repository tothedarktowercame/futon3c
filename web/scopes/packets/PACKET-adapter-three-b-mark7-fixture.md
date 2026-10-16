# Adapter three (b): the mark7 margin data as scope-view data

From claude-16 (E-scope-view, `holes/excursions/E-scope-view.md`, "Order of
work" item 5). Two repos, both branch master, commit in place (no
worktree): the adapter lives in /home/joe/code/futon6, the fixture it
writes lives in /home/joe/code/futon3c. Python only. futon6 has someone
else's uncommitted edits (`scripts/mark3_expository_loop.py`, a golden
json): do not touch or commit them.

## Goal

One new module, `futon6/scripts/scope_margin_scopes.py`, with a pure
function `scopes_data(payload, source) -> dict` that turns the margin
page's data (the `payload` dict `render_scope_margin.build` assembles, and
the paper source it embeds) into the shape `futon3c/web/scopes/scopes.js`
renders (shape: E-scope-view.md, section "The data the view takes"; an
instance: `futon3c/web/scopes/fixtures/analysis-turn-OfO380.json`). It
replaces the second half of `scope_margin/plates.js` (lines 139-198, the
`plate` function) as data; the page wiring is a later packet. Do not edit
`render_scope_margin.py` or `plates.js`.

## Inputs

The published page holds both blocks: `/var/www/zone.hyperreal.enterprises/wip/mark7-0708.2185-margin.html`,
`<script id="m7-data" type="application/json">` (payload) and
`<script id="m7-source" ...>` (source, a JSON string). The CLI reads the
page: `scope_margin_scopes.py PAGE.html --out FIXTURE.json`. The run
itself is `/home/joe/runs/mark7master-20260921` if you need to look.
Offsets in `payload.marks` and in proof nodes' `at` are code points into
`source`, zero-based, end exclusive; `payload.starts[i]` is the offset of
line i+1 (lines are 1-based in `lines` fields). Check one mark before
relying on it: `marks[0]` = `[9553, 9588, ...]` is "Let $\ck$ be an
accessible category".

## What to draw (plates.js's rules, as data)

- One segment, id `mark7-0708.2185`, role `paper`, label
  `"0708.2185 · <summary.run>"`, text = source.
- Three layers: `s1` "S1 source marks" ink `c`; `s3` "S3 proof graph" ink
  `m`; `s4` "S4 expository scopes" ink `y`.
- S1: one span per mark. `kind` = the kind name (`payload.kinds[k][0]`),
  `weight` 1.0 if grounded else 0.5 (plates.js inks an ungrounded mark at
  half), `note` = the tip. Id `s1-<index>`.
- S3: one span per proof node per `at` range (every node has exactly one
  today; handle a list). `kind` = node kind, `weight` 1.0, `note` =
  `"<gloss> · <verdict>"`. Id `<proof id>-<node id>`. The faint shading
  plates.js gives a whole proof body (0.3) and a refused proof (0.12) is a
  region effect, not a span: leave it out and say so in the note. The
  mock-verdict re-selection is a later adapter: leave it out.
- S3 edges: one per premise of each proof edge, `from` = premise node's
  span, `to` = conclusion's span, `relation` = the edge's `relation`
  (e.g. `iff`), `label` = its `warrant`.
- S4: one span per scope. Range = the scope's `span` when present
  (`weight` 1.0), else the lines `[starts[lo-1], starts[hi])` clipped to
  the text (`weight` 0.6). `kind` = scope kind, `note` = fill, then flags
  and the mock verdict. Id `<region id>-<scope>`.
- Legend: every kind, relation and label defined. S1 kinds from
  `glossary["S1 mark kinds"]`, S3 node kinds from `"S3 node kinds"`,
  warrants (labels) from `"S3 warrants"`, S4 kinds from `"S4 scope kinds"`.
  Edge relations (`iff` etc.): if `iatc_json` or the S3 vocabulary has a
  meaning table for relations use it, else the relation name itself; say
  which in the reply.
- `spot_check`: marks, nodes, edges (premise pairs), scopes drawn; and
  the summary's own counts beside them (3253 marks, 122 nodes, 54 edges,
  136 scopes) so a drop is visible.

## Acceptance

1. `futon6/tests/test_scope_margin_scopes.py` with the venv
   (`.venv/bin/python -m pytest -q tests/`; all of `tests/` green). Build
   a tiny payload by hand (two marks, one proof with two nodes and one
   edge, one region with a scope with and without `span`) and pin: span
   count, the ungrounded mark's weight, the edge's from/to/label, the
   lines-only scope's range from `starts`, and that every kind and label
   in the output has a legend entry. The bad case: a mark whose end runs
   past the text must raise, not be emitted.
2. Write the fixture to `futon3c/web/scopes/fixtures/mark7-0708.2185.json`
   and run `node futon3c/web/scopes/test_scopes.mjs`: its per-fixture
   audit (spans inside the text, edge destinations resolve, legend
   defines every kind and relation) must pass on it unchanged. Do not
   edit the test file; if the audit fails, the fixture is wrong.
3. Spot check in the reply: the four drawn counts beside the summary's
   counts, and one mark and one S4 span quoted with offsets and the words
   at those offsets in `source`.
4. One commit in futon6, one in futon3c; bell claude-16 back with a
   summary and both shas:
   `python3 /home/joe/code/futon3c/scripts/agency_send.py --from codex-68 --to claude-16 --kind bell`
   with the summary on stdin via a quoted heredoc.

Keep it to this. If `scopes.js` cannot draw something the data needs, say
what, do not change the renderer (codex-23 is editing it in parallel).
