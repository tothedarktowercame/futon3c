# Adapter three (c): the data the mark7 page needs before plates.js hands over its inking

From claude-16 (E-scope-view, `holes/excursions/E-scope-view.md`, "Adapter
three", the (b) review). Two repos, both branch master, commit in place
(no worktree): the adapter and its test in /home/joe/code/futon6, the
fixture in /home/joe/code/futon3c. Python only. Do not edit
`render_scope_margin.py`, `plates.js`, `scopes.ts`, `scopes.js` or
`test_scopes.mjs`. futon6 has a stranger's uncommitted edit (a golden
json under `data/showcases/`): leave it alone.

## Why

(b) is accepted: futon6 43bebf7, futon3c fdf16e6, and a test pin of mine,
futon6 5131c71 (the hand fixture's quoted scope now covers the words
"quoted scope"; keep that). Wiring the page to `renderScopes` in anchored
mode is the next packet, (d). The review found two ink rules of
`scope_margin/plates.js` that the (b) data cannot express, and the page
would lose both the moment plates.js stops inking:

1. plates.js shows one S1 group at a time (the select: symbols and terms /
   clauses / environments and displays), because all 3253 marks at once
   is a cyan wash. The view's only visibility control is the layer
   toggle. So the groups become layers (E-scope-view, "What drop-in
   covers": "three sub-layers of one plate: model as three layers"; the
   payload in fact has four groups).
2. plates.js inks a proof body S3 read at 0.3, a refused proof at 0.12,
   and a carved region at 0.3 (0.14 inside a proof) when nothing stronger
   is there (an else-if chain, `plate` at lines 150-162). `mixInk` in
   `scopes.ts` takes the maximum weight per ink, so faint spans beneath
   strong ones give that chain exactly. These become spans; (b)'s
   `omitted_region_effects` goes.

## Change 1: S1 layers by group (`scripts/scope_margin_scopes.py`)

`payload.kinds[k]` is `[name, group, meaning]`. On the real page the
groups are term (2072 marks), region (621), clause (500), structure (60);
plates.js's select never offered structure, the view will.

- Layer id `s1-<group>`, ink `c`, name `S1 marks: <gloss>` with gloss
  term -> "symbols and terms", clause -> "clauses", region ->
  "environments and displays", structure -> "structure". Emit a layer
  only for a group that has a mark, in the order term, clause, region,
  structure; then `s3`, `s4` as now. A group outside those four raises.
- Each S1 span's `layer` is its group's layer.

## Change 2: region effects as faint spans

Lines are 1-based; `[starts[lo-1], starts[hi])` clipped to the text, as
the lines path of `_scope_range` already does. Factor that into a
`_lines_range(lo, hi, starts, length)` both use. `_range` keeps refusing
an empty or out-of-range result.

- Proof body: every proof note (`lo`, `hi`) -> layer `s3`, kind
  `proof-body`, weight 0.3, id `<proof id>-body`, note "proof S3 read;
  its nodes quote the darker ranges". Legend `proof-body`: "inside a
  proof S3 read, not quoted" (plates.js's own swatch text).
- Refused proof: notes of type `refused` (`id`, `lo`, `hi`, `status`,
  `why`) -> layer `s3`, kind `proof-refused`, weight 0.12, id
  `<id>-refused`, note `<status> · <why>`. Legend `proof-refused`: "a
  proof S3 read whose graph was refused". This run has none; the hand
  fixture must have one.
- Carved region: `payload.carved` is `[{id, type, section, lines}]` ->
  layer `s4`, kind `carved/<type>`, weight 0.14 if type is `in-proof`
  else 0.3, id `carved-<id>`, note `<section> · L<lo>–<hi>`. Legend from
  `glossary["This page"][type]`, and a type with no entry raises, like
  an unknown mark kind does.
- `spot_check.drawn` gains `proof_bodies`, `refused`, `carved`;
  `spot_check.summary` gains `proofs` (summary.proofs) and
  `carved` (summary.carving.now.regions). Drop `omitted_region_effects`.
  Expected on the real page: 14 proof bodies, 0 refused, 27 carved (1
  in-proof).

Mock-verdict re-selection stays out (it is a second span set, a later
packet). Do not scale any weight for legibility; if full-strength ink
reads badly over the paper, that is for (d) to report.

## Acceptance

1. `tests/test_scope_margin_scopes.py`, both existing tests still green,
   extended to pin: a mark of group `clause` lands in layer `s1-clause`
   and the layer list names each present group once, in the order above,
   followed by `s3`, `s4`; a proof body's range from `starts` and weight
   0.3; a refused proof at 0.12; a carved `in-proof` region at 0.14 and
   another type at 0.3, with the legend text taken from
   `glossary["This page"]`; and the bad case: a carved type with no
   "This page" entry raises `ValueError` (not a self-named legend entry).
   Run `.venv/bin/python -m pytest -q tests/test_scope_margin_scopes.py`
   only; the full suite's 45 failures are known and not yours.
2. Regenerate the fixture from the page
   `/var/www/zone.hyperreal.enterprises/wip/mark7-0708.2185-margin.html`
   to `futon3c/web/scopes/fixtures/mark7-0708.2185.json` and run
   `node web/scopes/test_scopes.mjs` in futon3c; it must pass unchanged.
3. Reply: the layer list with the span count per layer; proof bodies
   drawn beside `summary.proofs`; carved drawn beside
   `summary.carving.now.regions`; and one proof body quoted: id, range,
   the first 40 characters of `source` at that range.
4. One commit in futon6, one in futon3c; bell claude-16 back with the
   summary and both shas:
   `python3 /home/joe/code/futon3c/scripts/agency_send.py --from codex-23 --to claude-16 --kind bell`
   with the summary on stdin via a quoted heredoc.

Keep it to this. If `scopes.js` cannot draw something the data needs,
say what; do not change the renderer.
