# Adapter three (d): plates.js hands its inking to renderScopes

From claude-16 (E-scope-view, `holes/excursions/E-scope-view.md`, "Adapter
three", the (c) review). One repo: /home/joe/code/futon6, branch master,
commit in place (no worktree). Do not edit anything in futon3c:
`scopes.ts`, `scopes.js`, `test_scopes.mjs`, the fixture. Do not edit
`scripts/scope_margin_scopes.py` or its test. futon6 has a stranger's
uncommitted edit (a golden json under `data/showcases/`): leave it alone.

## Why

(c) is accepted: futon6 a878e2b, futon3c 44d8762. The fixture
`futon3c/web/scopes/fixtures/mark7-0708.2185.json` (SHA-256
`d6b429c5964a8ef1010f665a117160dde016cc8c288c334eaed0b1e13acdcdf0`) now
says everything `scope_margin/plates.js` inks: four S1 layers, node
quotes, proof bodies, carved regions, scopes, each as a weighted span,
and `mixInk` takes the maximum weight per ink, so the view reproduces
plates.js's else-if chain. So plates.js stops inking. It keeps its first
half, placing every word and formula at its source offset (`units`, each
`{e, a, b}`), and hands them to `renderScopes(data, host, {anchored})`
from `futon3c/web/scopes/scopes.js`, which inks them through `inkUnits`.
One inker on `style.backgroundColor`, not two.

Verified before dispatch: the page rebuilds from the run in 0.2 s with

    .venv/bin/python scripts/render_scope_margin.py \
      ~/runs/mark7master-20260921 0708.2185 \
      /home/joe/code/storage/futon6/results-mark7-mark7master-20260921/extracted/run/typeset-preview/0708.2185 \
      OUT.html

and `python3 scripts/scope_margin_scopes.py OUT.html | sha256sum` gives
the fixture's sha. That must stay true after your change (the adapter
reads only `m7-data` and `m7-source`).

## Change 1: page assembly (`scripts/render_scope_margin.py`, `build`)

- Import `scope_margin_scopes` (same directory) and compute
  `scopes = scope_margin_scopes.scopes_data(payload, source)` after the
  payload is built.
- Read `scopes.js` from `futon6_config.sibling("futon3c") / "web/scopes/scopes.js"`
  (`futon6_config` is already in `scripts/`). A missing file stops the
  build with a clear error, as an undefined S1 kind does.
- Factor the script embedding into a function, say
  `assemble(page, payload, source, scopes, assets, scopes_js) -> str`,
  so a test can run it on a small page string. The body order becomes:
  `m7-data` JSON, `margin.js`, `m7-source` JSON, `plates.js` (classic
  scripts, as now), then `<script id="m7-scopes" type="application/json">`
  with `scopes` (compact `json.dumps`, `ensure_ascii=False`, `<` escaped
  as `<` like the other two blocks), then one
  `<script type="module">` whose text is `scopes.js` followed by
  `scope_margin/plates_view.js` (new file, Change 3). Inline module
  scripts keep their `export` statements legally and are deferred, so the
  module runs after plates.js has placed the units.

## Change 2: plates.js keeps placement, readout and terms; drops ink

Remove: `INK`, `color`, `plate`, the C/M/Y checkboxes and the S1 group
select (`ctl.c`, `ctl.cg`, `ctl.m`, `ctl.y`), the ink swatch rows of the
legend grid (the eight colour rows and the three mixes), and the
`.m7-banner input` change listener (line 238) that repainted on the mock
toggle. `paint` shrinks to the defined-term classes only (the `TERMCLS`
block under `ctl.t`), called once after placement. Keep everything else:
placement (lines 26-139 and 411-448), `quotes`, `proofs`, `refused`,
`carvedAt`, `deferredAt` (the readout `show` uses them), `ctl.t` and its
three term swatches, `show`, `litEdge`, `window.m7span`,
`window.m7units`, the banner summaries and tables.

Add, where the plate row was: `el('div', 'm7-scope-host', null, box)`.
The view's layer toggles and legend land there.

The intro paragraph gains one sentence, because a behaviour goes away:
the mock toggle used to re-select node quotes by mock verdict (dropping
unanchored nodes, inking proposals for re-anchored ones, dropping
rejected scopes). Now the ink ignores it; the readout still reports each
verdict. Say that in the paragraph, plainly.

## Change 3: the wiring (`scripts/scope_margin/plates_view.js`, new)

Runs inside the module after `scopes.js`, so `renderScopes` is in scope.

    const data = JSON.parse(document.getElementById('m7-scopes').textContent);
    const host = document.querySelector('.m7-scope-host');
    const seg = data.segments[0].id;
    const view = renderScopes(data, host, {anchored: {[seg]: window.m7units},
                                           layers: ['s1-term', 's3', 's4']});

Default layers are what plates.js showed by default: the term group and
both other plates. Then two things the view does that the page does not
want, handled in this file, not in the renderer:

- The view sets a `title` tooltip on every unit (layer, kind, note). The
  page's readout panel says more (defined term, symbol bindings, mock
  verdicts). Remove the titles after each paint: a `change` listener on
  `host` runs after the view's own checkbox listener, so
  `window.m7units.forEach(u => u.e.removeAttribute('title'))` there and
  once after the first render.
- The view's legend `<details>` opens by default; close it
  (`host.querySelector('.scope-legend').open = false`). The glossary
  details below already define every label.

## Change 4: styles (`scripts/scope_margin/margin.css`)

The view's classes have no styles in this page. Copy the rules for
`.scope-controls`, `.scope-layers`, `.scope-layer-toggle`,
`.scope-swatch`, `.ink-c`, `.ink-m`, `.ink-y`, `.scope-legend` (and its
`dl`, `dt`, `dt.undefined`, `dd`) and `[data-span].lit` from
`futon3c/web/scopes/index.html`, scoped under `.m7-plates`. Then hide the
anchored segment's article: `.m7-plates .scope-columns{display:none}`. It
is not empty: in anchored mode the view still builds an `article` with one
note per visible span, 2072 for s1-term alone, rebuilt on each toggle. If
that rebuild is slow to the eye, say so in the reply (a `console.time`
around the first `setLayers` is enough); it is a renderer option for a
later packet, not for this one.

## Acceptance

1. `tests/test_render_scope_margin.py`, existing tests still green, plus
   a test of `assemble` on a small page string: the five blocks appear in
   the order above; the `m7-scopes` block parses back equal to the
   `scopes` dict passed in; no raw `<` inside either JSON block; the
   module script contains `export function renderScopes` and
   `renderScopes(`; and a missing `scopes_js` path raises. One more
   assertion, which is the claim of this packet: the text of
   `scope_margin/plates.js` contains no `backgroundColor`.
   Run `.venv/bin/python -m pytest -q tests/test_render_scope_margin.py`
   only; the full suite's known failures are not yours.
2. `node --check scripts/scope_margin/plates.js`, and `node --check` on a
   temp `.mjs` holding `scopes.js` + `plates_view.js` concatenated. There
   is no headless browser on this host; say in the reply what was and was
   not run.
3. Rebuild the page with the command above to
   `/var/www/zone.hyperreal.enterprises/wip/mark7-0708.2185-margin-scopes.html`
   (a new name beside the existing page, which stays as it is). Then
   `python3 scripts/scope_margin_scopes.py <that page> | sha256sum` must
   equal `d6b429c5…` above, and the `m7-scopes` block of that page,
   parsed, must equal the fixture file parsed (python, `json.load` both).
4. Reply: files changed; the pytest line; the two sha checks; the page's
   byte size before and after; the URL
   `https://zone.hyperreal.enterprises/wip/mark7-0708.2185-margin-scopes.html`;
   the sentence you added about the mock toggle, quoted; and anything the
   view does on the page that this packet did not foresee (the unit
   `onclick` the view binds, which lights every unit sharing a span until
   the next paint, is one I know of; report, do not fix).
5. One commit in futon6; bell claude-16 back with the summary and the sha:
   `python3 /home/joe/code/futon3c/scripts/agency_send.py --from codex-23 --to claude-16 --kind bell`
   with the summary on stdin via a quoted heredoc.

Keep it to this. If `scopes.js` cannot do something the page needs, say
what; do not change the renderer.
