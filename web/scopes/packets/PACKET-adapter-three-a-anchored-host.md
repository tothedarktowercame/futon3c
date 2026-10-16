# Adapter three (a): anchored text in the scope view

From claude-16 (E-scope-view, `holes/excursions/E-scope-view.md`, "Two ways
to host it", item 2, and "Order of work" item 5). Repo:
/home/joe/code/futon3c, branch master, commit in place (no worktree). You
built `web/scopes/scopes.ts` at a73a9589b; this extends it. TypeScript and
the compiled `scopes.js` only; no Python, no Clojure.

## Goal

`renderScopes` gains an anchored mode: the host hands the view, per
segment, the elements it has already placed, each with its code-point
range in that segment's text, and the view inks those elements instead of
rendering the text. This is how the mark7 margin page
(`futon6/scripts/scope_margin/plates.js`, first half, lines 33-138) and a
VS Code editor host the view. Wiring the mark7 page itself is the next
packet, not this one.

## API

    renderScopes(data, host, { layers?: string[], anchored?: Record<string, Unit[]> })
    interface Unit { e: HTMLElement; a: number; b: number }   // [a, b) code points in segments[id].text

- A segment named in `anchored` is not rendered as text. Each of its units
  is inked from the active spans that overlap `[a, b)`; overlap is
  `span.start < b && a < span.end` (plates.js's `hit`), so a unit that
  straddles a span boundary counts as covered.
- Ink per unit: the same mixing the owned mode uses (`mixInk`), with a
  span's `weight` scaling its layer's ink as it does now. A unit under no
  active span gets no inline background (empty string, so the host's own
  styling shows through).
- Each inked unit carries `data-spans` (the ids, space separated) and the
  same hover readout the owned mode gives a piece: every active layer at
  that point, kind, mark, note.
- Notes for an anchored segment's spans appear in the aside exactly as for
  an owned segment. Edges: list them in the aside as now; do not try to
  draw lines across the host's layout.
- `setLayers` re-inks anchored units. Segments not named in `anchored`
  render exactly as before.

## The pure core, so it is testable without a DOM

    export function inkUnits(units: {a: number; b: number}[], spans: ScopeSpan[],
                             layers: ScopeLayer[], active: Set<string>): { color: string; spanIds: string[] }[]

`renderScopes` uses it; `test_scopes.mjs` tests it directly. The DOM step
is then only `e.style.backgroundColor = color` and the dataset.

## Acceptance

1. `node web/scopes/test_scopes.mjs` green, with new cases for `inkUnits`:
   - a unit inside one span on layer `c` and one on layer `m` gets
     `mixInk`'s colour for those two spans (compare to `mixInk` itself);
   - a unit outside every span gets `""` and no ids;
   - a unit straddling a span's end by one code point is covered;
   - a span of weight 0.5 inks at half: assert the channel value against
     the formula, not a copied constant;
   - a span on an inactive layer contributes nothing.
2. Owned mode unchanged: the existing assertions all still pass and
   `index.html` is not edited.
3. `scopes.js` recompiled from `scopes.ts` the same way as at a73a9589b;
   say in the reply which command you used.
4. Commit, then bell claude-16 back with a summary and the commit sha:
   `python3 scripts/agency_send.py --from codex-23 --to claude-16 --kind bell`
   with the summary on stdin via a quoted heredoc.

Keep it to this. If the owned-mode code needs reshaping to share the
readout with anchored units, do the smallest reshaping and name it in the
reply.
