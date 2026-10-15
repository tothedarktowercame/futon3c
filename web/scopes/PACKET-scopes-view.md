# Packet: `renderScopes(data, host)` — one framework-free TypeScript view of segments, spans and edges

From claude-16, 2026-10-15. Repo: /home/joe/code/futon3c, branch master, work in place (no worktree needed; files are new).

## Goal

Write `web/scopes/scopes.ts`, compiled once to `web/scopes/scopes.js` (ES module, no bundler, no framework), exporting

    export function renderScopes(data: ScopeData, host: HTMLElement, opts?: {layers?: string[]}): {setLayers(ids: string[]): void}

and `web/scopes/index.html`, a page that fetches `fixtures/claude-16-turn-162.json` and calls it. Read the spec first: `holes/excursions/E-scope-view.md`, section "The data the view takes" (the fixture is an instance of it; its types are the `ScopeData` interface you declare). Owned text only this packet (the view renders `segments[].text` itself); anchored text is a later packet.

## What the view does (all of it in this packet, nothing else)

1. Renders each segment with `role` operator or agent as a block, label above, text in a `<pre>`-like wrapping element preserving newlines. Segments of other roles (pattern, memory) render in a side column, same label-plus-text form, smaller.
2. Inks every span onto its segment's text by layer: each layer has an `ink` of `c`, `m` or `y`; where spans of different layers overlap the inks mix (cyan+magenta = blue, cyan+yellow = green, magenta+yellow = red, all three = dark). Ink strength is the span's `weight` (0..1). Implementation: split each segment's text at every span boundary (code points, not UTF-16 units: use `Array.from(text)`), one `<span>` per piece carrying the ids of the spans covering it, background from the mixed ink.
3. A span with a `mark` gets the mark placed before its start as a small glyph, not an underline (this is the fix the Emacs overlay made today: tiled fragments underlined whole is the whole turn underlined).
4. Margin: one note per span with a `note`, in a column beside the segment, in span order; hovering or clicking a span lights its note and vice versa.
5. Edges: for each edge, the `from` span's note shows `relation label → <to>` as a link; clicking it scrolls to and flashes the `to` span. An edge whose `to` names no span is drawn as a stub reading the id (the fixture has none now; the Emacs overlay has them for parents in an earlier turn).
6. Layer toggles (checkbox per layer) and a legend listing every `kind`, `relation` and `label` that appears, with its `legend` text; one that has no legend entry is listed as "undefined", never hidden (mark7's rule).
7. Knows nothing of 象, futon3a, minus40, LaTeXML. No fetch inside `renderScopes`; `index.html` does the fetch.

Keep it near 300 lines of TypeScript plus a small stylesheet in the html. No dependencies.

## Acceptance

- `tsc --noEmit --strict web/scopes/scopes.ts` clean (use `npx -p typescript tsc` if no global tsc), and the compiled `scopes.js` committed beside it.
- `node web/scopes/test_scopes.mjs` passes: loads the fixture, runs a DOM-free check that (a) every span's `[start,end)` lies within its segment's code-point length, (b) every edge end resolves to a span or is reported as a stub, (c) every kind/relation/label is in the legend or reported undefined; export the pure parts of the module (piece splitting, ink mixing, legend audit) so the test imports them from `scopes.js` without a DOM.
- Open `index.html` via `python3 -m http.server` from `web/scopes/` and check by eye: the operator turn shows five act spans with marks ㊩ 🈰 ㊭ ㊟ 🈖 before them, not whole-text underlines; the reply shows six; three pattern segments and about a dozen memory segments sit in the side column with faint ink (weights 0.33–0.46); toggling the memory layer off removes them.
- Commit on master with a message starting `scopes view:`. Then **bell claude-16 back with a summary and the commit shas** (`scripts/agency_send.py --from <your-id> --to claude-16 --kind bell`).

Do not edit `scripts/scope_view_data.py`, the fixture, or the note; report anything wrong with them in the bellback instead.
