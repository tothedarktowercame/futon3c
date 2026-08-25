# The red-ring chain — one edit, three surfaces

*Written 2026-08-25 (claude-13, at Joe's direction, after the chain was used to
add the R2 ring and nobody had written down that it works this way).*

A control-loop gap is recorded **once**, as a badge in a typed file, and three
surfaces derive from it without anyone re-typing anything. This is the WR-8
discipline ("typed files are sources of truth") applied across three repos.

## The source of truth

`p4ng/empirics-futon/wr-overlay.edn` — one entry per war-room ruling placed on
a control-map node:

```clojure
{:node "R2" :wr "WR-16" :holds false
 :note "..."                      ; becomes the gap text everywhere downstream
 :established "2026-08-25"}       ; OPTIONAL, see "Dates" below
```

`:holds false` is what makes a ring red. Nothing else needs saying.

## What derives from it

| consumer | command | produces |
|---|---|---|
| the paper's figure | `bb empirics-futon/gen_wr_overlay.bb` | `aif-control-map-futon.svg` — hollow/filled rings drawn on the base SVG, which is never edited |
| the paper's prose | `bb empirics-futon/gen_wr_overlay.bb macros` | `empirics-futon/wr-glyphs.tex` — `\wrholds`, `\wrfails`, `\WRBadges`, `\WRHollow`, so the caption's counts cannot drift from the figure |
| the stack's status board | `futon3c/holes/excursions/gen-wip-cards.py` | `wip-cards.json` — one WIP / Uxbridge card per red ring, joined to a supplying cascade mission where one exists |
| the live page | `futon3c/scripts/publish-cascade-snapshot.sh` | `zone.hyperreal.enterprises/wip/pipeline-pattern-cascade.html` |

So: flip one `:holds` to `false`, regenerate, and a ring appears on the paper's
figure, the caption's count increments, the WR-glyph macros update, and a new
card appears in the WIP layer with the ruling, the gap text, and its register.

**Worked example.** On 2026-08-25 R2's badge went from `:holds true` to
`:holds false`. No file downstream was hand-edited. `\WRHollow` went 4 → 5, the
figure grew a hollow ring at R2, and `gen-wip-cards.py` emitted `C-R2` with
`supplier: null` (no cascade mission supplies it) and `register: "plan"`.

## Dates — the one thing that needs care

The file carries `:as-of`, meaning *when the whole overlay was last swept*. A
badge revised after that sweep is younger than the sweep. Dating such a badge
to `:as-of` would make the WIP layer — whose stated purpose is to carry "the age
of each mark", so a card can be shown as visibly stale rather than merely shown
— lie about the one field it exists to report.

So a badge may carry its own `:established`, and `gen-wip-cards.py` prefers it
over the file-level `:as-of`. Use it whenever you revise a single badge without
re-verifying the rest. **Do not advance `:as-of` to make one date look right:**
that asserts a full re-verification you did not perform.

## What is NOT automatic

- `promotion_test` and `watu` are null on every card. Nothing derives them;
  they are the honest nulls the layer exists to surface.
- `supplier` is only filled where `cascade-map.edn` pairs a box with that node.
- The published page does not refresh itself. `publish-cascade-snapshot.sh`
  re-fetches every live endpoint, so running it pulls in unrelated changes
  alongside the card — it is a deliberate act, not a step in this chain.

## Related

- `futon3/library/war-room/*.flexiarg` — the rulings themselves (the `@why`).
- `futon3/library/problems/*.flexiarg` — the `@how` for each red ring.
