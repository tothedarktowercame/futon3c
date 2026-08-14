# Mission: M-latex-wysiwyg — browser WYSIWYG over a live Emacs LaTeX backend

**Date:** 2026-08-08
**Status:** :designed — ground measured on draft8, slices cut, nothing built yet.
**Prior art bound by name:** `M-smart-emacs-cursor` (WS actuator),
`futon-agency-ws.el` (transport), Arxana Essays (`futon4/dev/arxana-browser-essays.el`,
annotation persistence), mission scope hyperedges (`futon6/scripts/mission_scope_detect.py`,
offset markup), latexml-oxide 0.7.5 `--source-map`.

## HEAD (Joe, 2026-08-08, verbatim sense)

> Design the WYSIWYG features. Invertibility could be handled via overlays. Use
> an Emacs backend so we keep both the WYSIWYG HTML frontend and a viable LaTeX
> Emacs backend. We already have a nice overlay system in the Arxana Essays
> format, so in principle that could maintain the model. `M-smart-emacs-cursor`
> is prior art for a WebSockets-based controller in Emacs; we'd want to sync the
> scopes over WebSockets. By scopes I mean the offset markup is much the same as
> what I've been using in the arXiv mining work.

## 1. MEASURED GROUND (draft8.tex, latexml-oxide 0.7.5, 2026-08-08)

`--source-map` on the real paper: **1,605 `data-sourcepos` attributes, 0 errors,
3.3 s.** Format is `file:line:col-file:line:col`.

**The number that shapes everything:**

| nearest anchor to visible text | chars | share |
|---|---|---|
| true range (start ≠ end) | 17,562 | **19 %** |
| degenerate point (start = end) | 72,298 | **80 %** |
| no anchor at all | 79 | 0 % |

By construct, the split is clean and predictable:

- **always a range** — `math` (386), `section` (43), environments
- **always a point** — `<p>` (121 of 121), `div` (117), `em` (72)
- **nothing** — macro expansions. `\mylemma` → "Lemma of Descent" carries no
  node and no position. Verified twice, on a probe and on the paper.

So: **we get a start anchor for essentially every paragraph, and an explicit end
for almost none.** This is not fatal — see §3.2 — but it decides the architecture.

**File indices are opaque.** The `--help` text promises "a document-level
tag→file table"; 0.7.5 emits no such table in HTML or XML. Indices resolve
empirically to `\input` encounter order: `0=draft8.tex, 1=intro-generated.tex,
2=part3-exotype.tex`. Worth filing upstream; until then the build step must
record the mapping itself.

## 2. THE INVERTIBILITY QUESTION, ANSWERED PRECISELY

The rule is one line:

> **A rendered span is editable iff it is a verbatim image of a contiguous
> source span.**

That partitions the document into three classes, and the whole design follows:

- **Class A — directly editable.** Body prose that came from literal characters.
  Roughly the 80 % above: we know where it starts, and Emacs can find where it
  ends. Edit in the browser, splice into the file.
- **Class B — argument-editable.** Constructs whose *arguments* are literal but
  whose rendering is computed: section titles, captions, `\emph`, cite keys. You
  may edit the argument; you may not edit the rendered form.
- **Class C — not editable in place.** Macro expansions, counters, refnums,
  resolved cross-references, the bibliography, math (unless editing the TeX).
  The map is not injective and no overlay can make it so.

**The design decision: do not try to make Class C editable.** Make it *visibly*
non-editable and offer one gesture — jump to the source in Emacs. This is where
the hybrid stops being a compromise: for a LaTeX author who lives in Emacs, "put
my point on that macro definition" is the *better* affordance, not the fallback.

This is also the honest form of the guarantee. We are not claiming "we can always
invert." We are claiming: **we can always detect when we cannot, and refuse.**

## 3. WHY EMACS IS THE LOAD-BEARING HALF (not a fallback)

### 3.1 Markers dissolve the staleness problem
Offsets go stale the instant the file changes — from Emacs, from git, from
another agent. Emacs markers do not: they auto-adjust on every buffer edit.

So **the browser never holds offsets.** At build time each scope is converted to
a marker pair in the buffer; the browser holds only opaque `scope-id`s and asks
Emacs "what is scope 47" / "replace scope 47 with this text". Offsets exist only
at the boundaries: build time, and re-anchoring after a restart.

### 3.2 Emacs supplies the missing END
80 % of the text has a start anchor and no end. Emacs already knows where a
LaTeX paragraph or environment ends — `forward-paragraph`, AUCTeX's environment
motion. The source map says *where a thing begins*; Emacs says *how far it runs*.
Neither half is sufficient alone. This is the strongest argument for the split.

### 3.3 Emacs owns undo
Every browser edit becomes an ordinary entry in the buffer's undo history, with
its own undo boundary. `C-/` reverts a WYSIWYG edit. No bespoke undo stack.

## 4. THE ANCHOR FORMAT — reuse the scope shape

The mission-scope hyperedges already carry exactly the right thing:

```
hx/content: { position: 0, end: 486, match: "Mission: M-smart-emacs-cursor …" }
```

Position **and** quote. That is a W3C-style dual selector (TextPositionSelector +
TextQuoteSelector) and the quote is what makes re-anchoring safe. Keep it, add
the file:

```
{ scope-id, file, position, end, match, class: A|B|C }
```

**Apply protocol — never guess:**
1. Read the text at the marker pair.
2. If it equals `match`, splice.
3. If not, search a window around the marker for `match`. Unique hit → re-anchor
   and splice.
4. Zero hits, or more than one → **refuse**, tell the browser, offer jump-to-source.

Step 4 is the feature, not the failure path.

## 5. WHERE ARXANA ESSAYS FITS

Essays anchors annotations symbolically, by `:section-id` + `:endpoints` — coarse
but durable across rebuilds. Scopes are precise but rebuilt every conversion.
They are complements, not competitors:

- **Essays = persistence layer.** Annotations, retraction/strikethrough, compiled
  views, XTDB registry. Survives rebuilds because it is symbolic.
- **Scopes = live layer.** Precise editing targets for one session. Disposable.
- **Join = `section-id ↔ scope-id`,** recomputed at each build.

So an annotation made in the browser outlives the next `latexml_oxide` run, while
the edit targets are refreshed. Neither system has to change shape.

## 6. TRANSPORT

Reuse `futon-agency-ws.el` — one shared socket at `ws://localhost:7070/agency/ws`
with typed `subscribe`/`dispatch`. No new transport.

| message | dir | payload |
|---|---|---|
| `scope/sync` | E→B | scope table for a file (ids, classes, quotes) |
| `scope/point` | E↔B | cursor / selection position, both ways |
| `scope/edit` | B→E | `{scope-id, new-text, expected-quote}` |
| `scope/ack` | E→B | applied; new quote |
| `scope/reject` | E→B | `{scope-id, reason: stale\|ambiguous\|class-C}` |
| `build/done` | E→B | rebuilt; reload with fresh scope table |

**Safety decision, stated explicitly.** `smart-cursor.el` holds a deliberate
**non-editing invariant** — it proves control without mutating buffers, and its
e2e harness depends on that. **Do not relax it.** Add a separate `essay-edit`
channel with its own minor mode, per-edit undo boundary, and a kill-switch. The
actuator that can move the cursor and the actuator that can rewrite the paper
should not be the same object.

## 7. SLICES (small, gated — one behaviour each)

**S1 — read-only two-way navigation. No editing at all.**
Click a paragraph in the HTML → Emacs point lands in that paragraph, right file.
Move point in Emacs → the HTML highlights the matching block.
*Gate:* 20/20 sampled paragraphs across all three source files land within the
correct paragraph. Zero writes to any `.tex`.
This validates sourcepos accuracy, the marker conversion, the file-index mapping
and the WS round-trip — at zero risk to the paper.

**S2 — Class A editing, one paragraph.**
Edit body prose in the browser; quote-verified splice; undo boundary.
*Gate:* edit → `\iflatexml`-clean rebuild → PDF still 61 pages, 0 TeX errors.

**S3 — refusal UX.** Classes B and C render visibly non-editable; the refusal
paths from §4.4 are exercised deliberately and each offers jump-to-source.
*Gate:* attempting to edit an expanded macro never silently corrupts the source.

**S4 — Essays join.** Annotations persist across a rebuild via `section-id`.

## 8. OPEN RISKS

1. **The 80 % point-anchor finding is load-bearing.** If Emacs cannot reliably
   derive paragraph ends for the constructs that matter, S1's gate fails and the
   design needs revisiting. S1 exists to find that out before anything is built.
2. **No file table upstream.** Worked around, not solved. File with dginev.
3. **Math is a range but its interior is not addressable.** Editing math means
   editing TeX in a code affordance, not WYSIWYG. Treat as Class B at best.
4. **Concurrent edits.** Emacs is the single writer; the browser only proposes.
   If a second agent edits the file, markers survive but the browser's scope table
   goes stale until the next `build/done`.
