# E-unified-live-surface — fold the live trackers into one CLJS app

**Status: IDENTIFY (2026-07-05). Proposed by Joe (emacs-repl): "fold these live
surfaces into one of the cljs apps (:3100 / /wa#/) — and combine them along with the
evidence landscape viewer into one unified CLJS app." Charter by claude-18.
Owner: TBD. ARGUE with Joe before any build.**

## The surfaces today (inventory, probed 2026-07-05)

| Surface | Tech | Data | Where served |
|---|---|---|---|
| Live EFE map (`mission-efe-field-embed.html`) | python-GENERATED 3600² SVG + hand-rolled live-overlay JS (10s poll) | `/api/alpha/live-efe-map` (7070) + generated-time JSONs | `file://` from futon6/data |
| Cascade tracker (`pipeline-pattern-cascade-live.html`) | static HTML + fetch JS (60s poll, tickets box) | `/api/alpha/cascade-real{,/graph}` (7070) | `file://` from futon3c/holes/excursions |
| WebArxana `/wa#/` | CLJS app (embedded shadow build, I-0), incl. the k-collapsible `graph.cljs` engine | webarxana APIs (3100/7070) | :3100, in-JVM |
| Evidence-viewer | static viewer | evidence store on ITS host | **lucy's :7071 only** — the laptop's 7071 404s it, while the HOT evidence store is the laptop's (E-evidence-flow) |

Also in play: the War Machine CLJS UI (embedded build, same JVM) and claude-10's
evidence-landscape workstream (M-live-efe-map doc §MAP: "this mission CONSUMES/ALIGNS
with it — do not fork the sibling's contract").

## Why unify (the tension)

Four live surfaces, four delivery mechanisms (file:// ×2, :3100, lucy), zero shared
nav/state; the operator hand-juggles URLs and each new surface (tickets box, tonight)
re-invents fetch/poll/render plumbing. The k-collapse engine and a real component
system already exist in WebArxana — the newest surfaces are hand-rolled JS beside a
CLJS app that does this for a living.

## Constraints (hard)

- **I-0**: everything serves from the one JVM; CLJS watches embedded (dev/shadow.clj)
  — no standalone shadow-cljs in the request path.
- **Don't fork the sibling**: claude-10's evidence-landscape contract governs what
  "the evidence viewer" becomes; this excursion integrates, it does not re-own.
- **E-evidence-flow gates the evidence panel**: which store a unified viewer reads is
  exactly Q1–Q5 of that excursion; don't hardcode a host before it answers.
- **The generated field page stays generated** (v1): the python generator embeds
  terrain computed from heavy artifacts (BGE, marching squares, carpet solve);
  re-rendering that natively in CLJS is a rewrite with no operator payoff. Fold the
  PAGE in; don't port the renderer.
- **Claim discipline**: a nav shell is "one place to look," not "one app." Name the
  stages honestly.

## Options ladder (cheapest first — each stage is independently shippable)

1. **O1 — nav shell (hours):** a `/live` route in WebArxana: header + tiles hosting
   the EFE map and cascade tracker (iframe or served-static + link-out), plus the
   evidence viewer for whichever store E-evidence-flow blesses. Requires serving the
   two file:// pages from the JVM (静 static route onto futon6/data and
   futon3c/holes/excursions — small http.clj addition). Payoff: one URL, browsable
   from any mesh host (fixes the file://-only delivery too).
2. **O2 — CLJS-native panels (days, per panel):** port the cascade tracker's panels
   (and the tickets box) to CLJS components reading the same endpoints; shared
   poll/state machinery; the EFE map's live OVERLAY (not terrain) could become a CLJS
   layer over the served SVG. Do panels in payoff order, not wholesale.
3. **O3 — native field renderer (weeks):** CLJS re-implementation of the terrain
   render. Explicitly NOT recommended until O1/O2 prove insufficient — the generator
   works and regenerates on cron.

**Recommendation: O1 now, O2 selectively, O3 parked.**

## First slice (when opened)

O1: `/live` route + JVM static-serving of the two pages + links to WM UI and
evidence viewer. Acceptance: one bookmark shows all four surfaces from any browser
that can reach :3100; the two polling pages keep their existing liveness untouched.

## Scope boundary

- No WebArxana architectural rework; add a route, don't reorganize.
- Evidence-panel host choice deferred to E-evidence-flow.
- A clean kill is a success: if iframing degrades the pages (SVG size, poll
  behavior), the finding "keep separate pages + a links hub" closes this cheaply.

## O1 + O2 LANDED + REVIEWED (2026-07-05, codex-1, dispatched by claude-18)

**O1 `fccca05` (futon4):** `/live/{efe-map,cascade}.html` served fresh-per-request
from the JVM (no-cache), `:live` hash route + `live.cljs` shell with tiles, WM UI
link, evidence-viewer link-out with the E-evidence-flow caveat chip. CORS: the 7070
endpoints worked from the :3100 origin as-is (no header change needed).
**O2 `0cc27b1` (futon4):** shared 60s poller → atom; native panels: cascade summary
chips, Tickets ("40 of 520" honest cap), "Agent → mission lineage".

**Review (claude-18):** read both diffs; gates clean (kondo + check-parens, all four
touched files); `pgrep java` = 1 at rest (the one-off shadow build exited — I-0
held); Chromium against `:3100/wa#/live`: both iframes live (EFE badge "live layer
on", cascade tickets visible), zero page errors, native panels populated. **The
double closure landed:** this dispatch's own bell carried
`--mission E-unified-live-surface`, and on completion codex-1 appeared at the top of
the lineage panel it had just built — `agent:codex-1 →
futon3c-d/excursion/unified-live-surface`, the CORRECT canonical excursion scheme
(T-dispatch-clock-excursion-prefix fix proven end-to-end on a real dispatch) — while
`E-unified-live-surface` simultaneously dropped off the native tickets panel,
because it is now clocked. The instruments verified each other.

**Remaining (O2 continuation, unclaimed):** more panels by payoff (held, holes,
arrows); EFE overlay as a CLJS layer; O3 stays parked. Evidence panel still gated
on E-evidence-flow.
