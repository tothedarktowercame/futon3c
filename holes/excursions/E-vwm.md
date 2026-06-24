# E-vwm — Virtual War Machine: speculative fiction over the stack (design as defensive driving)

**Date:** 2026-06-23
**Status:** CHARTER + v1 log. Built backwards from the WM; v1 harness + horizon gap-map + generative Lucid Scenes.
**Owner:** claude-2 (build) + Joe (direction).
**Files:** `futon3c/holes/labs/vwm/vwm_harness.py`; generated scenes → `futon5a/holes/stories/vwm-lucid-scenes.md`.

## Why it exists

The WM's real failure is the **stall**: an agent enters a mission, its candidate patterns are a flat, low-cosine
cluster (top ~0.39), nothing clears confidence → R6 abstain → nothing applied ("agents get in there and can't
find anything they're confident about doing"). The VWM is a **speculative simulator** — it does "speculative
fiction over the stack" (no code changed): finds pattern cascades, looks at possible next actions, anticipates how
missions would go, and shows where they break. It repurposes VSATARCS's observe→anticipate→story loop and its
**Lucid Scenes** ("the WM dreaming itself being used"), made *generative*. Ties: the session-long **move-prior
bottleneck** (slice-1 ranks 116–661; the daisy's REINFORCE collapse vs evolution success), M-G-over-cascades,
[[project_aif_daisyworld]], Ostrom (shared pattern library = the "talking" that escapes the commons tragedy).

## v1 results (real/current data: mission-pattern-scopes + the pipeline-semilattice downward growth)

- **The stall is the default, quantified:** 132/194 missions (68%) have max candidate cosine < 0.5 (flat-low
  prior → abstain). The WM stalls on most missions.
- **Cascades unstick 91%:** for 120/132 stalls, the **downward-growth cascade** (what missions *like it* in the
  semilattice cluster actually applied) offers a warranted set of moves where single-pattern cosine-argmax
  stalls. Supports Joe's hypothesis — *with a bound:* "unstuck" = a *warranted move exists*, not that it's the
  *right* move; relevance to *this* mission is the agent-judged Lucid Scene. Warrant is cluster-generic + low
  absolute (its power = a coherent cluster-usage set vs a single noise-cosine pick).
- **Generative Lucid Scenes emitted** (VSATARCS-readable; the WM dreaming, first-person, stuck-point → dreamed
  cascade → where-it-breaks).

## The unlock — design as DEFENSIVE DRIVING (horizon scan / gap map)

Reframe (Joe): rather than the WM charging into a high-priority mission and stalling, the VWM **scans the horizon**
and flags the *classes of work we have no good-enough patterns for* → **seed class-level patterns there first**.
The seeding relaxation: we showed "seed the *perfect* pattern → findable" (slice-1); the relaxation is "seed
*good-enough-for-the-class* patterns → the whole cluster becomes tractable (and the VWM finds them when it looks)."

**v1 gap map** (clusters ranked by pattern-coverage = applied-rate × retrieval strength × #cited):
- **Well-cultivated:** cluster 0 (reader/paper/ukrns — 138 missions, 55% applied, **211** cited patterns), cluster
  2 (agent/aif/peripheral — 75%), cluster 6 (operator/eoi — 50%).
- **HORIZON GAPS (0% applied, 0 cited):** cluster 4 [hdm/ai4ci/**mathematical-reasoning**] (n=3, clearest real
  gap), cluster 7 [scan/focus/depositing] (n=1), cluster 8 [musn/realtime] (n=1), cluster 3 [training/programme]
  (n=1). Caveat: three gaps are n=1 (thin); cluster 4 is the robust one. Gaps are the *uncultivated periphery* —
  the stack invested its patterns in its mature core (cluster 0).

## Next

- **Sharpen relevance** — replace the cluster-generic warrant with a **learned / discharge-trained prior** (the
  evolutionary approach the daisy validated) so the cascade is mission-specific, not cluster-generic.
- **Seed good-enough class patterns** for a real gap (cluster 4: mathematical-reasoning/ai4ci) and re-scan.
- **Wire the generated Lucid Scenes into VSATARCS as a live view** (browse the WM's dreams).
- Narrate the highest-stakes scenes agent-in-the-loop (full speculative fiction).
