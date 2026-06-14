# E-typed-holes-example-self — M-typed-holes, typed as a (live) typed hole

**Paired excursion to M-typed-holes (2026-06-14). Deliberately an *excursion*,
not a mission: an excursion has no lifecycle of its own to type, so the recursion
bottoms out here — the framework types its own defining mission, and stops.**

## The move (reflexive + live)

M-typed-holes is a mission. Missions are typed-hole objects — `Examples.lean`
`MissionExample` typed E-mission-head exactly so (lifecycle `BV.seq`, two readings
`BV.copar`, ghost phase a `TypedHole`). Therefore **M-typed-holes is itself a
typed-hole object**, and we can type it with the very vocabulary it defines. This
is the self-representing stack (`substrate-2`) modelling the tool it is building —
applied to that tool's own mission. Making *this* an excursion (no `M-`) is what
keeps it from regressing: there is no `E-…`-lifecycle to type next.

What's new vs the earlier exemplars: E-mission-head and M-first-flights were
**completed** missions — every phase a filled node, typed retrospectively.
M-typed-holes is **in progress**, so it is the one case with **genuinely open
holes**: the unwritten phases are live ghost lines.

## The typing (as of 2026-06-14)

```edn
{:mission "M-typed-holes"
 :bv/copar                       ;; two readings, must cohere (well-formed iff they do)
 [{:reading :structural          ;; the mission doc's written sections
   :bv/seq [:IDENTIFY :MAP                       ;; SATED — written
            {:ghost :DERIVE} {:ghost :ARGUE}     ;; ghost lines (unwritten phases)
            {:ghost :VERIFY} {:ghost :INSTANTIATE}
            {:ghost :DOCUMENT}]}
  {:reading :self-model          ;; this mission's own typed-hole model (substrate-2)
   :note "the datatype it defines, pointed at itself"}]
 :satiety
 {:IDENTIFY    :full   ;; charter + 4 exemplars + residue analysis + mathlib audit
  :MAP         :full   ;; M-typed-holes-MAP.md (a75205d): ready/missing + survey Q1-Q6
  :DERIVE      {:hungry-for :design}   ;; the shared fill operator + the proving loop
  :ARGUE       {:hungry-for :design}
  :VERIFY      {:hungry-for :payoff}
  :INSTANTIATE {:hungry-for :payoff}
  :DOCUMENT    {:hungry-for :payoff}}}
```

- **`IDENTIFY`, `MAP` are sated** — real written nodes (with artifacts/shas).
- **`DERIVE`…`DOCUMENT` are ghost lines** — typed holes, `satiety` hungry. MAP
  already named what `DERIVE`'s hole is *hungry-for*: the **shared `fill` operator
  + the ArSE↔ScopeQuery↔store proving loop** (not the datatype, which exists).
- **The two readings** (`copar`): the doc's structure ∥ the mission's self-model.
  Well-formed exactly when they cohere — the E-mission-head §5 coupling, here
  pointed at the mission that formalised it.

## Why it's insight-bearing, not ceremony

Two things only this live/reflexive case shows:

1. **A typed-hole object with open holes, watched live.** Writing the next phase
   *is* a fill: when `DERIVE` lands, M-typed-holes' `satiety[:DERIVE]` goes
   `:hungry-for :design → :full`. The mission's own progress is a **fill-sequence**
   — self-similar with its own subject matter. The AIF reading is literal: an
   unwritten phase is prediction error; writing it is the action that discharges it.

2. **The recursion has a floor.** A mission typed by the typed-hole framework
   would invite typing *its* typing… This excursion is the base case: it types
   M-typed-holes and, being an excursion, is not itself a mission to be typed. The
   self-reference is total but finite.

So the dogfood is complete in a way the others can't be: the framework has now
typed a mission, a paper, a fold, a live query — **and its own defining mission,
mid-flight, with the holes still open.** The next phase-commit on M-typed-holes
will visibly fill one of the ghosts above.

*Cross-refs:* `holes/missions/M-typed-holes.md` (the mission being typed),
`M-typed-holes-MAP.md`, `M-typed-holes-example-mission-head.md` (the completed
sibling), `mathlib4/DarkTower/Examples.lean` (`MissionExample`, the Lean witness
for the mission genre), `futon4/holes/mission-lifecycle.md`.
