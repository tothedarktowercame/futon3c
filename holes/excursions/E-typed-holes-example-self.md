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
 :head {:prose :present :aif-workup :carried-back-2026-06-14}  ;; was the gap; now filled (see below)
 :bv/copar                       ;; two readings, must cohere (well-formed iff they do)
 [{:reading :structural          ;; the mission doc's written sections
   :bv/seq [:IDENTIFY :MAP :DERIVE :ARGUE :VERIFY   ;; SATED — written
            {:ghost :INSTANTIATE}                    ;; ghost lines (unwritten phases)
            {:ghost :DOCUMENT}]}
  {:reading :self-model          ;; this mission's own typed-hole model (substrate-2)
   :note "the datatype it defines, pointed at itself"}]
 :satiety
 {:IDENTIFY    :full   ;; charter + 4 exemplars + residue analysis + mathlib audit
  :MAP         :full   ;; M-typed-holes-MAP.md (a75205d): ready/missing + survey Q1-Q6
  :DERIVE      :full   ;; M-typed-holes-DERIVE.md — FILLED this session (was :hungry-for :design)
  :ARGUE       :full   ;; M-typed-holes-ARGUE.md — FILLED this session ("as planned"; AIF re-run queued)
  :VERIFY      :full   ;; M-typed-holes-VERIFY.md — FILLED this session; two-cert scope (design-cert LANDED Coverage.lean 84b61e9; wiring-cert PARTIAL, open arrow to D1)
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

1. **A typed-hole object with open holes, watched live — and now observed
   filling.** Writing a phase *is* a fill: this session `DERIVE` landed
   (`M-typed-holes-DERIVE.md`), so `satiety[:DERIVE]` went `:hungry-for :design →
   :full` — the ghost above became a sated node *as we watched*. The mission's
   own progress is a **fill-sequence**, self-similar with its subject matter; the
   AIF reading is literal (an unwritten phase is prediction error; writing it is
   the action that discharges it). This session **three** ghosts filled in a row —
   `DERIVE`, `ARGUE`, then `VERIFY` — `satiety` `hungry→full` each, watched live.
   Remaining open holes: `INSTANTIATE`, `DOCUMENT`. (`VERIFY` is the sharpest
   instance yet of the self-similarity: `M-typed-holes-VERIFY.md` types the
   VERIFY phase *itself* as a **scope with two typed holes** — `?design-cert`,
   `?wiring-cert` — each filled by a certificate that is *itself a fill* (a Lean
   proof; a runtime query-answer). So filling the `:VERIFY` ghost was an act of
   filling two sub-holes with witnessed fills — the framework verifying itself
   using the very (hole, fill) it defines. The design cert landed
   (`DarkTower/Coverage.lean`, `84b61e9`, reviewed PASS); the wiring cert is
   partial, its hunger *being* failure-condition #1 from the AIF workup. The
   `ARGUE` AIF-driven re-run is `E-typed-holes-aif-alternate.md`.)

   **A meta-hole the typing surfaced — now filled.** The first cut said "no
   `HEAD` node at all"; the precise truth is the charter had a **prose HEAD** but
   lacked its **AIF workup** (the design-pattern recast + sigil + birth vitals +
   failure conditions). That AIF workup was the missing node. It has since been
   **carried back** into `M-typed-holes.md` (`## HEAD — AIF workup`): the
   IF/HOWEVER/THEN/BECAUSE recast, the failure conditions, and a death clause —
   the adoption gate the non-AIF "as planned" route had no place for. The non-AIF
   route is now recorded in the charter as a **shortcut / graceful degradation**
   of the full HEAD→AIF lifeform path (fine when feasibility is high; the cost is
   the unstated adoption gate). So the self-typing did its job: it flagged a real
   missing node, and writing it *filled* it — the dogfood eating its own tail,
   honestly.

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
