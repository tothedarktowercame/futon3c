# M-typed-holes — worked example (IDENTIFY): E-mission-head, BV-typed

*Semi-formalisation of a clean, completed historical mission as a BV-typed
wiring diagram. Driven informally while the mathlib audit is out. Companion to
`M-typed-holes.md`; method from the BV-combs excursion (futon6 `9cd66b5`).
Source: `futon6/holes/missions/E-mission-head.md` (HEAD→DOCUMENT, Joe+Fable,
2026-06-10) + its informal writeup `futon6/holes/anatomy-of-a-futonic-mission.md`.*

## 1. Why this example

E-mission-head is the cleanest specimen we have: a mission that **documented
itself** as it grew, with an informal anatomy paper already written. It also has
**no mined `:composes` wiring** (its triple has `:commit`/`:differentiates`/
`:file` edges and exactly one `:hungry-for :payoff`) — so we *construct* its
wiring from the anatomy. That is exactly the "semi-formalise the informal" move,
and it pays off twice: the constructed wiring **exercises `par`/`copar`**, which
the BV-combs excursion found *structurally unreachable* on every mined
`:composes` skeleton (all linear). So E-mission-head shows the **target shape**
the mining can't yet emit — it is gap #3 (`no-par-copar`) made concrete.

## 2. The BV typing

Two structures, coupled.

**(a) The phase spine — a `seq` comb with holes.** The lifecycle is a
non-commutative chain; each not-yet-written phase is the anatomy's **ghost
line** — a *typed hole* (typed by which phase the standard form expects). Writing
a phase fills the hole; the mission *is* a comb that fills left-to-right:

`⟨HEAD ; IDENTIFY ; MAP ; DERIVE ; ARGUE ; VERIFY ; INSTANTIATE ; DOCUMENT⟩`

**(b) The two readings — a `copar` (held together).** The anatomy's whole point
(§5, the Mount Analogue): a mission is read two ways at once, and well-formedness
is their *agreement*:

`( scope-reading , organism-reading )`

- **scope** (anatomy / exteroception): the phase spine drawn as nested blocks;
  its device is the **ghost line** (a hole the structure expects).
- **organism** (AIF / interoception): HEAD→sigil→vitals; its device is the
  **open arrow** (a hole whose target is named but unbuilt).

**(c) The coupling — the typed wires that make it BV, not just `seq`+`copar`.**
§5's table is a list of *typed wires* connecting the two `copar` branches; this
cross-linking of a parallel pair is precisely where BV's interaction (the medial
rule, `seq` interleaved with `copar`) lives:

| scope end | organism end | wire (the coupling) |
|---|---|---|
| HEAD | starting beliefs | the fingerprint compiler |
| ghost line | **prediction error** | structure expects a phase the doc lacks |
| phase satisfied in passing | belief met by an unexpected route | hole filled off the expected wire |
| open arrow | unmet commitment | the deadline clause prices its persistence |
| failure condition | what counts as error | written before the construction |
| vitals card | interoception | computed *from* the structural doc |
| outline panel | exteroception | the same doc, drawn |

A machine-readable sketch (hand-authored — semi-formal, not mined):

```edn
{:mission "E-mission-head"
 :bv/copar           ;; two ascents, held together; well-formed = they cohere
 [{:reading :scope
   :bv/seq [:HEAD :IDENTIFY :MAP :DERIVE :ARGUE :VERIFY :INSTANTIATE :DOCUMENT]
   :holes  [{:kind :ghost-line :hole-type :expected-phase}]}      ;; unwritten phases
  {:reading :organism
   :sigil :compiled
   :holes [{:kind :open-arrow   :hole-type :named-but-unbuilt-target}
           {:kind :hungry-for   :hole-type :payoff}]}]            ;; the 1 mined hole
 :coherence-wires    ;; §5 couplings — typed wires across the copar (the BV interaction)
 [{:scope :HEAD            :organism :starting-beliefs  :via :fingerprint-compiler}
  {:scope :ghost-line      :organism :prediction-error  :via :standard-form-expectation}
  {:scope :open-arrow      :organism :unmet-commitment  :via :deadline-clause}
  {:scope :outline-panel   :organism :exteroception     :via :same-doc-drawn}
  {:scope :vitals-card     :organism :interoception     :via :computed-from-doc}]
 :fills-observed
 [{:hole :phase/INSTANTIATE :how :in-passing  :note "closed inside another phase"}
  {:hole :closure-fold-0    :how :failed      :note "first :success false — a fill that resisted"}]}
```

## 3. The typed holes (one structure, several dressings)

Every "incomplete" thing in E-mission-head is a *typed hole* — the M-typed-holes
datatype, on real data:

- **ghost line** — hole typed by the expected phase; in AIF terms a *prediction
  error* (the standard form predicts a phase the document lacks).
- **open arrow** — hole typed by a named-but-unbuilt target; the deadline clause
  is literally a *cost on an unfilled hole*.
- **`:hungry-for :payoff`** — the one mined hole; the mission hungry for its payoff.
- **`:success false` closure-fold** — a *fill attempt that failed*; the loop's
  honest grain (a hole that resisted filling, recorded not hidden).
- **phase satisfied in passing** — a hole filled by an *unexpected route* (belief
  updated without the expected observation).

## 4. What this contributes to M-typed-holes (IDENTIFY)

1. **It exercises BV beyond `seq`.** seq (phases) × copar (two readings) × cross-
   wires (coherence) × holes — the full deep-inference shape. The mined
   `:composes` skeletons collapse to `seq`; this example shows the *target* and
   so motivates surfacing concurrency/coupling in the mining (excursion gap #3).
2. **Active inference == hole-filling (the AIF leg locks).** "Well-formed exactly
   when the two readings cohere, and the coherence is computable" = *all holes
   filled consistently across both projections*. A **prediction error is an
   unfilled typed hole**; perception/action is the fill. So M-typed-holes' fill
   operator and the mission's active-inference loop are the same loop — tying the
   datatype to the `E-the-dark-tower-2` / turns-as-typed-processes substrate.
3. **Two-readings = two projections of one typed-hole object.** scope and
   organism are projections (rooms #4-wiring and a #1-mining/#6-coord blend) of
   the *same* hyperedge-with-typed-holes; coherence is the demand that a hole's
   fillers agree across projections — the cross-cutting invariant M-typed-holes
   exists to make checkable.

**Next worked examples (same treatment):** a mined `:composes` chain
(M-first-flights) typed *with its satiety holes surfaced* — i.e. the first-rung
satiety→`:composes` projection — would sit beside this one to show the contrast:
mined-but-linear (needs the holes wired on) vs. anatomy-rich-but-hand-authored
(shows the par/copar target).

## In Lean (DarkTower)

This exemplar is now instantiated in `mathlib4/DarkTower/Examples.lean`
§`MissionExample` (`36b44b7`, 0 sorry), against the landed DarkTower types:

| this doc (semi-formal) | DarkTower Lean |
|---|---|
| phase `seq` chain `⟨HEAD;…;DOCUMENT⟩` | `BV.seq` over `inductive Phase` (`BV.lean`) |
| two readings `copar` `(scope,organism)` | `BV.copar` over `inductive Reading` |
| ghost line = typed hole | `TypedHole` with `satiety = payoff` at the unwritten phase (`TypedHole.lean`) |
| seq reassociation (the medial setting) | `BV.Cong.seq_assoc` (proved); "write a phase" = `BV.Step.cong` |

So the EDN sketch above is no longer just notation: its connectives are the
actual `BV`/`TypedHole` constructors, and the well-formedness claims are
`rfl`/`decide`-checked Lean. The coherence cross-wires (§2c) remain prose — a
formal `copar`-interaction functor is future work.

*Cross-refs:* `M-typed-holes.md` (charter), `M-typed-holes-lean-manifest.edn`,
`mathlib4/DarkTower/Examples.lean` (`MissionExample`), futon6
`holes/bv-comb-typing.edn` (the BV-combs excursion), futon6
`holes/anatomy-of-a-futonic-mission.md` (the informal source).
