# E-lean-to-clojure-arrow

**Opened 2026-08-14 by claude-2, at Joe's direction.** Parked out of
`M-apm-demonstration` so INSTANTIATE can proceed.

> Joe, 2026-08-14: *"I'm not sure how I feel about these declines, but in effect
> this isn't our problem right now. We can make a follow on Excursion
> E-lean-to-clojure-arrow.md and sort it out there. What we need to do now is
> get a Clojure implementation that works for us. We don't need it to 'provably'
> work, although we have come as close as we can using current tools. We need to
> see it working."*

**Status:** parked, not started. Nothing here blocks the mission.

---

## The question

Is a formal Lean→Clojure arrow — a generator, projection or
meta-specification — definable for preregistrations, or does the correspondence
have to stay hand-written and pinned?

**Current answer: not yet definable, and we now know why.**

## What exists, as of parking

| artifact | what it is |
|---|---|
| `mmca-clj/src/mmca/baldwin_guidance_preregistration.clj` | RHS #1 — Baldwin. 17 self-naming violations, pinned revision, authorization gate. **Pin goes stale silently** (`f50d34cf`; 40 commits have since touched `DarkTower/`) |
| `futon3c/src/futon3c/apm/preregistration.clj` (moved out of mmca-clj 2026-08-15) | RHS #2 — APM round 1. 45 self-naming violations, **staleness check present**, authorization gate. 111 tests / 333 assertions green |
| `futon6/scripts/clean_to_lean.py` | the *other* arrow: CLean record → Lean term, render-as-correctness-gate, attack-verified |

Note the direction asymmetry: **record→term exists and is enforced;
type→schema does not exist at all.**

## Why the arrow is not yet definable — the loss list

`codex-4` enumerated what Lean expresses that EDN cannot (M-apm-demonstration
V.12). An arrow would have to be lossy in exactly these places:

1. **F1 degrades from unconstructible to detectable.** `WorkedFrame.changed`
   makes a scaffold-identical frame impossible to *build*; the EDN counterpart
   can only *report* `:f1-scaffold-identical-frame`.
2. **`DecisionRule` totality is unprovable in EDN** — a named rule and a
   non-empty outcome domain can be required; totality cannot.
3. **Dependent proofs become runtime checks** (non-empty pilot units, stop
   rules) — construction guarantees degrade to assertions.
4. Caller-supplied values stay caller-supplied. *(Not a loss — recorded because
   nothing was invented to hide one.)*

**The deciding experiment:** a **third RHS, structurally different from both**.
If the same losses recur, they are the arrow's signature and it can be
specified. If different ones appear, one or two instances were never going to
be enough. *Abstracting from a single instance would encode that instance's
accidents as laws.*

## The two declines, carried here unresolved

`codex-4` was given real tool freedom for RHS #2 and declined both:

- **Malli — sound.** Zero-dependency character preserved; the *job* was done by
  a hand-written shape layer reporting structural failures separately
  (`:malformed-trace-boolean`). Function implemented, dependency refused.
- **core.logic — right call, wrong reason.** Declined as *"moving toward
  generating the negative matrix — the excluded generator direction."*
  **That over-reads the fence.** The prohibition was on an arrow for the
  *correspondence*; generating negative cases is mutation testing, which
  `M-apm-demonstration` V.9 identified as core.logic's one earning job
  (`futon3c/src/futon3c/logic/structural_law.clj` uses `l/run*` to **return the
  violating set**). **The fence was claude-2's to write clearly and was not.**

Joe: *"I'm not sure how I feel about these declines."* Recorded as unresolved,
not settled.

## Open items

1. Should negative-case generation via `core.logic run*` replace hand-written
   negative matrices? (Independent of the arrow question — **do not conflate
   again**.)
2. Does a third RHS confirm the loss list as the arrow's signature?
3. Should `baldwin_guidance_preregistration.clj` gain the staleness check that
   `futon3c.apm.preregistration` now has? Cheap; not ours to change
   unilaterally.
4. Joe notes a heavier direction: the experimental design **lifting** to a
   category-theoretic model of the pipeline on the Lean side. Out of scope here;
   noted so it is not lost.

## Non-goals

- Building the arrow now. **We lack the instances.**
- Blocking `M-apm-demonstration`. This excursion exists so that mission can
  proceed to INSTANTIATE without carrying an unresolved abstraction question.
