# E-giraffe-top-twenty-verdict — validate the party vote by working through its top twenty

**Status:** OPEN — verdicts 2/20 (M-giraffe, M-peradam-grounding: validated, Joe 2026-11-10)
**Parent:** M-giraffe (closing check). Gauntlet: `M-peripheral-gauntlet.md`
Checkpoint 2.
**Owner:** Joe + claude-13

## Why (Joe, 2026-11-10)

> "I think we should accept the verdicts 'on face value for now' and try to
> work through them in order. Granted, Giraffe comes top in the list so the
> validation of this way of choosing is part of the closure for this mission,
> but we could make it as an E-giraffe-top-twenty-verdict at the end of the
> mission rather than requiring that we settle it right now."

The party vote (four orderings taking turns: Merlin for strategy, Thor for
unblocking, Questor for quick ready items, Thyra for harm reduction) was
proposed on 2026-11-10 as the answer to Joe's reading that Giraffe alone is
Thor (M-giraffe §"Orderings"). Its picks are accepted at face value and worked
in order. A verdict is recorded for each as it is reached. The excursion closes
when all twenty have a verdict. That record is M-giraffe's evidence that this
way of choosing works, or the reason it does not.

## The list (frozen 2026-11-10)

Sources: Giraffe run `20261109T011400.188Z-4eb7a1c5`, claude-6's pending index
of 2026-11-08, and the lexicon graph's convergence scores. Page:
`wip/formal-arxana/giraffe-local.html`. Verdicts are one of: validated (right
priority, worked or being worked), not now (with a reason), wrong (with a
reason, which goes back to the role that picked it).

| # | item | picked by | index readiness | verdict |
|---|---|---|---|---|
| 1 | M-giraffe | Merlin | specified, multi-day | **validated** (Joe, 2026-11-10) |
| 2 | M-peradam-grounding | Thor | specified, multi-day | **validated** (Joe, 2026-11-10; see below) |
| 3 | M-jvm-arxana | Questor | ready (one small handoff) | |
| 4 | E-APM-f11-defects | Thyra | ready (one small handoff) | |
| 5 | M-daily-scan | Merlin | waiting on a short answer | |
| 6 | E-evidence-flow | Thor | may be no longer wanted | |
| 7 | T-overlap-modes-as-factor-precision | Questor | ready (one small handoff) | |
| 8 | E-fetch-entity-miss-path | Thyra | ready (one small handoff) | |
| 9 | M-repl-circus | Merlin | specified, multi-day | |
| 10 | M-interim-director-proxy-metric-inventory | Thor | specified, multi-day | |
| 11 | M-essay-corpus-substrate | Questor | ready (one small handoff) | |
| 12 | E-fix-cx-cr-runners-to-clock-in | Thyra | ready (one small handoff) | |
| 13 | M-vsatarcs-writer | Merlin | specified, multi-day | |
| 14 | E-memory-whitepaper-v2-programme | Thor | may be no longer wanted | |
| 15 | M-trip-journal | Questor | ready (one small handoff) | |
| 16 | E-futon1b-by-id-ignores-system-as-of | Thyra | ready (one small handoff) | |
| 17 | M-formal-arxana | Merlin | idea (stale: the mission is active) | |
| 18 | M-pattern-retrieval-calibration | Thor | specified, multi-day | |
| 19 | E-agency-turn-hardening | Questor | ready (one small handoff) | |
| 20 | E-question-driven-scope-coverage | Thyra | ready (one small handoff) | |

## Known in advance

- #6 and #14 are Thor's (Giraffe ranks them for what they unblock), while the
  pending index reads them as possibly dead. Their verdicts settle which of
  the two is wrong.
- Most picks by Merlin, Questor and Thyra are in no Giraffe cluster: no needs
  edge has been read for them, so Giraffe alone cannot see them.

## Verdict notes

**#2 M-peradam-grounding (Joe, 2026-11-10).** The mission (futon7) had 3 of 5
boxes done; the two live boxes were stuck because its one-entry curated map
mapped 0 of 35 live events and no dokusan verdict had been given. Joe:

> "we're in a good position b/c we can mint T3.4 as a new peradam (95%
> confidence) and watch it flow through the system. The certificate could
> come from reading the apm-lead repo, e.g., via my stack-hud-apm-report and
> observing that 95% of the prelims problems are solved in Lean. Personally I
> think that would be good enough to earn a pass, and it was the confidence
> level I was waiting for to publish the repo."

The report's own counts, read by claude-13 the same day from
`stack-hud--apm-scan` in Joe's Emacs: 491 problems, 480 with Lean material,
**461 solved in Lean with zero sorry**, 19 open, 29 executable sorries left.
That is 461/480 = 96.0% of those with Lean material, or 461/491 = 93.9% of all
problems.

Joe's bar, same day: "It's 95%+ with Lean material, the other ones were deemed
untranslatable (e.g. missing content)". So the T3.4 certificate checks
solved-with-zero-sorry / problems-with-Lean-material >= 95%, with the 11
problems without Lean excluded as untranslatable. At the reading above:
461/480 = 96.0%, met.

