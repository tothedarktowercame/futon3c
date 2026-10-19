# E-象-repl-cycle: the four players as a Read/Evaluate/Print/Loop cycle

Agreed with Joe, 2026-10-19 ("Okay, I think we have a working model now").
Source of the cycle: `t4r/supplement.flexiarg` (workshop phases and roles;
"keeps the Print phase accountable to what Read reveals"; patterns that did
not fit are kept on record, not dropped).

## The mapping

| Phase | Player | What it does | What it writes |
|---|---|---|---|
| Read | 象 | Tags Joe's turn with the proforma vocabulary; retrieves a pattern set | proposed patterns + scores |
| Evaluate | active agent | Does the work; marked paragraphs name the acts they answer | reply, commits |
| Print | 馬 (reporter) | Checks reply and commits against the proposed set | per-pattern verdict: `used` / `didn't-apply` / `missed` |
| Loop | Joe's next turn | 象's tags on that turn (approve, disagree, redirect, retract ...) plus hard signals (commits, PROVED, accepted reviews) | outcome for the previous row |

`missed` means a pattern the turn needed that 象 did not propose (example:
the `t4r` patterns on the turn where Joe had to name the folder).

The outcome comes from Joe's next turn, not from 馬. That answers the earlier
worry that 馬 would be grading its own reading. `didn't-apply` is still 馬's
judgement.

## Diagram page

One row per turn, four columns: Read, Evaluate, Print, Loop. The Loop cell is
filled one turn later. Per-pattern counts accumulate over a session
(proposed / used / didn't-apply / missed, by outcome). A slower pass can use
them to adjust retrieval weights, record evidence against patterns that keep
not applying, and propose links for patterns that keep being missed.

## Relation to WM

Each row is a labelled example of (situation, proposed pattern set, patterns
used, witnessed outcome). `problems/g-over-cascade-is-undefined` says WM has no
outcome space for a cascade; these rows are one, at turn grain, labelled by the
operator. Read as data for `problems/r4-forward-model`, they are what a model
of "pattern set → outcome" would be trained and scored against before WM picks
a cascade. This is a source of examples, not a solution to R4 or R5.

## Build order

1. Print only: 馬 adds a per-pattern verdict to its card; the Turn view shows it.
   (Small packet, one seat.)
2. Loop join: attach 象's tags on turn n+1 as the outcome of row n. (Second
   packet, after reviewing 1.)
3. Diagram page rows and counts.
4. Slow pass over the counts.
