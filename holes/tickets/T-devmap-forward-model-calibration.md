# T-devmap-forward-model-calibration — grade a frozen forward model against what happened

Raised by joe + claude-13, 2026-08-24, out of the futon-2026 paper work.
Third in the series with `T-forward-model-vs-active-work.md` and
`T-exogenous-evidence-update-rule.md`.

**Status: DESIGNED, NOT STARTED.** Joe's call: ticket it rather than deep-dive
now. The rubric below is the design; nothing has been graded.

## Why this corpus is worth the trouble

`futon3/holes/features/*.devmap` — nine files, 3,379 lines, one per futon repo,
each a forward model written as typed fields plus regenerated prose:

```
@IFR    what this is for
@state  where it has got to
@next   what happens next
```

All nine carry `@IFR`, `@state`, `@next`. **None carries a date.**

The property that makes this a study rather than an anecdote:

```
9 files, 1 commit each, all authored 2026-04-27, never revised since.
```

Four months elapsed, zero edits. So the predictions are **outcome-blind by
construction, not by discipline** — nobody could have tuned them to look
prescient because nobody touched them. This is the condition futon-2026's Part
III corpus imposes on itself by rule ("fields describing what was knowable at
the time are filled blind to the outcome"), satisfied here for free.

**27 `@next` clauses** are gradeable in principle:

| devmap | clauses | | devmap | clauses |
|---|--:|---|---|--:|
| futon0 | 1 | | futon4 | 3 |
| futon1 | 3 | | futon5 | 2 |
| futon2 | 4 | | futon6 | 3 |
| futon3 | 6 | | futon7 | 4 |
| futon3a | 1 | | **total** | **27** |

## Why it belongs in the paper (SI, not the body)

futon-2026 argues on structural grounds that the cascade will not stay current.
This corpus is the same stack's *previous* forward model with outcomes now
known — direct evidence about the cascade's likely fate rather than an argument
about it. SI is the right home: N=9, one author, one day, so it is a case study
and must not be dressed as a statistic.

## Rubric (fix this BEFORE grading — that is the point)

**Two values per clause, because outcome has a time shape.** Joe's worked
example: "Stabilize invariant coverage" *kind of happened, then turned out to be
unmaintainable as specified*. A single score at t=now records that as a
near-miss, which misrepresents it — the thing was built; durability failed, not
prediction.

- **peak — did it ever land?** Coarse three-way: `reached` /
  `partially-reached` / `never-reached`. Deliberately coarser than `current`:
  establishing peak needs archaeology (git log, mission files, run archives), so
  it is the expensive and uncertain half. Grain matches evidence quality.
- **current — does it hold today?** Anchored five-band, not free 0–100:
  - `0` nothing in this direction exists
  - `25` attempted, abandoned or superseded early
  - `50` partially delivered, core missing
  - `75` delivered, with gaps or caveats
  - `100` delivered as specified and still holds
- **decay-reason** — filled only when peak > current, from a **closed** list:
  `unmaintainable-as-specified` · `superseded-by-redesign` ·
  `dependency-removed` · `abandoned-unfinished` · `still-held`.
  Free text here would yield 27 anecdotes; the fixed list yields a distribution,
  and the distribution — *how* this stack's forward models fail — is the result
  worth having.
- **evidence** — every score cites an artifact (path, sha, run record) or
  explicitly records `no-artifact-found`. Never blank.

**Reporting rule.** Distribution across bands, plus the count where
peak > current. **Never a mean.** "Mean satisfaction 61.3%" is false precision
on 27 subjective judgements.

**`ungradable` is a result, not missing data.** Some clauses have no satisfaction
condition even in principle ("Stabilize invariant coverage" as written). If N of
27 land there, that measures the forward model's own *checkability* — which is
exactly the cascade's defect today (22 rungs with no defined liveness check).
Expect this to be the most useful number in the study. Do not drop them.

## Who grades

**Not claude-13.** The named risk is generous self-assessment, and I have spent a
long conversation arguing a thesis these numbers bear on. Split:

1. claude-13 writes the rubric (this ticket) — done, and fixed before any result
   exists.
2. A Codex agent grades all 27 **blind to the expected result**, citing evidence
   per clause.
3. claude-13 reviews the grades against the artifacts.

Same discipline futon-2026 applies to its own two self-cases: apply the terminal
rule mechanically and report the result rather than explaining it away.

## Threats to validity (state these in the SI, do not bury them)

- N=9 devmaps / 27 clauses, one author, one authoring session, one stack. A case
  study. Not independent samples.
- Grading is post hoc even though the predictions are not. Mitigated by fixing
  the rubric first, not by the grader's good intentions.
- Survivorship: these are the devmaps that still exist. Deleted ones are invisible.
- Four-month horizon only. A clause could still land in month five.

## Explicitly out of scope

**Not porting devmap content.** Joe, 2026-08-24: "I'm not saying that we need to
port the content of the old devmaps." The devmap *form* is the ancestor of the
card schema (`card = devmap + typed register + date + promotion test`, see
`T-forward-model-vs-active-work.md`); the content stays where it is.

## Related

- `T-forward-model-vs-active-work.md` — the card/board design this grew out of.
- `T-exogenous-evidence-update-rule.md` — same series.
- Paper: futon-2026 SI (proposed); `p4ng/sec-evaluation-outline.tex` for the
  encoding disciplines this rubric borrows.
