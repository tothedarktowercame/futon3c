# E-convert-operator-turns-to-patterns — Joe's turns read in pattern terms, then carried out

Opened 2026-09-25 by claude-12 at Joe's request, as the requisition for the
seat that interprets his turns and as the record of the experiment.

## What we are doing

A two-seat chain. Joe writes a turn. **Seat A** reads it in pattern terms:
fragments of his text, each with an intent, the library patterns it cites
(`futon3/library/<id>.flexiarg`) or a typed hole, and a derived cascade.
**Seat B** gets Joe's original text together with seat A's reading and
carries out what he asked. claude-12 sets up the dispatch and reviews each
round as it comes in, so we learn what works and what does not and improve
it. Nothing runs in parallel; one turn at a time.

## Who does what

| role | seat | how |
|---|---|---|
| Seat A, interpreter | kimi-2 | session-mode's labeller (`session-mode-analysis-agent`, set in the `server` Emacs 2026-09-25 ~20:15Z). Every turn recorded by `session-mode-turn-tags-mode` goes to it with this requisition; its reading is written beside the turn record and shown on https://zone.hyperreal.enterprises/wip/turns/feed.html (margin notes, underlined cues, cascade column). |
| Seat B, executor | kimi-1 | one persistent seat on M-the-perfect-crime: every task carries `Requisition: M-the-perfect-crime — …`, so its conversation continues from task to task. Oriented 2026-09-25 on the mission doc and audit (job `invoke-1790366997195-24407-0c250847`). |
| Dispatch and review | claude-12 | sends seat B each "Real work:" turn with seat A's reading unchanged; reports per round what A read, what B did, and where the chain lost or added something. |

Trigger: a turn to claude-12 that begins "Real work:". Other turns are
conversation with claude-12.

Recording starts when the feature is turned on; there is no backfill.
claude-12's buffer has recorded since 2026-09-25 19:50Z; claude-1's buffer
records too and also goes to seat A.

## Seat B's standing rules (from its orientation brief)

- Joe's words are what is carried out; seat A's reading is how to
  understand them. Where they disagree, follow Joe and say so.
- A warrant at every change (comment or docstring line, and a `Warrant:`
  line in the commit) naming the turn, the fragment or pattern, and in one
  clause what it asked for.
- Anything thought needed but not asked for is listed under "Unwarranted,
  not done", not done.
- Explicit-path commits, never amend or stash; no loads into the shared
  JVMs; no War Machine runs; nothing under `data/`.

## Before this (2026-09-25, same day)

Two earlier rounds used a different chain: seat A wrote an authored cascade
(`storage/operator-turns/translations/`, `futon3c/scripts/operator-round/`)
and seat B was a fresh seat per turn through `kimi-task.sh`. In both rounds
seat A softened what Joe asked ("continue" became "refresh status"; "clean
up the kimi list" became "prune the display"), and seat B, following the
warrant rule, correctly declined the part left out. Round 2's code change
(futon3c `5b14a6f6`) is committed, not loaded, and did not fix the
reported case. A fresh seat per turn gave seat B no context; hence the
persistent seat above. A backfill of Joe's earlier turns, which he did not
ask for, was cancelled and its records set aside
(`/tmp/claude12/backfill-records/`). All earlier Kimi and codex seats were
deleted at Joe's request before this chain was set up.

## Rounds

(one entry per "Real work:" turn: turn id, seat A's reading, seat B's job
and commits, review)
