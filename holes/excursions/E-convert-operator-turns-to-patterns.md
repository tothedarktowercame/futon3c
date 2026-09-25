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

One entry per "Real work:" turn: turn id, seat A's reading, seat B's job
and commits, and the discrepancies claude-12's review found.

### Round 1 — claude-12-turn-91 (turn-sVtZeL), 2026-09-25 ~20:18Z

Joe: the EFE field page shows no Tornhill features; "I would like to get a
plan back (with details in the mission) about how the integration will be
effected".

- **Seat A (kimi-2):** two fragments. s1a report-problem (0–166), matched
  to the proposed `operator/pin-the-defect-with-its-evidence` (cited in
  prose; the validator accepts only canonical ids); s1b ask-action
  (171–275), `orchestration/recorded-handoff` (the plan and its home, the
  mission, both named). Sound.
- **Seat B (kimi-1):** job `invoke-1790367782402-24419-d2f9561c`; futon3c
  `66f018a6`, a 66-line section "Integration plan — Tornhill features into
  the EFE field page" in M-the-perfect-crime.md, warrant in the section and
  the commit. Four layers (data, derived metrics, Arxana panel, EFE page),
  each with an outside acceptance check. Implementation listed as
  unwarranted, correctly: a plan was asked for.
- **Discrepancies:**
  1. *Gap at the step Joe asked about.* Layer 4 aggregates a hotspot score
     "over the files/vars in each mission's scopes", but
     `futon6/data/efe-scopes.json` carries no file references (0 found), so
     nothing joins a mission to the code it touched. The plan's central join
     is undefined; a mission→code join (commit best-guess, or clock lineage
     × commits) is missing as its own step.
  2. *Beyond the ask.* It declared the HEAD sub-question ("add Tornhill
     anyway?") answered, as "yes-but-with-discipline" — a decision that was
     Joe's.
  File claims checked and true: both migration files name
  `code/indentation`; the Arxana panel renders only `(when (or churn
  complexity) …)`; the page's generator has no churn input.

### Round 2 — claude-12-turn-95 (turn-NWwXB2), 2026-09-25 ~20:30Z

Joe: record discrepancies here, and "Real work: what is the minimal visual
improvement … that will let me see that the Tornhill information is being
considered?"; a plan with a visual change existed and was not carried out.

- **Seat A (kimi-2):** three fragments: defer (the tracking instruction,
  addressed to claude-12), ask-action (the minimal visible change,
  `operator/commission-the-smallest-reviewable-act`, prose cite),
  report-problem (`process/spec-drift-by-displacement`). Sound; it kept the
  @claude-12 part separate, and seat B left it to claude-12.
- **Seat B (kimi-1):** job `invoke-1790368419659-24422-ce1a1c40`; futon6
  `5fdcd71` (`scripts/mission_efe_field.py`: a yellow ring per district hub,
  thickness ∝ log of commits touching the mission's own doc in 180 days,
  dashed grey ring for zero; legend and hover say the complexity axis is
  pending), futon3c `cb0e68d0` (mission note). Warrants in both. Dedupes
  commits across the worktree repos that share history; its count for
  M-the-perfect-crime (9) matched `git log` on futon3c (claude-12's recount:
  10, the extra one being `cb0e68d0` itself).
- **Discrepancies:**
  1. *The label claims more than the ring measures.* It counts commits to
     the mission's own doc, not code churn, yet the legend calls it "Tornhill
     change-frequency" and the hover "Tornhill churn". Tornhill's churn is
     change frequency of the code under study; this is activity on the
     planning document — the kind of claim the mission itself is about.
  2. *Not visible yet.* The page is a gitignored artefact regenerated
     locally; nothing published it, so https://zone.hyperreal.enterprises/wip/mission-efe-field.html
     is unchanged. Publishing (`futon6/scripts/publish-efe-field.sh`) was
     outside seat B's limits.
  3. *Small factual slip:* it says it recorded the plan "yesterday"; it was
     earlier the same day (66f018a6).

### Round 3 — claude-12-turn-97 (turn-tDqSTw), 2026-09-25 ~20:32Z

Joe: the EFE page shows agents (flying saucers) that don't exist; the layout
may lag, but the live annotations should match the Agency.

- **Seat A (kimi-2):** two fragments: report-problem (the phantom agents,
  pinned by URL, hedged), constrain (a freshness contract per layer: layout
  may lag, annotations may not; candidate
  `operator/set-the-freshness-contract-per-layer`). Sound, and s2 names the
  requirement exactly.
- **Seat B (kimi-1):** job `invoke-1790368573628-24424-a5431739`; futon6
  `89f030e` (`scripts/mission_efe_field.py`, 25 lines: the page's live
  layer draws an agent only if its Agency status is invoking/idle, shows
  "N stale agent annotation(s) withheld"). Warrant present. Diagnosis: the
  :7070 JVM predates futon3c `5b14a6f6`, so a restart would fix it.
- **Discrepancies:**
  1. *Wrong cause.* The published page is a static snapshot: the publish
     script inlines the overlay and the page "performs no browser-time live
     fetch" (`futon6/scripts/publish-efe-field.sh:123`). It lists 103
     agents; 49 of them are not on the roster now (the Kimi and codex seats
     deleted ~20:05Z, plus claude-7, claude-9). The phantoms are the
     snapshot's age, not the server's filter. Nothing in the fix makes the
     annotations track the Agency between publishes, which is what s2 asked.
  2. *The predicate repeats round 2 of the old chain.* invoking/idle counts
     almost every agent as active (the reason `5b14a6f6` changed 1 saucer);
     a deleted agent is simply absent from the registry, so filtering by
     status neither helps nor is needed for it.
  3. *"Needs a restart" is wrong on two counts:* a reload from master is
     enough to load `5b14a6f6`, and loading it would not remove these
     phantoms.
  File and count claims otherwise checked: one file, 25 lines, warrant in
  the commit.

