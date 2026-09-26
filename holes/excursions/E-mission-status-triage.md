# E-mission-status-triage

**Status:** DEFERRED (Joe, 2026-09-21: "a deep dive I don't want to take right now, although it is useful information").
**Opened by:** claude-5, 2026-09-21.

## Why it exists

Joe has a global dispreference for a large backlog of unfinished missions; "inbox zero" in this
sense means every mission complete or retired. Recorded as a long-run preference for the War
Machine's C in `futon2/holes/labs/wm-contract/ANSWERS-C-questions-2026-09-21.md` (142c898d,
baseline 944dddc1).

## Baseline

Census `futon2/holes/labs/wm-contract/runs/mission-census-2026-09-21/` (16abf75a; rerun with
`census.py`): 642 docs in canonical repos — closed with witness 71, closed without witness 45,
open 359, undetermined 167. 307 open docs untouched 30+ days (age only, not judged).

## When resumed, in order

1. Give the 167 undetermined docs a one-line status (open / closed + witness / retired + reason);
   agent drafts, Joe confirms in batches. Largest pile: futon3c.
2. The 45 unwitnessed closes: find the witness or write a retirement reason.
3. The 307 old open docs, a repo at a time: keep, retire with reason, or merge into a successor.

Closure counts only with a witness (all obligations discharged, or retirement with a recorded
reason); a status-line edit alone does not (M-the-perfect-crime, third sweep).
