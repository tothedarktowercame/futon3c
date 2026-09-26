# HANDOFF — M-the-perfect-crime: what the 2026-09-26 Tornhill work left open

Mission: `holes/missions/M-the-perfect-crime.md`, "Checkpoint 2026-09-26 — Tornhill at file
grain, joined to the agent chat". Prepared 2026-09-26 by a Claude Code terminal session
(not an Agency seat, so there is no bell-back address). **Whoever dispatches a packet: bell
yourself back as the reviewer, with summary + commit shas, per the coding-handoff protocol
in `~/code/CLAUDE.md`.**

The work is split into packets, one behaviour each, per that protocol. Discovery and
implementation are separate packets. Packet 0 and packet 3 are decisions for Joe, not coding.

| Packet | Kind | Needs Joe? |
|---|---|---|
| 0. Transcript text in public futon0 | cleanup | **yes, first** |
| 1a. `mission_activity.py` emits each mission's file list | code, futon6 | no |
| 1b. EFE code ring reads the Tornhill report | code, futon6 | no (after 1a) |
| 2. Chained-claim sweep over agent turns | discovery, futon0 | no |
| 3. Should futon1b hold these metrics? | decision | **yes** |
| R. Independent review of `tornhill.py` / `tornhill_chat.py` | review | no |

---

## What exists now (read before any packet)

- `futon0/analysis/audits/tornhill.py` — `collect` / `check`. Per repo, 90 days of HEAD
  history, no merges. Per code file: `revs`, `churn`, `complexity` {`total`, `mean`, `max`,
  `loc`} (indentation), `hotspot` (= revs × complexity.total), `age_days`,
  `born_in_window`, `models`, `main_model`, `sum_of_coupling`, and `trend` / `trend_ratio`
  for each repo's top 10. Per repo: `coupling` pairs {`a`, `b`, `shared`, `degree`, `twin`}.
- `futon0/analysis/audits/tornhill_chat.py` — `collect` / `check`. Joins commits → sessions
  (futon6 `data/session-commit-index.json`) → transcripts. Per file: `seats`, `main_seat`,
  `n_seats`, `operator_turns`, `operator_share`, `direct_turns`, `compacted_sessions`,
  `stance`, `corrections`. Also `sessions`, `seats`, `session_coupling`.
- Output: `~/.local/share/futon-audits/tornhill/tornhill-YYYY-MM-DD.json` and
  `tornhill-chat-YYYY-MM-DD.json`. They hold session ids and seat names, no turn text, and are
  not in any repo. Check results: `futon0/analysis/audits/tornhill*-2026-09-26.check.txt`.
- Tests: `futon0/analysis/audits/test_tornhill.py` (13, `python3 -m unittest test_tornhill`
  from that directory).
- Notebook: `marimo-zone/notebooks/tornhill-crime-scene-20260926.py`.
- Rules that apply throughout:
  - `[[run-data-is-data]]`: run output is not committed; commit the claim and its check.
  - `subsumption-claim-discipline.flexiarg`: every new number gets a check against a second
    reading of its source; missing input is shown as missing, never as zero.
  - Anything containing Joe's turn text goes under `~/.local/share/futon-audits/`, never in
    a repo. futon0, futon3c and futon6 are public.

---

## Packet 0 — transcript text already committed to public futon0 (Joe decides)

Found while writing this handoff. These tracked files in public
`tothedarktowercame/futon0` contain transcript text:

| File | Rows | Text fields |
|---|---:|---|
| `analysis/audits/operator-reply-pilot-2026-09-21/all_pairs.jsonl` | 412 | `operator_text`, `agent_text` |
| `analysis/audits/operator-reply-pilot-2026-09-21/sample_pairs.jsonl` | 100 | `operator_text`, `agent_text` |
| `analysis/audits/operator-reply-pilot-2026-09-21/dialogue_context.jsonl` | 1,633 | `text` |
| `analysis/audits/park-wake-pilot-2026-09-21/turns.jsonl.gz` | 8,817 | `trigger_text`, `final_preview` |

Committed in `93dfd2a` ("Add full-text operator reply pilot …") and the park/wake pilot
commit. Checked and free of long text fields: `pattern-stage-joins-2026-09-21.jsonl`,
`intent-vocabulary-2026-09-22-annotations.jsonl`, `product-census-2026-09-21-ledger.jsonl.gz`.
The `all_pairs.jsonl` rows also carry `/home/joe/.claude/...` source paths.

This is the same situation as futon1b's evidence slice on 2026-09-26. The procedure used
there is written up in `~/code/storage/_cleanup-2026-09-26/futon1b/README.md`: move the files
to an untracked location, point readers at it, `git filter-repo` the paths out of all
history, then delete and recreate the GitHub repo (a force-push leaves old commits
reachable by sha).

**Joe to decide:** purge futon0 the same way, or accept these files as published. If
purging: check whether other clones of futon0 exist on lucy-joe / metameso (see
`~/code/old-clones.txt` for how the futon1b clones were handled) before recreating the
repo. The pilot notebooks in marimo-zone read these files, so move them to
`~/.local/share/futon-audits/<study>/` where the pilot's private labels already live, and
update the readers (`extract_pairs.py`, `measure.py`, `render.py`, `test_pilot.py`, and the
two marimo notebooks) in the same change.

---

## Packet 1a — `mission_activity.py` emits each mission's resolved file list (futon6)

**Goal.** The EFE page cannot use per-file Tornhill data yet, because
`futon6/data/mission-activity.json` stores only aggregates per mission (`code.files_resolved`
is a count). Add the list.

**Files.**
- `:in` `futon6/scripts/mission_activity.py`. `parse_edges()` reads mission→var `touches`
  edges from `data/fold-embed/edges.jsonl`, `resolve_var()` maps a var to `(repo, relpath)`,
  and `main()` builds the per-mission `code` block.
- `:out` the same script. Add `code.files: [[repo, relpath], …]`, sorted and deduplicated,
  for every mission with at least one resolved file. Do not change existing fields.

**Constraints.** futon6 rules: work on master (`[[futon6-work-on-master]]`); no host paths
in code (`[[futon6-no-host-paths]]`; resolve roots via `futon6_config`). The script's
uncommitted local edits (a futon1b `v05` pull, 2026-09-25) are Joe's work in progress.
Leave them alone, and ask before committing anything that touches the same hunks.

**Acceptance.**
- For 3 missions, `len(code.files) == code.files_resolved`.
- A test in `futon6/tests/` on a synthetic edges file: two vars in the same file give one
  entry, and an unresolvable var is absent from `files` and counted in `vars_unresolved`.
- `git grep -n '/home/joe' -- scripts tests` shows no new hits.

---

## Packet 1b — the EFE code ring reads the Tornhill report (futon6, after 1a)

**Goal.** The pink "code churn" ring on `mission-efe-field.html`
(`futon6/scripts/mission_efe_field.py`, `code_churn_ring`) currently shows
`mission_activity.py`'s own git pass: commits to the mission's files, and one complexity
number. Make it a lens over the file-level Tornhill report. The mission's files come from
1a, and each file's numbers from `tornhill-*.json`.

**Files.**
- `:in` the newest `tornhill-YYYY-MM-DD.json` (and optionally `tornhill-chat-*.json` for
  seats). Locate it with `futon6_config.path("FUTON6_TORNHILL_REPORT", <default>)`, where
  the default is the newest file in `Path.home() / ".local/share/futon-audits/tornhill"`.
  This is not a literal host path; Rob's checkout will simply not have the file.
- `:out` `mission_efe_field.py`.
  - Per mission, over `code.files`: sum of `revs`, sum of `hotspot`, and the top 3 files by
    hotspot, with each file's trend (`trend_ratio`, or "new" if `born_in_window`).
  - Ring thickness is proportional to log(sum of hotspot).
  - The hover lists the top files and names the report file and its `generated` time.
  - If `tornhill-chat` is present, the hover also gives the number of distinct seats.

**States, all explicit (the defect class this mission names):**
- Report absent: one legend line, "No Tornhill report — code ring not drawn", and no rings.
  Never fall back silently to the old computation.
- Mission with no mission→code link: the existing dashed grey ring.
- Link, but none of its files in the report (unchanged in 90 days, or not code): its own
  dashed style, with a hover saying so.
- Measured: a solid ring.

**Acceptance.**
- Cross-check, written to `futon6/data/mission-efe-tornhill.check.txt` (gitignored like the
  rest of `data/`): for 3 measured missions, the hover's summed revisions equal a sum
  computed by hand from the JSON. For 1 of them, `git log --since=90.days --no-merges
  --full-history -- <file>` on each file agrees with the JSON's `revs`.
- A test renders the page with a two-file fixture report, and with no report, and asserts
  each state's marker is present.
- The legend no longer says the complexity axis is pending.
- The mission doc's "Next" line is updated in the same commit.

---

## Packet 2 — chained-claim sweep over agent turns (discovery only, futon0)

**Goal.** The mission's third sweep asked for this: "run the first sweep's verb list over
agent final turns, join each hit to the operator's next-turn stance, and live-check the
accepted ones". This packet does the first two steps only. It produces **candidates**, not
verdicts. The live-check is a later packet, after review.

**Files.**
- `:in`
  - Verb list: `futon3c/holes/M-the-perfect-crime.audit.edn` `:audit/verbs`: "subsumes",
    "subsumption", "becomes free", "for free", "falls out of", "by-product",
    "as a (free) bonus". Match case-insensitively, on word boundaries.
  - Pairs (agent final message → Joe's next turn): generalise
    `futon0/analysis/audits/operator-reply-pilot-2026-09-21/extract_pairs.py`. It pairs only
    three hard-coded seats (`SESSIONS` dict: claude-12, claude-4, claude-5). Take the session
    list from `tornhill-chat-*.json` `sessions` where `kind == "claude"` and
    `operator_turns > 0` instead.
  - Stance: `stance_labels.csv` in the same folder (codex-14, 100 pairs), plus Joe's blind
    labels at `~/.local/share/futon-audits/operator-reply-pilot-2026-09-21/joe_labels.csv`.
    Joe's label wins where both exist.
  - Hotspot context: `tornhill-chat-*.json`. Flag a hit whose session changed any file in
    the top 50 hotspots.
- `:out`
  - Private (contains text): `~/.local/share/futon-audits/perfect-crime-sweep/hits-YYYY-MM-DD.jsonl`,
    one row per hit: session id, seat, agent-turn uuid and timestamp, verb, the sentence
    containing it, Joe's next-turn uuid and timestamp, stance label and labeller if any,
    and the hotspot flag.
  - Committed: a script `futon0/analysis/audits/claim_sweep.py` (collect + check, standard
    library only) and `claim-sweep-YYYY-MM-DD.md` with **counts only, no text**: hits per
    verb, per seat and per week; how many have a stance label and what the labels are; and
    how many are in hotspot sessions.

**Acceptance.**
- `check`: for 10 sampled hits, the sentence is found at the recorded uuid in the
  transcript. For 5 sessions, the number of agent turns scanned equals a separate count.
- Tests on a synthetic transcript:
  - a verb inside a code block or quoted from the mission doc is not a hit (state the
    rule you choose);
  - "for free" at a word boundary is a hit;
  - a pair whose operator turn is a park resume is excluded (census rule).
- The summary names its limits: 100 + 20 labels; Claude only (Codex final messages are a
  later extension); a candidate is not a crime.

**Discipline.** Per the mission's Baudrillard caveat, the sweep must not become another
high-definition picture standing in for judgement. The summary says what the counts do
not show.

---

## Packet 3 — should futon1b hold these metrics? (Joe decides)

Plan layer 1 (2026-09-25) was to fix futon1b's migration type list (`code/indentation` is
listed but nothing writes it; `code/indentation-complexity` is written but not listed) and
re-run the futon4 L0 ingest (`futon4/scripts/ingest-three-columns.py`). The Tornhill
pipeline now reads git directly and is checked, so the store is not needed for the
notebook or the EFE page.

**Options.**
- **(a) Close the store route.** Mark plan layer 1 superseded in the mission, and leave
  the futon1b types empty. Still do plan layer 3 as a small packet: make
  `futon4/dev/arxana-browser-enrich.el` say "no churn data in store" when it has none. It
  renders only `(when (or churn complexity) …)`, so an empty store currently looks like a
  file with no churn.
- **(b) Store the Tornhill metrics.** A new ingest writes each file's `revs`, `churn`,
  `complexity`, `hotspot` and `trend_ratio` from `tornhill-*.json` into futon1b as
  hyperedges; fix the migration list; the Arxana panel reads them. Costs: a second copy of
  numbers that are cheap to recompute, and a new staleness question (as of which run?).

**Recommendation: (a), plus the layer-3 panel fix.** The layer-3 fix is needed under
either option.

---

## Packet R — independent review of the two scripts

`tornhill.py` and `tornhill_chat.py` were written directly by a Claude session, not
belled out, so author ≠ reviewer has not happened yet. Review per `~/code/CLAUDE.md`
("Coding-handoff protocol", jobs 1–3):
- Test adequacy first. Do the 13 tests pin the claims in the mission checkpoint? Would they
  catch a wrong complexity unit, a double-counted session, or coupling that includes a
  sweep commit?
- Then spot-check the two `check` outputs against a hand reading of one repo.

Record the verdict under the mission's 2026-09-26 checkpoint.
