# D0 — where each fact comes from

Date: 2026-09-30. Discovery only; no stack writes or reloads.

## Method and boundary

The mission asks for record-backed pre/move/post triples and requires proxies
to remain labelled as proxies (`holes/missions/M-象-cascade.md:124-134`). The
receiving loader additionally requires every fact to have a mechanical
locator (`../futon2/holes/labs/wm-contract/NOTE-xiang-cascade-seam.md:31-40`).
The present WM locators only inspect a repository path, declaration, registry
bundle entry, witness reference, or current test-registry record: C3-C6 are
declared at `../futon2/src/futon2/aif/observation_checks.clj:37-44` and their
checks begin at lines 59, 80, 101, and 126. None reads chat evidence, a turn
analysis, a bell, a park, or an agency act.

`scripts/xiang_cascade_d0.py` performs the census. It reads the local turn
records only and verifies every commit it counts with `git cat-file -e
<sha>^{commit}`. It does not turn text into an unmarked fact. The futon1b
health read was healthy (3/4 permits, 791 MB heap), but no bulk LIST was made:
the local record contains the evidence id when that join is possible, and the
point read is exactly `GET :7073/api/alpha/evidence/<evidence_id>`. This keeps
the discovery within the requested small-read limit.

The census convention comes from `scripts/xlate.py:221-280`: direct pattern
references count citations; a candidate occurrence is either the candidate
file itself or a later rationale naming an already-known candidate. D0
deduplicates these occurrences by turn id. This explains why the current
unique-turn count need not equal the older occurrence totals in the mission's
2026-09-28 snapshot (`holes/missions/M-象-cascade.md:69-101`).

## Family 1: go-ahead under `orchestration/consent-gate`

Membership is the union of the seven proposal ids in the mission's table,
using the census convention above. It contains **35 unique turns**:

`turn-3ecbrN`, `turn-41Idyu`, `turn-4UBgMf`, `turn-762cGc`, `turn-7wcDgZ`,
`turn-8RCieU`, `turn-92dK9B`, `turn-ByKcmL`, `turn-DIIjU8`, `turn-FTBPle`,
`turn-G0DwY4`, `turn-GsGsXl`, `turn-HfZ1oZ`, `turn-IfsNvX`, `turn-MZx4U8`,
`turn-PMHiM7`, `turn-TTlwiX`, `turn-V6QPZi`, `turn-VjI6w9`, `turn-Y0IzMQ`,
`turn-dfZ8vv`, `turn-fxM8ZH`, `turn-gFfHfw`, `turn-gW5UNZ`, `turn-gtZeub`,
`turn-iNKHir`, `turn-iwA4xd`, `turn-k8jiYu`, `turn-kjzrZ9`, `turn-lPDozk`,
`turn-v7njT5`, `turn-wYIXMd`, `turn-wpkrDw`, `turn-xGoo8o`, `turn-xgtW1F`.

| Fact | Side | Definition | Exact record read | Coverage | Direct/proxy | WM locator |
|---|---|---|---|---:|---|---|
| Seat identity | pre | Agent and session whose preceding turn Joe answered | `~/.emacs-graph/session-turn-analysis/<turn>.json`, `agent_id` + `session_id` | 35/35 | direct metadata | none; needs `:turn-record` or evidence locator |
| Interpreted target | pre | At least one family fragment has a nonblank `target` | `<turn>.json.analysis.json`, matching fragment's `target` | 35/35 | **proxy:** 象 semantic parse | none; needs `:turn-analysis` locator and must preserve proxy mark |
| Operator evidence | move | Durable evidence id acknowledged for Joe's turn | `<turn>.json`, `evidence_id`; point-check with `GET :7073/api/alpha/evidence/<id>` | 5/35 | direct id (point read not bulk-read here) | none; needs `:evidence-id` locator |

There is **no positive post-fact coverage** in these 35 local joins. In
particular, the example `turn-Y0IzMQ` really does end at a semantic approval;
the mission already says “nothing followed” (`M-象-cascade.md:99-101`). A
first guard can use `interpreted-target-present` only as an explicitly marked
proxy. I do **not** propose a produces clause yet: manufacturing “work
advanced” from approval wording would violate the mission's evidence rule.
The first build packet should recover the assistant predecessor/successor
chain for these evidence-backed turns and emit typed missing reasons for the
other 30.

Offers and agreements would make “an offer was pending” direct, but there is
no offer/agreement id on these records. Searching all offer acts by time and
seat would be a temporal guess, not a join. The same applies to grants,
withdrawals, parks, and promises: none of these 35 records names such a record,
so D0 does not list them as available facts.

## Family 2: `apparatus/done-is-observed-running`

I chose this existing library family because its operator turns correct work
that was said or expected to be working: **13 direct citations**, versus the
three unique alignments for the narrower `operator/prefer-the-one-time-redo`.
Unlike a go-ahead, the family can eventually guard on the reported work state,
which is the reason criterion 6 calls for a correction family
(`M-象-cascade.md:185-190`). Membership is direct `pattern_refs[].id`, not a
keyword search:

`turn-0anLV2`, `turn-2GmY4p`, `turn-D2GnQn`, `turn-LNcsB2`, `turn-MCErMC`,
`turn-QLrMMq`, `turn-V5DsUN`, `turn-Xy8ygM`, `turn-ZUtRsq`, `turn-co177a`,
`turn-g9JGYE`, `turn-iNKHir`, `turn-qiO3lB`.

| Fact | Side | Definition | Exact record read | Coverage | Direct/proxy | WM locator |
|---|---|---|---|---:|---|---|
| Seat identity | pre | Agent/session whose work Joe observed | `<turn>.json`, `agent_id` + `session_id` | 13/13 | direct metadata | none; needs `:turn-record` or evidence locator |
| Interpreted target | pre | Cited fragment has a nonblank target naming the observed defect/work | `<turn>.json.analysis.json`, cited fragment's `target` | 13/13 | **proxy:** 象 semantic parse | none; needs `:turn-analysis` locator |
| Preceding work summary | pre | Machine-added summary of the agent turn Joe answered | `<turn>.json`, `happened_summary` | 3/13 | direct recorded summary, but a proxy for the full assistant row | none; needs `:chat-turn-chain` locator |
| Preceding commit exists | pre | Every repo/sha in that summary resolves as a commit | commit rows in `happened_summary`, then `git -C /home/joe/code/<repo> cat-file -e <sha>^{commit}` | 2/13 | direct existence; attribution comes from the turn record | none as stated; C3 requires a path, C6 requires a witness file |
| Operator evidence | move | Durable evidence id for Joe's correction | `<turn>.json`, `evidence_id`; point read as above | 5/13 | direct id | none; needs `:evidence-id` locator |
| Following agent response | post | The next operator record in the same session has a nonblank machine-added summary of the intervening agent response | order `<turn>.json` by `session_id, created_at`; read successor `happened_summary` | 3/13 | direct summary, proxy for response content | none; needs `:chat-turn-chain` locator |
| Following commit exists | post | Every repo/sha recorded in the successor summary resolves | successor commit rows + `git cat-file -e` | 2/13 | direct existence | none as stated; add `:turn-commit` locator (session/turn/repo/sha), or materialise a C6 witness |

The first defensible rule is deliberately narrow: among turns with a recorded
preceding-work summary, citation of `done-is-observed-running` guards on
`work-report-present`; it predicts `following-agent-response-recorded`. That
is 3/13 and should be tested against a declared base rate. A stronger produces
fact, `following-commit-exists`, is only 2/13. Both facts need a new evidence
or turn-chain locator before the WM can evaluate them. The pattern's semantic
claim (“the corrected system is observed running”) is **not** established by
a response or a commit, so neither fact may be renamed “fixed.” C3 can later
test a named path at a pinned sha, C4 a named declaration, and C8 a current
test result; those are suitable direct postconditions only after the corpus
records the relevant path/decl/test id.

## What the records cannot support yet

- The corpus does not provide complete pre/post triples. Go-ahead has no
  positive post join; the correction family has post summaries for 3/13.
- `target` and `happened_summary` do not prove the target condition true.
  They remain semantic/summary proxies, as required by the mission
  (`M-象-cascade.md:194-195`).
- A commit proves bytes changed, not that Joe's correction was satisfied.
- Bells, parks, promises, grants, withdrawals, offers, and agreements are
  queryable records, but these family records lack foreign keys to them.
  Time-window matching would invent identity.
- The current WM cannot evaluate any tabled fact directly. Claude-8's seam
  correctly identifies the missing evidence-store locator
  (`NOTE-xiang-cascade-seam.md:31-34`). A `:chat-turn-chain` locator and a
  `:turn-commit` locator (or stored C6 witnesses) are the minimum additions.

## Script output (2026-09-30)

```json
{
  "families": {
    "go-ahead": {
      "coverage": {
        "interpreted_target": {
          "covered": 35,
          "turns": [
            "turn-3ecbrN",
            "turn-41Idyu",
            "turn-4UBgMf",
            "turn-762cGc",
            "turn-7wcDgZ",
            "turn-8RCieU",
            "turn-92dK9B",
            "turn-ByKcmL",
            "turn-DIIjU8",
            "turn-FTBPle",
            "turn-G0DwY4",
            "turn-GsGsXl",
            "turn-HfZ1oZ",
            "turn-IfsNvX",
            "turn-MZx4U8",
            "turn-PMHiM7",
            "turn-TTlwiX",
            "turn-V6QPZi",
            "turn-VjI6w9",
            "turn-Y0IzMQ",
            "turn-dfZ8vv",
            "turn-fxM8ZH",
            "turn-gFfHfw",
            "turn-gW5UNZ",
            "turn-gtZeub",
            "turn-iNKHir",
            "turn-iwA4xd",
            "turn-k8jiYu",
            "turn-kjzrZ9",
            "turn-lPDozk",
            "turn-v7njT5",
            "turn-wYIXMd",
            "turn-wpkrDw",
            "turn-xGoo8o",
            "turn-xgtW1F"
          ]
        },
        "operator_evidence_id": {
          "covered": 5,
          "turns": [
            "turn-ByKcmL",
            "turn-FTBPle",
            "turn-G0DwY4",
            "turn-iNKHir",
            "turn-xgtW1F"
          ]
        },
        "seat_identity": {
          "covered": 35,
          "turns": [
            "turn-3ecbrN",
            "turn-41Idyu",
            "turn-4UBgMf",
            "turn-762cGc",
            "turn-7wcDgZ",
            "turn-8RCieU",
            "turn-92dK9B",
            "turn-ByKcmL",
            "turn-DIIjU8",
            "turn-FTBPle",
            "turn-G0DwY4",
            "turn-GsGsXl",
            "turn-HfZ1oZ",
            "turn-IfsNvX",
            "turn-MZx4U8",
            "turn-PMHiM7",
            "turn-TTlwiX",
            "turn-V6QPZi",
            "turn-VjI6w9",
            "turn-Y0IzMQ",
            "turn-dfZ8vv",
            "turn-fxM8ZH",
            "turn-gFfHfw",
            "turn-gW5UNZ",
            "turn-gtZeub",
            "turn-iNKHir",
            "turn-iwA4xd",
            "turn-k8jiYu",
            "turn-kjzrZ9",
            "turn-lPDozk",
            "turn-v7njT5",
            "turn-wYIXMd",
            "turn-wpkrDw",
            "turn-xGoo8o",
            "turn-xgtW1F"
          ]
        }
      },
      "pattern_ids": [
        "editing/approve-as-gate",
        "operator/approve-then-ask",
        "operator/curt-ok-before-the-real-report",
        "operator/in-that-case-proceed",
        "operator/ratify-the-recommendation-inline",
        "operator/yes-push-it",
        "orchestration/lightweight-ack-advance"
      ],
      "turn_count": 35,
      "turn_ids": [
        "turn-3ecbrN",
        "turn-41Idyu",
        "turn-4UBgMf",
        "turn-762cGc",
        "turn-7wcDgZ",
        "turn-8RCieU",
        "turn-92dK9B",
        "turn-ByKcmL",
        "turn-DIIjU8",
        "turn-FTBPle",
        "turn-G0DwY4",
        "turn-GsGsXl",
        "turn-HfZ1oZ",
        "turn-IfsNvX",
        "turn-MZx4U8",
        "turn-PMHiM7",
        "turn-TTlwiX",
        "turn-V6QPZi",
        "turn-VjI6w9",
        "turn-Y0IzMQ",
        "turn-dfZ8vv",
        "turn-fxM8ZH",
        "turn-gFfHfw",
        "turn-gW5UNZ",
        "turn-gtZeub",
        "turn-iNKHir",
        "turn-iwA4xd",
        "turn-k8jiYu",
        "turn-kjzrZ9",
        "turn-lPDozk",
        "turn-v7njT5",
        "turn-wYIXMd",
        "turn-wpkrDw",
        "turn-xGoo8o",
        "turn-xgtW1F"
      ]
    },
    "observed-running-correction": {
      "coverage": {
        "following_agent_response": {
          "covered": 3,
          "turns": [
            "turn-0anLV2",
            "turn-V5DsUN",
            "turn-co177a"
          ]
        },
        "following_commit": {
          "covered": 2,
          "turns": [
            "turn-0anLV2",
            "turn-co177a"
          ]
        },
        "interpreted_target": {
          "covered": 13,
          "turns": [
            "turn-0anLV2",
            "turn-2GmY4p",
            "turn-D2GnQn",
            "turn-LNcsB2",
            "turn-MCErMC",
            "turn-QLrMMq",
            "turn-V5DsUN",
            "turn-Xy8ygM",
            "turn-ZUtRsq",
            "turn-co177a",
            "turn-g9JGYE",
            "turn-iNKHir",
            "turn-qiO3lB"
          ]
        },
        "operator_evidence_id": {
          "covered": 5,
          "turns": [
            "turn-LNcsB2",
            "turn-MCErMC",
            "turn-co177a",
            "turn-g9JGYE",
            "turn-iNKHir"
          ]
        },
        "preceding_commit": {
          "covered": 2,
          "turns": [
            "turn-0anLV2",
            "turn-V5DsUN"
          ]
        },
        "preceding_work_summary": {
          "covered": 3,
          "turns": [
            "turn-0anLV2",
            "turn-V5DsUN",
            "turn-co177a"
          ]
        },
        "seat_identity": {
          "covered": 13,
          "turns": [
            "turn-0anLV2",
            "turn-2GmY4p",
            "turn-D2GnQn",
            "turn-LNcsB2",
            "turn-MCErMC",
            "turn-QLrMMq",
            "turn-V5DsUN",
            "turn-Xy8ygM",
            "turn-ZUtRsq",
            "turn-co177a",
            "turn-g9JGYE",
            "turn-iNKHir",
            "turn-qiO3lB"
          ]
        }
      },
      "pattern_ids": [
        "apparatus/done-is-observed-running"
      ],
      "turn_count": 13,
      "turn_ids": [
        "turn-0anLV2",
        "turn-2GmY4p",
        "turn-D2GnQn",
        "turn-LNcsB2",
        "turn-MCErMC",
        "turn-QLrMMq",
        "turn-V5DsUN",
        "turn-Xy8ygM",
        "turn-ZUtRsq",
        "turn-co177a",
        "turn-g9JGYE",
        "turn-iNKHir",
        "turn-qiO3lB"
      ]
    }
  },
  "records": "/home/joe/.emacs-graph/session-turn-analysis"
}
```
