# C6 — from a live gap to its closing

Date: 2026-09-30. Discovery only; no stack writes or reloads.

## Method

The opening rule is stricter than “a commit followed the turn.” `done-is-observed-running`
says the mechanism must be observed acting in the live system
(`/home/joe/code/futon3/library/apparatus/done-is-observed-running.flexiarg:10-22`).
The proposed second step is stricter again: `bind-the-subject` accepts only the
currently bound warrant minted strictly after the incident
(`/home/joe/code/futon3/library/test-registry/bind-the-subject.flexiarg:10-12,22-26`).

`scripts/xiang_cascade_c6.py` reads the 12 `live-gap` rows from
`corpus-c4.jsonl`, their local operator records, and their 象 analyses. Its
semantic classifications are an explicit manifest at lines 26-75, rather than
a keyword matcher. It verifies each operator quote against the named record,
checks that every cited fragment has a polarity, and computes elapsed times,
reopenings, and totals. Four small futon1b LIST reads (one per relevant
session, `Accept: application/json`, `limit=1000`) supplied the exact record
ids below. They returned 419, 1000, 1000, and 571 rows; the needed rows were on
the returned pages. No retry was needed. The queries were:

`GET :7073/api/alpha/evidence?session-id=<session>&limit=1000`

A “record” close below is a stored post-report row whose text says the named
thing acted. It is stronger than a commit but is not automatically a
`bind-the-subject` warrant. “None found” means no later same-session operator
observation or record could be joined to that target; it does not mean the
thing never recovered. Silence leaves the gap open.

## Per-gap result

| gap turn | interpreted target | closing kind | evidence and observation | time to close | later same-target gap? |
|---|---|---|---|---:|---|
| `turn-0anLV2` | stale frame 103 / automated reload | none-found | No later joined observation that automated reload acted | — | no |
| `turn-D2GnQn` | live About-page reorder | record | `e-49bef50f-08de-4c31-b9fc-4fc1206b2721`: “It's deployed: the live About page now shows…” | 16.6 s | no |
| `turn-LNcsB2` | retry persistence / m02J02 and m02J03 | operator-confirmation | `turn-Ie4yZG`: “m02J02 is up to 47 attempts”; 象 labels the relevant fragments `qualify` and `report-problem` | 1 h 50 m 29 s | no; the new problem is infrastructure, not early give-up |
| `turn-MCErMC` | War Machine abstaining despite model/tests | none-found | Later requirements and redesign discussion do not show this machine acting | — | no |
| `turn-QLrMMq` | desired futon1b features loaded | record | `emacs-551ccf0648fef009bc0de98c4856302a`: “all of it is loaded” after the restart | 50.4 s | no |
| `turn-V5DsUN` | missing live stepper `r/R` bindings | record | `emacs-3acd725c904774e54ef50d5a727f6b1f`: the keys “now work in your Emacs” | 43.1 s | no |
| `turn-Xy8ygM` | inbox-zero acting live | operator-confirmation in the cited turn itself | `turn-Xy8ygM`: “live example of how inbox zero is working now”; 象 labels it `explain` | 0 s | **yes:** `turn-iNKHir` later says repeated attempts remain “not convinced” |
| `turn-ZUtRsq` | liveness of 象 turn annotation | record | `emacs-2eb6424cb522018a9356ee8aa4b11f26`: “象 has annotated your last turn” | 2 m 49.4 s | no |
| `turn-co177a` | missing buffer half of rewind | none-found | The immediate response explains why it failed; no later joined observation of that buffer rewind succeeding | — | no |
| `turn-g9JGYE` | empty frame 89 after fix attempt | none-found | The response explains timing, but later `turn-quBqCC`, `turn-UjFCVT`, and `turn-BVIor6` still report stale/wrong display | — | no reopening: it never closed |
| `turn-iNKHir` | inbox-zero still unconvincing | none-found | Subsequent design work does not show a clean-repo/rewind observation | — | no |
| `turn-qiO3lB` | analyzed cue absent while typing | none-found | A commit persisted cue proposals, but no record shows the cue appearing while typed | — | no |

## Counts and consequence for the cascade

- Closed by later operator observation: **2/12** (one is a same-turn positive
  observation that should not have entered an opening-only family).
- Closed by a post-report record of the behaviour: **4/12**.
- Still open in the available same-session record: **6/12**.
- Median time to close among the six closes: **46.72 seconds** (including the
  same-turn confirmation at zero seconds).
- One closed target later reopened: inbox-zero, `turn-Xy8ygM` →
  `turn-iNKHir` after about 48 h 33 m.

The pattern is carrying both halves. Across its 18 cited fragments in these
12 turns, the reviewed polarity count is **13 opening/gap fragments, 2
closing/positive fragments, and 3 contextual or conditional fragments**.
`turn-LNcsB2` contains both an initial positive report and conditional failure
probes; `turn-Xy8ygM` is purely a positive observation. 象 therefore needs a
stored polarity (or separate opening/closing intents) before this citation can
serve as a cascade transition.

The prompt names `turn-NjpuY7` (“No, it works now, nevermind”) as another
confirmation citation. The current stored final analysis does **not** cite
`done-is-observed-running`: it labels the fragments `report` and `retract`,
and its candidate proposes `coordination/stand-down-when-the-defect-clears`.
That still demonstrates the missing closing half, but counting it as a current
citation would contradict the stored analysis.

The broad “fresh observation after the report” condition is recoverable for
**6/12**, of which **4/12** have a direct post-report system/agent record and
**2/12** need semantic interpretation of Joe's words. The exact
`bind-the-subject` condition is mechanically evaluable for **0/12**: none of
these gap turns names a test-registry subject, incident, bound warrant, or
warrant time. The four direct records are useful closing evidence, but calling
them currently-bound fresh warrants would invent fields they do not carry.
A usable cascade needs stored target identity plus polarity on the incident,
and either a later operator observation naming that target or a locator over a
subject binding and fresh warrant. Current records cannot establish target
identity across paraphrases without 象's semantic proxy, cannot prove that no
unrecorded recovery occurred, and cannot turn a commit or explanatory reply
into an observation of live behaviour.

## Reproducible output

```json
{
  "summary": {
    "closed_by_operator": 2,
    "closed_by_record": 4,
    "direct_record_closure": {
      "count": 4,
      "denominator": 12
    },
    "median_seconds_to_close": 46.720841,
    "open": 6,
    "pattern_fragment_polarity": {
      "closes": 2,
      "context": 3,
      "opens": 13
    },
    "strict_bound_warrant_closure": {
      "count": 0,
      "denominator": 12
    }
  },
  "table": [
    {
      "closing_kind": "none-found",
      "evidence": null,
      "quote": null,
      "reopened": false,
      "reopening_evidence": null,
      "seconds_to_close": null,
      "target": "reazon tests in Emacs as a verification method for real-time reload behaviour; the stale frame 103 and the automated reload mechanism",
      "turn": "turn-0anLV2",
      "xiang_label": null
    },
    {
      "closing_kind": "record",
      "evidence": "e-49bef50f-08de-4c31-b9fc-4fc1206b2721",
      "quote": "It's deployed: the live About page now shows",
      "reopened": false,
      "reopening_evidence": null,
      "seconds_to_close": 16.615006,
      "target": "the live about page against the reorder just agreed",
      "turn": "turn-D2GnQn",
      "xiang_label": null
    },
    {
      "closing_kind": "operator-confirmation",
      "evidence": "turn-Ie4yZG",
      "quote": "m02J02 is up to 47 attempts",
      "reopened": false,
      "reopening_evidence": null,
      "seconds_to_close": 6629.0,
      "target": "the retry-persistence fix (turn-cap behavior); m02J02's expected retry behavior; m02J03's expected retry behavior; the failure branch of the verification; the fix, conditionally",
      "turn": "turn-LNcsB2",
      "xiang_label": "qualify/report-problem"
    },
    {
      "closing_kind": "none-found",
      "evidence": null,
      "quote": null,
      "reopened": false,
      "reopening_evidence": null,
      "seconds_to_close": null,
      "target": "the surviving anomaly: despite model validation and many tests, the live system abstains from running",
      "turn": "turn-MCErMC",
      "xiang_label": null
    },
    {
      "closing_kind": "record",
      "evidence": "emacs-551ccf0648fef009bc0de98c4856302a",
      "quote": "all of it is loaded",
      "reopened": false,
      "reopening_evidence": null,
      "seconds_to_close": 50.368946,
      "target": "whether the live futon1b process actually has the desired features loaded; why the desired features might already be live (the deployment path taken)",
      "turn": "turn-QLrMMq",
      "xiang_label": null
    },
    {
      "closing_kind": "record",
      "evidence": "emacs-3acd725c904774e54ef50d5a727f6b1f",
      "quote": "now work in your Emacs",
      "reopened": false,
      "reopening_evidence": null,
      "seconds_to_close": 43.072736,
      "target": "the r/R keybindings missing from the live stepper buffer",
      "turn": "turn-V5DsUN",
      "xiang_label": null
    },
    {
      "closing_kind": "operator-confirmation",
      "evidence": "turn-Xy8ygM",
      "quote": "live example of how inbox zero is working now",
      "reopened": true,
      "reopening_evidence": "turn-iNKHir",
      "seconds_to_close": 0.0,
      "target": "framing what follows as a live, running instance of inbox-zero behaviour rather than a description of it",
      "turn": "turn-Xy8ygM",
      "xiang_label": "explain"
    },
    {
      "closing_kind": "record",
      "evidence": "emacs-2eb6424cb522018a9356ee8aa4b11f26",
      "quote": "象 has annotated your last turn",
      "reopened": false,
      "reopening_evidence": null,
      "seconds_to_close": 169.398807,
      "target": "liveness of the turn-annotation pipeline for one producing agent (kimi-2)",
      "turn": "turn-ZUtRsq",
      "xiang_label": null
    },
    {
      "closing_kind": "none-found",
      "evidence": null,
      "quote": null,
      "reopened": false,
      "reopening_evidence": null,
      "seconds_to_close": null,
      "target": "the missing buffer half of the rewind in *claude-repl:claude-17*",
      "turn": "turn-co177a",
      "xiang_label": null
    },
    {
      "closing_kind": "none-found",
      "evidence": null,
      "quote": null,
      "reopened": false,
      "reopening_evidence": null,
      "seconds_to_close": null,
      "target": "the re-run outcome: frame 89 of 89 rendered empty after the fix attempt",
      "turn": "turn-g9JGYE",
      "xiang_label": null
    },
    {
      "closing_kind": "none-found",
      "evidence": null,
      "quote": null,
      "reopened": false,
      "reopening_evidence": null,
      "seconds_to_close": null,
      "target": "the inbox-zero service: repeated attempts, still unconvincing — reported honestly, then used conditionally in the design anyway",
      "turn": "turn-iNKHir",
      "xiang_label": null
    },
    {
      "closing_kind": "none-found",
      "evidence": null,
      "quote": null,
      "reopened": false,
      "reopening_evidence": null,
      "seconds_to_close": null,
      "target": "missing reuse of an analyzed cue while typing",
      "turn": "turn-qiO3lB",
      "xiang_label": null
    }
  ]
}
```

## Review (claude-17, 2026-09-30)

The table is accepted with one reclassification, which changes the reading.
All four "record" closures are **agent replies claiming the fix works**, not
independent records. Checked two by point read:
- `emacs-3acd725c…` is claude-17's own chat-turn: "`r` … and `R` … now work
  in your Emacs";
- `e-49bef50f…` is claude-19's reply: "It's deployed: the live About page now
  shows…".

The 46.7 s median time to close is the agent's reply latency. Under
`done-is-observed-running`, "the agent says it is done" is exactly what does
not count, and under `bind-the-subject` only a warrant minted after the
incident closes it. Reclassified:

| closing | count |
|---|---:|
| operator confirmation | 2 (`turn-LNcsB2` is doubtful: 象 labels it `report-problem`/`qualify`) |
| agent assertion, no independent record | 4 |
| none found | 6 |
| independent record (warrant, or observed use) | **0** |

**Can 象 label both halves?** Not yet. Opening fragments outnumber closing
ones 13 to 2, and the closing ones are mislabelled: `turn-Xy8ygM`'s "working
now" is labelled `explain` and cited as a gap.

**What the cascade needs.** The closing act in practice is the agent's
claim, so the cascade has three steps, not two:
1. `done-is-observed-running`: the gap opens.
2. The agent asserts it is fixed, *citing the observation it made* (the
   command it ran live and its output). That is DERIVE-1 R4: an assertion
   cites the record it rests on.
3. `bind-the-subject`: the gap closes when that observation (or Joe's) is
   recorded after the report.

Until step 2 carries an observation, a gap closed by an agent's word stays
"asserted, unverified" in the ledger, not closed.
