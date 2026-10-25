# LAB — Tension measure over operator turns (2026-10-25)

Read-only analysis packet (claude-11, approved by Joe 2026-10-25). Question:
how often does an operator turn raise a tension, how often is it resolved
within the next two operator turns of the same session, and which patterns
were proposed on the resolving rounds? Framing: `holes/missions/war-machine-response-to-hobbes.md`,
`futon3/holes/war-room.md`; the aim is fewer tensions, in the manner of
Alexander's patterns. This measure is meant to become one of the loop's own
observations.

Tool: `scripts/tension_measure.py` (read-only; `--store`, `--since`, writes JSON).
Rules are fixed in the script and were not tuned on the data.

## Run

```
python3 -m py_compile scripts/tension_measure.py
python3 scripts/tension_measure.py --out /tmp/tension-report.json
```

Store `~/.emacs-graph/session-turn-analysis`, operator-origin records,
since 2026-09-28: **3068 operator turns across 77 sessions**.

## Counts (raised / resolved / unresolved / open)

| signal | raised | resolved | unresolved | open |
|--------|--------|----------|------------|------|
| D (declared mark) | 35 | 19 | 16 | 0 |
| X (象-annotated act) | 1932 | 1477 | 381 | 74 |
| D ∪ X | 1967 | 1496 | 397 | 74 |

So roughly **64% of operator turns raise a 象-annotated tension** (1932/3068),
and of those with a full two-turn window, **76% resolve within two operator
turns** (1477/(1477+381)). Declared marks are rare: 35 tensions across 5
sessions, all with full windows, 54% resolved.

Unresolved splits into re-raised in window vs no resolution signal:
397 = 19 re-raised + 378 no-signal (re-raise is rare; most unresolved
tensions are simply left behind).

## D/X agreement (turn granularity, n=3068)

| | both | D only | X only | neither |
|--|------|--------|--------|---------|
| turns | 6 | 3 | 1177 | 1882 |

Joe's intuition holds strongly: when he marks a tension 象 almost always sees
one too (only 3 declared turns without an annotated act), while 象 sees a
tension on 1183 turns where he declared none. X is the broad signal, D the
rare, deliberate one.

## Patterns proposed on resolved-tension rounds (top 15, D∪X)

Descriptive only; no claim that a pattern caused anything.

| pattern | on resolved | on unresolved | on open |
|---------|------------|---------------|---------|
| features/operator-turns-enter-the-observation-vector | 12 | 1 | 2 |
| orchestration/recorded-handoff | 11 | 0 | 0 |
| problems/operator-turns-become-inference-observations | 9 | 5 | 0 |
| musn/pause-backtrace | 8 | 3 | 0 |
| problems/unlinked-reasoning-cues-require-rediscovery | 8 | 0 | 0 |
| problems/fundamentals-drowned-by-backlog | 8 | 2 | 0 |
| problems/r20-interoceptive-tripwires | 8 | 0 | 0 |
| transition/f0-f4-boundary | 7 | 0 | 0 |
| problems/g-over-cascade-is-undefined | 7 | 1 | 0 |
| stack-coherence/futon1-determinism | 6 | 2 | 0 |
| contracts/declare-the-reductions | 6 | 2 | 0 |
| war-room/wr-2-pattern-library-stays-canonical | 6 | 0 | 0 |
| memory/verify-in-the-serving-process | 5 | 0 | 3 |
| musn/aif-live-scores | 5 | 0 | 0 |
| math-formalization/stamp-siblings-from-one-compiled-branch | 5 | 0 | 0 |

These are the turns where the pattern miner was running, so the list partly
reflects which sessions were mining, not a causal effect.

## Ten examples

Resolved (5):
- turn-Ei1bca — about "the unstick rule: either outcome is better than just failing to do anything" — "either is better than just failing…"
- turn-qt6deO — about "the need for a password here at all" — "I'm not sure why we are requiring a password…"
- turn-hFxHzc — about "the AIF-agent framing: 象 needs less processing than the War Machine" — "though we don't need the same degree of processing…"
- turn-3BnHz3 — about "the *象 stepper* side window persisting when its window switches buffers" — "claude-18 on the same frame…"
- turn-TFlLbx — about "the timing: the mining may bring demo-ready sooner" — "indeed we may get to demo-ready faster"

Unresolved (5):
- turn-ETzZFi — about "wm-author was not contacted, for reasons Joe cannot see" — "I don't understand why wm-author was not contacted" (no signal in window)
- turn-Tca03g — about "the agent's heavy use of technical jargon" — "you are using loads of technical jargon." (re-raised next turn)
- turn-Wri6Ix — about "the error message" — "It said could not load scope data" (re-raised as the diagram panel's error)
- turn-kpBAIH — about "rerunning the 45-minute CI before the known failures are fixed" — "i don't see why we should rerun the CI…" (no signal in window)
- turn-RKTxLv — about "(order) ports first until reply text is restored" — "Ports first. Until reply text is restored…" (no signal in window)

## Hand check

10 classifications (4 resolved, 4 unresolved, 2 open) were read against the
raw records: **10/10 agree** with the fixed rules. Two of the resolved ones
(turn-Ei1bca, turn-3BnHz3) resolve by "moves to a different about" — the
conversation demonstrably moved on without an explicit approve/agree/continue;
that is what the rule counts as resolution, but a stricter reading would call
them left-behind rather than resolved.

## Caveats

- The "moves to a different about" resolution rule makes resolution easy to
  reach in busy sessions; the 76% figure is an upper bound under these rules.
- D is small (35 tensions); its 54% resolved rate is fragile.
- Sessions are keyed by `session_id` and ordered by `created_at`; ties within
  a second are not broken further.

## Evaluating the sensor (2026-10-25, follow-up)

Joe doubted that word overlap senses tension. The first run already showed the
symptoms: the re-raise rule fired 19 times in 1,932 X tensions, and the
"moves to a different about" rule resolves nearly everything that is not
re-raised — so "resolved" was close to "not re-raised". Tool:
`scripts/tension_sensor_eval.py` (`py_compile` gate passes); sample, labels
and Joe's sheet live under `~/notes/experiments/2026-10-25-tension-eval/`.

Method: 60 X tensions with a full two-turn window, stratified 20/20/20
(resolved-by-act, resolved-by-different-about, unresolved), fixed seed 7,
spread over 26 sessions. 馬-sonnet labelled all 60 blind to the rule's
classification (6 whistles of 10 items); every supplied quote was
string-verified verbatim against the later turns (0 unverified, 60/60 labels).

Confusion matrix (rule stratum × 馬's label):

| rule stratum | resolved | re-raised | dropped | not-a-tension |
|---|---|---|---|---|
| resolved-act (20) | 11 | 2 | 7 | 0 |
| resolved-about (20) | 2 | 3 | 15 | 0 |
| unresolved (20) | 8 | 5 | 6 | 1 |

Readings:
- **The different-about rule measures dropping, not resolution.** 15/20 of
  its "resolutions" are 馬-labelled `dropped` (Joe moved on without the
  concern being dealt with); only 2/20 are genuinely resolved. The 76%
  resolution rate in the first run is therefore mostly turnover.
- **The approve/agree/continue rule is better but not clean**: 11/20 truly
  resolved, but 7/20 are `dropped` — an approve act elsewhere in the window
  does not necessarily address the tension.
- **The unresolved stratum is half wrong in the other direction**: 8/20 are
  actually resolved, i.e. the rule is pessimistic when resolution happens
  without an approve/agree/continue act and the next about differs.
- **Re-raise recall is 5/10** on 馬's `re-raised` items: word-overlap
  Jaccard ≥ 0.3 on paraphrased `about` strings catches half the returns.
- **X is not too broad**: only 1/60 items is `not-a-tension` by 馬's reading.

Conclusion: word overlap on `about` is a weak sensor for both re-raise and
resolution; resolution needs a notion of *the reply addressing the concern*,
not conversation movement. Joe's sheet (15 blind items, 5 per stratum) at
`~/notes/experiments/2026-10-25-tension-eval/joe-15.md` tests 馬 as a
stand-in when he fills it in. Caveat: many operator turns have no recorded
`reply_text`, so some sheet items show "(no reply)".
