# C2: baseline declared before measurement (criterion 2)

claude-17, 2026-09-30, written before any rule is induced or measured. The C1
corpus (35ec7d66) is the input.

**What the rule predicts.** For a family F, an induced rule claims that F's
move, taken when its guard holds, is followed by a post fact P. That is, P
appears in the turn's own frame, between Joe's turn and his next one.

**Trivial facts are excluded.** An agent reply follows almost every operator
turn, so `agent_reply_present` is not a candidate P. The candidates are
`commit_resolved >= 1`, `parks_made >= 1` and `parks_released >= 1`.

**Baseline.** For each candidate P, the baseline is P's rate over all operator
turns in the same sessions that are **not** in F, computed from the same
frames. A rule counts as better than the baseline for P only when, on held-out
turns, P's rate among F's turns exceeds the baseline rate. The held-out check
is leave-one-out over F's full triples. The baseline and the family rate are
reported with counts, not only as percentages. With fewer than 10 full triples
in F, the result is reported as "too few to judge", however the numbers fall.

**Guard.** A guard is a conjunction of pre facts that hold for at least 2/3 of
F's training turns, taken from the pre-frame facts in C1 (agent reply, commits,
parks). The 象 reading is never part of the guard: it is the move, and it
stays marked as a proxy.

**Families.** The go-ahead family (26 full triples) is measured first. The
correction family has 7 full triples, so it is reported as "too few to judge".
`turn-iNKHir` is in both families; it counts in each, and this is noted.

## Results (2026-09-30)

| Family | Full triples | Guard | Produces | Held-out hit / miss / guard-not-met | Baseline count / denominator | Verdict |
|---|---:|---|---|---:|---:|---|
| go-ahead | 26 | agent reply present + resolved commit | resolved commit | 16 / 2 / 8 | 210 / 389 | better-than-baseline |
| done-is-observed-running correction | 7 | agent reply present + resolved commit | resolved commit | 2 / 3 / 2 | 212 / 371 | too-few-to-judge |
