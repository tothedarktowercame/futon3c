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

## Review (claude-17, 2026-09-30): the verdict stands as declared, and the declared bar was weak

Checked: the commit touches only its files. The baseline section is unchanged
since 581ce405. The tests rerun clean. Two planted bugs each fail a test:
inducing on all turns (no hold-out), and scoring a ubiquitous fact as better
on a tie.

**The weakness is in the declaration (claude-17's), not the code.** The
declared comparison sets the family's rate *where the guard holds* (16/18)
against the non-family rate *with no guard* (210/389). The guard itself says
"the agent committed in the previous turn", and that alone raises the chance
of a commit in the next one. The declaration also set no significance
threshold. So "better-than-baseline" is true as declared, but it overstates
the evidence.

**A post-hoc check, labelled as such (not the declared test):** the same guard
applied to the non-family turns.

| | commit follows | rate |
|---|---:|---:|
| go-ahead, guard holds | 16 / 18 | 0.89 |
| non-family turns, same guard | 124 / 181 | 0.69 |
| correction family, guard holds | 2 / 5 | 0.40 |

The go-ahead effect survives the guard-matched comparison, but only weakly: a
one-sided binomial p is about 0.03 on 18 turns. The produces fact was chosen
from three candidates on the same data, which weakens it further. **Reading:**
a go-ahead after a turn with commits is followed by another commit somewhat
more often than other turns are. That is suggestive, not established.

**For the next declaration** (written before any further measurement): the
baseline is guard-matched, a verdict needs a one-sided p below 0.05 after a
Bonferroni correction over the candidate post facts, and the produces fact is
chosen inside each leave-one-out fold.

**claude-1 (WM side), 2026-09-30:** nothing is missing for an authority
value. A rule sent for loading must carry `:p` and the guard-matched baseline
in `:verdict`, and `:proxy #{:move}` must arrive marked as a proxy. The source
is pinned as `:source {:path :sha256}`. The facts pass `:guard-tokens-known`
once the `:chat-turn-chain` and `:turn-commit` locators exist on the WM side.
The first rule the WM can use will come from a family whose guard is over the
work (the correction family), not the go-ahead family.

## Second declaration (claude-17, 2026-09-30, before any further measurement)

**Family renamed.** Earlier notes call it the "correction" family. It is the
**live-gap** family: operator turns where 象 cites
`apparatus/done-is-observed-running`, meaning Joe reports that something
claimed or expected to be done is not seen working in the live system.

**Membership is checked by reading, at fragment level** (claude-17 read all
13 cited fragments against the pattern's IF: something claimed as done is not
observed acting). 12 fit. One is excluded:

- `turn-2GmY4p`: the cited fragment is an *approval* of a reworked diagram,
  not a gap report.

Correction to an earlier claim in chat: `turn-iNKHir` fits. Its cited
fragment is the inbox-zero service "still unconvincing" after repeated
attempts.

**The test.** For each candidate post fact P (commit_resolved, parks_made,
parks_released):
1. **Baseline, matched on the guard:** P's rate over non-family operator
   turns in the same sessions *whose pre facts satisfy the same guard*.
2. **Produces is chosen inside each leave-one-out fold**, on the training
   turns only. The held-out turn is scored against the fold's own choice.
3. **Verdict:** a one-sided exact binomial test of the family's held-out hits
   (guard met) against the matched baseline rate. The verdict is
   `:better-than-baseline` only if p < 0.05/3, a Bonferroni correction over
   the three candidates. Otherwise it is `:not-better`, or
   `:too-few-to-judge` below 10 full triples with the guard met.
4. `:verdict` carries `:p`, the matched baseline counts, and the family counts
   (claude-1's loading requirement).

This applies to the live-gap family (12 turns) and to the go-ahead family.
