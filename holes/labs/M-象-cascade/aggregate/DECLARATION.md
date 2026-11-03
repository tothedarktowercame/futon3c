# Declaration before measuring (claude-16, 2026-11-02)

Written after similarity calibration and before any grouped recurrence result was seen.

Question: once proposals are assimilated, does structure recur across time?

Move labels for a turn, in three arms:
- **A, names.** Cited pattern ids and proposal ids as they stand.
- **B, merged.** Proposals grouped greedily, strongest pairs first by complete link at cosine
  >= 0.70 (all-MiniLM-L6-v2). Calibration: two known duplicates scored 0.74 and 0.77; random
  proposal pairs have q99.9 of 0.56. A group containing a proposal at >= 0.70 to a library
  pattern takes that pattern as its label. Library first, per Joe.
- **C, family.** Each proposal takes the library parent its author declared; if that does not
  resolve in the library, the proposal keeps its own name. Cited patterns stay as cited.

Corpus: `~/.emacs-graph/session-turn-analysis` readings, ordered by time within a session.
Split: median turn time. Statistics, computed for each arm:
- S1, same-turn pairs seen in both halves.
- S2, transitions between adjacent turns of one session seen in both halves.

Null: 2,000 shuffles of label sets among turns inside each half, breaking turn order and,
for S1, label co-occurrence by reassigning single labels.

Bar: an arm passes a statistic if observed >= 2 x null mean and one-sided p <= 0.0042
(0.05 over 12 tests, rounded down). Arm A is the control and is expected to fail.
A pass shows structure recurs. It does not show the structure is right or useful.

Merge precision is checked separately: claude-16 reads 30 random merged pairs and counts how
many are the same move. Under 80% and arm B's merges are not to be applied, whatever it scores.

Nothing here writes to the library.

## Second declaration (claude-16, 2026-11-02), after the first test failed its positive control

Twenty planted pairs, each in 6 turns of each half, did not pass the first test: the planted
labels also recurred under shuffle. So S1 and S2 cannot detect recurrence, and their null results
say nothing about structure. They are withdrawn as evidence.

New statistic. Select pairs from the first half only: co-occurring in a turn (S3) or in adjacent
turns of a session (S4), at least twice. Count how often the selected pairs occur in the second
half. Null: 500 shuffles of single labels among turns of the second half, keeping turn sizes.
Pass: p <= 0.0042 (one-sided). The positive control must pass S3 before any other arm counts;
S4 has no planted control, since plants are not placed on adjacent turns. Arms A, B, C are
declared; D, the domain arm, is exploratory.
