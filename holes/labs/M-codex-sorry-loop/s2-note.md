# S2 sorry-census note

- Source: git-tracked `*.lean` at `apm-lean` revision `26be1cb`.
- Counting rule: exact word `sorry`, including comments and strings.
- Reconciliation: 348 census rows = 348 `git grep -w` matches; delta 0.
- Coverage: 142 files; every row has one fixed-vocabulary classification.
- `:missing-mathlib-lemma`: 23.
- `:hard-proof-step`: 168.
- `:statement-issue`: 0.
- `:scaffold`: 157.
- `:unclassified`: 0.

The large scaffold count includes lexical mentions in comments plus generated,
historical, candidate, canary, and pilot sources. Active declarations ending
in the explicit placeholder proposition `True` are also scaffold, rather than
being inflated into proof work.

Targets were ranked lexicographically by distinct problems unblocked, clean
standalone statement extractability, then proximity to an existing Mathlib
API. Scores use `U` as a count and `E,M` on a 0--2 triage scale. The census
agrees with the preregistered expectation: Young convolution ranks first,
appearing across five canonical problems; its extracted L² construction is
the recorded direct unblocker for `a95J08` and `a96A04`.

This was lexical/context triage only. No Lake command was run and no
`apm-lean` file was modified. The required clean-status gate cannot literally
pass because `ApmCanaries/Current.lean.bak` was already untracked before this
work; tracked diff remains empty and that file was left untouched.
