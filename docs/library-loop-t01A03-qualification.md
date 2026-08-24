# Preserved t01A03 Library Loop candidate

## Qualification disposition, 2026-08-24

Joe elected to preserve this candidate unbanked and move the endurance
qualification to `t00J02`, the problem named by the Library Loop technote's
acceptance test. The t01A03 solver branch
`qualification/library-loop-t01A03-solver` is clean at
`3f2660db399e1fb1d8a23dd2be6b97bff7bf7461`; its qualification trunk remains
clean at `ac2983e9da3c202e9706e98b53cc8752d067e40e`.

Turn 5's gate receipt
`de012e1e610f003a2774165dbf8aedf43c9eb5d4ec261e892e459f3e53ce8db4`
is green (receipt SHA-256
`33deab4fb95f1ef7ad6b3b9ed14fc1a76a40d25a23986bd92544ce7a158991c4`).
The candidate's canonical status records `classification: solved` and zero
Main/scratch sorries. Its files-only run is therefore preserved at
`:turn-ready`, turn 6, checkpoint 0 with all ten receipts intact.

This is a successful solved candidate, but it does not satisfy the rebuild's
separate acceptance requirement of at least 20 end-to-end turns plus one
strategy checkpoint. It must not be padded with fabricated turns or banked by
bypassing checkpoint authority. It remains available for a later lawful
terminal review/bank; no bank occurred as part of this disposition.

The prepared endurance run is now `t00J02`, using dedicated solver/trunk
branches at election commit `ac2983e9` and a fresh persistent Codex session.

The remainder records the original preparation evidence. Its former
“prepared, not started” status has been superseded by the disposition above.

The proposed first 20-turn qualification is bound to the reviewed apm-lean
representative election commit
`ac2983e9da3c202e9706e98b53cc8752d067e40e` and the construction-program
election at `161f1f1c`. It uses:

- trunk worktree `/home/joe/code/apm-lean-library-loop-t01A03-trunk`, branch
  `qualification/library-loop-t01A03-trunk`;
- solver worktree `/home/joe/code/apm-lean-library-loop-t01A03-solver`, branch
  `qualification/library-loop-t01A03-solver`;
- persistent Codex session `01a03340-7655-7b31-a8ef-d67850944f52` selected by
  explicit UUID (never `--last`); and
- files-only runner state under `data/apm-lane/runs/t01A03`, initialized with
  base = head = `ac2983e9`, phase `:turn-ready`, turn 1, checkpoint cadence 20.

The session was bootstrapped once under a read-only sandbox with a strict
no-command/no-inspection/no-mutation acknowledgement. Its only response was
`T01A03_LIBRARY_LOOP_SESSION_READY`; no mathematical turn has been sent.

The standing goal preserves the frozen `apm_t01a03` theorem and its exact
residual `Nontrivial (apm_t01a03_H1 Circle)`. Promotion requires genuine
topological-circle singular homology: an axiom-clean `H₁(S¹) ≅ ℤ`, a named
generator mapping to `1`, its nonzero proof, and consumption by the compiled
no-retraction reduction. The cellular model alone and a relocated assumption
do not qualify.

Production commands are the canonical futon3c executables
`scripts/library-loop-audit` and `scripts/library-loop-status`. They are
configured by absolute path. The audit requires declaration authority in the
problem's `targets.edn`; the status command derives its ruling only from exact
landed Git state, real Main elaboration, and consistent committed status data.
Neither command pushes, banks, or starts Codex.

Preparation verification is limited to `scripts/library-loop status t01A03`,
Git/common-repository checks, and local tests. Running `resume`, `bank`, or the
20-turn loop remains explicitly out of scope until authorized.
