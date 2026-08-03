# S3 artifact (a96J01, zai-1, cohort 2) — preserved 2026-08-03

Job `invoke-1785739461339-861-868a3d9d` reached **overrun** (the ~30-min Agency
cap) at 320 events with no commit. The runner's file survives in the apm-lean
working tree as an untracked path, so nothing needed reverting; this is a copy
taken before any decision about it.

Ground truth (`lake env lean problems/a96J01/lean/Main.lean`, operator-run):

- 352 lines, **0 sorries**
- **exit 1** — errors confined to the final assembly:
  - `347:8  No goals to be solved`
  - `351:28 Invalid field 'trans_le': ... does not contain Real.le.trans_le`
  - `353:29 Application type mismatch: Summable.of_nonneg_of_le h_sup_nonneg`
    (has `∀ n, 0 ≤ ⨆ x, f n x`, expected `∀ b, 0 ≤ 1 / (b+1)`)

Classification: **partial, non-compiling** — cap-death mid-assembly. The
runner's last self-report ("all lemmas compile") was true of the lemmas; the
main theorem's final steps are where it ran out of budget.

This repeats cohort-2's S5 failure mode. The packet asked for an honest
compiling partial commit rather than running out of time with uncommitted work,
and the runner reasoned past it — a pacing instruction did not survive contact
with an interesting proof. D1 in E-memory-resourcing-and-strategy: a commit is
durable, a job result is not.

Scribe relevance: the three tail errors are arc-lane material (error -> fix
spans), and the cap-death shape is trajectory-lane material.
