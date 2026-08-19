# f15-guide running log — frame 15, m93J06

Kept live so the frame's instrument findings survive a lost wake. Written by
`f15-guide`; not a store write.

## Open state verified before dispatching (2026-08-19)

- Bound: `conductor/status` returns `bound? true`, phase `guided-solve`, v9.
- Solver checkout `/home/joe/code/apm-frames/frame-15-m93J06-solver`, branch
  `exp/frame-15-m93J06-solver`, HEAD `a92ffb6c` — cut from
  `:reg/environment-revision`, NOT from apm-lean's current
  `repair/m99J06-val-ae`. No void-frame (f13) work inherited. Confirmed by
  `git -C <worktree> rev-parse HEAD`.
- `problems/m93J06/lean/Main.lean` in the worktree is byte-identical to the
  apm-lean anchor (md5 `ee1438589c49835b52d7d7f47ac5891d`; blob
  `793cfe3b51305ee997c7ba60a17553633999063e`). One `sorry`, line 265.
- `Soundness.lean` absent from the worktree, as declared. Probe commit
  `9ba574a6` touched only that file (+97, 1 file), so the frozen statement is
  untouched by the probe — verified by blob equality, not by trusting the pin.
- Bundle present: `problem.md`, `problem.tex`, `proof-outline.md`,
  `informal-solution.md`, `status.json`.
- Roster: `f15-solver`, `f15-student`, `f15-scribe`, `f15-proctor`, `analyst-3`
  all registered.

## Registration pin findings (carried from the ack)

- `:lean-revision 4331becf` = mathlib4, per `conductor_open.clj` `:lean-repo`.
  Ground control corrected my reading; recorded so the correction is not lost.
- `(:problem :regime) e7b9ec02` resolves in NO repository. Copied verbatim into
  all fourteen registrations; has never anchored anything. Declared
  `:dead-regime-pin` by ground control rather than silently fixed, so this frame
  stays comparable to its predecessors.
- The apm-lean anchor is `:reg/environment-revision a92ffb6c`. Two objects, one
  word "pin" — now disambiguated in the brief and containment witness.

## Prior for `:memory-contributes-to-close` — RULED

The honest prior is **ZERO attested instances on a sound problem**. f9/f10
recorded `memory-use steps = 0` (no instrumentation ran); the f10 "route memory"
reading was a prose attribution by a reader, not an attested step. **f15 is a
first measurement, not a replication.** Adjudicate on per-id `USED`/`IGNORED`
attestation only; never on resemblance, in either direction.

## Dispatch 1 — solver

    action-id : f15-dispatch-solver-1        v9 -> v10
    job-id    : invoke-1787153737431-5077-eb8f0615
    park-id   : park-d2befad8-efcf-4d96-9ecf-fae5fd9abba9  (engine-parked,
                surface `problem`, deadline 1787156438227 ≈ 16:20:38Z)
    cascade   : dispatch-recall-a7b88744-17ea-4bcd-b533-57139c93161d

### MEASUREMENT — D60 reproduced live, and it is worse here than in f13

The issued lexical query was:

    exponent OR closing OR conjunct OR existence

Four prose/meta words. Not one of `ode`, `flow`, `lipschitz`, `picard`,
`holder`, `gronwall`, `contdiff` — i.e. not one term that names this problem's
mathematics. f13's issued query (`finding OR equality OR clause OR strong`) had
the same shape, so this is the second consecutive frame in which
`default-query-term-limit` = 4 plus PROBLEM-CORPUS-IDF ordering spent every slot
on vocabulary shared by every problem in the corpus. The defect is not
intermittent.

### Offers: 5 surfaced, 1 lexical, 4 by pattern

| memory-id | via |
|---|---|
| `e-codexpilot-prove-eLpNorm-to-essential-supremum-by-superlevel-and-probability-monotonicity` | `:content-match` |
| `e-2ad2b4fe-6157-4dde-bffb-37d91290b45a` | `:pattern` |
| `e-codexpilot-apply-radial-R3-integration-through-a-one-dimensional-profile` | `:pattern` |
| `e-codexpilot-avoid-euclidean-measurable-space-diamond-by-direct-general-instantiation` | `:pattern` |
| `e-codexpilot-derive-local-L1-from-weak-L2-by-layer-cake-and-optimized-splitting` | `:pattern` |

`:memory-use/status :pending-outcome`, `used-ids []`, `rejected-ids []`,
`withheld-ids []` at dispatch. The single `:content-match` hit is a measure-theory
memory (eLpNorm → essential supremum); the four pattern-routed ones are radial
R³ integration, a measurable-space instance diamond, and layer-cake weak-L²
splitting. **On their face none is about ODEs.** That judgement is mine and is
NOT the measurement — the solver's per-id `USED`/`IGNORED` disposition is, and
the packet demands it explicitly and forbids flattering the count.

Exactly as the registration predicted: the lexical route underperformed and the
pattern cascade carried the load. Note the pattern cascade carried it toward
*integration theory*, because that is what the depositing corpus is made of.

## Pending

- Await job `invoke-1787153737431-5077-eb8f0615` (engine park; server-side turn
  enqueue on completion — surface `problem` is not a buffer surface).
- On wake: read the solver report, record the attempt, then decide guidance
  (typed `:answer` to a REPORTED residual only) vs advance.
