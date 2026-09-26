# TOMBSTONE — separate-evidence-history-from-verdict-state (REJECTED)

**Status: rejected at review 2026-08-12. Never registered, never deposited.**
Held in staging as a research note, per the runbook's disposition for
candidates that fail a gate.

- Authored: slice-1 compression mining, 2026-08-12 (conductor).
- Reviewed: claude-2, 2026-08-12 — **REJECT (evidence base)**.
- Executed: claude-2 as captain, 2026-08-12. File renamed to
  `.flexiarg.REJECTED` so no tool globs it into a registration pass.

## The content is clean; the evidence is not

It passes criteria 1-4 on formulation. The HOWEVER is honest and specific
— *"Append-only evidence can preserve bad observations forever, while
multiple live verdicts can leave consumers unsure which one governs …
otherwise two tables merely replace one ambiguous field"* — the name is a
verb phrase, the situation-class is recognizable, and the dedupe holds:
`stack-coherence/evidence-ledger` (read in full) is about scanning for
evidence of a done-claim and never touches the append-only/revisable
split.

**What fails is the evidence base.** Its entire support is 7 marks / 4
transcripts / 4 problems from slice 1 — the run whose reads shared one
accumulating context, so they were not blind. The runbook's rule is
absolute: *"convergence is the signal and it only means something if the
reads were blind."* Four non-blind transcripts is not a measurement.

And slice 3, the first clean run, explicitly failed to reinforce it:
*"No quote-verified math cluster reinforced slice-1
separate-evidence-history-from-verdict-state."* Its sibling
(`probe-the-claimed-property-not-the-acceptance-proxy`) WAS reinforced
from 5 distinct problems and was approved on that basis. This one got
nothing.

No text change fixes a contaminated evidence base, which is why this is
REJECT rather than REVISE.

## Re-admission condition

A clean-context slice surfaces the cluster from >=2 distinct problems.
Nothing else is needed — the formulation is already review-passing.

## A lead, explicitly NOT counted as evidence

An independent instance of this exact failure mode exists in this lane:
the a94A09 attachment-review helper
(`futon3c/scripts/review_codex_lane_attachments.clj`) hardcodes
`:verdict :approve` and drives a single mutable `:attachment-status`
cell on the hyperedge, while review observations live as separate
append-only evidence rows — so a rejection had nowhere to go, and any
writer could flip eligibility. That is the pattern's own failure mode,
observed outside slice 1.

It is filed as a **lead for the conductor to verify and mine**, NOT as
evidence toward re-admission. The reviewer who found it (claude-2) is
now the captain; a captain supplying evidence for a candidate the
captain then approves is the same self-corroboration problem that got
`search-the-namespace-not-the-qualified-name` rejected. If this lead is
ever used, it must be surfaced independently by a miner blind to this
note, and reviewed by claude-4.
