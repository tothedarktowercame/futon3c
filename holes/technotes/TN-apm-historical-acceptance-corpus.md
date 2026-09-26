# TN-apm-historical-acceptance-corpus — labelled real incidents as acceptance tests

claude-clink-1, 2026-08-27. Joe's observation: historical frames are cheap
acceptance tests for new apparatus, because the verdict is already known.

**Why this beats authored witnesses.** The trace predicates added in `ae438faa`
are proved by `rfl` against mutants the author wrote — `unstoppedViolationMutant`
and friends. Those establish that the predicate discriminates on cases the
author imagined. Every defect found today was one nobody imagined. A predicate
that rejects a hand-built mutant and accepts f46's real trace is worthless, and
nothing in the current proofs would reveal that.

Each row below is a real, on-disk artefact with a verdict established by
forensics today. A new checker is acceptable only if it produces the stated
verdict. **A checker that accepts a REJECT row is too weak, whatever its
theorems say.**

## Holdout / memory-carrier predicates

| case | artefact | ground truth | required verdict |
|---|---|---|---|
| f46 a1 | `jit-all-open-v2-f46/live/student-attempt-1.edn` + superseded job | shelf snapshot empty; search served both withheld ids (`e-apm-promotion-0fb7d520…`, `…c99db7af…`); student used both and disclosed | **REJECT** |
| f48 a1 | `jit-all-open-v2-f48/live/student-attempt-1.edn` + superseded job | 1 of 4 withheld ids (`e-apm-promotion-9b8d0aec…`, depositor `f48-scribe`, subject `a98A03`) served by search and used | **REJECT** |
| f47 a1 | `jit-all-open-v2-f47/live/student-attempt-1.edn` | no breach under shelf-membership holdout; used `e-23a2940f-…`, deposited by `f39-guide` on `a97A01` — the frame's own problem — which was **never on any shelf**, so never in `withheld-ids` | **ACCEPT** under shelf-membership holdout; **REJECT** under depositor-truth holdout |

f47 is the discriminating case and the most valuable row here. It is the only
artefact that separates the holdout we have from the holdout we intend
(`TN-codex3-apm-repair-plan` §2, `TN-opus-f48-critical-findings` §1). A
depositor-truth gate that accepts f47 has not been implemented, however green
its tests are.

## Progress / liveness predicate

| case | artefact | ground truth | required verdict |
|---|---|---|---|
| f49 | `jit-all-open-v2-f49/` | minted 13:39:03, regulator `:failed` (`live-job-state-invalid`) by 13:40:43, `live/` holds only `workspace-leases.edn` — no preflight, no dispatch, no role job. Never durably disabled by the machine; a human did it at 14:43 | **REJECT** — stall with `coordinatorDisabled` false |
| frozen ticks | `jit-all-open-v2/coordinator.edn` | ticks 3975 → 3977 after `stop!` returned, durable state read `:regulator/status :running` with an empty in-JVM runner table | **REJECT** |

## Delivery predicate

| case | artefact | ground truth | required verdict |
|---|---|---|---|
| pre-fix | jobs `invoke-…2379-903d5af6`, `…2380-a5e8d1dd` | `status "delivered"`, `note "bell-job-ready"`, destination a poll URL; no auto-bellback job, no inbox file; caller `claude-clink-1` is pull-only | **REJECT** |
| post-fix | job `invoke-…2384-1be867e4` | `status "delivered"`, `surface "inbox"`, destination the real file path, `note "bell-job-ready-inbox"`; file present at 14:46:18 | **ACCEPT** |

## Evidence-durability predicate

| case | artefact | ground truth | required verdict |
|---|---|---|---|
| f46 a1 pre-fix | surviving record reads `used-ids []` | successor announced after the predecessor's terminal collection was dropped; evidence recoverable only from the Agency job ledger | **REJECT** |
| f48 a1 pre-fix | same shape, `:terminal-repair-attempts 1` | same | **REJECT** |
| post-`b642640a` | `:superseded-terminals` populated | predecessor archived before successor announcement | **ACCEPT** |

## How to use this

Run the corpus at admission alongside the authored witnesses, not instead of
them: authored mutants cover the shape, these cover what the machine actually
did. When a new incident is diagnosed, add a row — the corpus is the only asset
that grows from failures rather than being invalidated by them.

Two cautions. The artefacts under `data/*` are gitignored, so this corpus
references files that are not themselves version-controlled (register defect
D4); pinning it needs the durable-evidence work. And f46's and f48's
pre-fix attempt records were overwritten in place — the REJECT rows for those
rely on the copies recovered into each frame's `superseded/` directory today,
which exist only until the Agency ledger entries age out.
