# TN-opus-f48-critical-findings — a holdout that only held on one channel

claude-clink-1 (APM frame-watcher), 2026-08-27. Written at Joe's request after f48 was
voided and the campaign stopped.

Interleaved evidence for the assurance question:
https://claude.ai/code/artifact/b01d0577-1557-4b1f-9322-526222a79f75

## 1. The defect

Amendment 8's same-problem holdout was enforced on the shelf and on the cascade, and not on
the search channel.

The dispatch request carries `:shelf/holdout` and `:shelf/withheld-ids`.
`typed-role-submission/register!` filters that request through `authority-fields`, a set
literal that listed neither key. The persisted job authority therefore had no holdout, and
`role-memory-search/search!` — which authenticates through exactly that authority — had no
way to know the withheld ids existed. It returned them on request.

Validation was correct throughout. `allowed = shelf ∪ search ∪ cascade − withheld` is
evaluated when the terminal is collected, which is after the student has read the memory.
The check reported the breach; nothing prevented it.

Fixed in `d3cf69df`: the holdout travels on the job authority, `search!` removes withheld ids
before returning, and the receipt records `:shelf/holdout`, `:holdout/withheld-count` and
`:holdout/excluded-ids` so enforcement is auditable rather than a silent drop. A job with no
holdout filters nothing, so attempts 2 and 3 are unaffected. Verified in the running JVM, not
only in a test process: a withheld id is not served, an unheld id still is.

## 2. Incidence

Three frames have run with a holdout. Two breached.

| frame | problem | shelf | withheld served by search | used | outcome |
|---|---|---|---|---|---|
| f46 a1 | a96J08 | **empty** | 2 of 2 | both | discarded, re-run |
| f47 a1 | a97A01 | 67 | 0 | — | stands |
| f48 a1 | a98A03 | 76 | 1 of 4 | 1 | discarded, re-run |

f46's shelf snapshot was empty, so every memory its student had came through search,
including both withheld ids. The scan that produced this table can only see frames whose
attempt record still carries a holdout — three, because the live slot is reused (§3). Earlier
frames cannot be checked.

## 3. The repair destroys the evidence of what it repairs

`live/student-attempt-N.edn` is a single slot. On a terminal-repair re-dispatch,
`live-job-driver` persists `(assoc (dissoc state :terminal-collection) …)` over that same
path. `:terminal-collection` is where the discarded attempt's `used-ids` and report lived.

The consequence is that a breached attempt leaves behind a record that looks clean. f46/a1's
surviving record reads `used-ids []` — indistinguishable from an attempt that used nothing.
The only trace on disk is two keywords in `:terminal-repair/findings`.

Both discarded attempts were recovered from the Agency job ledger via
`:terminal-repair/original-job-id` while those entries were still resident, and written to
each frame's `superseded/` directory. `data/*` is gitignored, so those are the only copies
once the ledger entries age out.

Fixed by codex-2 in `b642640a` (retain superseded terminal repair evidence).

## 4. The students disclosed; the machine discarded their work

Neither breach was concealed. f48's student wrote that the id "was also in the withheld
holdout list; it reached me through the authorized, receipt-recorded search channel and I
reported that transparently in the failure account." f46's recorded that its shelf was empty
so everything came through search.

Both attempts were then thrown away, including compiled Lean: f48/a1 had
`apm_a98a03_vol_image_mul` (`volume ((r * ·) '' s) = ofReal |r| * volume s`), which exists
under no name in this Mathlib; f46/a1 had four elaborated lemmas including the pole-cancelling
pairing identity.

This is worth stating plainly because the incentive runs the wrong way: an attempt that says
nothing about a memory it was served is more likely to survive than one that reports it. The
disclosure is what triggered the discard. Joe's disposition (2026-08-27) was that the attempts
had wasted time, energy and usage and should be voided rather than salvaged; recorded here so
the incentive question is not mistaken for a proposal to salvage them.

## 5. Why the formal model did not cover this

`same_problem_holdout_uses_depositor_truth` exists. It was added to the model on
2026-08-27 at 07:16 (`266194dd`, apm-lean), seventy minutes before f46 ran at 08:26 and
breached the holdout.

The theorem is not wrong. Its hypothesis is `SameProblemHoldoutValid`, defined as

    ∀ memory ∈ shelf, memory.ValidOrigin ∧
    ∀ memory, memory ∈ shelf → (memory.memoryId ∈ withheld ↔ memory.origin.problemId = problemId)

The quantifier ranges over `shelf`. Search and cascade are not inside it. The model proves
which *shelf entries* are withheld and says nothing about which channels must honour the
withholding. The defect sat exactly in the gap between those two statements.

This is a scope result, not a soundness result, and it generalises. Across 22–27 Aug the
model gained 130 theorems in 46 commits; the live runs recorded 86 regulator failures across
25 distinct codes. No modelled property was violated. The two most frequent failures —
`live-regulator-tick-threw` and `live-supervisor-launch-audit-failed`, 13 each — have no
corresponding theorem, because neither is a property of the frame cycle. They are the
scaffolding around it: the launch path that mints frames, and exceptions escaping the tick.

f49 is the same class: minted 13:39, dead at 13:41 on its first tick with
`live-job-state-invalid`, before dispatching any role job.

## 6. Stopping the machine is not what `stop!` does

`live-regulator/stop!` cancels the scheduled runner and removes it from the table. It does
not clear `:coordinator/enabled?` in the registry, so anything that calls
`durable-coordinator/recover-all!` starts it again. After I called it, ticks continued
(3975 → 3977) and the durable state re-read `:regulator/status :running` while the in-JVM
runner table was empty.

`durable-coordinator/stop!` with **two** arguments — registry path and coordinator id — is
the durable stop; it disables the entry and returns `:durably-disabled? true`. That is what
finally stopped `jit-queue:jit-all-open-v2`.

Two further traps in the same area, both of which produced wrong reports today:

- A `stop!` that has returned does not mean no tick is running. A tick already in flight
  finishes its work, and it can mint a frame afterwards. f49's ledger and lease were written
  after I had checked the runner table and the job list and reported the machine stopped.
- Neither the runner table nor the active-job list reveals an in-flight tick. Both were
  honestly empty while work was still landing on disk.

The registry still holds three enabled entries (`library-lane:t00J02`,
`jit-queue:jit-m94A03-retry-v3`, `jit-queue:jit-all-open-nontopology-v1`). None is running.

## 7. State at the stop

- `jit-all-open-v2-f48` voided, `:apparatus-invalidated`, certificate
  `f3a9273fb04afd0af8f03ba28275475a83271dc2c471d6677aa9957c84a68746`, failed invariants
  `[:student-memory-used-despite-holdout :student-memory-used-without-surfacing]`, at ledger
  version 15, actor `claude-clink-1`.
- `jit-all-open-v2-f49` exists as a ledger, a certificate directory and a workspace lease.
  It dispatched nothing.
- f46 is closed and cannot be voided: `frame-void/prepare` refuses any frame that is not the
  active one, and f46's ledger has no active frame.
- The campaign coordinator is durably disabled.

## 8. What would give assurance

Not more of the cycle's interior. Three checks that run before a frame spends anything, each
seconds, each corresponding to a failure that has already cost hours:

1. Hand every channel that can serve a memory a known withheld id and assert none returns it.
   This is §1, and it would have fired before f46.
2. Assert the loaded JVM namespaces match the committed source. This is the recurring
   coupling failure.
3. Assert the contract the Clojure validator uses matches the Lean emitter's output.

A fourth belongs with them on the evidence of §6: the machine should be able to answer
truthfully whether it is running, from one place, including in-flight ticks.

## 9. Method note

I reported the breach, then withdrew it, then re-established it. The withdrawal was wrong and
its cause is worth recording: I read `used-ids` from the attempt record, and `withheld-ids`
from the same path minutes later, after a repair re-dispatch had rewritten the file. Two
reads of one mutable slot are not one observation. The live slot invites exactly this error,
which is a second reason §3 mattered.
