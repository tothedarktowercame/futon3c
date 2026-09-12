# F227 submit-step trace (2026-09-12)

## Scope and pins

This is a read-only reconstruction of the f227 terminal-repair lineage.  Source
citations use the code revision which processed f227:
`e49f47ea321d2cd3f59087945fec193420f8dd24`.  Record paths are repository
relative; the relevant EDN records are one-line records, so their citation is
line 1.  SHA-256 pins are:

| Record | SHA-256 |
|---|---|
| `data/apm-campaigns/jit-all-open-v3/queue-state.edn` | `bb9a3b0df9992680339f8b22e94689ca778e68b84afe929060550bace66c5550` |
| `data/apm-campaigns/jit-all-open-v3/jit-all-open-v3-f227/live/guide-intervention-2.edn` | `e97054abd7645a1c064fee9010b87603cd2ec569db456ccced23ddec11b8dbb0` |
| `data/apm-campaigns/jit-all-open-v3/jit-all-open-v3-f227/live/guide-intervention-2-review.edn` | `270f3e712a673b237b26eaf8e3e049cc30e735c3b740eb1df2dc00880a42aa57` |
| `data/apm-role-submissions/apm-role-5a1aa8b0dfa0f1cd1f4634d20cb262ad87f7c182d9ea6bcc506cdd844d064817.edn` | `fbfe350f4bec916f29ac12b48039d2a6fc1185174406d0ee776d396503efd6e8` |
| `holes/labs/M-apm-demonstration/analysis/v3-decommission-2026-09-12/README.md` | `e83d17ca56985db76bf0bb69106c2df996fe7c3c5a51b659327a5a08a7a40e9f` |

Agency job observations below came from read-only `GET
/api/alpha/invoke/jobs/<id>` on 2026-09-12.  They are identified by immutable
job IDs and event sequence numbers; the durable frame records above independently
pin the job lineage and eventual authenticated submission.

## Verdict

**REFUTED.** A submit-step repair was never sent to a fresh seat.  The one
recorded repair was an ordinary repair of the *outer Guide job*.  The nested
promotion review's `:typed-submission-missing` remained under `:report/error`,
while the outer Guide failure had `:findings []`.  Consequently
`submission-only-failure?` was false and the driver did not add
`:repair/kind :submit-step`.

The ordinary Guide repair did use a newly announced job with no retained
session identity, but it successfully submitted the Guide payload.  Freshness
therefore does not explain the missing promotion-review submission: the repair
was aimed at the wrong job and authority.

## 1. What repair was requested?

The parked f227 entry records one repair-history job,
`apm-role-785ee0eaf05e1231bd760e1f971225e52ce8135fd20b8a38635a10fa1f2b0ad0`,
with `:fault-origin :agent` and `:findings nil`.  Its nested terminal report is
the different reviewer job
`apm-role-5a1aa8b0dfa0f1cd1f4634d20cb262ad87f7c182d9ea6bcc506cdd844d064817`,
whose `:report/error` is `{:error/code :typed-submission-missing}`
(`queue-state.edn:1`, pinned above).

The history ID `785ee0...` is the original `f227-guide` producer, not a repair
successor.  Its malformed prose result was normalized to
`:report-edn-lint-failed`; the durable error lists `Invalid symbol: ok:.`,
`Invalid symbol: status:.`, `Invalid number: 338fa11e….`, and
`:syntax-quote node expects 1 value.`  The outer failure is therefore
`:promotion-stage-terminal-invalid` with `:findings []`
(`guide-intervention-2.edn:1`, pinned above).

At the audited source, typed-submission migration is selected from
`(:findings validated)` (`live_job_driver.clj:1287-1295` at `e49f47ea`), and
the repair request receives `:repair/kind :submit-step` only when
`submission-only-failure?` accepts those findings
(`live_job_driver.clj:1403-1433` at `e49f47ea`).  An empty vector does not
qualify.  The repair archive then records the current job ID and findings,
which explains the apparently surprising `785ee0...`/nil history entry
(`live_job_driver.clj:1503-1559` at `e49f47ea`).

The actual successor was
`apm-role-ad830f3...` (full ID is present in `guide-intervention-2.edn:1`).
Its request had `:repair/fault-origin :agent`, `:repair/findings []`,
`:repair/attempt 1`, `:fresh-session? false`, and
`:repair/of-job-id 785ee0...`; it had **no** `:repair/kind`.  This answers the
first target: submit-step was not set because the driver classified the outer
Guide parse failure, not the nested reviewer submission failure.

## 2. Agent and session target

Agency's record for `785ee0...` identifies agent `f227-guide` and session
`zai-b9f485da-c4f9-49f1-91b3-b0804f73b1fe`; it ran from 01:02:07Z to
01:03:42Z.  The successor `ad830f3...` also targeted agent `f227-guide`, but
its job record has `session-id: null`; it ran from 01:08:56Z until cancellation
at 01:09:04Z.  It did not target `f227-promotion-proctor` or the original
reviewer's retained session
`01a092fb-9ef6-7903-ada7-ebc94980fa6c`.

This is consistent with the prompt builder: repair prose, including the
special submit-step instruction, is emitted only when the corresponding
repair fields are present (`live_learning_phases.clj:1078-1168` at
`e49f47ea`), and `terminal-repair-request` merely carries that selected kind
into the announced request (`live_learning_phases.clj:1171-1231` at
`e49f47ea`).

## 3. What the jobs returned

The original Guide job `785ee0...` reached `:done`.  At event 21,
01:03:37Z, it successfully executed the typed submission command and created
submission `338fa11e...`; its final conversational response was prose rather
than the required EDN-only report.  Thus the Guide's work was accepted, but
its wrapper report failed parsing.

The ordinary Guide repair `ad830f3...` was cancelled, but before cancellation
its event 5 at 01:09:03Z copied the prior JSON and successfully created typed
submission `a41fd4dc...`.  It contains no promotion-review typed-completion
attempt or error.  It repaired the already-submitted Guide output, not the
reviewer output.

The independent reviewer `5a1aa8...` was a normal (non-repair) job for agent
`f227-promotion-proctor`, session
`01a092fb-9ef6-7903-ada7-ebc94980fa6c`; it finished `:done` at 01:08:34Z.
Its exact submission attempts were:

1. Events 9, 11, and 14 used a mistyped/nonexistent job ID beginning
   `apm-role-5a1aa8f1fcb...` and received Python `urllib` `HTTP Error 404:
   Not Found`.
2. Event 15 read the correct record and exposed the exact `5a1aa8b0dfa...`
   job ID, authority, and `:submission nil`.
3. Event 16 generated a template for unrelated job `apm-role-b70b67...`.
4. Event 18 attempted that authority and received the exact typed error
   `{"error/code":"role-submission-conflict","ok":false,"submission/id":"57f8b6fe..."}`.
5. It ended with a valid-looking EDN report in conversational prose, but no
   authenticated submission for its own authority.

The promotion port turns a terminal done job without a matching authenticated
submission into `:promotion-stage-terminal-invalid`, nesting
`:typed-submission-missing` at `:report/error`
(`live_promotion.clj:433-450` at `e49f47ea`).  The Guide promotion call then
returns that nested result outward (`live_learning_phases.clj:1395-1409` and
`:1414-1472` at `e49f47ea`).  Nothing converts it into the outer driver's
finding vector, so the rescue selector never sees it.

## 4. Difference from the hand recovery

The hand recovery created authenticated submission
`856f8294163582224d19a6c00aeb35b750c6030788da2c0385b3afcc7e1e02fe`
for the exact original reviewer authority `5a1aa8b0dfa...`.  The decision at
`holes/labs/M-apm-demonstration/frame-park-decisions.edn:4399` is timestamped
`2026-09-12T13:31:25.296810759Z` and states that the original reviewer supplied
the missing completion from its retained session.  The decommission record
corroborates the recovery job and identifiers
(`holes/labs/M-apm-demonstration/analysis/v3-decommission-2026-09-12/README.md:8-13`).

The minimal difference is therefore **authority and retained content**: hand
recovery addressed the original promotion-review job using its retained
session/report; automation addressed the outer Guide job and resubmitted the
Guide payload.  The nested reviewer state file itself correctly retains the
review request and ticket (`guide-intervention-2-review.edn:1`, pinned above),
but the enclosing Guide reconciliation did not drive a repair through that
nested authority.

## PROPOSED repair direction (not implemented)

Give the nested promotion review its own terminal reconciliation before
`live-promotion` returns failure to the Guide driver.  Specifically, at the
independent-review branch (`live_promotion.clj:1084-1110` at `e49f47ea`), feed
the retained promotion-review request/ticket and
`:report/error :typed-submission-missing` through the existing shared
live-job-driver repair path.  That path can then create a submit-step for
`f227-promotion-proctor` and the exact reviewer authority.  Persist its active
request and repair history in `guide-intervention-2-review.edn`, and only
return an outer Guide failure after that nested repair reaches a terminal
outcome.

This should be one implementation packet centered on the nested
independent-review boundary, with a replay pin for the exact f227 reviewer
job/authority and the `role-submission-conflict` error above.  Do **not** map
the nested error into the outer Guide's findings: that would again make the
Guide repair mechanism answer the promotion review's acceptance question.
