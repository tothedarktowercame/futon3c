# Audit: promotion-review reason-code vocabulary

Date: 2026-08-26

Scope: the APM promotion-review path in `src/futon3c/apm`, its tests and
generated contract, the promotion-proctor role cards, and the f42 durable live
records. This is a read-only audit; it proposes no code or role-card change.

## 1. Is there a defined reason-code vocabulary?

**There is no single defined, exhaustive, machine-readable vocabulary for the
code-like prefixes in promotion-review `:reason` strings, and nothing validates
a prefix against such a vocabulary.**

There are three related things, but none is that contract:

1. The active role card is
   `holes/labs/M-apm-demonstration/role-cards/promotion-proctor-v2.md`, selected
   by `src/futon3c/apm/queued_frame_adapter.clj:118-132`. It documents five
   residual-fit rejection reasons (`:residual-already-closed`,
   `:no-open-residual`, `:already-in-file`, `:not-actionable`, and
   `:hook-problem-centric`) at
   `holes/labs/M-apm-demonstration/role-cards/promotion-proctor-v2.md:29-44`,
   plus `:reviewer-inputs-missing` at lines 71-75. But its output contract says
   `:reason` is a string beginning with either a residual description or a
   rejection code; it does not enumerate the permitted prefixes
   (`promotion-proctor-v2.md:60-69`). Thus the card supplies reviewer guidance
   and examples, not a closed vocabulary.

2. The draft v3 card adds more documented codes at
   `holes/labs/M-apm-demonstration/role-cards/promotion-proctor-v3.md:54-69`,
   `:75-91`, and summarizes additions at `:102-116`. It also explicitly says
   v2 remains selected at `:126-128`. This is another prose list, not a schema,
   and it was not f42's active default.

3. The machine has controlled vocabularies for adjacent fields. The promotion
   pipeline defines `review-verdicts` as
   `#{:approve :reassign :reject :cannot-judge}` at
   `src/futon3c/apm/promotion_pipeline.clj:20-23`. The review store defines the
   attachment-changing subset `#{:approve :reassign :reject}` at
   `src/futon3c/apm/promotion_review_store.clj:13-19`. The mechanical guard also
   owns deterministic finding codes and turns them into a prose `:reason`
   string at `src/futon3c/apm/promotion_pipeline.clj:54-86`; for example,
   `:candidate-not-materialized` is separately produced by disposition
   validation at `src/futon3c/apm/promotion_pipeline.clj:154-179`. These are
   machine findings or verdict enums, not a closed reviewer-reason vocabulary.

The actual reason validation requires only a nonblank string (and separately a
nonblank residual): `src/futon3c/apm/promotion_pipeline.clj:259-284`. The active
prompt likewise asks only for the same nonblank reason and residual
(`src/futon3c/apm/live_promotion.clj:269-278`, `:311-323`).

I searched exact occurrences of `:candidate-not-materialized`,
`:hook-problem-centric`, `:duplicate-reviewed-memory`, and
`:pattern-file-unavailable` across `src`, `test`, `resources`, and `holes`
(excluding campaign `data`). The first is machine-produced at
`src/futon3c/apm/promotion_pipeline.clj:159-162`; the second appears in the v2
and v3 cards cited above; **the latter two were not found anywhere in those
trees**. In f42 they occur only as reviewer-authored prefixes inside free-form
reason strings, for example
`data/apm-campaigns/jit-all-open-nontopology-v1/jit-all-open-nontopology-v1-f42/live/scribe-reduce.edn:1`
and `.../jit-all-open-nontopology-v1-f42/live/guide-intervention-1-review.edn:1`.

## 2. Does anything downstream key on `:reason`?

**No downstream production code branches on a promotion review's `:reason`
value or parses its code-like prefix.** Minting a new prefix therefore does not
currently alter scoring, attachment status, promotion, snapshot membership,
receipts, or campaign-trace semantics. It is a provenance/documentation
ambiguity, not a value-dependent control-flow bug.

The complete production uses found for a promotion review reason are:

- It participates verbatim in the controller-owned review identity digest,
  then is copied verbatim into persisted evidence as `:review/reason`:
  `src/futon3c/apm/promotion_review_store.clj:52-62` and `:75-103`.
- It must be nonblank during review validation:
  `src/futon3c/apm/promotion_pipeline.clj:271-278`.
- Snapshot visibility requires the persisted `:review/reason` to remain
  nonblank, but does not inspect its value:
  `src/futon3c/apm/memory_snapshot.clj:151-173`.

The fields that drive behavior are instead `:verdict`, projection status,
materialization, attachment status, and pattern IDs. Disposition branching is
at `src/futon3c/apm/promotion_pipeline.clj:154-179`; publication selects
`:approve`/`:reassign` at `:135-139` and records verdict/status/patterns at
`:181-219`. Persisted attachment projection passes only memory ID, evidence ID,
verdict, and patterns downstream—not reason—at
`src/futon3c/apm/promotion_review_store.clj:212-221`.

Receipts store the whole review vector without interpreting reason
(`src/futon3c/apm/countdown_control.clj:916-925`). The campaign trace discards
reason and exports only verdicts (`src/futon3c/apm/campaign_trace.clj:19-33`).

Search method for this negative finding: `rg` over all Clojure files under
`src/futon3c/apm` and `test/futon3c/apm` for `review/reason`,
`(:reason review)`, `:reason %`, `promotion-reviews`, and the four concrete f42
prefixes, followed by inspection of every production hit. No equality, `case`,
set-membership, prefix, regex, or scoring consumer of a promotion-review reason
was found.

One qualification: changing reason text changes the review evidence identity
because reason is an input to the digest
(`promotion_review_store.clj:52-62`). That affects identity/idempotence, but it
does not assign semantics to any particular reason code.

## 3. Is verdict constrained where reason is not?

**Yes. Verdict is a closed enum and is validated; reason is deliberately
represented as prose by the role card and is validated only for presence. The
code does not state whether leaving its apparent code prefix open was a
conscious extensibility choice or an omission.**

At typed ingestion, string-valued verdicts are normalized to keywords at
`src/futon3c/apm/live_promotion.clj:73-95`. After review persistence, the live
path calls `validate-review*` at `src/futon3c/apm/live_promotion.clj:643-675`.
That validator rejects any verdict outside the four-member `review-verdicts`
set (`src/futon3c/apm/promotion_pipeline.clj:259-284`). It simultaneously
checks reason only as a nonblank string (`:273-278`).

The store's narrower `attachment-verdicts` set is operational: only approve,
reassign, and reject reviews are canonicalized and projected
(`src/futon3c/apm/promotion_review_store.clj:137-180`). `:cannot-judge` remains
in the returned review vector rather than changing an attachment
(`promotion_review_store.clj:248-265`), and complete-disposition validation
treats it as the apparatus finding `:promotion-pass-unresolved`
(`src/futon3c/apm/promotion_pipeline.clj:154-179`).

The persisted evidence validator also checks exact verdict agreement, pattern
agreement, provenance, authorship, and session, but does not inspect reason
semantics: `src/futon3c/peripheral/memory_lifecycle.clj:118-178`. Snapshot
visibility adds only the nonblank reason check cited in section 2.

## 4. Typed-submission contract-migration retry semantics

**`:typed-submission-contract-migration` itself is single-shot. The generated
contract sets `:typed-submission-migration-max 1` at
`src/futon3c/apm/generated_contract.clj:49-53`. It is, however, an extra
compatibility attempt after the ordinary terminal repair has already been
used—not the sole retry available after the original job.**

The normal per-role budget is one collection and one repair
(`src/futon3c/apm/live_job_driver.clj:9-10`, and the per-role generated values
at `src/futon3c/apm/generated_contract.clj:33-48`). Once a job lacks a typed
submission, validation produces `[:typed-submission-missing]`
(`src/futon3c/apm/live_job_driver.clj:248-252`). A normal repair increments
`:terminal-repair-attempts`; after that counter is positive, one further
migration is allowed only while `:typed-submission-migration-attempts` is zero
(`live_job_driver.clj:253-257`). Dispatching that migration does not increment
the already-spent ordinary repair counter, and sets the migration counter
directly to 1 (`live_job_driver.clj:339-354`). The repair request marks a fresh
session and the migration kind at
`src/futon3c/apm/live_learning_phases.clj:555-579`.

If the migration also returns no typed submission, it is no longer migration
eligible and the ordinary repair budget is already exhausted. The driver then
calls the student-only missing-observation provider rather than dispatching a
second migration (`src/futon3c/apm/live_job_driver.clj:292-315`). The test
explicitly proves one migration announcement followed by
`:live-job-terminal-repair-exhausted` in the generic driver fixture:
`test/futon3c/apm/live_job_driver_test.clj:200-240`.

For f42 attempt 2, the durable record confirms both counters are 1 and the
active request is the migration:
`data/apm-campaigns/jit-all-open-nontopology-v1/jit-all-open-nontopology-v1-f42/live/student-attempt-2.edn:1`.
Thus the attempt had the original job, one ordinary terminal repair, and one
additional contract-migration job; there is no second migration.

The premise that the durable record cannot distinguish a failed attempt from a
sound candidate dropped solely for missing typed submission needs refinement:

- The fallback explicitly labels the receipt `:student-observation-recovered`,
  records the collection failure reason, and assigns candidate disposition
  `:rejected-evidence` when candidate preservation returned a failure
  (`src/futon3c/apm/live_learning_phases.clj:377-429`).
- Candidate preservation commits and pins a ref before validation, then returns
  `:student-candidate-validation-failed` with the candidate head, ref, and full
  validation result (`src/futon3c/apm/workspace_lifecycle.clj:275-323`).
- But the fallback receipt deliberately selects only `:error/code`, `:head`,
  and `:ref` from that failure (`src/futon3c/apm/live_learning_phases.clj:415-419`).
  Therefore f42's receipt does preserve the rejected candidate's raw head
  (`21ff7f4d...`) and durable ref, contrary to “no candidate head,” but it drops
  the `:validation` map that would say why validation failed. It also has no
  student-authored `:outcome`, because no typed submission was collected.

So the machine can distinguish “no typed report, candidate preservation then
validation failed” from “no work observed,” and it retains a ref to the
candidate. The receipt cannot establish whether the Lean content was sound or
why candidate validation rejected it without separately re-reading/revalidating
that preserved ref. That missing validation detail is the consequential audit
finding; changing retry budgets is a separate policy question.

## Suggested follow-up scope (no change made here)

If reason prefixes are intended to be machine-status labels, give them a
separate keyword field with a closed vocabulary and validation, leaving
`:reason` as prose. If they remain reviewer prose, interfaces and reports
should not present the leading colon token as apparatus state. Separately, a
repair packet could preserve the candidate validation findings in the
observation-recovery receipt so a rejected proof artifact is diagnosable
without rerunning mutable workspace validation.
