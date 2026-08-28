# TN-apm-two-global-validations — shape, and the contracts between stages

Joe, 2026-08-28. Recorded because it reframes two days of failures.

## The claim

Two global validations are missing, and both should be things Lean can record
and check against real Clojure receipts:

1. **Data shape** — given a receipt the Clojure side produced, does it conform
   to the declared shape?
2. **The contracts between stages** — what stage N hands to stage N+1, stated
   precisely enough to be checked.

And the consequence for how failures are read:

> The "guide produced free text not the correct handoff" shouldn't be thought
> of as bad behaviour of the guide — it is evidence that the contract was never
> clear in the first place, and certainly not able to pass a validation of the
> kind I have in mind.

> The "live behaviour failing" is the best direct evidence of a failing
> validation.

## Why this is not the framing we have been using

Yesterday and today every failure was classified as apparatus, substrate, or
harness — a taxonomy about *who is at fault*. Joe's framing is about *what was
never specified*. Those give different repairs:

- Fault framing: the guide misbehaved; make the guide behave, or dispose of
  frames it spoils. Both were proposed today and both were wrong.
- Contract framing: the handoff from `promote-solver` to `guide-intervention`
  was never expressed in a form whose satisfaction could be checked before the
  turn ended. The guide could not reliably comply because compliance was
  described in prose in a packet.

The second reading also explains why the failure recurred identically on
re-dispatch. A misbehaving agent is stochastic; an unspecified contract fails
the same way every time, which is what we observed.

## What exists today, and the gap

The Lean model does carry some of validation 1. `APMCampaignTraceChecker`
defines observation shapes and decides them, and `generated_contract.clj` is
mutation-tested so policy content cannot drift silently. That machinery works —
it caught real drift this week.

Validation 2 is the gap, and it has a specific shape. The model proves
properties **of transitions**: `resolved_pass_always_advances`,
`unresolved_pass_never_advances`, `typed_submission_prevents_authority_forgery`,
`every_live_role_has_one_submission_schema`. What it does not model is the
**artifact that flows across a transition** as an obligation on the producing
stage. `every_live_role_has_one_submission_schema` says each role *has* a
schema. It does not say a role's turn *terminates only by producing a
conforming instance of it*.

So a role can reach a terminal job state without producing the artifact the
next stage requires, and the machine only discovers this afterwards, when
`validate-completion` reports `:typed-submission-missing`. The contract is
enforced by post-hoc rejection rather than by construction.

## Live failures re-read as validation failures

Each of these is a validation that either fired too late or does not exist:

| failure | validation that was missing or late |
|---|---|
| guide returned prose, `:submission nil` | no checkable obligation that a role turn ends in a conforming submission |
| `authority-fields` dropped `:shelf/withheld-ids` | no shape check that the persisted authority carries what its consumers read |
| `job->terminal` stripped the delivery observation | no shape check across a projection boundary |
| `delivering` unclassified by `job_port` | no contract that a consumer classifies every state its producer emits |
| non-seat callers resting at `pending` | no contract that every caller shape reaches a terminal disposition |
| `campaign_trace` emitting no observations | producer declared, never obligated |

Six failures, one pattern: **an artifact crosses a boundary and nothing checks
that what arrived is what the other side requires.**

## What "the model catching up" would mean

Concretely, and in the order that makes each step checkable:

1. Every stage boundary declares the artifact it hands on, in the generated
   contract, derived rather than hand-listed.
2. A role turn cannot terminate as successful without a conforming artifact —
   enforced at the boundary, not discovered by a later validator.
3. Lean states the boundary obligations as properties over real receipts, and
   the mutation suite binds them, as it already binds policy content.
4. The obligations are what the role is given. A packet that says "completion
   is accepted only through the typed submission tool" in prose is not the same
   as a contract the role can be held to and checked against.

Step 4 is the one that would have prevented f49: not better instructions to the
guide, but a handoff whose satisfaction is decidable while the turn is still
open.

## The uncomfortable part

Joe: *"we have so many of these failures, it seems clear that the model has not
yet caught up with the reality of what's needed for this project."*

The model is sound and its theorems hold. It models the frame cycle's interior
faithfully. What it does not yet model is the set of obligations that make the
cycle's stages composable in a running system — and that set is exactly where
every failure of the past two days has landed.
