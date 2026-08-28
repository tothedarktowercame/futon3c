# TN-apm-in-jvm-validation-and-usage-limits

Joe, 2026-08-28, after the f49 diagnosis.

## 1. In-JVM validation with core.logic

Codex-3's delivery conformance suite reported one row as **unconstructable**:
the in-JVM caller shape, because "jobs persist only a caller string" — F△ and
C□ dispatch that way and are indistinguishable from any unregistered caller.

That framing is too pessimistic, and Joe's correction is the useful one: the
information exists **in the running process**. It is persistence that loses it.
A receipt-based validation can only see what was written down; an in-JVM check
can see the live condition. So "unconstructable from receipts" is not
"unvalidatable".

Joe has used `core.logic` for this before. Two claims worth holding onto:

- It can validate operational conditions **while things are running**, and be
  switched off once there is sufficient assurance the model holds — and
  switched back on if behaviour gets wonky. Validation becomes a dial, not a
  fixed cost.
- **What the Clojure adapter produces from Lean could itself be core.logic.**
  Today the emitter produces a JSON contract that `generated_contract.clj`
  checks as data. If it emitted relations instead, the same declarations would
  be checkable against live state rather than only against receipts.

That second point bears directly on the two global validations
(`TN-apm-two-global-validations.md`). Shape and stage-contract obligations
stated as relations are checkable *while a turn is open*, which is exactly what
f49 needed and post-hoc rejection could not give.

Not a proposal to re-instrument everything. The narrow claim: in-JVM
validation should not be ruled out because receipts are lossy, and the
Lean→Clojure adapter is a plausible place for it to live.

## 2. Usage limits are a substrate condition the machine cannot see

f49's `guide-intervention-1` failed three times identically. The cause was not
prose, not an unclear contract, and not a misbehaving agent:

    :error/message ".../apm-role-report-….edn:1:88: error: Invalid symbol:
                    claude.ai/settings/usage?from=cc_cli_limit_message."

The Claude seat hit its usage limit mid-turn. The CLI's limit notice was
captured as the role's output, written into the report EDN, and made it
unparseable. Everything else follows: identical failure on every re-dispatch
(quota does not clear), truncation mid-word, and output that looked like
narration because it *was* narration up to the moment the limit hit.

The machine classified this as `:typed-submission-missing`, spent the frame's
single repair allowance on it, and halted a 121-problem queue.

Joe: presumably the five-hour rolling window rather than the weekly budget —
in which case *the overnight delay was itself the remedy*, and waiting was the
correct behaviour arrived at by accident.

### What should happen instead

- **Detect it.** The limit notice is recognisable text. A role turn whose
  output contains it is a substrate failure, not a role failure.
- **Do not spend the repair budget.** Re-dispatch cannot fix a quota wall; each
  attempt burns an allowance against a condition that will not change.
- **Then either wait or substitute.** A *planned* pause until the window
  resets, or dispatch to a different provider — GLM is already in the system as
  the student seat. An unplanned halt is what we had; a planned pause is the
  same wait with the machine knowing why.

This is the retry-on-substrate-failure case Joe raised on 2026-08-27
("on substrate failures, maybe retry after 10 minutes?"), with a concrete
detector and a concrete reason the retry interval is hours rather than minutes.
