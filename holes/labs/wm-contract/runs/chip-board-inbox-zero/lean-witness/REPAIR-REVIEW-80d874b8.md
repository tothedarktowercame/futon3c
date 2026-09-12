# Repair review: 80d874b8

**ACCEPT for the three specific fixes.** Mathlib commit `a9a24a3b9e`
extends the original witness with a separate `Repaired` namespace. The old
model and its counterexamples remain visible as historical evidence.

The repaired relation admits a successful ZAP only with a named, certified
repository; it proves every commit has a strictly earlier FEEL for that repo.
Nil-ZAP execution is impossible and nil-ZAP refusal is constructible. Full
modeled replay equality implies equality of effect lists; altered effects
refuse. The four compare-move reference branches are checked. The pinned
six-row inbox fixture still executes and satisfies the repaired invariant.

Validation completed on 2026-09-12:

- Lean: zero elaboration errors; all **17** printed axiom sets exclude sorryAx.
- Inbox readback: **17 lines match**, zero deltas, retained production bytes
  unchanged. The original observation packet remains absent, so its input
  digest is still only a retained carrier, not independently recomputed.
- Focused repaired controls: **4 tests / 13 assertions**, passing (nil refusal,
  tampered effects, actual compare-move board termination, unknown-verb limit).
- Cascade registry controls: **2 tests / 10 assertions**, passing.
- Lint: 0 errors/warnings; parens OK; diff checks clean.

## Status decision

`:lean/status :pending` is retained in both producers. The repair removes the
three named defects, but does not establish unconditional runtime conformance
to the declared semantics. In particular, absence of ZAP in a cascade board is
not enough if registered SMELL emits COMMIT. The isolated registry-replacement
control still produces that effect and a true replay result.

During review, an in-progress runtime edit caused one load-time EOF; no tests
ran in that failed process. Its author then committed `deca3aa8`. The successful
checks above ran after that commit, and the validation log records the exact
source hashes. The new verbs digest is carried on the run and compared during
replay, but is not an approval of the function's semantics. An already-replaced
function can generate its own matching digest and still violate the no-act
property. Neither certificate producer currently copies that digest into its
certificate. The executor also reads the mutable registry separately per step.
This review does not accept `deca3aa8` as an unconditional registry-safety proof.

Unknown verbs still pass current board/wire validators then throw at execution;
implementing compare-move repairs that verb, not all arbitrary EDN programs.
The model uses named string repositories and missing targets, not arbitrary
Clojure values or arbitrary external effect-handler behavior.

The historical validation.log and transcript were preserved. New Lean/readback
output is `validation-after-80d874b8.log`; `check.py` now checks the repaired
model, emits a scoped validated result, and names the remaining status blockers.
No live namespace, registry, certificate, queue or attempt was modified.
