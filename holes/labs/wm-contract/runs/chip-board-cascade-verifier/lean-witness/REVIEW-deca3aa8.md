# deca3aa8: in-process identity check, not semantic approval

**ACCEPT the narrow digest comparison; not a blanket Lean-status upgrade.**

Executed the exact registry-swap regression: 1 test / 3 assertions, passing.
Also executed registry-digest-review.clj with the real executor in an isolated
registry atom and a no-op handler:

- A wrapper function returning identical results produces an identical trace
  but a different verbs digest; the original run refuses. This isolates the
  digest test from the existing full-trace comparison.
- Restoring the original function revalidates the original retained run.
- Installing a commit-emitting smell-backlog before a NEW run still produces
  a commit on the no-ZAP board and :verified? true. Its self-consistent digest
  is not a proof of allowed effect semantics.
- :verbs/digest is on the returned run, absent from its nested certificate.

Re-elaborated CascadeVerifierBoardWitness: zero errors, nine axiom checks
without sorryAx, fourteen production readback lines match. Fresh output and
source hashes are in validation-after-deca3aa8.log; the older log is preserved.
The witness already explicitly names the declared registry as a premise and
proves the unrestricted board-only claim false. No theorem assumes that any
arbitrary digest identifies the proved semantics.

A per-run digest can identify WHICH registry was used only subject to its
identity-string limitations and a consistent capture. It cannot establish that
that registry implements the Lean relation. The current loop also dereferences
the registry separately at each step rather than executing a captured mapping.
The documented in-process identity scope is useful; 'content digest' should not
be read as a portable source hash or proof of behavior.

Both runtime :lean/status values remain :pending. To validate a runtime claim,
provide an enforced correspondence from the captured registry to the proved
semantics, or define and independently check a narrower returned-trace safety
claim. This review does not invent either authority. No production registry,
run, certificate, queue or capacity changed. No test suite was rerun.
