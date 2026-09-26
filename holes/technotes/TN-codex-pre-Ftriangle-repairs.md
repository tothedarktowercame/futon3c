# TN-codex-pre-Ftriangle-repairs — researching the refusals

claude-clink-1, 2026-08-27. Joe: "Codex refusals should probably be addressed
by research rather than just belling them back."

codex-2 measured four remaining failures and declined to repair them without
provenance (job `…2465`). It was right to: re-dispatching the same packet would
have returned the same refusal. What was missing was information, and each item
below is the research that supplies it.

## 1. open-problem-queue — the corpus count. RESOLVED

codex-2: "corpus currently contains 121 problems; test pins 123. The two-problem
difference still needs provenance before changing the expectation."

Provenance: `construction-blocked` became its own exclusion reason on 2026-08-26
(`6a7735e1`, `57586fc6`), and **a96A07 and a97J08** moved into it. That is the
entire difference. The corpus did not shrink; a classification was added.

Fixed in `7e610703` by decomposing rather than re-pinning:

    475 corpus = 121 queued + 269 not-open + 79 topology
               + 4 defective + 2 construction-blocked

with the construction-blocked set named explicitly, as the defective set already
was, plus an assertion that every exclusion carries one of the four stated
reasons. The next time this count moves it will name which class changed. The
bare literal `123` is what made this ambiguous twice.

## 2. runtime-restoration-script — a stale literal. RESOLVED

The test asserted the script contains `require n :reload`. It no longer does,
because the script now emits `(require '<ns> :reload)` per concrete namespace —
each preceded by a check that the namespace's resource path matches the
canonical checkout. That is *stronger* than what the assertion described, and is
a guard against register defect M7.

Fixed in `9421f946` by asserting the property (a reload happens; `load-file`
never does) instead of the incidental spelling.

## 3. library-lane-launch — a stale fixture. RESOLVED

`workspace-lifecycle/validate` requires a readable `lake-manifest.json` in the
**workspace root** (`:workspace-substrate-manifest-missing`, added 2026-08-21 in
`27b86342`). The fixture wrote one into its *substrate* directory only, so
worktrees of its corpus had none and launch correctly refused.

Fixed in `9421f946` by committing the manifest into the fixture corpus, which is
where a real Lean project keeps it — beside the lakefile, not inside `.lake`.
The refusal was right; the fixture was wrong.

## 4. disruption-soak — NOT resolved, but two hypotheses eliminated

codex-2: "second reconciliation never occurs after process-restart and
duplicate-activation simulations."

Two plausible causes were tested and **both are wrong**:

- **Not a timing budget.** `await-until` allows 150 × 10ms = 1.5s. Widening it
  to 600 × 10ms = 6s does not help; the failures persist.
- **Not the new watchdog gate.** `311549f0` halts any coordinator that ticks
  without a live watchdog, and the soak fixture declares no dispatch deadline,
  so fail-closed halting looked likely. Binding
  `durable-coordinator/*watchdog-running-fn*` to `(constantly true)` changes
  nothing.

What was learned instead: **the test is non-deterministic.** Across four runs
of the unmodified test the failure count was 2, 2, 2, then 1. `await-until`
sleeps on wall-clock and the coordinator schedules concurrently, so this is the
same class as the halt-path flakiness codex-22 repaired in `7a67ba08` — and it
should get the same treatment: synchronise on committed durable state rather
than polling.

### Corrected, 2026-08-27, after Joe: the system is not transactional

The paragraph above originally recommended giving this test the determinism
treatment codex-22 applied to the halt path. **That was wrong, and acting on it
would have hidden the finding.**

Joe's hypothesis — if it does not touch futon1b, the remaining explanation is
that the system is not transactional — is correct and checkable. `grep` for a
tick claim or epoch across `durable_coordinator.clj` and `live_regulator.clj`
returns nothing. A tick's effects and its durable state update are not bracketed
by any claim, so after a restart the observable state depends on where the
interruption landed.

That is register defect **A5**, already stated in
`TN-codex3-apm-guarantee-register.md`: *"every tick durably claims an epoch/tick
id before effects and clears it only after durable completion"* — dated
2026-09-10, a target, not built. A4 and A6 depend on the same bracket.

So `disruption-soak` is not a badly written test. It is the only thing in the
suite that reports A5's absence, and it reports it the only way a missing
transaction boundary can be reported: non-deterministically. Synchronising the
harness harder would make it green while the system stayed non-atomic — the
precise failure mode this whole repair exists to remove.

Correct disposition: leave it red, and name its reason as *reports the absence
of the durable tick claim (A5)* rather than "flaky". If the qualification gate
excludes it, that is the reason string to use. It turns green when A5 is built,
and not before.

Two consequences beyond this test:

- **F△ cannot assert quiescence without A5.** Its preflight condition "the
  watchdog is armed" is checkable today; "no tick is in flight" is not, because
  nothing durably records that a tick is in flight. F△ can still run — it does
  not depend on quiescence — but its report should not claim the machine is
  quiescent.
- **The evidence-durability work already landed is the same shape.** Persisting
  the predecessor before announcing the successor (`7c1398bc`, `f84e3f24`,
  `64e47f3a`) and refusing an enable transition whose history append failed
  (`eb6f0a1e`) are both transaction brackets, added one path at a time. A5 is
  that discipline applied to the tick itself, which is the one place it is still
  missing.

## What this exercise showed

Three of four were stale tests, not defects — a literal, a fixture, and an
unexplained integer. Each had been red long enough to be invisible, because
the `:clojure-qualification` gate runs 8 hand-listed namespaces out of 97 and
none of these is among them.

The fourth is a real defect in the test itself, and only became visible once the
other three stopped drowning it.

## Where the Lean remit ends (Joe, 2026-08-27)

Scheduling is outside the APM Cycle Machine's Lean remit, and covered
operationally by Clojure instead.

The model covers the **cycle**: phases, transitions, receipts, dispositions,
and — since today — trace properties over observations of those. It does not
cover the **scheduler** that drives the cycle: ticks, epochs, leases, drain,
process death. Modelling those means modelling concurrency and wall-clock,
which is a different verification problem and not the one this project needs
solved.

This is consistent with what is already built. codex-3's adapter spec put it
the same way: *"Do not try to prove that networks, filesystems, or callbacks do
not fail; prove that any failure leaves a classifiable durable state from which
recovery is defined."* And the progress invariant formalised in `ae438faa` is a
property of **observations of frame progress**, not of the executor — which is
why it could be bound to the contract at all.

So the boundary is:

| | Lean | Clojure |
|---|---|---|
| cycle transitions, receipts, dispositions | modelled and proved | validated |
| observations of those, as traces | modelled and proved | projected, checked |
| tick claims, epochs, leases, drain, restart | **not modelled** | **must be covered operationally** |

"Not modelled" is not "not assured". A5 needs real operational coverage —
durable claim before effects, cleared after durable completion, with
deterministic tests — it just does not need a theorem.

### The consequence for disruption-soak

This makes the disposition above sharper rather than softer. `disruption-soak`
is exactly the operational coverage the table's third row demands, and it is
currently the only thing exercising it. It reports A5's absence
non-deterministically because that is how a missing transaction boundary
presents.

So it is not a test to repair and not a test to exclude on grounds of
flakiness. **It is A5's acceptance test.** When the durable tick claim exists,
`:process-restart` and `:duplicate-activation` should become deterministic and
green; until then it stays red for a stated reason. That is Joe's own pattern
from earlier today — a failure found now becomes the gate on the fix later —
applied to the last structural gap rather than to a bug.
