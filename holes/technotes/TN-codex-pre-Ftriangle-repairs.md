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

Until it is deterministic its failures cannot be interpreted, so making it
deterministic comes before diagnosing what it reports. A flaky test in the
disruption-recovery path is worth no more than the halt-path one was.

## What this exercise showed

Three of four were stale tests, not defects — a literal, a fixture, and an
unexplained integer. Each had been red long enough to be invisible, because
the `:clojure-qualification` gate runs 8 hand-listed namespaces out of 97 and
none of these is among them.

The fourth is a real defect in the test itself, and only became visible once the
other three stopped drowning it.
