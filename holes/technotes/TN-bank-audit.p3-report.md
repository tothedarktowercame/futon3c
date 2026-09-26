# P3 frame-close solve-pinning report

Date: 2026-08-26

## Commits and files

- P1 classification: `1f53cf03c53b25e407a7812e440c4971577f4294`
- P2 verification/pinning: `870e872512d8ab7e018f62d41de90394b32d7991`
- P3 frame-close wiring and tests: `ee201259e04d68fc9bde51b9bf6e6d3b527471cf`
- This report is committed separately after the implementation commit.

P3 touched:

- `src/futon3c/apm/queued_frame_terminal.clj`
- `src/futon3c/apm/queued_frame_adapter.clj`
- `src/futon3c/apm/countdown_control.clj`
- `test/futon3c/apm/queued_frame_terminal_test.clj`
- `holes/technotes/TN-bank-audit.p3-report.md`

No pre-existing shared-checkout changes were staged or modified.

## Behavior

`queued-frame-terminal/retire!` now requires an injected `pin-solve-fn`. It
calls that effect exactly once for a solved terminal, before persisting the
problem-bank metadata receipt and before auditing or retiring either workspace.
Non-solved terminals do not call the effect.

The production effect is supplied by `countdown_control` and delegates to
`bank-audit/verify-and-pin!` with frame ID, problem ID, exact terminal solver
head, and the problem repository. Both the ordinary JIT queue path through
`queued-frame-adapter/live-effects` and the direct progress-retirement call
receive the effect. The latter is non-solved and therefore skips it, but the
provider contract remains uniform.

The problem-bank receipt always records `:solve/pin-status`. This packet uses
the consistent non-solved policy `:solve/pin-status :skipped`; it omits ref and
reason for skipped outcomes. Pinned receipts add `:solve/pin-ref`. Refused
receipts add `:solve/pin-reason`. These fields are inserted before
`:receipt/id` is computed, so the existing ledger digest covers them.

`:branch-retained?` is now true exactly when `:solve/pin-status` is `:pinned`.
It is false for refusal or skip. It no longer certifies mere continued
existence of an `exp/` branch.

A refusal is data, not a retirement failure. A thrown pin effect is caught and
normalized to:

```clojure
{:status :refused :reason :pin-effect-threw}
```

An invalid effect return is similarly recorded as `:pin-result-invalid`.
Neither prevents metadata persistence, workspace retirement, seat retirement,
or frame closure.

The terminology remains distinct: `build-problem-bank` still constructs the
existing metadata receipt. New behavior and fields consistently use “verified
solve pinning”; no Git operation is called “problem banking.”

The resulting refs remain directly discoverable for P4 by the prefix
`refs/apm/banked-solves/`, with frame/problem/head encoded in each ref.

## Gates

Lint command:

```text
clj-kondo --lint src/futon3c/apm/queued_frame_terminal.clj src/futon3c/apm/queued_frame_adapter.clj src/futon3c/apm/countdown_control.clj test/futon3c/apm/queued_frame_terminal_test.clj
```

Output:

```text
linting took 86ms, errors: 0, warnings: 0
```

Parenthesis command:

```text
emacs -Q --batch -l ../futon4/dev/check-parens.el --eval '(arxana-check-parens-cli)' -- src/futon3c/apm/queued_frame_terminal.clj src/futon3c/apm/queued_frame_adapter.clj src/futon3c/apm/countdown_control.clj test/futon3c/apm/queued_frame_terminal_test.clj
```

Output:

```text
OK
```

Focused terminal/retirement command:

```text
clojure -M:test \
  -v futon3c.apm.queued-frame-terminal-test/terminal-banking-retains-exact-branch-and-needs-independent-audits \
  -v futon3c.apm.queued-frame-terminal-test/refused-solve-pin-is-recorded-without-blocking-close \
  -v futon3c.apm.queued-frame-terminal-test/non-solved-terminal-skips-pin-effect \
  -v futon3c.apm.queued-frame-terminal-test/throwing-pin-effect-is-recorded-without-blocking-close \
  -v futon3c.apm.queued-frame-terminal-test/problem-bank-id-covers-solve-pin-evidence \
  -v futon3c.apm.queued-frame-terminal-test/f30-shaped-replay-skips-durably-retired-solver-before-student-retry
```

Output:

```text
Testing futon3c.apm.queued-frame-terminal-test

Ran 6 tests containing 29 assertions.
0 failures, 0 errors.
```

P2 regression command and output:

```text
clojure -M:test -n futon3c.apm.bank-audit-test

Ran 5 tests containing 16 assertions.
0 failures, 0 errors.
```

Adapter regression command and output:

```text
clojure -M:test -n futon3c.apm.queued-frame-adapter-test

Ran 14 tests containing 64 assertions.
0 failures, 0 errors.
```

`git diff --check` passed with no output. No Lean or Lake command was run in
P3. The final check:

```text
test ! -e /home/joe/code/apm-lean/.lake/build/lib/Mathlib.olean
```

exited zero; the project-local `Mathlib.olean` remains absent.

## Existing suite failures

The complete terminal namespace currently reports:

```text
Ran 10 tests containing 67 assertions.
21 failures, 0 errors.
```

All 21 failures belong to
`five-problem-no-dispatch-terminal-qualification`. Its first queue tick fails
before preparation or retirement; repeated ticks remint ordinal zero. I ran
that single test in a detached worktree at pre-P3 commit `3d91dd0e`; it had the
same 21 failures in 23 assertions. P3 did not alter or route around it.

The complete `futon3c.apm.countdown-control-test` namespace currently reports:

```text
Ran 35 tests containing 147 assertions.
2 failures, 0 errors.
```

Both failures belong to
`campaign-priors-follow-queue-order-and-final-receipt-snapshots`: the fixture
expects f28/f29/f30 priors while the mutable campaign data now yields f35/f42.
Running that exact test in a detached worktree at pre-P3 commit `3d91dd0e`
reproduced the same 2 failures in 2 assertions. P3 did not modify the fixture or
campaign data.

## Acceptance evidence

The focused tests establish:

1. A solved terminal calls the pin effect once with the exact solver head;
   pinning precedes receipt persistence and workspace retirement; the receipt
   carries status and ref.
2. A `:sorry-ax` refusal still produces an `:ok` close with both workspace
   retirement receipts, and records status/reason.
3. A non-solved terminal records `:skipped` and makes zero pin-effect calls.
4. A thrown pin effect still produces an `:ok` close and records
   `:pin-effect-threw`.
5. Changing pin evidence changes the problem-bank `:receipt/id`, and both
   variants pass the existing address/digest check.

## Spec observations and P4 shape

No substantive P3 premise was wrong. The existing default solved-frame close
is constructed through `queued_frame_adapter/live-effects`, while the direct
call near the named `countdown_control` line is the progress-retry retirement
path. Consequently the production injection had to be passed through the
adapter configuration as well as supplied to that direct call; otherwise
normal solved frames would have no provider.

I agree with the proposed P4 sweep rather than inline master mutation. P3 gives
the sweep a stable enumeration surface (`git for-each-ref
refs/apm/banked-solves/`) and content-addressed identities. A sweep can compare
each pinned proof against master, use the P1 content rule for idempotence, and
emit the convention's separate Lean and metadata commits without holding frame
closure open on shared-repository writes. No P4 behavior was implemented here.
