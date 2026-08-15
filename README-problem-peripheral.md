# README — the problem peripheral

One registered experimental problem per cycle. It is the APM demonstration's
runtime: it provisions isolated checkouts, dispatches a solver and a student,
records what happened, and closes by validating its own trace against the frozen
registration.

Built 2026-08-15. `src/futon3c/peripheral/problem.clj`.

## The one rule the whole thing implements

> **The machine owns the fields it is answerable for.** A caller may supply data,
> but anything a gate later reads is written by the engine from its own records.

That is why so much here is *derived* rather than passed in. If a value could be
chosen at the moment it is checked, the check measures the chooser.

## Phases

```
:register → :frame → :guided-solve → :intervene → :student-attempts
          → :adjudicate → :promote → :close → :completed
```

`:completed` is a **terminal sentinel with no tools**. The engine clears the cycle
the moment an advance *returns* the last phase, so the last phase is a transition and
never a state — without the sentinel, `:close`'s tools are unreachable.

Each phase has `required-outputs` that must be present before it can advance.

## What is engine-derived

These tools are computed by the cycle machine from its own state; **the backend is
never called for them**, so no caller can supply their values:

| tool | phase | produces |
|---|---|---|
| `:record-measurement` | `:close` | every required field, measured or unset-with-a-reason |
| `:emit-capability-probes` | `:close` | one probe per capability, citing the attesting step's evidence id |
| `:emit-trace` | `:close` | the validator's trace, projected from cycle outputs |
| `:validate-trace` | `:close` | `{trace? producer-failures}` + the validator report |
| `:write-authorization` | `:close` | the authorization — **only if launchable** |

Environment fields on attempts, and the registration pin, are stamped from the
recorded `:assign-checkouts` result by `:output-stamp-fn` before outputs are merged.

## Running a cycle

```clojure
(require '[futon3c.peripheral.problem :as problem]
         '[futon3c.peripheral.runner :as runner])

(def p (problem/make-problem))          ; or (make-problem backend dispatch-fn state-root provisioner-fn)

(def s (:state (runner/start p {:session-id   "…"
                                :problem-id   "t94J02"
                                :cycle/mode   :store-mode     ; or :harness-mode
                                :lean-repo    "/home/joe/code/mathlib4"
                                :agency-endpoint "http://localhost:7070/api/alpha/invoke/jobs?limit=200"
                                :authorization-revision "<40-hex commit>"
                                :authorization-output   "/path/to/authorization.edn"})))

(runner/step p s {:tool :begin-problem-cycle :args [...]})
;; … tools per phase, advancing with :advance-problem-phase …
```

The four location/output values are **cycle context**, not tool arguments —
`:write-authorization` reads them from state. A caller choosing them at close time
would be choosing where the gate looks.

`make-problem`'s later arities inject the dispatcher, the state root and the
provisioner; **tests use those so they never bell a live agent or create real git
worktrees.**

## Stepping through (backing up and re-running)

`:problem-save` and `:problem-load` are available in **every** phase.

- state is written to `data/problem-state/<cycle-id>/v<N>.edn`, **one file per
  version, write-once**, published by temp-file + atomic rename;
- loading v3 after reaching v5 **leaves v4 and v5 intact** — that is the point;
- a load records a **branch marker**, and cannot cross into another cycle or another
  `:cycle/mode`;
- runtime values (`:cycle-config`, `:evidence-store`) are excluded from saves and
  re-attached from the *live* peripheral on load. A restored cycle writes evidence to
  the store that is actually open.

## Isolation

`:assign-checkouts` provisions the solver's worktree via `scripts/frames.bb`; each
student attempt provisions its **own** at dispatch. Every checkout is distinct and
every base revision identical — same starting environment, different trees. Assignment
is all-or-nothing at `:register`; a *later* failure rolls back nothing, because by then
the solver may hold the whole cycle's work.

Memory channel is fixed by role and cannot be overridden: solver `:push+pull`, student
`:pull-only`.

## Closing

`:emit-trace` refuses if any entity producer is absent — **an empty collection proves
its producer ran; a missing key does not.** `:validate-trace` accumulates producer
failures and validator failures together. `:write-authorization` **refuses on a
non-launchable report and records the refusal**, because a refusal that leaves no
trace is indistinguishable from a cycle that never tried.

A close today correctly refuses:

```
missing producer: retrieval-probe
:guidance-measurement-mismatch
:f9-capability-probe-missing
measurement: 4 measured / 13 unset
:launchable? false     authorization: refused, not written
```

## Known gap — deliberate

**Retrieval probes have no producer.** `available-artifact-ids` has three incompatible
definitions in the mission record and none is executable: the mission's own F7 line
defines *available* as *retrieved*, which makes the check unfalsifiable. Building it
would have meant shipping a tautology or a fabrication. `:need-retrieval` therefore
has no probe and F9 bites — correctly.

**F7 and `:registration-gates-launch` were dropped from round 1**, with reasons
recorded in all three of the Lean, the validator constants, and the registration. The
launch obligation is discharged *by construction*: `Launch` requires a `ReadyToRun`
whose failure cases are proved uninhabited, so it never refuses at runtime and emits
no event to attest.

## Tests

```
clojure -X:test :nses '[futon3c.peripheral.problem-test futon3c.peripheral.cycle-test
                        futon3c.peripheral.proof-test futon3c.apm.preregistration-test
                        futon3c.apm.cycle-harness-test]'
```

**119 tests, 349 assertions.** `check-parens` needs its separator or it silently checks
its own defaults:

```
emacs --batch -l ../futon4/dev/check-parens.el -f arxana-check-parens-cli -- --no-defaults <files>
```

## Where things live

| | |
|---|---|
| registration (frozen) | `holes/labs/M-apm-demonstration/round1-registration.edn` |
| role cards (hashed into it) | `holes/labs/M-apm-demonstration/role-cards/` |
| validator | `src/futon3c/apm/preregistration.clj` |
| Lean model | `mathlib4/DarkTower/APMDemonstrationPreregistration.lean` |
| worktree provisioner | `scripts/frames.bb` |
| design record | `holes/missions/M-apm-demonstration.md` (I.30–I.65) |
