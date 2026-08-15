# README — the problem peripheral

One registered experimental problem per cycle. It is the APM demonstration's
runtime: it provisions isolated checkouts, dispatches a solver and a student,
records what happened, and closes by validating its own trace against the frozen
registration.

Built 2026-08-15. `src/futon3c/peripheral/problem.clj`.

## The one rule the whole thing implements

> **The machine owns the fields it is answerable for.** A caller may supply data,
> but the engine writes the fields it is answerable for from its own records.

That is why so much here is *derived* rather than passed in. If a value could be
chosen at the moment it is checked, the check measures the chooser.

**The rule holds of the derived fields, not of everything.** Frame hashes,
dispositions, containment probes, snapshots and offers arrive as **caller-relayed**
advance payloads and are shape- and consistency-checked only. In particular the
emitter-side F1 gate that hashes actual files (`cycle-harness/emit-frame!`) sits on
the `run-cycle!` path, **not** the peripheral path — here the validator compares two
relayed strings. That is defensible for a Codex-as-REPL process, but **do not read a
relayed claim as a measured one.**

**Real in production:** provisioning, dispatch, state IO, and the five derived close
tools. **Relayed:** everything else, including via `(make-problem)` with no arguments,
which uses the mock backend for every tool the wrappers do not intercept.

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

`:emit-trace` **completes and records `producer-failures`** when an entity producer is
absent — it does not refuse; refusal is deferred to `:write-authorization`, which
keeps the whole picture auditable. **An empty collection proves its producer ran; a
missing key does not.** `:validate-trace` accumulates producer failures and validator
failures together. `:write-authorization` **refuses on a
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

## Reading a refusal

**No cycle can currently produce a written authorization, and that is the accepted
normal for round 1.** Three refusal codes are **expected and benign** — they mean the
machine is working and the known gaps are known:

| expected-and-benign | why |
|---|---|
| `missing producer: retrieval-probe` | no producer exists (below) |
| `:f9-capability-probe-missing` | `:need-retrieval` cannot be probed without one |
| `:guidance-measurement-mismatch` | `record-measurement` cannot reach Agency evidence, so "attempts or closer hops" is unset while the validator computes it independently |
| `:direct-channel-evidence-unavailable` | same cause as `:guidance-evidence-unavailable` — no live Agency evidence (offline validation) |

Rehearsal-1 (2026-08-15, cycle `a01A06-1eb0b137…`) confirmed the three
in-JVM codes empirically — and demonstrated that `:direct-channel-used` fires
as a true positive (the conductor's own in-window bell to the student seat was
caught). The retrieval-probe codes may clear once the recall channel fix is
verified: the packet-F producer starves without working recall.

**Anything else in a refusal is disqualifying** and means that cycle is not sound.
A reviewer of frame-40 should be able to tell a healthy refusal from a broken one by
this list alone.

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

### Two things that look like checks and are not

- **`:solver-seat-mismatch` is dead on this path.** `validate-trace-from-state` passes
  the registration's own pin as the invoked seat, so it compares the pin against
  itself. Correct given engine ownership — but do not rely on it firing here.
- **`:f7-missed-available-artifact` is still computed** though F7 was dropped from the
  runtime invariants. It is vacuous by construction now that retrieval probes are
  stripped at close (`[] ⊆ []`), but it is a check with nothing behind it.

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
