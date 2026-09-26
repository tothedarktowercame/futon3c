# HOWTO: Build and qualify a Lean-validated executable system

This note records the method used in August 2026 to turn the APM countdown
controller into a Lean-owned, mutation-tested, executable protocol. It is a
repeatable recipe for APM and for similar systems where Clojure performs live
effects but a smaller formal model should own the admissible behavior.

The central rule is:

> Lean owns protocol policy. Clojure performs effects and emits durable
> observations. Lean accepts or rejects those observations. Neither side may
> silently reconstruct the other side's authority by hand.

## Resulting repository layout

The build used two isolated branches/worktrees:

- Lean model: `/home/joe/code/mathlib4-apm-validation`, branch
  `feature/apm-cycle-model-v3`;
- Clojure implementation: `/home/joe/code/futon3c-apm-control`, branch
  `frame/18-control`.

Important Lean artifacts:

- `DarkTower/APMCycleMachine.lean` — protocol state and invariants;
- `DarkTower/APMCycleContractEmitter.lean` — deterministic canonical contract;
- `DarkTower/APMCampaignTraceChecker.lean` — executable refinement checker;
- `DarkTower/APMQualification.lean` — explicit non-vacuity witnesses;
- `scripts/check_apm_trace_fixtures.sh` — positive and mutation fixture runner.

Important Clojure artifacts:

- `src/futon3c/apm/generated_contract.clj` — generated-contract boundary;
- `src/futon3c/apm/campaign_trace.clj` — canonical durable trace projection;
- `src/futon3c/apm/qualification.clj` — six-part qualification harness;
- `src/futon3c/apm/countdown_control.clj` — qualified launch boundary;
- `holes/labs/M-apm-demonstration/generated/apm-cycle-contract-v3.json`;
- `holes/labs/M-apm-demonstration/apm-qualification-v1.edn`;
- `data/apm-validation/qualification-report-v1.edn`.

Reference commits from the first completed build are:

- Lean: `2f722232c9` (`Witness non-vacuous APM qualification`);
- Clojure: `ed6315e8` (`Require qualified APM contracts at launch`).

## 1. Begin from observed failures

Do not begin by formalizing idealized data shapes. Collect failures from the
live system and turn each into a behavioral refusal or mutation target. The APM
build incorporated, among others:

- Verify bypassing Solver-memory promotion;
- five-minute work limits drifting from request timeouts;
- client timeout being confused with successful work;
- restart losing the durable job identity;
- Student sessions sharing state or using the wrong memory snapshot;
- campaign regulator and `*problem*` buffer collisions;
- a projection displaying a different ledger digest;
- problem success being conflated with frame success;
- Analyst waking before close, twice, or beyond its tenure.

For every discovered defect, add both a model invariant and a mutation that is
rejected. A fix without a killed mutation is not yet part of the validated
system.

## 2. Model the smallest complete protocol spine in Lean

Model behavioral authority, not transport implementation. For APM this meant:

1. exact registration identity;
2. the complete eleven-phase order;
3. total next-phase transitions;
4. executable solver, checkpoint, Student, Guide, Analyst, and timeout bounds;
5. durable dispatch and restart observations;
6. snapshot review and Student-session bindings;
7. cross-campaign isolation and projection binding;
8. terminal frame outcome and post-close Analyst succession.

Keep non-vacuity visible. Include concrete accepted witnesses and simple
theorems such as the canonical phase count. Compile with no `sorry` or `admit`:

```sh
cd /home/joe/code/mathlib4-apm-validation
lake env lean DarkTower/APMCycleMachine.lean
lake env lean DarkTower/APMCycleContractEmitter.lean
lake env lean DarkTower/APMCampaignTraceChecker.lean
lake env lean DarkTower/APMQualification.lean
```

When one Lean file imports another file changed in the same worktree, rebuild
the imported module if its `.olean` is stale:

```sh
lake build DarkTower.APMCycleMachine
```

## 3. Emit the canonical contract deterministically

The emitter owns the machine-readable phase order, transition table, and
numeric policies. Generate it from Lean:

```sh
cd /home/joe/code/mathlib4-apm-validation
lake env lean --run DarkTower/APMCycleContractEmitter.lean \
  > /tmp/apm-cycle-contract-v3.json
```

Compare it byte-for-byte with the registered artifact:

```sh
cmp /tmp/apm-cycle-contract-v3.json \
  /home/joe/code/futon3c-apm-control/holes/labs/M-apm-demonstration/generated/apm-cycle-contract-v3.json
```

Do not normalize a mismatch away. Either the checked-in artifact is stale or
the emitter changed; inspect, test, and intentionally update the pin.

The Clojure loader must validate the whole emitted policy, not merely parse
JSON. It must reject changed bounds, missing promotion, incomplete transition
tables, or policy drift before registration.

## 4. Port policy without rewriting effect adapters

Existing effectful Clojure code can remain when it is structurally sound.
Replace its policy constants with values from the generated contract:

- registration phase order;
- next-phase ordering;
- Solver maximum rounds and checkpoint interval;
- request and seat turn timeouts;
- Student and Guide counts;
- Analyst tenure.

Avoid an all-at-once rewrite. The APM build retained existing ledger, Agency,
workspace, memory, projection, and Analyst adapters while moving their
admissibility rules behind the generated contract.

If some schema is not generated yet, declare that as an executable residual
hole. Do not describe a hand-maintained field as Lean-owned.

## 5. Emit observations; let Lean decide

The Clojure trace emitter should be deliberately judgment-free. It projects
durable observations into canonical JSON, including:

- campaign, manifest, and contract identity;
- every ordered phase edge and ledger before/after digest;
- receipt and prior-receipt references;
- announced, activated, terminal, and resumed job identity;
- command-owned exit, claim persistence, and receipt persistence;
- timeout observation and whether it was treated as success;
- Solver snapshot digest, depositor/reviewer, and Student bindings;
- campaign regulator, buffer, continuation, Analyst session, and projection;
- problem outcome, frame result, receipt closure, and Analyst tenure records.

Publish traces atomically. Do not have Clojure pre-approve its own trace. Run
the Lean checker on the produced artifact:

```sh
cd /home/joe/code/mathlib4-apm-validation
lake env lean --run DarkTower/APMCampaignTraceChecker.lean \
  /path/to/trace.json
```

Exit `0` with `APM-TRACE-ACCEPTED` is acceptance. A parse failure or nonzero
exit is a failed gate.

## 6. Maintain positive and mutation fixtures

At least one complete positive trace is required; APM uses both a fully closed
frame and a terminal partial frame whose problem proof was banked as solved.
The positive corpus also witnesses two concurrent campaign lanes and three
fresh Student attempts.

Run the complete fixture gate:

```sh
cd /home/joe/code/mathlib4-apm-validation
scripts/check_apm_trace_fixtures.sh \
  /home/joe/code/futon3c-apm-control/test/resources/apm-traces
```

The script must accept every positive fixture and reject every mutant. Mutation
classes should cover ordering, ledger/receipt continuity, dispatch/restart,
memory, concurrency/isolation, terminal outcomes, and Analyst behavior.

Also mutation-test the qualification system itself. It must reject:

- an empty positive-fixture set;
- an invariant class with no mutant;
- a stale generated artifact digest;
- a residual hole without an executable test identifier;
- a numeric bound that exists only in documentation.

## 7. Apply the six-part behavioral bridge

The qualification manifest implements this recipe:

1. registration names the concrete generated artifact;
2. execute the model's bounds over that artifact;
3. assert non-vacuity with witnessed preconditions and accepted traces;
4. mutation-test every modeled invariant class;
5. pin residual uncertainty in executable hole tests;
6. keep bounds in executable tests, never only in docstrings.

Generate a report from actual subprocess exits and observed digests:

```sh
cd /home/joe/code/futon3c-apm-control
clojure -M -e '(require (quote futon3c.apm.qualification))
(prn (futon3c.apm.qualification/run-qualification!
 "holes/labs/M-apm-demonstration/apm-qualification-v1.edn"
 "data/apm-validation/qualification-report-v1.edn"))
(shutdown-agents)'
```

The `shutdown-agents` call prevents Clojure agent pools from keeping this
one-shot command alive after the report is written.

Accept the report only when:

- its top-level `:ok` is true;
- registered and observed artifact digests equal the current artifact digest;
- non-vacuity is witnessed;
- every mutation class has at least one killed mutant;
- residual holes have executable test identifiers;
- bounds have an executable test identifier;
- every gate records `:command-own-exit 0` and `:pass? true`.

## 8. Bind qualification to launch

Future v2 campaigns must run qualification before `set-alight!`. The launch
audit recomputes the contract digest and rejects missing, failing, or stale
reports. Test this boundary without dispatching live roles:

```clojure
(futon3c.apm.countdown-control/dry-run-v2-launch)
```

Require `:ok true` and `:dispatches []`. Mutation-test it with a stale report
digest and require launch refusal before dispatch.

Do not retrofit or relaunch historical v1 or partial ledgers. They remain
readable evidence under the contract with which they were registered.

## 9. Clear implementation gates

For Clojure changes, run lint, parentheses, focused tests, and a broader suite:

During ordinary APM edits, use the bounded fast gate; it excludes tests marked
`^:slow` that execute real pinned Lean qualification:

```sh
scripts/apm-test-fast.sh
```

Run the slow real-artifact gate once before qualification/release:

```sh
scripts/apm-test-slow.sh
```

The persistent coordinator/controller integration tier is intentionally
separate from the edit loop:

```sh
scripts/apm-test-integration.sh
```

```sh
cd /home/joe/code/futon3c-apm-control
clj-kondo --lint src/futon3c/apm test/futon3c/apm

emacs --batch -Q \
  -l /home/joe/code/futon4/dev/check-parens.el \
  --eval '(dolist (f command-line-args-left)
            (with-temp-buffer (insert-file-contents f) (check-parens)))' \
  FILES...

clojure -M:test \
  -n futon3c.apm.qualification-test \
  -n futon3c.apm.generated-contract-test \
  -n futon3c.apm.campaign-trace-test \
  -n futon3c.apm.campaign-machine-test \
  -n futon3c.apm.campaign-ledger-test \
  -n futon3c.apm.live-job-driver-test \
  -n futon3c.apm.memory-snapshot-test \
  -n futon3c.apm.problem-projection-test \
  -n futon3c.apm.analyst-campaign-test \
  -n futon3c.apm.countdown-control-test
```

The first completed build passed 56 tests and 214 assertions in this broader
gate, with two accepted refinement traces and 23 rejected trace mutants.

## 10. Preserve explicit residual holes

The completed build intentionally leaves three boundaries unproved:

1. `hole-generated-receipt-schemas-v1.edn` — receipt schemas and per-phase
   requires/produces declarations remain EDN-owned;
2. `hole-effectful-http-refinement-v1.edn` — Lean validates durable Agency
   observations but does not prove the JVM HTTP implementation;
3. `hole-external-memory-emacs-refinement-v1.edn` — Lean validates snapshot
   and projection identities but does not prove external memory bytes or Emacs
   buffer replacement.

These are not waivers. Each hole names the modeled observations and the test
that would close it. When one is closed, move its authority into the emitter or
formal refinement, add mutations, regenerate the qualification report, and
only then remove the hole.

## Practical sequencing

A productive slice order is:

1. Lean registration and transition model;
2. deterministic contract emitter and round trip;
3. generated policy consumption in the live controller;
4. canonical trace and Lean refinement checker;
5. durable dispatch and restart semantics;
6. memory and concurrent-campaign isolation;
7. terminal close and post-close learning;
8. non-vacuity and meta-qualification;
9. qualified launch integration and documentation.

Commit Lean and Clojure slices separately so that each authority boundary is
reviewable. Use isolated branches/worktrees pinned by commit. Never make a
running campaign depend on the mutable developer checkout, and never weaken an
invariant merely to advance one more frame.
