# Wiring census — the problem peripheral, 2026-08-15

Measured, not grepped: run the full traverse against a `MockBackend`, then ask
the mock which tools reached it. A tool that reaches the mock has **no
implementation** — `MockBackend/execute-tool` answers anything it holds no canned
value for with `{:ok true :result nil}` (`tools.clj:117-124`). That is why a
traverse can be green while half the machine is absent.

    phases walked : ALL EIGHT -> sentinel
    CENSUS        : 8 of 20 phase tools have an implementation

## Implemented (8)

| tool | by |
|---|---|
| `:assign-checkouts` | `CheckoutProvisioningBackend` |
| `:dispatch-student-fresh` | `CheckoutProvisioningBackend` |
| `:dispatch-solver` | `GroundControlBackend` |
| `:record-measurement` | engine derived tool |
| `:emit-capability-probes` | engine derived tool |
| `:emit-trace` | engine derived tool |
| `:validate-trace` | engine derived tool |
| `:write-authorization` | engine derived tool |

Plus `:begin-problem-cycle` and `:advance-problem-phase` (`ProblemCycleBackend`,
2026-08-15) and `:problem-save` / `:problem-load` (`ProblemStateBackend`), which
are not phase tools.

## Not implemented (12)

    :read-registration   :validate-registration  :snapshot-store  :freeze-stratum
    :pin-resources       :emit-frame             :guide-solver    :read-substrate
    :read-attempt-result :write-disposition      :write-use       :promote-artifact

Every one of them currently answers `{:ok true :result nil}`.

## What this means for frame-1

The close phase is the built end. Everything the validator consumes — the frame,
the containment probe, the dispositions, the offers and uses, the promotions — is
produced by a tool that does nothing, so at a live run the caller would have to
relay all of it by hand and the machine would check relayed strings against
relayed strings.

**The traverse being green is not evidence that the cycle works.** It is evidence
that the cycle *transitions*. Those are different claims, and the census is the
one that answers "are the phases wired".

---

## Census closed — 2026-08-15, four packets later

    8 of 20  ->  11 of 19  ->  19 of 19

`:pin-resources` was deleted rather than implemented (the engine already owns
what it would pin), which is why the denominator moved 20 -> 19. **Nothing
reaches the mock backend any more**, and the traverse walks all eight phases to
the sentinel with every tool doing real work.

### The close now runs, and refuses

```
producer-failures : [:retrieval-probe]
validator failures: :missing-trace-entity-producer :malformed-cycle-attempts
                    :malformed-deposit-state :malformed-trace-boolean
                    :direct-channel-evidence-unavailable :guidance-evidence-unavailable
                    :f8-unwitnessed-containment :f9-capability-not-realized
                    :f9-capability-probe-missing
authorization     : refused, not written
measurement       : 3 measured / 14 unset
```

Classified against README-problem-peripheral.md's expected-and-benign list:

| code | verdict |
|---|---|
| `:missing-trace-entity-producer` (retrieval-probe) | **benign** — the known gap, no producer exists |
| `:f9-capability-probe-missing` | **benign** — follows from the above |
| `:guidance-evidence-unavailable` | **benign** — variant of the listed guidance code; no live Agency in a synthetic run |
| `:direct-channel-evidence-unavailable` | **benign, same cause** — not on the list; add it |
| `:malformed-cycle-attempts`, `:malformed-deposit-state`, `:malformed-trace-boolean` | **harness** — the census supplies skeletal attempts, not the machine's fault |
| `:f9-capability-not-realized` | **follows from the probe gap** |
| `:f8-unwitnessed-containment` | **UNRESOLVED** — fired although a real witness file was supplied. Investigate before frame-1. |

**The machinery is no longer the blocker.** What remains is input quality, two
genuinely unbuilt things (retrieval probes, live Agency evidence), and one open
question in F8.
