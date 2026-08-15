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
