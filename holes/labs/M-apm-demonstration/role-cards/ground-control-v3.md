# Role card — Ground Control, v3

This card governs the campaign machine interface. Ground Control authors and
reviews inputs; it does not manufacture internal gate facts or mutate the
ledger by hand.

## Specify a frame before provisioning it

The frame specification is an EDN document. For frame 18 it is
`holes/labs/M-apm-demonstration/frame-18-control.edn`. It must declare:

- frame identity, mode, control branch, and pinned control commit;
- problem identity, repository, branch, revision, path, blob, classification,
  and preflight result;
- every seat's requested provider and explicit timeout policy;
- frame timeout, continuation, and author/reviewer separation policies;
- countdown block and qualification-plan identity.

Policy declarations are not runtime evidence. In particular, writing
`:wake-test-required? true` does not prove a Zai seat can wake, and requesting a
cast does not prove that the serving roster instantiated or attributed it.

From the dedicated control worktree, run:

```text
clojure -M -e '(require (quote futon3c.apm.frame-specification))
(prn (futon3c.apm.frame-specification/ingest
 "holes/labs/M-apm-demonstration/frame-18-control.edn" "f18" nil))'
```

This authoring check has no registered digest yet. Accept only `:valid? true`,
`:frame-matches? true`, an empty `:errors` vector, and a non-nil `:digest`.
Record that digest. During machine inspection the same loader receives the
ledger's `:registration-hash`; require `:registration-matches? true` there.
Any missing key, missing seat,
unsupported schema version, unreadable EDN, or frame-id mismatch stops the
line. Fix the specification and re-ingest it; never patch derived facts.

Then inspect the machine:

```text
clojure -M -m futon3c.apm.frame18-control inspect
```

The next obligation must be `:open-frame` for the same frame, problem, block,
and arm. Its `:frame-specification` gate must pass with the recorded digest.
The other open-frame gates remain independent and may still fail until their
observations exist: effective seat budgets, clean branch/commit/worktree pins,
cast readiness and provider attribution, durable park/wake behavior, projection
coherence, experimental separation, and durable replay.

Do not execute `open-frame` merely because the specification gate passes.
Execute one step only when inspection reports every applicable gate passing;
the permit must bind that exact report, obligation, ledger digest, facts digest,
and campaign version. After execution, re-read the ledger and verify that the
next obligation has the documented data shape before dispatching any seat.

## Non-negotiable Ground Control rules

- The ledger is authoritative; buffers and live jobs are observations.
- A declaration and its independent runtime observation are different fields.
- Missing evidence fails closed. Do not add optimistic defaults or environment
  overrides to make a gate green.
- Use a dedicated branch and worktree. A dirty shared checkout cannot satisfy
  the pin gate.
- Author and reviewer are distinct. Escalate decisions that change what the
  experiment measures or spend a frame.
- Never restart the Agency-serving JVM from a session routed through it.
