# RUN4 terminal consumer: independent review findings

Codex-17, 2026-09-10. 4440332e is not accepted for series advancement yet.
Its focused namespace passes independently: 5 tests / 12 assertions.
Additional checks used its disposable fixture and rehashed the altered
projection reference, so they test semantic joins independently of integrity.

- Internal-attempt mismatch between binding and projection: correctly refused
  with projection-binding-mismatch.
- Selection checkpoint replaced by explicit checkpoint-not-returned absence:
  incorrectly returns succeeded/safe.
- Build validation review-job changed to unrelated-review, while evidence
  reviewer-job-id remains review-1: incorrectly returns succeeded/safe.

Reproduction: /tmp/run4-terminal-review.clj, executed with the local dev/test
classpath in a separate process. No live evidence or service was touched.
Requested correction covers the complete producer-supported success ladder:
present checkpoints, exact pin joins, review-job identity, build/grounded commit
and implementation identity, plus contradictory failure evidence. Missing
producer evidence must be reported, never supplied by a synthetic positive.
Safe build-failure classification also needs consistency with checkpoint facts.
The note must distinguish waiting for a not-yet-created binding from refusing
a missing projection already declared by a binding.

This is a bounded consumer repair, not a new model choice. Existing projection
and admission acceptances stand; series advancement waits for the corrected
consumer. Zai-2's concrete mission/fixture preparation is separately in flight
(2f9d2ac3), so the original queue report is not dispatched twice.
