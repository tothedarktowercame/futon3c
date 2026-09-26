# Independent packet review — 2026-09-11

Accepted `736db8cf` for the bounded initialization historical-admission queue.

The canonical packet audit independently passed all eight hashes, current source joins, exact casting codex-17/codex-12/codex-12, disabled template and target one.

The packet-specific materialized async test independently passed **1 test / 28 assertions / zero failures or errors**. Exact invocation:

```sh
clojure -Sdeps '{:aliases {:futon2-test-support {:extra-paths ["../futon2/test"]}}}' -M:test:test-all:futon2-test-support -i :slow -n futon3c.wm.run4-initialization-collision-packet-roundtrip-test
```

This exercises the actual historical runner, store admission, cohort close, durable projection/binding/readers, observation and queue hold. Requested U88 remains authenticated-not-enacted; no task result or repair resolution is inferred. Duplicate tick retains one attempt and one close. Disposable roster/environment ports are not live availability evidence.

Two earlier invocations without sibling test-support classpath failed namespace loading; they establish no test result. The successful command above supplies that classpath explicitly.

Retained test log: `/tmp/run4-init-packet-review.out`; SHA256: `8caaffd96c551cf7b9df8a2d064834ae6430bb2ed988f6dddd1b245a4b4b5f09`.

Fresh Agency observations during review: codex-12 idle, codex-10 idle, codex-17 invoking. Therefore installation/start ownership is handed to codex-10 with a fresh post-park check of both pinned actors before admission. Do not spoof availability or change casting. Installation must freshly validate first stop-line, exact pins, loaded effective consumers, private credential boundary and untouched target-one capacity. Joe authorizes this live work after these gates; no additional user approval is required.

No live queue or capacity was created by this review. Preserve all earlier receipts and failed cohorts. One historical queue entry can only reach awaiting-validation/held, requiring a separately verified production successor.
