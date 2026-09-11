# Disabled Zai-staffed successor-v2-selection historical admission

This packet binds a Zai-staffed (`zai-2` author / `zai-1` independent
reviewer) verified historical repair action for
`repair-run4-u88-production-successor-20260911-v2--attempt-001-untyped-failure`
to a distinct one-attempt cohort and frozen RUN4 series. The retained
codex-cast verification `99b0fed181c7142a4d498abd08e9bb7dcf6247ec4decf3f41d6434a1724c31c6`
(actors codex-10/codex-12) is immutable and untouched; this packet's fresh
receipts are:

- qualification `repair-successor-v2-selection-revalidation-zai-20260911-v1`
  (offline-evidence, sha256 `655a867699f6fdd6ca789c903a4c7281e06066c3b20bcd387d45abc20703ea17`)
  produced once by `produce-zai-qualification.clj` with the exact three-check
  population of the accepted successor-selection qualification
  (actual-stop-line-selection-controls, new-admission-and-replay-controls,
  exact-target-inspection-and-lock-controls) against explicit current source
  pins; before/after source audit recorded no drift.
- independent review: real Agency job `invoke-1789149846493-20228-9dfa3dda`
  by zai-1 (5 tool_use events), verdict APPROVE, digest marker verified.
- verification (offline-verification, sha256
  `4ab2f8819d45ede8591d13d93d2d0d9fbd125a9d270c29cda32a933fbe95a9d7`),
  admitted by `run-zai-verifier.clj` at the then-current explicit descendant
  HEAD `810be2a9a19d70b054d9ef7ceb43a2349b7a923d` (first-commit
  `8788443d7cf0c806261933e2c68009d84f57819d`).

## Disposable lifecycle gate

```sh
cd /home/joe/code/futon3c && clojure \
  -Sdeps '{:aliases {:zai-gate {:extra-paths ["test" "../futon2/test" "dev"]
    :jvm-opts ["--add-opens=java.base/java.nio=org.apache.arrow.memory.core,ALL-UNNAMED"
               "-Dio.netty.tryReflectionSetAccessible=true" "-Djava.net.preferIPv4Stack=true"
               "-Xmx2g" "-XX:MaxDirectMemorySize=768m"]}}}' \
  -M:zai-gate -i "holes/labs/wm-contract/runs/RUN4-repair-successor-v2-selection-zai-admission-2026-09-11/run_zai_packet_gate.clj"
```

Structural reuse of the accepted packet roundtrip machinery: recreates the
zai verification through the real verifier (committed qualification bytes,
real zai-1 job fetched live from Agency), activates this packet's cohort,
materializes `server-config.disabled.edn`, validates historical-action
applicability, and drives one queue tick in hermetic roots with a zai roster.
Passed 2026-09-11 (exit 0): tick :held/:terminal-evidence-incomplete,
binding :verified/:historical-verification-awaiting-validation, projection
attempt `successor-v2-selection-zai-historical-admission-001`, casting
zai-2/zai-1/zai-1. Log retained in `zai-packet-gate-2026-09-11.log`.

The packet and queue are disabled. No roots, cohort capacity, handler config,
or queue were installed. Private installation, only after independent review
(zai-1 final packet review job id recorded in
FINAL-ZAI-PACKET-REVIEW-2026-09-11.md), consists of materializing
`server-config.disabled.edn` with the existing server-owned credential and
mission/admissibility ports, attaching the enabled historical action and
four-key execution-cohort binding, then constructing the runtime queue entry
from `queue.disabled.edn` and calling `futon3c.wm.run4-series-queue/start!`.
The coordinator owns the actual live install and the single queue entry.
Historical execution may produce only awaiting-validation; it is not U88 task
success, does not resolve the repair, and cannot discharge the separate
init38690 collision.
