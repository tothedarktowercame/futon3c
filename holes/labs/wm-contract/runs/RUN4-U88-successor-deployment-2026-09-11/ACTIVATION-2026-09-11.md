# Successor activation and live start — 2026-09-11

Joe authorized the bookkeeping repair and actual U88 trial. Reviewed packet: futon2 9ed27522 and futon3c b6819675; serving cohort summary 66cca57a. Actual-core consumer proof 2b0319f5, independently rerun after hermetic store correction cd6d17a5: 9 tests / 56 assertions pass. Independent actual cohort boundary d0c0e450: 1 test / 6 assertions pass.

All five packet/template/binding hashes matched the review report. Provisioned separate cohort root `/home/joe/run4/U88-cohort-20260911` and six successor roots under `/home/joe/run4/U88-successor`, mode 0700. The existing protected Joe credential was reused only inside server construction; it was not printed or copied.

The activation write through Babashka produced the correct immutable record, then reported an unsupported FileLockImpl.close call. No repeat activation was attempted. A fresh Clojure JVM independently validated the recorded activation SHA/identity/target with execution-preflight, yielding target 1 / remaining 1. Subsequent execution uses the Clojure serving JVM, not Babashka.

Canonical consumer namespaces were reloaded; the successor template and exact execution-cohort binding were materialized through the accepted server-owned ports. Trusted preparation confirmed the exact cohort map reached runner options, fresh pin eligibility and effective loaded flags passed, and the combined handler was reconfigured atomically. Startup environment was not changed and no restart occurred.

Both Zai-2 and Codex-12 were idle and invoke-ready immediately before the authenticated HTTP series step. Response: trial-started, ordinal 1, click `wm-click-8d1d9141-5e33-4032-aebd-dbadc2d9755f` at 02:13:32Z. The actual cohort now contains attempt-001/001-time-step.edn; the runner reached preference-refresh. This establishes consumption of the successor cohort, not task completion or acceptance. Monitoring continues.

The first click and old admission/root remain retained; nothing was reset or relabeled. No Claude or Codex16 was dispatched.
