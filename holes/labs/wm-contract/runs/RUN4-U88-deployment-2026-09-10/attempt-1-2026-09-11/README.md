# U88 first authorized trial — 2026-09-11

Joe authorized execution of the prepared inner-loop example with Codex/Zai staffing. The actual authenticated HTTP series-step returned `trial-started`, ordinal 1, click `wm-click-560a7b67-0f29-4696-b062-66a84ea049a7` at 01:47:54Z. This was the machine's serving path, not a manually belled substitute worker.

Result: `cohort-complete`, before task selection or construction. The binding records `run-record-status :absent` and `run-record-absence :runner-did-not-observe-topology-route`. No U88 implementation, independent review, route-conformance result, or acceptance is established. A readiness wake for the restored Zai author was observed in the actual runner stack; this is not task execution.

Cause: `full-loop-runner/config` defaults `:cohort? true`; `run-opportunity-core!` calls `full-loop-cohort/start-attempt!` before selection. The U88 sheet omits a cohort choice. The actual cohort ledger reads `{:cohort/id :wm-outer-loop-46-v1 :target 3 :remaining 0}`. The existing stopping rule therefore ends this opportunity before its task pin can be consumed. The series identity alone did not establish a new execution cohort.

Durable admission and binding are retained verbatim with source hashes. The visibility artifact remains `working/pending`: it is a last observation, not evidence that the worker is still active. Click status confirms the runner has stopped. No authoritative RUN4 terminal projection exists, so the controller must not advance or manufacture a success/failure classification.

Implementation remaining: bind the RUN4 series to an explicit preregistered execution cohort at the actual runner consumer, and test exhaustion/identity preflight before click admission. Preserve the old cohort's stopping rule and this consumed admission. A successor attempt must have explicit identity and must not silently retry or reset the old cohort. The previous disposable roundtrip stubbed task-core execution and consequently did not exercise this live cohort boundary.

No reset, retry, acceptance, or cohort-rule bypass was performed. No Claude or Codex16 worker was used.
