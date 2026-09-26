# Authoritative offline verification

Executed `execute-verifier.clj` against separately frozen `verifier-input.execution.edn` (SHA256 `8dcc92bec835acabeb9b41b87698edeef5f3ac4c392c0db9736e1c51cf2196e6`). The original reviewed input remains unchanged. The execution input explicitly advances only the source HEAD to `eed108f51c5e3419f9f58a6b8ba234eb0d842c4f`; ancestry and all eleven source bytes passed the authoritative verifier again immediately before publication.

Artifact SHA256: `99b0fed181c7142a4d498abd08e9bb7dcf6247ec4decf3f41d6434a1724c31c6`. Schema: `:wm/historical-repair-verification-v1`. Repair: successor-v2 selection failure only. Author Codex-10, reviewer Codex-12, executed review job `invoke-1789141837364-20188-b159df86`. Outcome remains awaiting-validation, repair-resolved false. No live repair-store admission, cohort capacity, queue tick or successor occurred.

The wrapper captures and verifies input and review response bytes before invoking the existing verifier. clj-kondo: zero errors/warnings; check-parens passed. Execution log: `/tmp/run4-successor-verifier.out`.

This artifact requires independent review. The exact-HEAD verifier ran before this artifact commit; reviewing its retained evidence is distinct from rerunning it against a changed HEAD. Do not weaken exact-HEAD validation or silently edit retained inputs.

Later initialization diagnosis from job `bc1ca3e7` identifies a qualified-ID collision associated with disposable service testing. It does not establish whether the originating executions were concurrent or sequential, and it does not discharge the finding. Required follow-on investigation must cover both test-store isolation and durable execution authority; preserving live finding evidence is mandatory.
