# WM apparatus registration tranche two — 2026-09-19

Four explicit registrations requested by claude-12, author codex-2. Each namespace executes once in this packet. No retries, source changes, live machine clicks, or shared-JVM reloads.

WM-08 is a newly authorized execution after `f9cbde39` fixed the whole-repository HEAD stability refusal. Its prior refused execution remains recorded in `../wm-three-apparatus-registrations-2026-09-19/`.

Scopes follow futon2 `holes/labs/wm-contract/AUDIT-built-vs-pending-2026-09-19.md`:

| Subject | Namespace | Explicit code scope | Explicit test scope |
|---|---|---|---|
| WM-08-failure-classification | futon2.aif.interpretation-job-test | test/futon2/aif/interpretation_job_test.clj | same file |
| WM-04-observation-contract | futon2.aif.observation-checks-test | src/futon2/aif/observation_checks.clj | test/futon2/aif/observation_checks_test.clj |
| WM-07-delivery | futon2.aif.parameter-delivery-test | src/futon2/aif/parameter_delivery.clj | test/futon2/aif/parameter_delivery_test.clj |
| WM-09-predecessor-history | futon2.aif.receipt-construction-test | test/futon2/aif/receipt_construction_test.clj | same file |

For the two test-only audit scopes, the test file supplies both required nonempty manifests. The runner separately captures actual loaded dependencies. These scopes warrant the named controls, not every behavior of the apparatus.

Each spec uses repo-root `/home/joe/code/futon2` and command `clojure -M:test -n <namespace>`. Invocations from futon3c use the default durable backend:

```sh
clojure -M -m futon3c.test-registry.validation register holes/labs/wm-apparatus-tranche-two-2026-09-19/<subject>.edn
```

Adjacent files retain each spec, start/finish timestamps, stdout/stderr and CLI exit code. Subject subdirectories retain runner logs and loaded closure receipts. CLI exit and test-process exit are distinct observations.

## Results

### WM-08-failure-classification

CLI exit: 0.

```text
evidence-id test-registry-cb038eec76183b9b2f9d9d0af3475864fbb09c7635737a577f9a5333d071b5ee
warrant? true
results {:assertions 467, :duration-ms 362462, :errors 0, :exit 0, :failures 0, :tests 14}
bound WM-08-failure-classification -> test-registry-cb038eec76183b9b2f9d9d0af3475864fbb09c7635737a577f9a5333d071b5ee
```

### WM-04-observation-contract

CLI exit: 1.

```text
evidence-id test-registry-edb5e9c24f8d10e6bbbdbb9692dc8826921abb9bb92a1c301f1ea0a5aef09481
warrant? false
results {:assertions 29, :duration-ms 64746, :errors 0, :exit 1, :failures 3, :tests 9}
```

### WM-07-delivery

CLI exit: 0.

```text
evidence-id test-registry-16a4c8315a4f534b7ef7676eefd7fd4fd8cb380f8c60837a28619b3d4be338ca
warrant? true
results {:assertions 35, :duration-ms 1034, :errors 0, :exit 0, :failures 0, :tests 9}
bound WM-07-delivery -> test-registry-16a4c8315a4f534b7ef7676eefd7fd4fd8cb380f8c60837a28619b3d4be338ca
```

### WM-09-predecessor-history

CLI exit: 0.

```text
evidence-id test-registry-c075d3d723ba11975e1ee11ddf9f7dae00cd18433028c582e1884e5448e0f5cf
warrant? true
results {:assertions 315, :duration-ms 4707, :errors 0, :exit 0, :failures 0, :tests 26}
bound WM-09-predecessor-history -> test-registry-c075d3d723ba11975e1ee11ddf9f7dae00cd18433028c582e1884e5448e0f5cf
```

WM-04-observation-contract has no warrant or new binding. Its 9 tests / 29 assertions produced 3 failures and no errors: `c1-lean-warrant`, observation_checks_test.clj lines 85, 86, 90, each got nil for :observed where a boolean was expected. The complete failure log is retained; no diagnosis-driven repairs or retries were made. The other three runs produced bound warrants. Durable HTTP evidence responses and EDN payloads are adjacent.

## Fresh-JVM report

Command from futon3c: `clojure -M -m futon3c.test-registry.validation report`. CLI exit 0.

```text
AGG-single-kl-reduction stale test-registry-7c20e8379cab6701951f27cf052d332a624f5fea49f107ff20f63938102299fb
EV-uniform-run-record current test-registry-d66d73e7c7ee1e70a633728b9ea82bb812f886240dea01ad059d2d0a861280bf
LF-certificate-witness-checker current test-registry-c52af2af5c2a5164f2ecc2be85cf0296b72dfec31f4b3a3eeb269a1a51ee94f6
OPS-ordinary-run/click current test-registry-4eebcf94553ffb79d0b52353d133ec1ee682929b991084a0ecfe9193c37785c4
WIRE-f-on-tick stale test-registry-1ef986623fd9683af9ec1f6d3ca2840bffe5f1df3d621962405bdc06a610b5ce
WIRE-habit-accumulate unverifiable test-registry-78d77a1a608a93370f56c3fafec3b24b47bcfd2d48e7c4a46cc1fea2153d95d8
WM-01-carrier-contract current test-registry-73bb30ae5375d4f4c6865c01d91af85e985251f20feaf573587e45591ad2e02e
WM-01-numerical-operations stale test-registry-3f63a376e2f965c64156051c197a7a4fcd2245e191d514b730a35e818d00b4f9
WM-04-machinery current test-registry-0bcd621ffb9d79815de40b2d31f797e0ae7f5ff5afad5201125fa75cc0cbec0f
WM-07-delivery current test-registry-16a4c8315a4f534b7ef7676eefd7fd4fd8cb380f8c60837a28619b3d4be338ca
WM-08-failure-classification current test-registry-cb038eec76183b9b2f9d9d0af3475864fbb09c7635737a577f9a5333d071b5ee
WM-09-predecessor-history current test-registry-c075d3d723ba11975e1ee11ddf9f7dae00cd18433028c582e1884e5448e0f5cf
test-registry/scoped-stability current test-registry-04e41efff20272519b4a59d1168a372c4800f5728362644195aa4dfc86b560fc
test-registry/validation-cli unverifiable test-registry-13f52fc77768ff4453d68593598d04d99d4ac98fcf252ab60af1bdf6283192a7
SUMMARY {:current 9, :stale 3, :unverifiable 2}
```

An unbound failed subject is absent because this report enumerates subject bindings. No binding was invented for a non-warrant run. No Clojure source was modified, so no additional gate or test invocations were performed.
