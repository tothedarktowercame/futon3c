# Three WM apparatus registrations — 2026-09-19

Requested by claude-12. Author: codex-2. No source changes, shared-JVM reloads, or live machine clicks. Each named namespace is executed exactly once by the validation registry runner; failures or refusals are retained without retries.

## Scopes and commands

Evidence source: futon2/holes/labs/wm-contract/AUDIT-built-vs-pending-2026-09-19.md, WM-04-machinery, WM-08-failure-classification, and WM-01-carrier-contract rows.

- WM-04-machinery: code `src/futon2/aif/observation_admission.clj`; test `test/futon2/aif/observation_admission_test.clj`; command `clojure -M:test -n futon2.aif.observation-admission-test`.
- WM-08-failure-classification: the requested `test/futon2/aif/interpretation_job_test.clj` scope, entered in both code-paths and test-paths because both manifests must be nonempty; command `clojure -M:test -n futon2.aif.interpretation-job-test`.
- WM-01-carrier-contract: the requested `test/futon2/aif/wm01_conversion_agreement_test.clj` scope, entered in both manifests for the same reason; command `clojure -M:test -n futon2.aif.wm01-conversion-agreement-test`.

The runner also records the actual loaded dependency closure. The test-only explicit scopes do not assert complete implementation coverage beyond what these suites exercise.

Each invocation runs from `/home/joe/code/futon3c`:

```sh
clojure -M -m futon3c.test-registry.validation register holes/labs/wm-three-apparatus-registrations-2026-09-19/<subject-id>.edn
```

The specs use repo-root `/home/joe/code/futon2` and the CLI default durable HTTP backend. Each spec has matching `.started`, `.finished`, `.stdout`, `.stderr`, `.exit` receipts; runner logs and loaded closure receipts are in the corresponding subject directory. A CLI exit code is separate from the test process exit, which appears in its results.

## Results

### WM-04-machinery

CLI exit: 0.

```text
evidence-id test-registry-0bcd621ffb9d79815de40b2d31f797e0ae7f5ff5afad5201125fa75cc0cbec0f
warrant? true
results {:assertions 34, :duration-ms 677, :errors 0, :exit 0, :failures 0, :tests 5}
bound WM-04-machinery -> test-registry-0bcd621ffb9d79815de40b2d31f797e0ae7f5ff5afad5201125fa75cc0cbec0f
```

### WM-08-failure-classification

CLI exit: 1.

```text
evidence-id test-registry-5bcfa17c6d543ca4a8a4d7682405a21755aa70a52e043d1872a4ee1b2cd050b9
warrant? false
results {:assertions 467, :duration-ms 374874, :errors 0, :exit 0, :failures 0, :tests 14}
```

### WM-01-carrier-contract

CLI exit: 0.

```text
evidence-id test-registry-73bb30ae5375d4f4c6865c01d91af85e985251f20feaf573587e45591ad2e02e
warrant? true
results {:assertions 31, :duration-ms 1407, :errors 0, :exit 0, :failures 0, :tests 3}
bound WM-01-carrier-contract -> test-registry-73bb30ae5375d4f4c6865c01d91af85e985251f20feaf573587e45591ad2e02e
```

WM-08 ran once for 374,874 ms (about 6.25 minutes), with 14 tests / 467 assertions passing. Its durable run record has `:postcheck :reason :inputs-changed-during-run`, so it produced **no warrant and no binding**. The diagnostic isolates the difference to `:git-head`: `5169171b5fcbe5df5a6327e2870ac64eb36b9f47` → `be60e4fb9b3cd2afc8d941606634d3c4bf3f9893`. The scoped code/test manifests and environment fingerprint were unchanged. This is the recorded registry refusal under concurrent commits; it was neither repaired nor retried. Full HTTP evidence and EDN payload are retained.

## Fresh-JVM report

Command: `clojure -M -m futon3c.test-registry.validation report` from futon3c; exit 0.

```text
AGG-single-kl-reduction stale test-registry-7c20e8379cab6701951f27cf052d332a624f5fea49f107ff20f63938102299fb
EV-uniform-run-record stale test-registry-617bb5ce91010f286ad47a757a0a69ddcf605c2809620406114685d439ea1791
LF-certificate-witness-checker current test-registry-c52af2af5c2a5164f2ecc2be85cf0296b72dfec31f4b3a3eeb269a1a51ee94f6
OPS-ordinary-run/click current test-registry-4eebcf94553ffb79d0b52353d133ec1ee682929b991084a0ecfe9193c37785c4
WIRE-f-on-tick stale test-registry-1ef986623fd9683af9ec1f6d3ca2840bffe5f1df3d621962405bdc06a610b5ce
WIRE-habit-accumulate unverifiable test-registry-78d77a1a608a93370f56c3fafec3b24b47bcfd2d48e7c4a46cc1fea2153d95d8
WM-01-carrier-contract current test-registry-73bb30ae5375d4f4c6865c01d91af85e985251f20feaf573587e45591ad2e02e
WM-01-numerical-operations stale test-registry-3f63a376e2f965c64156051c197a7a4fcd2245e191d514b730a35e818d00b4f9
WM-04-machinery current test-registry-0bcd621ffb9d79815de40b2d31f797e0ae7f5ff5afad5201125fa75cc0cbec0f
test-registry/validation-cli current test-registry-13f52fc77768ff4453d68593598d04d99d4ac98fcf252ab60af1bdf6283192a7
SUMMARY {:current 5, :stale 4, :unverifiable 1}
```

WM-08 is absent from this report because reporting enumerates bound subjects; no fabricated binding was added for its non-warrant run. Both resulting warrants are bound and current in this fresh JVM. No Clojure source was edited; no additional gate/test runs were performed.
