# Validation CLI durable backend and refusal receipts — 2026-09-19

Implementation: `224da745`. This packet preserves the execution receipts and observations; it does not change DAGs, warrants in place, or a running JVM.

## Backend diagnosis

`src/futon3c/evidence/store.clj:45` has a private resolver that wraps atoms; it does **not** resolve an HTTP backend from configuration. The previous CLI handed it the fresh JVM's default atom. The durable standalone convention is `src/futon3c/test_registry.clj:944`, which constructs an HTTP backend. Validation now follows that convention (explicit agency URL, then FUTON3C_AGENCY_URL/FUTON3C_AGENCY_BASE, then localhost:7070). Explicit isolated backends remain supported; passing the process-default `store/!store` now resolves the HTTP backend.

`before.json` records direct GET /api/alpha/evidence/<id> observations. Four current lane chains were already present: WM `3f63a376`, WIRE-f `1ef98662`, AGG `7c20e837`, habit `78d77a1a`. Earlier EV chain `490a2cc6` was also present. These were recovered in the sense of resolving existing durable evidence, **not re-minted**. EV chains `c91be0ba` and `6c81db28` returned 404; their recorded success in an originating JVM does not establish durable persistence. Their historical executions cannot be recovered from this backend.

## Registered executions

All commands ran from `/home/joe/code/futon3c` using `clojure -M -m futon3c.test-registry.validation register <spec-path>`. Specs, stdout/stderr, exit codes, runner logs, and closure receipts are adjacent.

| Spec | Purpose | New durable warrant | Result |
|---|---|---|---|
| register.edn | Warrant this repair, subject test-registry/validation-cli | test-registry-13f52fc77768ff4453d68593598d04d99d4ac98fcf252ab60af1bdf6283192a7 | 7 tests, 55 assertions, 0 failures/errors |
| remint-c91be0ba.edn | Replacement for missing c91be0ba chain | test-registry-8e466ec7c4d104bcfc4bc9870eb2c9aa2a596e9c8b6ebdad10aee5b9e3c6a353 | 2 tests, 7 assertions, 0 failures/errors |
| remint-6c81db28.edn | Replacement for missing 6c81db28 chain | test-registry-afad45cf94ff37ce8916d226d6ac3be2ee11afd70f3d39f29eb2d4ace2af6c1e | 2 tests, 7 assertions, 0 failures/errors |

The replacements are **new executions against current committed checker code**, not reconstructions of old chains. Both preserve the original checker command/code/test scope from `/tmp/register-checker-fix.edn`. The second is the latest EV binding. Existing append-only binding history was retained, including concurrent bindings by other workers. `after.json` confirms HTTP 200 for all three new evidence IDs from outside their minting JVMs.

The repair suite ran exactly once, through the fixed CLI, after committing its code scope. It permanently covers fabricated IDs, backend selection, durable refused bind/enqueue/close, unchanged reader results, closure freshness, and original exception preservation when a refusal trace cannot be appended. `clj-kondo.txt` records zero errors/warnings and `check-parens.txt` records OK for all three modified Clojure files. No source changed after the registered run.

## Fresh-JVM report

Command: `clojure -M -m futon3c.test-registry.validation report`.

```
AGG-single-kl-reduction stale test-registry-7c20e8379cab6701951f27cf052d332a624f5fea49f107ff20f63938102299fb
EV-uniform-run-record current test-registry-afad45cf94ff37ce8916d226d6ac3be2ee11afd70f3d39f29eb2d4ace2af6c1e
LF-certificate-witness-checker current test-registry-c52af2af5c2a5164f2ecc2be85cf0296b72dfec31f4b3a3eeb269a1a51ee94f6
OPS-ordinary-run/click current test-registry-4eebcf94553ffb79d0b52353d133ec1ee682929b991084a0ecfe9193c37785c4
WIRE-f-on-tick stale test-registry-1ef986623fd9683af9ec1f6d3ca2840bffe5f1df3d621962405bdc06a610b5ce
WIRE-habit-accumulate unverifiable test-registry-78d77a1a608a93370f56c3fafec3b24b47bcfd2d48e7c4a46cc1fea2153d95d8
WM-01-numerical-operations stale test-registry-3f63a376e2f965c64156051c197a7a4fcd2245e191d514b730a35e818d00b4f9
test-registry/validation-cli current test-registry-13f52fc77768ff4453d68593598d04d99d4ac98fcf252ab60af1bdf6283192a7
SUMMARY {:current 4, :stale 3, :unverifiable 1}
```

No subject has `no-warrant`. **The requested all-current-or-stale acceptance condition is not fully met:** WIRE-habit-accumulate resolves its warrant but registry checking returns `:environment-mismatch`, with `:changed-files ("src/futon2/aif/policy.clj")` and `:next-action :rerun-the-declared-namespace`. See `conformance-diagnostic.edn`. This is a real loaded-dependency mismatch during concurrent WM work, not loss of evidence. The existing verifier distinction was preserved; no unrelated lane was re-warranted to hide it. AGG moved from an earlier environment mismatch to stale as concurrent scoped changes landed. A concurrent EV warrant `5e72c138` had an LC_ALL mismatch before the requested replacements. The eight-row report includes subjects added by other workers while this packet ran.

## Adversarial and durable refusal demonstrations

`refusal-demo.clj` uses isolated, retained on-disk ledgers and the real HTTP backend. The bind refusal and close refusal rethrow their typed exceptions, append full attempted arguments, reason, details and timestamp to their respective ledgers, and leave one valid binding and one open incident. No synthetic incident was appended to the live queue.

```
:refused-bind {:record/type :test-registry.validation/refusal, :reason :binding-invalid, :details {:subject-id "acceptance/refusal-demo", :warrant-id "", :actor "codex-2"}}
:refused-close {:record/type :test-registry.validation/refusal, :reason :warrant-not-fresh, :details {:incident-at "2026-09-19T16:18:01.230830508Z", :warrant-finished-at "2026-09-19T16:13:09.010754866Z"}}
:subjects-count 1 :open-incidents 1
```

Inspect `demo-subjects.ednlog` and `demo-queue.ednlog` for the durable `:entry/type :refusal` entries. The permanent tests also cover :fresh-warrant-not-bound and :incident-invalid and append failure without masking the original exception.

A separate fresh JVM ran `clojure -M -m futon3c.test-registry.validation report holes/labs/test-registry-validation-cli-2026-09-19/adversarial-report.edn`, using an isolated index but the same real durable backend:

```
acceptance/fabricated no-warrant test-registry-fabricated-validation-cli-repair
test-registry/validation-cli current test-registry-13f52fc77768ff4453d68593598d04d99d4ac98fcf252ab60af1bdf6283192a7
SUMMARY {:current 1, :no-warrant 1}
```

Thus fixing backend selection did not turn an invented ID into a warrant. Neither demonstration weakens the real ledger or reloads :6768.
