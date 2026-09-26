# Declared-scope stability repair — 2026-09-19

Source commit: `f9cbde39`.

The mid-run comparison in `src/futon3c/test_registry.clj` now removes only `:git-head` from each capture before comparing. Code/test hashes, file manifests and the environment comparison remain enforced. Existing load-closure and committed-scope checks are unchanged. The initial `:git-head` remains in the run payload; `:execution/post-code` retains the complete post-run capture, including HEAD, for successful and refused runs.

The new `futon3c.test-registry-stability-test` uses fixture captures and the real registration/record-writing path. It constructs an unrelated mid-run commit (warrant minted, distinct HEADs retained, subsequent check current), declared-file drift (typed refusal and changed file evidenced), and environment drift (typed refusal). Only these three tests run; the existing registry test namespace supplies fixture helpers.

Gates: clj-kondo zero errors/warnings; check-parens OK. See adjacent gate outputs.

The suite executed exactly once through the fixed validation CLI, after committing source scope:

```sh
clojure -M -m futon3c.test-registry.validation register holes/labs/registry-scoped-stability-2026-09-19/register.edn
```

evidence-id test-registry-04e41efff20272519b4a59d1168a372c4800f5728362644195aa4dfc86b560fc
warrant? true
results {:assertions 13, :duration-ms 1913, :errors 0, :exit 0, :failures 0, :tests 3}
bound test-registry/scoped-stability -> test-registry-04e41efff20272519b4a59d1168a372c4800f5728362644195aa4dfc86b560fc

The CLI exit code, stdout/stderr, execution log and closure receipt are retained. The subject is `test-registry/scoped-stability`. No WM-08 registration or shared JVM reload was performed.

`report.stdout` is a separate fresh-JVM `clojure -M -m futon3c.test-registry.validation report` invocation.
