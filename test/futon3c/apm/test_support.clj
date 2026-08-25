(ns futon3c.apm.test-support
  "Helpers that keep the APM unit suites fast.

  The countdown manifest in use is version 2, and
  `countdown-manifest/validate` qualifies every unit of a v2 manifest by
  provisioning a revision-addressed worktree and running `lake env lean` on
  the pinned problem file — ten Lean compiles, ~25 s, plus filesystem side
  effects. Measured 2026-08-24: three tests that reach `registration-body`
  accounted for 84.7 s of an 84.9 s run of 112 tests.

  Unit tests that need a registration body or a dry-run launch use
  `with-stubbed-qualification`; the real Lean path is covered once, under
  `^:slow`, by `countdown-control-test/v2-manifest-qualifies-under-real-lean`
  (`scripts/apm-test-slow.sh`)."
  (:require [futon3c.apm.countdown-manifest :as countdown-manifest]))

(defn stub-qualify-unit
  "Stand-in for `countdown-manifest/qualify-unit`: report the unit's pinned
  eligibility baseline as the observation, without a worktree or Lean."
  [unit]
  (let [baseline (:eligibility/baseline unit)]
    {:valid? true
     :qualification-checkout ::stubbed
     :qualification-revision (get-in unit [:problem :revision])
     :observation baseline
     :expected baseline}))

(defmacro with-stubbed-qualification
  "Run BODY with unit qualification stubbed. Git pin validation still runs."
  [& body]
  `(with-redefs [countdown-manifest/qualify-unit stub-qualify-unit]
     ~@body))
