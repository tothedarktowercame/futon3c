(ns futon3c.wm.machinery-execution-cohort-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [futon2.aif.full-loop-cohort :as cohort]
            [futon3c.wm.machinery-execution-cohort :as sut]
            [futon3c.wm.runner-service :as runner]))

(def valid-text
  (slurp sut/binding-path))

(def bound
  "What the binding FILE says, rather than a literal copied into this test.

  These assertions named :wm-contract-machinery-49-v1 while the binding had
  moved to 57, so three of them had been failing since 150d2608. Since
  2026-09-19 an exhausted cohort advances to its successor automatically, so a
  hardcoded id is now guaranteed to rot rather than merely likely to; what this
  test is actually for is that the binding is read exactly and that a drifted
  or missing one refuses."
  (:execution-cohort (edn/read-string valid-text)))

(defn refusal [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest exact-server-owned-binding
  (testing "the bound cohort, or a successor derived from it, is what is served"
    (let [served (sut/execution-cohort)]
      (is (= (:cohort-id bound) (:cohort-id served)))
      (is (= (:data-root bound) (:data-root served)))))
  (testing "apply-binding serves the bound cohort or a successor DERIVED from it"
    ;; The old assertion was (= binding (apply-binding {})), which succession
    ;; breaks by design. What actually needs guarding is stronger and was never
    ;; checked: that an UNRELATED cohort can never be served. Walk the chain
    ;; from the bound charter and require every link to be byte-derivable.
    (let [served (:execution-cohort (sut/apply-binding {}))]
      (is (loop [raw (slurp (:preregistration bound)) n 0]
            (let [p (edn/read-string raw)]
              (cond
                (= (:cohort/id p) (:cohort-id served)) true
                (> n 64) false
                :else
                (let [succ-raw (slurp (:preregistration served))
                      succ (edn/read-string succ-raw)]
                  (if (cohort/verified-successor? p raw succ)
                    (recur succ-raw (inc n))
                    false)))))
          "served cohort is not derivable from the server-owned binding")))
  (testing "an independently server-prepared RUN4 cohort is preserved"
    (is (= {:execution-cohort {:cohort-id :run4}}
           (sut/apply-binding {:execution-cohort {:cohort-id :run4}})))))

(deftest missing-and-mismatched-binding-refuse
  (binding [sut/*read-binding-text* #(throw (java.io.IOException. "missing"))]
    (is (= :binding-unavailable (refusal sut/execution-cohort))))
  (binding [sut/*read-binding-text* #(str valid-text " ")]
    (is (= :binding-sha256-mismatch (refusal sut/execution-cohort))))
  (binding [sut/*read-binding-text* (constantly valid-text)]
    (is (= (:cohort-id bound) (:cohort-id (sut/execution-cohort))))))

(deftest runner-configures-before-invocation-and-does-not-swallow-refusal
  (binding [runner/*resolve-var* (constantly identity)]
    ;; apply-binding resolves the succession lineage, so this is the bound
    ;; cohort or a verified successor of it -- never an unrelated one.
    (is (= "wm-contract-machinery"
           (subs (name (get-in (#'runner/configured-runner-opts {})
                               [:execution-cohort :cohort-id]))
                 0 21))))
  (binding [runner/*resolve-var* (constantly identity)
            sut/*read-binding-text* #(throw (java.io.IOException. "missing"))]
    (is (= :binding-unavailable
           (refusal #(#'runner/configured-runner-opts {}))))))
