(ns futon3c.wm.machinery-execution-cohort-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.wm.machinery-execution-cohort :as sut]
            [futon3c.wm.runner-service :as runner]))

(def valid-text
  (slurp sut/binding-path))

(defn refusal [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest exact-server-owned-binding
  (is (= :wm-contract-machinery-47-v1
         (:cohort-id (sut/execution-cohort))))
  (is (= "/home/joe/code/futon2/data/wm-full-loop-machinery-47"
         (:data-root (sut/execution-cohort))))
  (is (= (assoc {} :execution-cohort (sut/execution-cohort))
         (sut/apply-binding {})))
  (testing "an independently server-prepared RUN4 cohort is preserved"
    (is (= {:execution-cohort {:cohort-id :run4}}
           (sut/apply-binding {:execution-cohort {:cohort-id :run4}})))))

(deftest missing-and-mismatched-binding-refuse
  (binding [sut/*read-binding-text* #(throw (java.io.IOException. "missing"))]
    (is (= :binding-unavailable (refusal sut/execution-cohort))))
  (binding [sut/*read-binding-text* #(str valid-text " ")]
    (is (= :binding-sha256-mismatch (refusal sut/execution-cohort))))
  (binding [sut/*read-binding-text* (constantly valid-text)
            sut/binding-sha256 (digest/sha256 valid-text)]
    (is (= :wm-contract-machinery-47-v1
           (:cohort-id (sut/execution-cohort))))))

(deftest runner-configures-before-invocation-and-does-not-swallow-refusal
  (binding [runner/*resolve-var* (constantly identity)]
    (is (= :wm-contract-machinery-47-v1
           (get-in (#'runner/configured-runner-opts {})
                   [:execution-cohort :cohort-id]))))
  (binding [runner/*resolve-var* (constantly identity)
            sut/*read-binding-text* #(throw (java.io.IOException. "missing"))]
    (is (= :binding-unavailable
           (refusal #(#'runner/configured-runner-opts {}))))))
