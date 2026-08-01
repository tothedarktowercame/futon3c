(ns futon3c.analysis.memory-arm-e1-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.analysis.memory-arm-e1 :as e1]))

(defn- offered [at job-id status ids vias]
  {:evidence/at at
   :evidence/body
   {:phase :offered
    :job-id job-id
    :recall-query (str "query-" job-id)
    :recall-status status
    :memory-use {:memory-use/surfaced-ids ids
                 :memory-use/surfacing-via vias}}})

(deftest trace-partitions-dispatches
  (let [document
        {:entries
         [(offered "attributed" "attributed" :ok ["m1"]
                   [{:memory-id "m1" :via :pattern}])
          (offered "empty" "empty" :recall-empty [] [])
          (offered "unattributed" "unattributed" :ok ["m2"] [])
          (assoc-in (offered "unusable" "unusable" :recall-empty [] [])
                    [:evidence/body :recall-query] nil)]}
        trace (e1/extract-trace document)]
    (is (= ["empty"] (:empty-dispatches trace)))
    (is (= [{:dispatch-id "attributed"
             :memory-id "m1"
             :via-pattern true}]
           (:surfacings trace)))
    (is (= 0 (:earliest-attributed-index trace)))
    (is (= 4 (:total-dispatches trace)))
    (is (= ["unattributed"] (:unattributed-non-empty trace)))
    (is (= ["unusable"] (:unusable trace)))))

(deftest trace-rejects-an-unclassified-dispatch
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"not every offered receipt"
       (e1/extract-trace
        {:entries [(offered "bad" "bad" :ok [] [])]}))))

(deftest observables-are-non-vacuous
  (testing "an unattributed non-empty dispatch fails attribution completeness"
    (is (false? (e1/attribution-complete?
                 {:unattributed-non-empty ["blind-spot"]}))))
  (testing "a first attribution at index 99 of 129 fails the registered coverage check"
    (is (false? (e1/coverage-not-tail?
                 {:earliest-attributed-index 99
                  :total-dispatches 129})))))

(deftest registered-classification-boundaries
  (testing "fewer than twenty surfacings is indeterminate"
    (is (= :indeterminate
           (e1/classify {:surfacings (repeat 19 {:via-pattern true})}))))
  (testing "zero pattern surfacings is silent"
    (is (= :pattern-arm-silent
           (e1/classify {:surfacings (repeat 20 {:via-pattern false})}))))
  (testing "exactly one quarter is substantial"
    (is (= :pattern-arm-substantial
           (e1/classify {:surfacings (concat (repeat 5 {:via-pattern true})
                                             (repeat 15 {:via-pattern false}))}))))
  (testing "a nonzero share below one quarter is marginal"
    (is (= :pattern-arm-marginal
           (e1/classify {:surfacings (concat [{:via-pattern true}]
                                             (repeat 19 {:via-pattern false}))})))))
