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
         [(offered "2026-01-01T00:00:00Z" "attributed" :ok ["m1"]
                   [{:memory-id "m1" :via :pattern}])
          (offered "2026-01-01T00:00:01Z" "empty" :recall-empty [] [])
          (offered "2026-01-01T00:00:02Z" "unattributed" :ok ["m2"] [])
          (assoc-in (offered "2026-01-01T00:00:03Z" "unusable" :recall-empty [] [])
                    [:evidence/body :recall-query] nil)]}
        trace (e1/extract-trace document)]
    (is (= ["empty"] (:empty-dispatches trace)))
    (is (= [{:dispatch-id "attributed"
             :memory-id "m1"
             :via-pattern true}]
           (:surfacings trace)))
    (is (= 0 (:earliest-attributed-index trace)))
    (is (= 4 (:total-dispatches trace)))
    (is (= 3 (:corpus-span-seconds trace)))
    (is (= 3 (:attributed-span-seconds trace)))
    (is (= ["unattributed"] (:unattributed-non-empty trace)))
    (is (= ["unusable"] (:unusable trace)))))

(deftest trace-rejects-an-unclassified-dispatch
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"not every offered receipt"
       (e1/extract-trace
        {:entries [(offered "2026-01-01T00:00:00Z" "bad" :ok [] [])]}))))

(deftest observables-are-non-vacuous
  (testing "an unattributed non-empty dispatch fails attribution completeness"
    (is (false? (e1/attribution-complete?
                 {:unattributed-non-empty ["blind-spot"]}))))
  (testing "a 24,800 second window fails to cover half a 379,000 second corpus"
    (is (false? (e1/coverage-not-tail?
                 {:attributed-span-seconds 24800
                  :corpus-span-seconds 379000})))))

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
