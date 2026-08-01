(ns futon3c.analysis.memory-arm-e1-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.analysis.memory-arm-e1 :as e1]))

(defn- offered [job-id status ids vias]
  {:evidence/body
   {:phase :offered
    :job-id job-id
    :recall-status status
    :memory-use {:memory-use/surfaced-ids ids
                 :memory-use/surfacing-via vias}}})

(deftest trace-partitions-dispatches
  (let [document
        {:entries
         [(offered "attributed" :ok ["m1"]
                   [{:memory-id "m1" :via :pattern}])
          (offered "empty" :recall-empty [] [])
          (offered "unusable" :ok ["m2"] [])]}
        trace (e1/extract-trace document)]
    (is (= ["empty"] (:empty-dispatches trace)))
    (is (= [{:dispatch-id "attributed"
             :memory-id "m1"
             :via-pattern true}]
           (:surfacings trace)))
    (is (= ["unusable"] (:unusable trace)))))

(deftest trace-rejects-an-unclassified-dispatch
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"not every offered receipt"
       (e1/extract-trace
        {:entries [(offered "bad" :ok [] [])]}))))

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

