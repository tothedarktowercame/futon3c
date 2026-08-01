(ns futon3c.memory-use-kind-recording-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.dispatch-with-recall :as dispatch]))

(deftest offered-receipt-records-reviewed-kind-per-memory
  (let [entry
        (dispatch/offered-evidence
         {:problem "a95A04" :from "ground-control"}
         {:status :ok
          :trace-id "recall-kind-test"
          :query {:query "a95A04 interval integral"}
          :memories [{:memory/id "e-regulative"
                      :memory-use/kind :regulative}
                     {:memory/id "e-unadjudicated"}]}
         "job-kind-test" "session-kind-test")
        reasons (get-in entry [:body :memory-use
                               :memory-use/inclusion-reasons])]
    (is (= {:memory-id "e-regulative"
            :reason
            "reviewed attachment surfaced by terrain-conditioned dispatch recall"
            :memory-use/kind :regulative}
           (first reasons)))
    (is (= {:memory-id "e-unadjudicated"
            :reason
            "reviewed attachment surfaced by terrain-conditioned dispatch recall"}
           (second reasons)))
    (is (not (contains? (second reasons) :memory-use/kind)))))
