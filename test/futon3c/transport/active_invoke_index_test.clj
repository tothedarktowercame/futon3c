(ns futon3c.transport.active-invoke-index-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.registry :as reg]
            [futon3c.transport.http :as http]))

(defn- job [id state agent]
  {:job-id id
   :state state
   :agent-id agent
   :created-at "2026-08-31T00:00:00Z"})

(deftest active-index-agrees-with-full-scan-and-rebuilds
  (let [ledger-atom (var-get #'http/!invoke-jobs-ledger)
        index-atom (var-get #'http/!active-invoke-job-index)
        before-ledger @ledger-atom
        before-index @index-atom
        history (into {}
                      (map (fn [n]
                             [(str "done-" n)
                              (job (str "done-" n) "done" "zai-1")]))
                      (range 500))
        ledger {:version 1
                :jobs (assoc history
                             "queued" (job "queued" "queued" "zai-1")
                             "running" (job "running" "running" "zai-1")
                             "delivered" (job "delivered" "delivered" "zai-2"))}]
    (try
      (reg/reset-registry!)
      (doseq [id ["zai-1" "zai-2"]]
        (reg/register-agent!
         {:agent-id {:id/value id :id/type :continuity}
          :type :zai
          :capabilities []
          :invoke-fn nil}))
      (reset! ledger-atom ledger)
      (reset! index-atom nil)
      (testing "restart reconstruction agrees with the authoritative scan"
        (is (:pass? (http/active-invoke-job-counts-consistency)))
        (is (= #{"queued" "running" "delivered"}
               (:job-ids @index-atom))))
      (testing "a lifecycle mutation refreshes the index"
        (with-redefs-fn {#'http/persist-invoke-jobs-ledger! identity}
          (fn []
            (#'http/update-invoke-jobs-ledger!
             #(assoc-in % [:jobs "running" :state] "done"))))
        (is (:pass? (http/active-invoke-job-counts-consistency)))
        (is (= #{"queued" "delivered"} (:job-ids @index-atom))))
      (finally
        (reset! ledger-atom before-ledger)
        (reset! index-atom before-index)))))
