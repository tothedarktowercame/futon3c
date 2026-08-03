(ns futon3c.agents.pull-receipt-integration-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.registry :as registry]
            [futon3c.agents.zai-api :as zai]
            [futon3c.peripheral.memory-backend :as memory-backend]
            [futon3c.peripheral.pull-receipts :as pull]
            [futon3c.transport.http :as http]))

(use-fixtures :each
  (fn [test-fn]
    (registry/reset-registry!)
    (test-fn)))

(defn- memory-call
  [id args]
  {:id id
   :type "function"
   :function {:name "memory_search" :arguments (json/generate-string args)}})

(deftest tool-site-records-overlapping-pull-offers-by-round
  (let [store (atom {:entries {} :order []})
        seen-contexts (atom [])
        results (atom [{:ok true :result {:items [{:id "e-one"} {:id "e-two"}]}}
                       {:ok true :result {:items [{:id "e-two"} {:id "e-used"}]}}])
        base-ctx {:agent-id "zai-1" :cwd "/tmp"
                  :session-id-atom (atom "same-seat-session")
                  :session-id-fallback "same-seat-session"
                  :evidence-store store :dispatch-id "job-pull"
                  :turn-id "turn-pull" :profile :zai}]
    (with-redefs [memory-backend/memory-search
                  (fn [ctx _args]
                    (swap! seen-contexts conj ctx)
                    (let [result (first @results)]
                      (swap! results subvec 1)
                      result))]
      (let [first-result (#'zai/execute-tool
                          nil (assoc base-ctx :round 1)
                          (memory-call "tc-1" {:tags ["memory"]}))
            second-result (#'zai/execute-tool
                           nil (assoc base-ctx :round 4)
                           (memory-call "tc-2" {:author "codex-5"}))]
        ;; Receipt instrumentation cannot rewrite model-facing tool results.
        (is (true? (get-in first-result [:result :ok])))
        (is (true? (get-in second-result [:result :ok])))))
    (is (= ["e-one" "e-two" "e-used"]
           (pull/pull-surfaced-ids store "job-pull")))
    (is (= ["job-pull" "job-pull"] (mapv :dispatch-id @seen-contexts)))
    (is (= [1 4]
           (mapv #(get-in % [:evidence/body :round])
                 (pull/pull-offer-receipts store "job-pull"))))))

(deftest agency-dispatch-id-reaches-invoke-options
  (let [seen (atom nil)]
    (registry/register-agent!
     {:agent-id {:id/value "zai-pull" :id/type :continuity}
      :type :codex
      :invoke-fn (fn [_prompt _session-id opts]
                   (reset! seen opts)
                   {:result "done" :session-id "seat-session"})
      :capabilities [:explore]})
    (is (:ok (#'http/invoke-agent-with-session-recovery!
              "zai-pull" "work" nil "agency-job-42")))
    (is (= "agency-job-42" (:dispatch-id @seen)))))

(deftest turn-start-records-explicit-dispatch-binding
  (let [store (atom {:entries {} :order []})
        invoke (zai/make-invoke-fn
                {:agent-id "zai-binding" :api-key "test-key"
                 :initial-session-id "seat-session" :evidence-store store
                 :memory-mode :none :cwd "/tmp"})]
    (with-redefs [zai/chat!
                  (fn [& _]
                    {:choices [{:message {:role "assistant" :content "done"}}]})]
      (is (= "done" (:result (invoke "work" nil {:dispatch-id "agency-job-43"})))))
    (let [start (get-in @store [:entries (first (:order @store))])]
      (is (= "agency-job-43" (get-in start [:evidence/body :dispatch-id])))
      (is (str/starts-with? (get-in start [:evidence/body :turn-id])
                            "zai-turn-")))))
