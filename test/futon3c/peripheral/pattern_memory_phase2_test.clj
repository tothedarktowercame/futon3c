(ns futon3c.peripheral.pattern-memory-phase2-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [futon3b.query.relations :as relations]
            [futon3c.agents.zai-api :as zai]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.store :as estore]
            [futon3c.peripheral.real-backend :as rb]
            [futon3c.peripheral.tools :as tools])
  (:import [java.io File]
           [java.util UUID]))

(defn- temp-dir! []
  (let [d (File. "/tmp" (str "futon3c-memory-phase2-"
                             (UUID/randomUUID)))]
    (.mkdirs d)
    d))

(defn- cleanup! [^File dir]
  (doseq [file (reverse (file-seq dir))]
    (.delete ^File file)))

(defn- phase2-recall-fixture
  [_ctx endpoint {:keys [include-bodies?]}]
  (let [base [{:memory/id "e-earlier-session"
               :memory/hook "Normalize the denominator before field_simp."
               :memory/state :current
               :memory/witness-status :independently-witnessed
               :memory/volatile? false
               :memory/provenance {:author "zai-3"
                                   :session-id "earlier-session"}}
              {:memory/id "e-counterexample"
               :memory/hook "This rewrite is unsafe under a zero denominator."
               :memory/state :challenged
               :memory/witness-status :challenged
               :memory/volatile? false}
              {:memory/id "e-volatile"
               :memory/hook "The tactic was unavailable in one toolchain."
               :memory/state :current
               :memory/witness-status :self-asserted
               :memory/volatile? true}]
        memories (if include-bodies?
                   (mapv #(assoc % :memory/body
                                 {:body (str "full body for " (:memory/id %))})
                         base)
                   base)]
    {:ok true
     :endpoint endpoint
     :domain :mathematics
     :elapsed-ms 1.25
     :memories memories
     :audit {:edge-count 3 :returned-count 3
             :proposed-excluded 1 :missing-entry 0 :missing-body 0}}))

(deftest search-select-use-and-independent-outcome-vertical-slice
  (testing "earlier-session memories flow through search, selection, receipt, and separate outcome"
    (let [dir (temp-dir!)]
      (try
      (with-redefs [relations/proof-path-dir (fn [] (.getPath dir))]
        (let [store (atom {:entries {} :order []})
              config {:cwd "/home/joe/code/futon3c"
                      :notions-index-path
                      "../futon3a/resources/notions/patterns-index.tsv"
                      :evidence-store store
                      :memory-domain :mathematics
                      :memory-recall-fn phase2-recall-fixture
                      :agent-id "zai-phase2"
                      :session-id-fn (constantly "phase2-session")}
              backend (rb/make-real-backend config)
              search-result
              (tools/execute-tool backend :psr-search
                                  ["mandatory psr" {:top-k 5}])
              candidate
              (first (filter #(= "coordination/mandatory-psr"
                                 (:pattern-id %))
                             (get-in search-result
                                     [:result :candidates])))
              select-result
              (tools/execute-tool backend :psr-select
                                  ["coordination/mandatory-psr"
                                   {:task-id "phase2-fresh-task"
                                    :rationale "fresh theorem"}])
              outcome-receipt
              (boundary/append!
               store
               {:evidence/subject
                {:ref/type :task :ref/id "proof/phase2-fresh-task"}
                :evidence/type :pattern-outcome
                :evidence/claim-type :observation
                :evidence/author "lean/external-checker"
                :evidence/session-id "checker-session"
                :evidence/body
                {:memory-outcome/witness-status :independently-witnessed
                 :checker "lake env lean"
                 :exit 0}
                :evidence/tags [:proof :external-check]})
              outcome-id (:evidence/id outcome-receipt)
              pur-result
              (tools/execute-tool
               backend :pur-update
               ["coordination/mandatory-psr"
                {:outcome :success
                 :prediction-error
                 "Natural semantic query missed the intended pattern."
                 :memory-ids ["e-earlier-session"]
                 :memory-rejections
                 [{:memory-id "e-counterexample"
                   :reason "hypotheses exclude the counterexample"}]
                 :outcome-id outcome-id}])
              memory-use (get-in pur-result [:result :memory-use])
              receipt (:receipt memory-use)
              persisted-receipt
              (estore/get-entry* store (:evidence-id memory-use))]
          (is (true? (:ok search-result)))
          (is (true? (:ok select-result)) (:error select-result))
          (is (true? (:ok outcome-receipt)) (:error/message outcome-receipt))
          (is (true? (:ok pur-result)) (:error pur-result))
          (is (= ["e-earlier-session" "e-counterexample" "e-volatile"]
                 (mapv :memory-id (:memory-hooks candidate))))
          (is (= :challenging
                 (get-in candidate [:memory-hooks 1 :role])))
          (is (true? (get-in candidate [:memory-hooks 2 :volatile?])))
          (is (= "earlier-session"
                 (get-in select-result
                         [:result :attached-memories 0
                          :memory/provenance :session-id])))
          (is (map? (get-in select-result
                            [:result :attached-memories 0 :memory/body])))
          (is (= ["e-earlier-session"] (:memory-use/used-ids receipt)))
          (is (= {:description
                  "Natural semantic query missed the intended pattern."}
                 (get-in pur-result
                         [:result :pur :pur/prediction-error])))
          (is (= ["e-counterexample"]
                 (:memory-use/rejected-ids receipt)))
          (is (= [{:memory-id "e-counterexample"
                   :reason "hypotheses exclude the counterexample"}]
                 (:memory-use/rejection-reasons receipt)))
          (is (= ["e-volatile"] (:memory-use/unused-ids receipt)))
          (is (= outcome-id (:memory-use/outcome-id receipt)))
          (is (= :outcome-attached (:memory-use/status receipt)))
          (is (= :memory-use
                 (get-in persisted-receipt [:evidence/body :event])))
          (is (= outcome-id
                 (get-in persisted-receipt
                         [:evidence/body :memory-use
                          :memory-use/outcome-id])))))
        (finally
          (cleanup! dir))))))

(deftest recall-ablation-and-failures-remain-explicit
  (let [no-recall
        (rb/make-real-backend
         {:cwd "/home/joe/code/futon3c"})
        unavailable
        (rb/make-real-backend
         {:cwd "/home/joe/code/futon3c"
          :memory-domain :mathematics
          :memory-recall-fn
          (fn [_ endpoint _]
            {:ok false :endpoint endpoint :memories []
             :error {:error/code :substrate-read-failed}})
          :agent-id "zai-phase2"
          :session-id-fn (constantly "phase2-session")})
        ablated-select
        (tools/execute-tool no-recall :psr-select
                            ["coordination/mandatory-psr"])
        failed-select
        (tools/execute-tool unavailable :psr-select
                            ["coordination/mandatory-psr"])
        stale-select
        (tools/execute-tool unavailable :psr-select
                            ["pattern/does-not-exist"])]
    (is (= :recall-unavailable
           (get-in ablated-select [:result :memory-hole :kind])))
    (is (= :memory-domain-not-configured
           (get-in ablated-select
                   [:result :memory-hole :error :error/code])))
    (is (= :substrate-read-failed
           (get-in failed-select
                   [:result :memory-hole :error :error/code])))
    (is (false? (:ok stale-select)))))

(deftest zai-pur-dispatch-preserves-memory-citations-and-outcome-reference
  (let [backend
        (tools/make-mock-backend
         {:pur-update (fn [_ _] {:ok true})})
        _ (#'zai/execute-tool
           backend
           {:agent-id "zai-phase2"}
           {:id "tc-pur"
            :function
            {:name "pur_update"
             :arguments
             (json/generate-string
              {:pattern_id "coordination/mandatory-psr"
               :outcome "success"
               :memory_ids ["e-earlier-session"]
               :memory_rejections
               [{:memory_id "e-counterexample" :reason "irrelevant"}]
               :outcome_id "e-external-outcome"})}})
        [{:keys [tool args]}] (tools/recorded-calls backend)]
    (is (= :pur-update tool))
    (is (= ["e-earlier-session"] (get-in args [1 :memory-ids])))
    (is (= "e-counterexample"
           (get-in args [1 :memory-rejections 0 :memory_id])))
    (is (= "e-external-outcome" (get-in args [1 :outcome-id])))))
