(ns futon3c.agency.logic-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.logic :as logic]))

(def ^:private clean-registry
  "A well-formed registry snapshot: two local agents, one proxy."
  {"claude-1" {:agent/id {:id/value "claude-1" :id/type :continuity}
               :agent/type :claude
               :agent/invoke-fn (fn [_p _s] {:result "ok"})
               :agent/capabilities [:explore :edit :test :coordination/execute]
               :agent/session-id "sess-claude-1"
               :agent/status :idle
               :agent/metadata {}}
   "codex-1"  {:agent/id {:id/value "codex-1" :id/type :continuity}
               :agent/type :codex
               :agent/invoke-fn nil
               :agent/capabilities [:explore :edit :test :coordination/execute]
               :agent/session-id "sess-codex-1"
               :agent/status :idle
               :agent/metadata {}}
   "claude-r" {:agent/id {:id/value "claude-r" :id/type :continuity}
               :agent/type :claude
               :agent/invoke-fn nil
               :agent/capabilities [:explore]
               :agent/session-id "sess-remote"
               :agent/status :idle
               :agent/metadata {:proxy? true :remote? true
                                :origin-url "http://192.168.1.100:7070"}}})

(def ^:private clean-routing
  {"claude-1" {:invoke-route :local :invoke-ready? true}
   "codex-1"  {:invoke-route :ws :invoke-ready? true}
   "claude-r" {:invoke-route :none :invoke-ready? false}})

(def ^:private peripheral-specs
  "Minimal peripheral topology for testing."
  {:peripherals
   {:explore {:peripheral/id :explore
              :peripheral/tools #{:read :glob :grep}
              :peripheral/entry #{:default :from-reflect}
              :peripheral/exit #{:found-target :ready-to-edit :user-request :hop-reflect}}
    :edit    {:peripheral/id :edit
              :peripheral/tools #{:read :edit :write}
              :peripheral/entry #{:from-explore :from-test :user-request}
              :peripheral/exit #{:tests-pass :hop-test :hop-reflect}}
    :test    {:peripheral/id :test
              :peripheral/tools #{:read}
              :peripheral/entry #{:from-edit :user-request}
              :peripheral/exit #{:pass :fail :hop-edit :hop-reflect}}
    :reflect {:peripheral/id :reflect
              :peripheral/tools #{:read}
              :peripheral/entry #{:from-any}
              :peripheral/exit #{:par-generated}}}})

;; =============================================================================
;; Clean state — all invariants hold
;; =============================================================================

(deftest query-violations-clean-when-registry-well-formed
  (testing "No violations in a well-formed registry + routing + topology"
    (let [db (logic/build-db {:registry clean-registry
                              :routing clean-routing
                              :peripherals peripheral-specs})
          v (logic/query-violations db)]
      (is (empty? (:untyped-agents v))
          "All agents have typed IDs")
      (is (empty? (:duplicate-sessions v))
          "No shared session-ids")
      (is (empty? (:route-inconsistencies v))
          "Routes match invoke-fn presence")
      (is (empty? (:dead-end-peripherals v))
          "All peripherals have exits")
      (is (empty? (:entry-exit-asymmetry v))
          "hop-X exits have matching from-X entries")
      (is (empty? (:invalid-observed-hops v))
          "No observed hops to check")
      (is (not (logic/violations? v))
          "violations? returns false for clean state"))))

;; =============================================================================
;; Registration violations
;; =============================================================================

(deftest query-violations-catches-untyped-agents
  (testing "Agent without proper TypedAgentId is flagged"
    (let [registry (assoc clean-registry
                     "bad-agent" {:agent/id {:id/value "bad-agent"}
                                  :agent/type :mock
                                  :agent/capabilities []
                                  :agent/status :idle
                                  :agent/metadata {}})
          db (logic/build-db {:registry registry})
          v (logic/query-violations db)]
      (is (some #{"bad-agent"} (:untyped-agents v))))))

(deftest query-violations-catches-duplicate-sessions
  (testing "Two agents sharing a session-id is flagged"
    (let [registry (-> clean-registry
                       (assoc-in ["codex-1" :agent/session-id] "sess-claude-1"))
          db (logic/build-db {:registry registry})
          v (logic/query-violations db)]
      (is (= 1 (count (:duplicate-sessions v))))
      (let [[a1 a2 sid] (first (:duplicate-sessions v))]
        (is (= "sess-claude-1" sid))
        (is (= #{"claude-1" "codex-1"} #{a1 a2}))))))

;; =============================================================================
;; Invoke routing violations
;; =============================================================================

(deftest query-violations-catches-route-inconsistency
  (testing "Agent with invoke-fn but routed :ws is flagged"
    (let [routing (assoc clean-routing
                    "claude-1" {:invoke-route :ws :invoke-ready? true})
          db (logic/build-db {:registry clean-registry
                              :routing routing})
          v (logic/query-violations db)]
      (is (seq (:route-inconsistencies v)))
      (is (some #(= "claude-1" (:agent-id %)) (:route-inconsistencies v))))))

(deftest query-violations-reports-invoking-agents
  (testing "Agents in :invoking status are reported (informational)"
    (let [registry (assoc-in clean-registry
                     ["claude-1" :agent/status] :invoking)
          db (logic/build-db {:registry registry})
          v (logic/query-violations db)]
      (is (some #{"claude-1"} (:agents-invoking v))))))

;; =============================================================================
;; Hop topology violations
;; =============================================================================

(deftest query-violations-catches-dead-end-peripheral
  (testing "Peripheral with empty exit set is flagged"
    (let [specs (assoc-in peripheral-specs
                  [:peripherals :stuck]
                  {:peripheral/id :stuck
                   :peripheral/tools #{}
                   :peripheral/entry #{:from-any}
                   :peripheral/exit #{}})
          db (logic/build-db {:peripherals specs})
          v (logic/query-violations db)]
      (is (some #{:stuck} (:dead-end-peripherals v))))))

(deftest query-violations-catches-asymmetric-hop
  (testing "hop-X exit without matching from-X entry is flagged"
    (let [specs {:peripherals
                 {:alpha {:peripheral/id :alpha
                          :peripheral/tools #{}
                          :peripheral/entry #{:default}
                          :peripheral/exit #{:hop-beta}}
                  :beta  {:peripheral/id :beta
                          :peripheral/tools #{}
                          ;; Missing :from-alpha — asymmetry!
                          :peripheral/entry #{:default}
                          :peripheral/exit #{:done}}}}
          db (logic/build-db {:peripherals specs})
          v (logic/query-violations db)]
      (is (some (fn [[from to]] (and (= :alpha from) (= :beta to)))
                (:entry-exit-asymmetry v))))))

(deftest query-violations-catches-invalid-observed-hop
  (testing "Observed hop that violates topology is flagged"
    (let [db (logic/build-db
               {:peripherals peripheral-specs
                :observed-hops [{:from :test :to :explore :session-id "s1"}]})
          v (logic/query-violations db)]
      ;; :test -> :explore is invalid (:explore entry is #{:default :from-reflect})
      (is (seq (:invalid-observed-hops v)))
      (is (some #(and (= :test (:from %)) (= :explore (:to %)))
                (:invalid-observed-hops v))))))

(deftest query-violations-allows-valid-observed-hop
  (testing "Observed hop that respects topology is not flagged"
    (let [db (logic/build-db
               {:peripherals peripheral-specs
                :observed-hops [{:from :explore :to :edit :session-id "s1"}]})
          v (logic/query-violations db)]
      (is (empty? (:invalid-observed-hops v))))))

;; =============================================================================
;; Integration: from-any entry accepts all sources
;; =============================================================================

(deftest from-any-allows-all-hops-to-reflect
  (testing ":reflect has :from-any entry — any peripheral can hop there"
    (let [db (logic/build-db
               {:peripherals peripheral-specs
                :observed-hops [{:from :explore :to :reflect :session-id "s1"}
                                {:from :edit :to :reflect :session-id "s2"}
                                {:from :test :to :reflect :session-id "s3"}]})
          v (logic/query-violations db)]
      (is (empty? (:invalid-observed-hops v))))))
