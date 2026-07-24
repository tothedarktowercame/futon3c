(ns futon3c.peripheral.live-wm-selection-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.live-wm-selection :as live]
            [futon3c.peripheral.strategic-cascade :as cascade]
            [futon3c.peripheral.wm-memory :as wm-memory]))

(def root "holes/labs/M-typed-memories/")

(defn read-fixture [name]
  (edn/read-string (slurp (str root name))))

(def live-input
  (merge
   (read-fixture "live-wm-selection-input-20260724.edn")
   {:phase5 (read-fixture "phase5-outer-cascade.edn")
    :phase6 (read-fixture "phase6-strategic-outcomes.edn")
    :phase7 (read-fixture "phase7-strategic-policy-shadow.edn")
    :rung2 (read-fixture "rung2-operator-update.edn")}))

(defn compact-memory
  [edge]
  {:memory/id (first (:memory-ids edge))
   :memory/domain :war-machine
   :memory/state :current
   :memory/attachment-status :reviewed
   :memory/witness-status :independently-witnessed
   :memory/pattern-ids [(:control-pattern-id edge)]
   :memory/mission-ids [(:mission-id edge)]
   :memory/hook (str "Reviewed live test memory for " (:relation edge))
   :memory/body {:event :test-replay
                 :relation (:relation edge)}})

(def memories
  (mapv compact-memory (:control-edges live-input)))

(defn recall-fixture
  [memory-rows]
  (fn [_ endpoint _]
    {:ok true
     :endpoint endpoint
     :elapsed-ms 1.0
     :audit {:edge-count
             (count (filter #(some #{endpoint} (:memory/pattern-ids %))
                            memory-rows))
             :returned-count
             (count (filter #(and
                              (some #{endpoint} (:memory/pattern-ids %))
                              (= :reviewed
                                 (:memory/attachment-status %)))
                            memory-rows))}
     :memories
     (filterv #(and (some #{endpoint} (:memory/pattern-ids %))
                    (= :reviewed (:memory/attachment-status %)))
              memory-rows)}))

(deftest live-selection-composes-the-reviewed-chain
  (let [result
        (live/run-verification
         {:recall-fn (recall-fixture memories)
          :trace-id "live-selection-unit"}
         live-input)]
    (is (= :verified-live-selection (:status result)))
    (is (= #{"M-aif-policy-conditioned-eig"
             "M-shared-memory-control-build-test"
             "M-wm-aif-policy-grain-compliance"}
           (set (:candidate-domain result))))
    (is (= "pi-s-9dbc2ceb3317bc38050c41ce"
           (:selected-policy-id result)))
    (is (= ["M-shared-memory-control-build-test"
            "M-aif-policy-conditioned-eig"]
           (:selected-mission-ids result)))
    (is (= 5 (count (:live-memory-ids result))))
    (is (= "M-wm-tripwires"
           (get-in result [:blockers 0 :mission-id])))
    (is (= :exploratory-sample-too-small
           (get-in result [:calibration :status])))
    (is (= 13 (get-in result [:calibration :sample-count])))
    (is (false?
         (get-in result
                 [:authority :demonstrated-better-selection?])))
    (is (false? (get-in result [:actuation :authorized?])))
    (is (true?
         (get-in result
                 [:components :rung2 :evaluation
                  :recovered-from-misleading-seed?])))))

(deftest cache-gated-selection-becomes-machine-authorized
  (let [verified
        (live/run-verification
         {:recall-fn (recall-fixture memories)
          :trace-id "bounded-autonomy-unit"}
         live-input)
        authorized
        (live/authorize-bounded-autonomy
         (assoc verified :serving-cache-gate
                {:status :warm
                 :maximum-endpoint-ms 1000
                 :accepted-endpoint-latencies
                 [{:pattern-id "p4ng/R9-independent-witness"
                   :elapsed-ms 20.0}]}))]
    (is (= :machine-authorized-bounded-autonomy
           (get-in authorized [:actuation :status])))
    (is (true? (get-in authorized [:actuation :authorized?])))
    (is (false? (get-in authorized [:actuation :executed?])))
    (is (= 13 (get-in authorized
                      [:actuation :machine-gates
                       :armed-tripwire-count])))
    (is (= live/operator-decision-evidence-id
           (get-in authorized
                   [:actuation :operator-decision-evidence-id])))
    (is (= live/delivery-qa-endpoint
           (get-in authorized [:actuation :delivery-qa :endpoint])))))

(deftest bounded-autonomy-rejects-an-unwarmed-selection
  (let [verified
        (live/run-verification
         {:recall-fn (recall-fixture memories)}
         live-input)]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"machine gates are incomplete"
         (live/authorize-bounded-autonomy
          (assoc verified :serving-cache-gate
                 {:status :warmed-and-rechecked
                  :maximum-endpoint-ms 1000
                  :accepted-endpoint-latencies
                  [{:pattern-id "p4ng/R9-independent-witness"
                    :elapsed-ms 1001.0}]}))))))

(deftest review-removal-and-warrant-retraction-fail-closed
  (testing "removing review prevents a memory from admitting its mission"
    (let [r6-memory-id
          (get-in live-input [:control-edges 1 :memory-ids 0])
          unreviewed
          (mapv #(if (= r6-memory-id (:memory/id %))
                   (assoc % :memory/attachment-status :proposed)
                   %)
                memories)
          result
          (wm-memory/dark-candidate-projection
           {:recall-fn (recall-fixture unreviewed)}
           ["p4ng/R6-candidate-pattern-action-space"]
           [(nth (:control-edges live-input) 1)]
           {:limit 10})]
      (is (empty? (get-in result [:projection :candidates])))))
  (testing "retracting a relation warrant removes its support"
    (let [edges
          (mapv #(if (= "p4ng/R5-policy-evaluation"
                        (:control-pattern-id %))
                   (assoc % :status :retracted)
                   %)
                (:control-edges live-input))
          phase5 (:phase5 live-input)
          result
          (cascade/execute-outer-cascade
           {:recall-fn (recall-fixture memories)}
           (:cascade phase5) edges (:dependencies phase5)
           (:transition-warrants phase5)
           {:budget 4 :memory-limit 10})]
      (is (not (contains?
                (set (map :mission-id
                          (get-in result
                                  [:admissible-projection :candidates])))
                "M-aif-policy-conditioned-eig"))))))

(deftest comparison-ranking-cannot-escape-the-admissible-set
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"escaped the admissible"
       (live/run-verification
        {:recall-fn (recall-fixture memories)}
        (update live-input :scheduler-habit-ranking
                conj "M-outside-phase4")))))

(deftest serving-cache-gate-warms-rechecks-and-fails-closed
  (testing "a cold first read is discarded and the immediate warm read wins"
    (let [calls (atom 0)
          result
          (live/enforce-serving-cache-gate
           (fn []
             (if (= 1 (swap! calls inc))
               {:selected-policy-id "cold"
                :recall-audits [{:pattern-id "R9" :elapsed-ms 7000.0}]}
               {:selected-policy-id "warm"
                :recall-audits [{:pattern-id "R9" :elapsed-ms 100.0}]}))
           1000)]
      (is (= "warm" (:selected-policy-id result)))
      (is (= :warmed-and-rechecked
             (get-in result [:serving-cache-gate :status])))
      (is (= 2 (get-in result [:serving-cache-gate :attempt-count])))))
  (testing "two slow reads are a system-readiness failure"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"failed its immediate recheck"
         (live/enforce-serving-cache-gate
          (constantly
           {:recall-audits [{:pattern-id "R9" :elapsed-ms 7000.0}]})
          1000)))))
