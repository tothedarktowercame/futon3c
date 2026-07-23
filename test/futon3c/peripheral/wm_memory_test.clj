(ns futon3c.peripheral.wm-memory-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.wm-memory :as wm-memory]))

(def pattern "p4ng/R15-hierarchical-temporal-depth")
(def mission "M-wm-strategic-mission-selection")

(def edge
  {:mission-id mission
   :control-pattern-id pattern
   :relation :repairs-control
   :status :witnessed
   :provenance [{:kind :review :reviewer "independent-wm-checker"}]
   :memory-ids ["e-wm-r15"]})

(def memory
  {:memory/id "e-wm-r15"
   :memory/domain :war-machine
   :memory/state :current
   :memory/attachment-status :reviewed
   :memory/witness-status :independently-witnessed
   :memory/pattern-ids [pattern]
   :memory/mission-ids [mission]
   :memory/hook "Separate strategic and tactical policy grains."
   :memory/body {:observation :mixed-policy-grain
                 :intervention :construct-same-mission-cascade-menu}})

(deftest controller-writer-is-a-stamped-wrapper-over-memory-record
  (let [call (atom nil)
        result
        (wm-memory/record-episode!
         {:agent-id "war-machine/dark"
          :session-id "wm-shadow-4"
          :record-memory-fn
          (fn [ctx payload]
            (reset! call {:ctx ctx :payload payload})
            {:ok true :id "e-written"})}
         {:mission-id mission
          :control-pattern-ids [pattern]
          :event-kind :intervention
          :witness-ids ["wm-checker/phase4"]
          :name "policy-grain-intervention"
          :hook "Construct a same-mission cascade menu."
          :body {:before :singleton-construction
                 :after :nondegenerate-menu}})]
    (is (= {:ok true :id "e-written"} result))
    (is (= :war-machine (get-in @call [:ctx :domain])))
    (is (= :self-asserted (get-in @call [:ctx :witness-status])))
    (is (= mission (get-in @call [:ctx :mission-id])))
    (is (= #{mission pattern "wm/event/intervention" "wm-checker/phase4"}
           (set (map :ref/id (get-in @call [:payload :subjects])))))))

(deftest dark-query-recalls-bodies-and-does-not-touch-live-ordering
  (let [recall-fn
        (fn [ctx endpoint opts]
          {:ok true
           :endpoint endpoint
           :domain (:domain ctx)
           :trace-id (:trace-id opts)
           :memories [memory]
           :audit {:returned-count 1}})
        result
        (wm-memory/dark-candidate-projection
         {:recall-fn recall-fn :trace-id "phase4-dark-test"}
         [pattern] [edge] {:limit 3
                           :decision-id "wm-dark-decision"
                           :session-id "wm-dark-session"})
        candidate (get-in result [:projection :candidates 0])]
    (is (= :dark (:status result)))
    (is (= :shared-memory/recall-by-endpoint (:query-code result)))
    (is (= :futon2.aif.memory-contract/use-receipt
           (:receipt-code result)))
    (is (= ["e-wm-r15"]
           (get-in result [:memory-use-receipt :memory-use/used-ids])))
    (is (false? (:live-ordering-changed? result)))
    (is (= mission (:mission-id candidate)))
    (is (= :mixed-policy-grain
           (get-in candidate
                   [:support-relations 0 :memories 0
                    :memory/body :observation])))))

(deftest proposed-and-cross-domain-material-cannot-certify
  (let [cross-domain (assoc memory :memory/domain :mathematics)
        proposed (assoc edge :status :proposed)
        recall-fn
        (fn [_ endpoint _]
          {:ok true :endpoint endpoint :memories [cross-domain]})]
    (testing "a cross-domain memory remains auditable but cannot admit"
      (let [result
            (wm-memory/dark-candidate-projection
             {:recall-fn recall-fn} [pattern] [edge] {})]
        (is (empty? (get-in result [:projection :candidates])))
        (is (= 1 (get-in result
                         [:projection :audit
                          :cross-domain-memory-count])))))
    (testing "a proposed relation cannot admit even with a WM witness"
      (let [result
            (wm-memory/dark-candidate-projection
             {:recall-fn
              (fn [_ endpoint _]
                {:ok true :endpoint endpoint :memories [memory]})}
             [pattern] [proposed] {})]
        (is (empty? (get-in result [:projection :candidates])))
        (is (= 1 (get-in result [:projection :audit :proposal-count])))))))

(deftest reviewed-control-corpus-retrieves-support-and-challenge-per-pattern
  (let [{:keys [episodes control-edges]}
        (-> "holes/labs/M-typed-memories/phase4-wm-corpus.edn"
            io/file slurp edn/read-string)
        patterns (->> episodes (mapcat :memory/pattern-ids) distinct vec)
        recall-fn
        (fn [_ endpoint _]
          {:ok true
           :endpoint endpoint
           :memories
           (filterv #(some #{endpoint} (:memory/pattern-ids %)) episodes)})
        result
        (wm-memory/dark-candidate-projection
         {:recall-fn recall-fn :trace-id "phase4-reviewed-corpus"}
         patterns control-edges {:limit 10})
        recall-by-pattern (into {} (map (juxt :endpoint identity))
                                (:recalls result))]
    (doseq [pattern-id patterns]
      (let [memories (:memories (get recall-by-pattern pattern-id))]
        (is (some #(= :current (:memory/state %)) memories) pattern-id)
        (is (some #(= :challenged (:memory/state %)) memories) pattern-id)
        (is (every? map? (map :memory/body memories)) pattern-id)))
    (is (= #{"M-wm-aif-policy-grain-compliance"
             "M-shared-memory-control-build-test"
             "M-aif-policy-conditioned-eig"}
           (set (map :mission-id
                     (get-in result [:projection :candidates])))))
    (is (not (contains?
              (set (map :mission-id
                        (get-in result [:projection :candidates])))
              "M-wm-tripwires")))
    (is (= 1 (get-in result
                     [:projection :audit :witnessed-block-count])))))
