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
    (is (= :futon2.aif.memory-contract/wm-projection-receipt
           (:receipt-code result)))
    (is (= ["e-wm-r15"]
           (get-in result
                   [:projection-receipt
                    :wm-projection/projection-selected-ids])))
    (is (= :algorithmic-selection
           (get-in result [:projection-receipt :wm-projection/signal])))
    (is (not-any? #(= "used-ids" (name %))
                  (keys (:projection-receipt result))))
    (is (false? (:live-ordering-changed? result)))
    (is (= mission (:mission-id candidate)))
    (is (= :mixed-policy-grain
           (get-in candidate
                   [:support-relations 0 :memories 0
                    :memory/body :observation])))))

(deftest decision-identity-is-the-only-outcome-join-key
  (let [projection
        (:projection-receipt
         (wm-memory/dark-candidate-projection
          {:recall-fn
           (fn [_ endpoint _]
             {:ok true :endpoint endpoint :memories [memory]})}
          [pattern] [edge]
          {:decision-id "wm-decision-20260731"
           :session-id "wm-session-20260731"}))
        check
        (wm-memory/decision-keyed-external-check-entry
         {:evidence-id "e-wm-check-20260731"
          :decision-id "wm-decision-20260731"
          :author "wm/test-independent-checker"
          :session-id "wm-check-session-20260731"
          :at "2026-07-31T09:00:00Z"
          :outcome :pass
          :witness-status :independently-witnessed
          :checker "test-only independent checker fixture"})
        triple (wm-memory/witnessed-projection-triple projection check)]
    (is (= :offered-projection-selected-witnessed
           (:wm-outcome-triple/type triple)))
    (is (= "wm-decision-20260731"
           (:wm-outcome-triple/decision-id triple)))
    (is (= ["e-wm-r15"] (:wm-outcome-triple/offered-ids triple)))
    (is (= ["e-wm-r15"]
           (:wm-outcome-triple/projection-selected-ids triple)))
    (is (= "e-wm-check-20260731"
           (:wm-outcome-triple/witness-evidence-id triple)))
    (testing "timestamp or session proximity cannot substitute for identity"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"decision ids differ"
           (wm-memory/witnessed-projection-triple
            projection
            (assoc-in check [:evidence/subject :ref/id]
                      "another-decision")))))
    (testing "the append-only 2026-07-23 mission-keyed check stays unjoinable"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"exact decision subject"
           (wm-memory/witnessed-projection-triple
            projection
            {:evidence/id "e-phase4-wm-r15-check-20260723"
             :evidence/subject
             {:ref/type :mission
              :ref/id "M-wm-strategic-mission-selection"}
             :evidence/type :pattern-outcome
             :evidence/claim-type :observation
             :evidence/author "wm/independent-phase4-checker"
             :evidence/session-id "wm-phase4-live-20260723"
             :evidence/at "2026-07-23T12:40:00Z"
             :evidence/body
             {:outcome :pass
              :memory-outcome/witness-status :independently-witnessed
              :checker "phase4 dark projection review"}
             :evidence/tags [:war-machine :external-check]}))))))

(deftest decision-keyed-check-writer-does-not-invent-checker-identity
  (let [store (atom {:entries {} :order []})
        check {:evidence-id "e-wm-check-write"
               :decision-id "wm-decision-write"
               :author "wm/test-checker"
               :session-id "wm-check-session"
               :at "2026-07-31T09:05:00Z"
               :outcome :pass
               :witness-status :independently-witnessed
               :checker "test checker fixture"}
        result
        (wm-memory/record-decision-keyed-external-check!
         {:evidence-store store} check)
        stored (get-in @store [:entries "e-wm-check-write"])]
    (is (not (contains? result :error/code)))
    (is (= "wm/test-checker" (:evidence/author stored)))
    (is (= {:ref/type :decision :ref/id "wm-decision-write"}
           (:evidence/subject stored)))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"invalid decision-keyed"
         (wm-memory/decision-keyed-external-check-entry
          (dissoc check :author))))))

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
