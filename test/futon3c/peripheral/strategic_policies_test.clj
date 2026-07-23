(ns futon3c.peripheral.strategic-policies-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.strategic-cascade :as strategic-cascade]
            [futon3c.peripheral.strategic-policies :as policies]
            [futon3c.peripheral.wm-memory :as wm-memory]))

(defn- read-edn
  [path]
  (-> path io/file slurp edn/read-string))

(defn phase7-input
  []
  (let [{:keys [episodes control-edges]}
        (read-edn
         "holes/labs/M-typed-memories/phase4-wm-corpus.edn")
        {:keys [cascade dependencies transition-warrants]}
        (read-edn
         "holes/labs/M-typed-memories/phase5-outer-cascade.edn")
        recall-fn
        (fn [_ endpoint _]
          {:ok true
           :endpoint endpoint
           :memories
           (filterv #(some #{endpoint} (:memory/pattern-ids %))
                    episodes)})
        outer-result
        (strategic-cascade/outer-frontier
         {:cascade cascade
          :dependencies dependencies
          :transition-warrants transition-warrants
          :budget (count (:shown cascade))
          :query-step-fn
          (fn [pattern-id _]
            (wm-memory/dark-candidate-projection
             {:recall-fn recall-fn :trace-id "phase7-shadow-test"}
             [pattern-id] control-edges {:limit 10}))})]
    {:outer-result outer-result
     :fixture
     (read-edn
      "holes/labs/M-typed-memories/phase7-strategic-policy-shadow.edn")}))

(deftest strategic-policy-identity-preserves-order-and-grain
  (let [a ["M-a" "M-b"]
        b ["M-b" "M-a"]]
    (is (= (policies/strategic-policy-id a)
           (policies/strategic-policy-id a)))
    (is (not= (policies/strategic-policy-id a)
              (policies/strategic-policy-id b)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (policies/strategic-policy-id [])))))

(deftest strategic-habit-is-normalized-with-unseen-mass-and-no-e-t-leak
  (let [{:keys [outer-result fixture]} (phase7-input)
        strategic-policies
        (:policies
         (policies/construct-strategic-policies outer-result))
        events (:selection-events fixture)
        habit (policies/fit-strategic-habit
               strategic-policies events 0.5)
        without-tactical
        (policies/fit-strategic-habit
         strategic-policies
         (filterv #(= :strategic (:grain %)) events)
         0.5)
        unseen
        (assoc (first strategic-policies)
               :policy-id "pi-s-never-selected")
        with-unseen
        (policies/fit-strategic-habit
         (conj strategic-policies unseen) events 0.5)]
    (is (= 1.0 (:normalization habit)))
    (is (= 2 (:excluded-tactical-event-count habit)))
    (is (= (:estimates habit) (:estimates without-tactical)))
    (is (pos? (get-in with-unseen
                      [:estimates "pi-s-never-selected"
                       :probability])))
    (is (= :selection-frequency-habit-not-outcome-value
           (:semantics habit)))))

(deftest temperature-changes-the-balance-without-changing-e-s
  (let [{:keys [outer-result fixture]} (phase7-input)
        strategic-policies
        (:policies
         (policies/construct-strategic-policies outer-result))
        habit (policies/fit-strategic-habit
               strategic-policies
               (:selection-events fixture) 0.5)
        g-rows (get-in fixture [:shadow-cases 1 :predicted-g-s])
        cold (policies/rank-shadow-policies
              strategic-policies habit g-rows 0.01)
        hot (policies/rank-shadow-policies
             strategic-policies habit g-rows 1000000.0)]
    (is (= ["M-shared-memory-control-build-test"
            "M-aif-policy-conditioned-eig"]
           (:model-winner-mission-ids cold)))
    (is (= ["M-shared-memory-control-build-test"]
           (:model-winner-mission-ids hot)))
    (is (= (:e-s (first (:ranked-policies cold)))
           (get-in habit
                   [:estimates
                    (:policy-id (first (:ranked-policies cold)))])))
    (is (< (Math/abs (- 1.0 (:normalization cold))) 1.0e-12))
    (is (< (Math/abs (- 1.0 (:normalization hot))) 1.0e-12))))

(deftest phase7-shadow-window-is-complete-but-cannot-self-promote
  (let [{:keys [outer-result fixture]} (phase7-input)
        result (policies/run-shadow-window outer-result fixture)]
    (is (= :shadow-complete (:status result)))
    (is (= 2 (get-in result
                     [:policy-construction :policy-count])))
    (is (= 2 (get-in result
                     [:evaluation :held-policy-count])))
    (is (= 1.0
           (get-in result
                   [:evaluation :explanation-completeness])))
    (is (= 1.0
           (get-in result
                   [:evaluation :model-reviewed-agreement])))
    (is (= 3 (get-in result
                     [:evaluation :disagreement-count])))
    (is (true? (get-in result
                       [:evaluation :identity-preserved?])))
    (is (true? (get-in result
                       [:evaluation :provenance-preserved?])))
    (is (true? (get-in result
                       [:evaluation :candidate-set-preserved?])))
    (is (true?
         (get-in result
                 [:promotion :promotion-eligible-for-review?])))
    (is (false? (get-in result [:promotion :promote?])))
    (is (= :operator-review-required
           (get-in result [:promotion :decision-reason])))
    (is (nil? (:selected-mission result)))
    (is (false? (:live-ordering-changed? result)))
    (testing "outcome-looking fields cannot alter E_S"
      (let [events (mapv #(assoc % :outcome-probability
                                (if (= :tactical (:grain %)) 1.0 0.0))
                         (:selection-events fixture))
            policy-set
            (get-in result [:policy-construction :policies])
            habit
            (policies/fit-strategic-habit policy-set events 0.5)]
        (is (= (get-in result [:e-s :estimates])
               (:estimates habit)))))))
