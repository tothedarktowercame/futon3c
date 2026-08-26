(ns futon3c.apm.frame-park-decisions-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.problem-queue-supervisor :as queue]))

(defn park [frame receipt]
  {:state/type :solver-human-intervention-frame-park
   :frame/id frame :problem/id (str "p-" frame)
   :residual (str "residual-" frame)
   :last-valid-receipt/id receipt
   :solver/final-head (str "head-" frame)
   :solver/state-path (str "/state/" frame)
   :solver/rounds-completed 50
   :student/decision :claude-required
   :decision/owner :claude-supervisor
   :decision/status :awaiting-decision
   :decision/bell-required true})

(defn state-with [parks]
  (let [plan (queue/queue-plan
              [{:problem/id "p" :repository "r" :revision "x"
                :path "Main.lean" :blob "b" :classification :non-excluded}])]
    (-> (queue/initial-state plan)
        (assoc :parked parks)
        (#'queue/addressed))))

(deftest receipt-keyed-decisions-stop-bells-without-consuming-dispositions
  (let [parks [(park "f36" "r36") (park "f38" "r38") (park "f39" "r39")
               (park "f-unmatched" "r-unmatched")]
        decisions (mapv (fn [receipt]
                          {:last-valid-receipt/id receipt
                           :decision/status :decided
                           :decision/disposition :partial})
                        ["r36" "r38" "r39"])
        result (queue/reconcile-park-decisions (state-with parks) decisions)
        decided (take 3 (get-in result [:state :parked]))
        unmatched (last (get-in result [:state :parked]))]
    (is (:ok result))
    (is (= #{"r36" "r38" "r39"} (set (:matched-receipt-ids result))))
    (is (every? #(and (= :decided (:decision/status %))
                      (false? (:decision/bell-required %))) decided))
    (is (= (mapv :residual parks) (mapv :residual (get-in result [:state :parked]))))
    (is (= (mapv :last-valid-receipt/id parks)
           (mapv :last-valid-receipt/id (get-in result [:state :parked]))))
    (is (= :awaiting-decision (:decision/status unmatched)))
    (is (true? (:decision/bell-required unmatched)))
    (is (= :partial (get-in (first decided)
                            [:decision/record :decision/disposition])))))

(deftest unmatched-decision-record-is-inert
  (let [state (state-with [(park "f36" "r36")])
        result (queue/reconcile-park-decisions
                state [{:last-valid-receipt/id "other"
                        :decision/status :decided}])]
    (is (:ok result))
    (is (false? (:changed? result)))
    (is (= state (:state result)))
    (is (= 1 (count (:unmatched-records result))))))
