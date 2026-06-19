(ns futon3c.metric.resolution-state-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.metric.resolution-state :as sut]))

(deftest mission-resolution-state-follows-contract-buckets
  (is (= {:resolution-state/source :metric/resolution-mission-v0
          :resolution-state/resolvedness 1.0
          :resolution-state/actionable? false
          :resolution-state/raw {:phase "complete" :normalized "complete"}}
         (sut/resolution-state
          {:hx/endpoints ["futon0-d/mission/futonzero-capability"]
           :hx/props {:mission/phase "complete"}})))
  (is (= 0.1 (:resolution-state/resolvedness
              (sut/resolution-state
               {:hx/endpoints ["futon3c-d/mission/substrate-metric"]
                :hx/props {"mission/phase" "DERIVE"}}))))
  (is (:resolution-state/actionable?
       (sut/resolution-state
        {:hx/endpoints ["futon3c-d/mission/substrate-metric"]
         :hx/props {:mission/phase :instantiate}}))))

(deftest sorry-resolution-state-handles-keyword-like-statuses
  (let [open (sut/resolution-state
              {:hx/endpoints ["futon2-d/sorry/example"]
               :hx/props {:sorry/status ":open"}})
        addressed (sut/resolution-state
                   {:hx/endpoints ["futon2-d/sorry/example"]
                    :hx/props {"sorry/status" ":addressed"}})
        closed (sut/resolution-state
                {:hx/endpoints ["futon2-d/sorry/example"]
                 :hx/props {:sorry/status :closed}})]
    (is (= 0.0 (:resolution-state/resolvedness open)))
    (is (:resolution-state/actionable? open))
    (is (= 0.65 (:resolution-state/resolvedness addressed)))
    (is (:resolution-state/actionable? addressed))
    (is (= 1.0 (:resolution-state/resolvedness closed)))
    (is (not (:resolution-state/actionable? closed)))))

(deftest pattern-and-file-providers-are-explicit
  (is (= 0.2 (:resolution-state/resolvedness
              (sut/resolution-state
               {:hx/endpoints ["futon3c-d/pattern/example"]
                :hx/props {:pattern/status "candidate"}}))))
  (is (= 1.0 (:resolution-state/resolvedness
              (sut/resolution-state
               {:hx/endpoints ["futon3c-d/pattern/example"]
                :hx/props {:pattern/state "validated"}}))))
  (testing "file is unknown and non-actionable directly"
    (is (= {:resolution-state/source :none
            :resolution-state/resolvedness :unknown
            :resolution-state/actionable? false
            :resolution-state/raw {:reason :file-has-no-native-resolution-state}}
           (sut/resolution-state
            {:hx/endpoints ["futon3c-d/file/src/futon3c/metric/e1.clj"]
             :hx/props {}})))))

(deftest unknown-grain-is-safe
  (let [state (sut/resolution-state {:node/id "x"})]
    (is (= :unknown (:resolution-state/resolvedness state)))
    (is (not (:resolution-state/actionable? state)))))
