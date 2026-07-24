(ns futon3c.aif.live-recommendation-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.aif.live-recommendation :as recommendation]))

(def decision
  {:action {:type :advance-mission
            :target "M-shared-memory-control-build-test"}
   :reason :reviewed-live-reason-bearing-policy
   :selected-policy-id "pi-s-9dbc2ceb3317bc38050c41ce"
   :selected-mission-ids
   ["M-shared-memory-control-build-test"
    "M-aif-policy-conditioned-eig"]
   :strategic-memory
   {:influenced? true
    :authority :live
    :memory-ids ["e-live-r6" "e-live-r5"]
    :counterfactuals
    {:fixed ["M-aif-policy-conditioned-eig"
             "M-shared-memory-control-build-test"]
     :additive-controller ["M-aif-policy-conditioned-eig"
                           "M-shared-memory-control-build-test"]
     :scheduler-habit ["M-wm-aif-policy-grain-compliance"
                       "M-shared-memory-control-build-test"]}
    :actuation {:status :pending-downstream-gates
                :authorized? false
                :executed? false}}})

(deftest judgement-decision-is-the-only-presentation-winner
  (let [judgement
        {:decision decision
         :ranked-actions
         [{:action {:type :apply-cascade :target "M-held-placeholder"}
           :controller-score 0.0
           :score-provenance :placeholder
           :held-for-arming? true}
          {:action {:type :no-op}
           :controller-score -10.0}
          {:action {:type :advance-mission
                    :target "M-other-presentation-winner"}
           :controller-score -20.0}]}
        result (recommendation/project judgement)]
    (testing "held and no-op rows cannot manufacture a winner"
      (is (= :recommendation-issued (:status result)))
      (is (= "M-shared-memory-control-build-test"
             (get-in result [:recommendation :target])))
      (is (= :judgement.decision
             (get-in result [:recommendation :source])))
      (is (= "pi-s-9dbc2ceb3317bc38050c41ce"
             (get-in result [:recommendation :policy-id]))))
    (testing "all three comparisons remain named and inspectable"
      (is (= "M-aif-policy-conditioned-eig"
             (get-in result [:rankings :fixed :winner :target])))
      (is (= "M-aif-policy-conditioned-eig"
             (get-in result
                     [:rankings :additive-controller :winner :target])))
      (is (= "M-wm-aif-policy-grain-compliance"
             (get-in result
                     [:rankings :scheduler-habit :winner :target]))))
    (testing "presentation explicitly performs no selection"
      (is (false? (get-in result
                          [:selection-boundary :recomputed?])))
      (is (false? (get-in result [:actuation :authorized?])))
      (is (false?
           (get-in result
                   [:recommendation :requires-operator-override?]))))))

(deftest missing-reason-bearing-decision-is-a-readiness-failure
  (let [result
        (recommendation/project
         {:decision {:action :abstain
                     :strategic-memory {:influenced? false}}
          :ranked-actions
          [{:action {:type :advance-mission :target "M-tempting-fallback"}
            :controller-score -100.0}]})]
    (is (= :authoritative-decision-unavailable (:status result)))
    (is (nil? (:recommendation result)))
    (is (= :missing-actionable-reason-bearing-decision
           (get-in result [:selection-boundary :failure])))
    (is (false? (get-in result
                        [:selection-boundary :recomputed?])))
    (is (false? (get-in result [:actuation :authorized?])))))
