(ns futon3c.aif.loop-learning-test
  "H3 (SPEC-flat-removal-and-cascade-decision, 2026-09-17): the judgement
   carries a cascade decision (built with the REAL
   futon2.aif.policy/select-action-cascades, passing
   futon2.aif.decision-gate/emit!) or a typed abstention; :ranked-actions is
   gone. loop-learning consumes the posterior and the enacted first step."
  (:require [clojure.test :refer [deftest is testing]]
            [futon2.aif.decision-gate :as gate]
            [futon2.aif.policy :as policy]
            [futon3c.aif.loop-learning :as ll]))

(defn- cascade-action
  [cascade-id precedence]
  {:kind :cascade-candidate
   :cascade-id cascade-id
   :precedence (vec precedence)
   :construction-receipt {:cascade-id cascade-id :moves (count precedence)}
   :interpretation-receipts (mapv (fn [p] {:pattern p :admitted true})
                                  precedence)})

(defn- cascade-decision
  []
  (gate/emit!
   (policy/select-action-cascades
    [{:action (cascade-action :C1-advance
                              [{:type :advance-mission :target "M-foo"}])
      :controller-score 11.4}
     {:action (cascade-action :C2-sorry
                              [{:type :address-sorry :target "sorry/x"}])
      :controller-score 12.1}]
    {:beta 0.25})))

(deftest cascade-decision-contributes-its-enacted-first-step-and-posterior
  (let [block (ll/loop-learning-pass
               {:judgement {:decision (cascade-decision)
                            :priorities
                            [{:type "missing-head" :id "h1" :summary "no head h1"}]}})]
    (testing "patterns-applied includes the enacted cascade first step with its mass"
      (let [enacted (some #(when (:posterior-mass %) %) (:patterns-applied block))]
        (is (some? enacted))
        (is (true? (:applied? enacted)))
        (is (pos? (:posterior-mass enacted)))))
    (testing "gap-signal mining still works off :priorities"
      (is (= 1 (count (:sorries-mined block)))))))

(deftest abstention-invents-no-enacted-pattern
  (let [block (ll/loop-learning-pass
               {:judgement {:decision {:status :abstained
                                       :refusals [{:target "M-foo"
                                                   :kind :want-not-declared}]}
                            :priorities []}})]
    (is (seq (:patterns-applied block)))
    (is (every? #(nil? (:posterior-mass %)) (:patterns-applied block))
        "no pattern may claim enactment from an abstention")))
