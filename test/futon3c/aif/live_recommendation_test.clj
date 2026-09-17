(ns futon3c.aif.live-recommendation-test
  "H3 (SPEC-flat-removal-and-cascade-decision, 2026-09-17): the admissible
   cascade decision is built with the REAL futon2.aif.policy/select-action-cascades
   over candidates carrying construction and interpretation receipts, and
   passes futon2.aif.decision-gate/emit! before projection; the abstention is
   a real typed abstention shape."
  (:require [clojure.test :refer [deftest is testing]]
            [futon2.aif.decision-gate :as gate]
            [futon2.aif.policy :as policy]
            [futon3c.aif.live-recommendation :as recommendation]))

(defn- cascade-action
  [cascade-id precedence]
  {:kind :cascade-candidate
   :cascade-id cascade-id
   :precedence (vec precedence)
   :construction-receipt {:cascade-id cascade-id :moves (count precedence)}
   :interpretation-receipts (mapv (fn [p] {:pattern p :admitted true})
                                  precedence)})

(defn- entries
  []
  [{:action (cascade-action :C1-test-first
                            [{:type :advance-mission :target "M-foo"}
                             {:type :address-sorry :target "sorry/x"}])
    :controller-score 11.434408130577516}
   {:action (cascade-action :C2-fix-first
                            [{:type :advance-mission :target "M-bar"}])
    :controller-score 11.434408130577516}
   {:action (cascade-action :C3-fix-only [{:type :no-op}])
    :controller-score 12.101074797244184}])

(defn- cascade-decision
  []
  (gate/emit! (policy/select-action-cascades (entries) {:beta 0.25})))

(deftest cascade-decision-projects-target-enacted-step-mass-and-beta
  (let [judgement {:decision (cascade-decision)}
        result (recommendation/project judgement)]
    (testing "the decision passed the gate before projection"
      (is (map? (:decision judgement))))
    (testing "issued: target, enacted first step, posterior mass, β with status"
      (is (= :recommendation-issued (:status result)))
      (is (= :judgement.decision
             (get-in result [:recommendation :source])))
      (is (contains? #{:C1-test-first :C2-fix-first :C3-fix-only}
                     (get-in result [:recommendation :cascade-id])))
      (is (contains? #{:advance-mission :no-op}
                     (get-in result [:recommendation :type])))
      (let [rec (:recommendation result)]
        ;; the display target is the cascade id; the enacted step carries the
        ;; step's own :target
        (is (contains? #{:C1-test-first :C2-fix-first :C3-fix-only} (:target rec)))
        (is (contains? #{"M-foo" "M-bar"} (:target (:enacted-step rec)))))
      (is (= {:value 0.25 :status :declared}
             (get-in result [:recommendation :beta])))
      (is (pos? (get-in result [:recommendation :posterior-mass]))))
    (testing "the posterior marginals are presented, not re-selected"
      (is (seq (:posterior result)))
      (is (pos? (-> result :posterior first :posterior-mass))))
    (testing "presentation explicitly performs no selection"
      (is (false? (get-in result
                          [:selection-boundary :recomputed?])))
      (is (false? (get-in result [:actuation :authorized?])))
      (is (false?
           (get-in result
                   [:recommendation :requires-operator-override?]))))))

(deftest typed-abstention-is-a-readiness-state-grouped-by-kind
  (let [abstention {:status :abstained
                    :refusals
                    [{:target "M-foo" :kind :want-not-declared
                      :clause "no want tokens declared"}
                     {:target "M-bar" :kind :beta-not-declared}
                     {:target "M-baz" :kind :want-not-declared}]}
        result (recommendation/project {:decision abstention})]
    (is (= :abstained-readiness (:status result)))
    (is (nil? (:recommendation result)))
    (is (= {:want-not-declared 2 :beta-not-declared 1}
           (into {} (map (fn [[k v]] [k (count v)]))
                 (:refusals-by-kind result))))
    (is (true? (get-in result [:selection-boundary :readiness])))
    (is (= :withheld-selector-abstained
           (get-in result [:actuation :status])))
    (is (false? (get-in result [:actuation :authorized?])))))

(deftest missing-or-inadmissible-decision-is-a-readiness-failure
  (testing "no decision at all"
    (let [result (recommendation/project {:priorities []})]
      (is (= :authoritative-decision-unavailable (:status result)))
      (is (nil? (:recommendation result)))
      (is (= :missing-actionable-reason-bearing-decision
             (get-in result [:selection-boundary :failure])))
      (is (false? (get-in result [:actuation :authorized?])))))
  (testing "a flat-shaped decision is NOT projected — no flat branch exists"
    (let [result (recommendation/project
                  {:decision {:action {:type :advance-mission
                                       :target "M-flat"}
                              :selected-policy-id "pi-s-flat"}})]
      (is (= :authoritative-decision-unavailable (:status result)))
      (is (nil? (:recommendation result))))))
