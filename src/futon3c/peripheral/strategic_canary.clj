(ns futon3c.peripheral.strategic-canary
  "Phase-8 operator-gated strategic advice.

   Only the advice-only rung is implemented. Confirm-to-enact and bounded
   autonomy fail closed until an operator-reviewed window explicitly promotes
   them. This namespace has no actuator and cannot mutate mission ordering."
  (:require [clojure.set :as set]))

(def algorithm :strategic-canary/advice-only-v1)
(def canary-rungs [:advice-only :confirm-to-enact :bounded-autonomy])

(defn- nonblank-string?
  [value]
  (and (string? value) (not-empty value)))

(defn- fallback
  [requested-rung reason details]
  {:status :advice-withheld
   :algorithm algorithm
   :requested-rung requested-rung
   :effective-rung :advice-only
   :fallback-controller :current-additive
   :old-controller-available? true
   :rollback-reason reason
   :rollback-details details
   :enactment {:authorized? false :executed? false}
   :selected-mission nil
   :live-ordering-changed? false})

(defn advice-only
  "Issue one replay-backed recommendation while leaving choice to the operator."
  [shadow-result fixture]
  (let [{:keys [freeze-id requested-rung decision-id
                operator-choice-mission-ids operator-confirmed?
                tripwire-clear? query-limit maximum-query-limit
                observed-outcome]} fixture
        trace
        (first (filter #(= decision-id (:decision-id %))
                       (:shadow-traces shadow-result)))
        recommendation (first (:ranked-policies trace))
        budget (:source-budget shadow-result)
        observed-memory-ids (set (:memory-ids-used observed-outcome))
        recommendation-memory-ids (set (:memory-ids recommendation))
        basic-valid?
        (and (nonblank-string? freeze-id)
             (contains? (set canary-rungs) requested-rung)
             (nonblank-string? decision-id)
             (vector? operator-choice-mission-ids)
             (seq operator-choice-mission-ids)
             (boolean? operator-confirmed?)
             (boolean? tripwire-clear?)
             (integer? query-limit)
             (pos? query-limit)
             (integer? maximum-query-limit)
             (pos? maximum-query-limit))
        explanation-valid?
        (and trace recommendation
             (true? (:explanation-complete? recommendation))
             (seq (:proposal-reasons recommendation))
             (seq (:provenance recommendation))
             (seq (:memory-ids recommendation))
             (true? (get-in recommendation
                            [:hard-support :admitted?])))
        budget-valid?
        (and (map? budget)
             (<= (:spent budget) (:initial budget))
             (<= query-limit maximum-query-limit))
        outcome-valid?
        (and (map? observed-outcome)
             (contains? #{:useful-progress :no-useful-progress}
                        (:outcome observed-outcome))
             (= :independently-witnessed
                (:witness-status observed-outcome))
             (nonblank-string? (:witness-id observed-outcome))
             (vector? (:memory-ids-used observed-outcome))
             (seq (:memory-ids-used observed-outcome))
             (set/subset? observed-memory-ids
                          recommendation-memory-ids))]
    (cond
      (not basic-valid?)
      (fallback requested-rung :invalid-canary-fixture
                {:decision-id decision-id})

      (not= :advice-only requested-rung)
      (fallback requested-rung :reviewed-window-required-before-advance
                {:available-rung :advice-only})

      (not tripwire-clear?)
      (fallback requested-rung :tripwire-fired
                {:decision-id decision-id})

      (not budget-valid?)
      (fallback requested-rung :query-or-resource-bound-failed
                {:budget budget
                 :query-limit query-limit
                 :maximum-query-limit maximum-query-limit})

      (not explanation-valid?)
      (fallback requested-rung :explanation-or-provenance-incomplete
                {:decision-id decision-id})

      (not outcome-valid?)
      (fallback requested-rung :independent-outcome-incomplete
                {:observed-outcome observed-outcome})

      :else
      {:status :advice-issued
       :algorithm algorithm
       :requested-rung requested-rung
       :effective-rung :advice-only
       :recommendation
       {:policy-id (:policy-id recommendation)
        :mission-ids (:mission-ids recommendation)
        :shadow-probability (:shadow-probability recommendation)
        :e-s (:e-s recommendation)
        :predicted-g-s (:predicted-g-s recommendation)
        :hard-support (:hard-support recommendation)
        :proposal-reasons (:proposal-reasons recommendation)
        :memory-ids (:memory-ids recommendation)}
       :counterfactual-baseline (:counterfactual-baseline trace)
       :operator
       {:choice-mission-ids operator-choice-mission-ids
        :confirmed? operator-confirmed?
        :agrees-with-advice?
        (= operator-choice-mission-ids
           (:mission-ids recommendation))
        :override-required? false
        :retains-choice? true}
       :observed-outcome observed-outcome
       :memory-use
       {:surfaced-ids (:memory-ids recommendation)
        :used-ids (:memory-ids-used observed-outcome)}
       :resource-audit
       {:outer-budget budget
        :query-limit query-limit
        :maximum-query-limit maximum-query-limit
        :within-bounds? true}
       :tripwire {:clear? true}
       :fallback-controller :current-additive
       :old-controller-available? true
       :rollback-reason nil
       :enactment
       {:authorized? false
        :executed? false
        :why :advice-only-operator-retains-control}
       :next-rung
       {:rung :confirm-to-enact
        :eligible? false
        :why :reviewed-window-required}
       :selected-mission nil
       :live-ordering-changed? false})))
