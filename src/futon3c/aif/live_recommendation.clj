(ns futon3c.aif.live-recommendation
  "Presentation-only projection of the authoritative judgement decision.

   Selection belongs to the reason-bearing WM selector. This namespace never
  re-ranks `ranked-actions`, so a held cascade, placeholder score, no-op, or
   stale presentation heuristic cannot manufacture a second winner."
  (:require [clojure.walk :as walk]))

(def algorithm :wm-live-recommendation/decision-projection-v2)

(defn- value
  [m k]
  (or (get m k) (get m (name k))))

(defn- keyword-value
  [x]
  (cond
    (keyword? x) x
    (string? x) (keyword x)
    :else x))

(defn- ranking-items
  [mission-ids]
  (mapv (fn [rank mission-id]
          {:rank rank
           :type :advance-mission
           :target mission-id})
        (range 1 (inc (count mission-ids)))
        mission-ids))

(defn- ranking
  [semantics mission-ids]
  (let [items (ranking-items (vec (or mission-ids [])))]
    {:semantics semantics
     :winner (first items)
     :items items}))

(defn project
  "Display `judgement.decision` and its named counterfactuals unchanged.

   A missing/non-actionable decision is a system-readiness failure, not an
   invitation for this presentation layer to choose another action."
  ([judgement] (project judgement {}))
  ([judgement _opts]
   (let [judgement (walk/keywordize-keys judgement)
         decision (value judgement :decision)
         action (value decision :action)
         action-type (keyword-value (value action :type))
         strategic (value decision :strategic-memory)
         influenced? (true? (value strategic :influenced?))
         counterfactuals (or (value strategic :counterfactuals) {})
         actuation (or (value strategic :actuation)
                       {:status :pending-downstream-gates
                        :authorized? false
                        :executed? false})]
     (if (and (map? decision)
              (map? action)
              (not= :abstain action-type)
              influenced?)
       {:status :recommendation-issued
        :algorithm algorithm
        :recommendation
        (assoc action
               :source :judgement.decision
               :policy-id (value decision :selected-policy-id)
               :mission-ids (vec (or (value decision
                                             :selected-mission-ids)
                                     []))
               :memory-ids (vec (or (value strategic :memory-ids) []))
               :recommendation-authority :live
               :live-selection? true
               :advisory? false
               :requires-operator-override? false)
        :rankings
        {:authoritative
         (ranking :reason-bearing-strategic-policy
                  (value decision :selected-mission-ids))
         :fixed
         (ranking :fixed-endpoint-order
                  (value counterfactuals :fixed))
         :additive-controller
         (ranking :legacy-additive-controller
                  (value counterfactuals :additive-controller))
         :scheduler-habit
         (ranking :tactical-scheduler-habit
                  (value counterfactuals :scheduler-habit))}
        :strategic-memory strategic
        :selection-boundary
        {:source :judgement.decision
         :recomputed? false
         :operator-override-required? false
         :actuation-owner :downstream-act-gate}
        :actuation actuation
        :comparison
        {:authoritative-policy-id (value decision :selected-policy-id)
         :authoritative-mission-ids
         (vec (or (value decision :selected-mission-ids) []))
         :newer-strategic-memory-influenced? true}}
       {:status :authoritative-decision-unavailable
        :algorithm algorithm
        :recommendation nil
        :strategic-memory strategic
        :selection-boundary
        {:source :judgement.decision
         :recomputed? false
         :failure :missing-actionable-reason-bearing-decision
         :operator-override-required? false}
        :actuation
        {:status :withheld-system-readiness-failure
         :authorized? false
         :executed? false}}))))
