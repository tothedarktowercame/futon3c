(ns futon3c.aif.live-recommendation
  "Presentation-only projection of the authoritative judgement decision.

   Selection belongs to the reason-bearing WM selector. This namespace never
   re-ranks or re-scores, so a placeholder, no-op, or stale presentation
   heuristic cannot manufacture a second winner.

   H3 (SPEC-flat-removal-and-cascade-decision, 2026-09-17): the flat decision
   is removed. A tick's :decision has passed futon2.aif.decision-gate/emit!
   and is exactly one of:
   - a cascade decision from futon2.aif.policy/select-action-cascades
     (:selection-law {:applied :cascade-selection-posterior :posterior …},
     :beta {:value … :status …}); what is enacted is the FIRST element of the
     chosen candidate's :precedence; or
   - a typed abstention {:status :abstained :refusals […]} — a readiness
     state, displayed as its refusals grouped by kind, never an error and
     never a fallback selection.
   There is no branch for the old flat shape and no compatibility read of
   :ranked-actions or its flat siblings."
  (:require [clojure.walk :as walk]))

(def algorithm :wm-live-recommendation/decision-projection-v3)

(defn- value
  [m k]
  (or (get m k) (get m (name k))))

(defn- keyword-value
  [x]
  (cond
    (keyword? x) x
    (string? x) (keyword x)
    :else x))

(defn cascade-decision?
  "True when DECISION is a cascade-posterior decision (H3's only acting kind)."
  [decision]
  (and (map? decision)
       (= :cascade-selection-posterior
          (keyword-value (get-in decision [:selection-law :applied])))))

(defn abstention?
  "True when DECISION is a typed abstention (a readiness state, not an error)."
  [decision]
  (and (map? decision) (= :abstained (keyword-value (:status decision)))))

(defn first-acting-pattern
  "The enacted step of a cascade candidate: the first element of its
   :precedence (same rule as futon2.aif.policy/cascade-first-action and
   futon2.aif.decision-gate)."
  [action]
  (if (and (map? action) (seq (:precedence action)))
    (first (:precedence action))
    (if (map? action) (:type action) action)))

(defn enacted-step
  "The step a cascade DECISION enacts (first of the chosen candidate's
   :precedence); nil for abstentions."
  [decision]
  (when (cascade-decision? decision)
    (first-acting-pattern (:action decision))))

(defn decision-target
  "Display target of a cascade decision: the candidate's :cascade-id, falling
   back to the enacted step's own :target."
  [decision]
  (when (cascade-decision? decision)
    (let [action (:action decision) step (first-acting-pattern action)]
      (or (:cascade-id action) (:target step) (:target action)))))

(defn posterior-marginals
  "The decision's recorded cascade posterior, marginalised over each
   candidate's first acting pattern, best-first:
   [{:step … :posterior-mass … :rank 1…} …]. Presentation only — never a
   re-selection. When the posterior's candidate keys are unreadable (the
   served snapshot stringifies non-keyword map keys), fall back to the
   decision's recorded :softmax-weights — the same first-acting-pattern
   marginal map emitted by select-action-cascades."
  [decision]
  (let [posterior (get-in decision [:selection-law :posterior])
        weights (or (get-in decision [:selection-law :softmax-weights])
                    (:softmax-weights decision))
        marginal-map (cond
                       (and (map? posterior)
                            (every? #(or (map? %) (keyword? %)) (keys posterior)))
                       (reduce (fn [m [candidate p]]
                                 (update m (first-acting-pattern candidate)
                                         (fnil + 0.0) p))
                               {} posterior)
                       (map? weights) weights
                       :else nil)]
    (when marginal-map
      (->> marginal-map
           (sort-by (fn [[_ p]] (- p)))
           (mapv (fn [i [step p]]
                   {:step step :posterior-mass p :rank (inc i)})
                 (range))))))

(defn- abstention-projection
  [decision algorithm-kw]
  {:status :abstained-readiness
   :algorithm algorithm-kw
   :recommendation nil
   :refusals (:refusals decision)
   :refusals-by-kind
   (into (sorted-map)
         (group-by (comp keyword-value :kind) (:refusals decision)))
   :selection-boundary
   {:source :judgement.decision
    :recomputed? false
    :readiness true
    :operator-override-required? false}
   :actuation
   {:status :withheld-selector-abstained
    :authorized? false
    :executed? false}})

(defn project
  "Display `judgement.decision` unchanged: a cascade decision as its target,
   the enacted first step, the posterior mass and β with its status; a typed
   abstention as its refusals grouped by kind (a readiness state). A missing
   or inadmissible decision is a system-readiness failure, not an invitation
   for this presentation layer to choose another action."
  ([judgement] (project judgement {}))
  ([judgement _opts]
   (let [judgement (walk/keywordize-keys judgement)
         decision (value judgement :decision)
         algorithm-kw algorithm]
     (cond
       (abstention? decision)
       (abstention-projection decision algorithm-kw)

       (cascade-decision? decision)
       (let [step (enacted-step decision)
             action (:action decision)]
         {:status :recommendation-issued
          :algorithm algorithm-kw
          :recommendation
          (merge (if (map? step) step {:type step})
                 {:source :judgement.decision
                  :cascade-id (:cascade-id action)
                  :enacted-step step
                  :target (decision-target decision)
                  :beta (:beta decision)
                  :posterior-mass (:chosen-action-mass decision)
                  :selection-law-applied :cascade-selection-posterior
                  :recommendation-authority :live
                  :live-selection? true
                  :advisory? false
                  :requires-operator-override? false})
          :posterior (posterior-marginals decision)
          :selection-boundary
          {:source :judgement.decision
           :recomputed? false
           :operator-override-required? false
           :actuation-owner :downstream-act-gate}
          :actuation
          {:status :pending-downstream-gates
           :authorized? false
           :executed? false}})

       :else
       {:status :authoritative-decision-unavailable
        :algorithm algorithm-kw
        :recommendation nil
        :selection-boundary
        {:source :judgement.decision
         :recomputed? false
         :failure :missing-actionable-reason-bearing-decision
         :operator-override-required? false}
        :actuation
        {:status :withheld-system-readiness-failure
         :authorized? false
         :executed? false}}))))
