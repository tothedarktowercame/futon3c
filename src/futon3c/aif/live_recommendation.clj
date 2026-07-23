(ns futon3c.aif.live-recommendation
  "Pure projection separating a live strategic selection from policy comparison
   and downstream actuation authority.

   The War Machine recommendation is an authoritative, reason-bearing strategic
   selection, not an operator-approval request. A selector abstention is
   diagnostic evidence; it cannot turn the operator surface into an abstain
   page. Actual abstention belongs to the downstream act-gate, which this
   namespace never authorizes.")

(def algorithm :wm-live-recommendation/separated-v1)
(def default-ranking-limit 5)

(defn- value
  [m k]
  (or (get m k) (get m (name k))))

(defn- finite-number?
  [x]
  (and (number? x) (Double/isFinite (double x))))

(defn- action-of
  [entry]
  (value entry :action))

(defn- controller-score
  [entry]
  (some #(when (finite-number? %) (double %))
        [(value entry :controller-score)
         (value entry :G-total)
         (value entry :G-efe)]))

(defn- habit-bias
  [entry]
  (let [bias (value entry :habit-prior-bias)]
    (if (finite-number? bias) (double bias) 0.0)))

(defn- selection-gain
  [judgement]
  (let [state (value judgement :selection-gain)
        gain (if (map? state) (value state :selection-gain) state)]
    (if (and (finite-number? gain) (pos? (double gain)))
      (double gain)
      1.0)))

(defn- ranking-item
  [entry tau]
  (let [action (action-of entry)
        score (controller-score entry)
        bias (habit-bias entry)]
    {:rank (value entry :rank)
     :action action
     :type (value action :type)
     :target (or (value action :target)
                 (value action :target-class))
     :controller-score score
     :habit-prior-bias bias
     :selection-potential
     (when score (+ (- (/ score tau)) bias))}))

(defn- controller-ranking
  [ranked tau]
  (->> ranked
       (keep #(let [item (ranking-item % tau)]
                (when (:controller-score item) item)))
       (sort-by (juxt :controller-score :rank))
       vec))

(defn- habit-ranking
  [controller-items]
  (->> controller-items
       (sort-by (juxt (comp - :selection-potential)
                      :controller-score
                      :rank))
       vec))

(defn- actionable?
  [item]
  (not= :no-op (some-> (:type item) keyword)))

(defn- strategic-checkpoint
  [judgement controller-items]
  (let [checkpoint (value judgement :strategic-checkpoint)
        selector-report (some-> (value judgement :decision)
                                (value :strategic-memory))
        status (some-> (value checkpoint :status) keyword)
        recommendation (value checkpoint :recommendation)
        mission-ids (vec (or (value recommendation :mission-ids) []))
        mission-id (first mission-ids)
        memory-ids (vec (or (value recommendation :memory-ids) []))
        contributions (vec (or (value recommendation :relation-contributions)
                               []))
        candidate (first (filter #(= mission-id (:target %))
                                 controller-items))
        eligible? (and (= :advice-issued status)
                       (string? mission-id)
                       candidate
                       (seq memory-ids)
                       (seq contributions))]
    (if eligible?
      {:trace-present? true
       :influenced? true
       :status :eligible
       :reason :reviewed-strategic-checkpoint
       :mission-ids mission-ids
       :memory-ids memory-ids
       :relation-contributions contributions
       :candidate candidate}
      {:trace-present? (map? checkpoint)
       :influenced? false
       :status (if (map? checkpoint) :ineligible :absent)
       :reason (cond
                 (map? checkpoint) :strategic-checkpoint-incomplete
                 (map? selector-report)
                 (or (some-> (value selector-report :reason) keyword)
                     :selector-reports-no-strategic-influence)
                 :else :no-live-strategic-trace)
       :selector-report selector-report
       :mission-ids mission-ids
       :memory-ids memory-ids
       :relation-contributions contributions})))

(defn project
  "Build a live strategic recommendation and inspectable comparison trace.

   The controller head is the reduction-safe recommendation. A complete,
   reviewed strategic checkpoint may replace it, but an absent or incomplete
   checkpoint is recorded as non-influential rather than silently inferred.
   The selector decision is retained as a comparison diagnostic and
   never blocks the recommendation."
  ([judgement] (project judgement {}))
  ([judgement {:keys [ranking-limit]
               :or {ranking-limit default-ranking-limit}}]
   (let [ranked (vec (or (value judgement :ranked-actions)
                         (value judgement :admissible-actions)
                         []))
         gain (selection-gain judgement)
         tau (/ 1.0 gain)
         controller-items (controller-ranking ranked tau)
         habit-items (habit-ranking controller-items)
         controller-head (first (filter actionable? controller-items))
         habit-head (first (filter actionable? habit-items))
         strategic (strategic-checkpoint judgement controller-items)
         recommendation
         (or (:candidate strategic) controller-head)
         legacy-decision (value judgement :decision)
         legacy-action (value legacy-decision :action)
         legacy-reason (value legacy-decision :reason)]
     (when recommendation
       {:status :recommendation-issued
        :algorithm algorithm
        :recommendation
        (assoc recommendation
               :source (if (:influenced? strategic)
                         :reviewed-strategic-checkpoint
                         :controller-head)
               :recommendation-authority :live
               :live-selection? true
               :advisory? false
               :requires-operator-override? false)
        :rankings
        {:controller
         {:semantics :controller-score-ascending
          :winner (first controller-items)
          :items (vec (take ranking-limit controller-items))}
         :habit-adjusted
         {:semantics :lnE-minus-controller-score-over-temperature
          :temperature tau
          :selection-gain gain
          :winner (first habit-items)
          :items (vec (take ranking-limit habit-items))}
         :counterfactual
         {:semantics :habit-disabled-controller-ranking
          :winner (first controller-items)
          :items (vec (take ranking-limit controller-items))}}
        :strategic-memory strategic
        :selection-boundary
        {:legacy-decision
         {:action legacy-action
          :reason legacy-reason}
         :legacy-abstained? (= :abstain (some-> legacy-action keyword))
         :blocks-recommendation? false
         :operator-override-required? false}
        :actuation
        {:status :pending-downstream-gates
         :authorized? false
         :executed? false
         :gate-owner :downstream-act-gate
         :note "Strategic selection is live; only the downstream act-gate may abstain from enactment."}
        :comparison
        {:controller-winner-target (:target controller-head)
         :habit-winner-target (:target habit-head)
         :rankings-disagree?
         (not= (:target controller-head) (:target habit-head))
         :newer-strategic-memory-influenced?
         (:influenced? strategic)}}))))
