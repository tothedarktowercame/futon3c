(ns futon3c.peripheral.strategic-outcomes
  "Phase-6 dark outcome modelling and factor ablations.

   Executable support is supplied by the Phase-4/5 admissible projection.
   Outcome probabilities are learned only from independently witnessed
   transitions. Centrality and the current additive controller remain named
   engineering potentials; they are never relabelled as probabilities."
  (:require [clojure.set :as set]
            [futon3c.peripheral.dynamic-queries :as dynamic-queries]))

(def algorithm :strategic-outcomes/dark-ablation-v1)
(def operator-update-algorithm
  :dynamic-queries/outcome-conditioned-operator-update-v1)
(def default-prior {:alpha 1.0 :beta 1.0})

(defn- finite-number?
  [value]
  (and (number? value) (Double/isFinite (double value))))

(defn- clamp
  [low high value]
  (max low (min high value)))

(defn- validate-transition
  [{:keys [transition-id mission-id outcome witness-status witness-id]
    :as transition}]
  (when-not
   (and (string? transition-id)
        (not-empty transition-id)
        (string? mission-id)
        (not-empty mission-id)
        (contains? #{:success :failure} outcome)
        (= :independently-witnessed witness-status)
        (string? witness-id)
        (not-empty witness-id))
    (throw (ex-info "invalid independently witnessed strategic transition"
                    {:transition transition})))
  transition)

(defn fit-outcome-model
  "Fit independent beta-binomial mission outcome estimates.

   The beta prior is declared and remains visible. These probabilities concern
   witnessed useful progress only; they do not encode executable support,
   proposal potential, centrality, habit, blocking, or Rung-3 entropy."
  ([transitions]
   (fit-outcome-model transitions {}))
  ([transitions {:keys [prior min-observations]
                 :or {prior default-prior min-observations 3}}]
   (let [{:keys [alpha beta]} prior
         transitions (mapv validate-transition transitions)
         ids (mapv :transition-id transitions)
         _ (when-not (and (finite-number? alpha) (pos? alpha)
                          (finite-number? beta) (pos? beta)
                          (integer? min-observations)
                          (pos? min-observations)
                          (= (count ids) (count (set ids))))
             (throw (ex-info "invalid strategic outcome model input"
                             {:prior prior
                              :min-observations min-observations
                              :transition-ids ids})))
         summarize
         (fn [rows]
           (let [n (count rows)
                 successes (count (filter #(= :success (:outcome %)) rows))
                 failures (- n successes)]
             {:observation-count n
              :success-count successes
              :failure-count failures
              :posterior-alpha (+ (double alpha) successes)
              :posterior-beta (+ (double beta) failures)
              :outcome-probability
              (/ (+ (double alpha) successes)
                 (+ (double alpha) (double beta) n))
              :transition-ids (mapv :transition-id rows)
              :witness-ids (->> rows (map :witness-id) distinct sort vec)}))]
     {:algorithm :strategic-outcomes/beta-binomial-v1
      :semantics :probability-of-independently-witnessed-useful-progress
      :prior {:alpha (double alpha) :beta (double beta)}
      :min-observations min-observations
      :mission-estimates
      (into {}
            (map (fn [[mission-id rows]]
                   [mission-id (summarize rows)]))
            (group-by :mission-id transitions))
      :global-estimate (summarize transitions)
      :training-transition-count (count transitions)
      :rung3-entropy-consumed? false
      :support-consumed-as-outcome? false})))

(defn- wilson-interval
  [successes n]
  (when (pos? n)
    (let [z 1.959963984540054
          z2 (* z z)
          p (/ (double successes) n)
          denominator (+ 1.0 (/ z2 n))
          centre (/ (+ p (/ z2 (* 2.0 n))) denominator)
          margin
          (/ (* z
                (Math/sqrt
                 (+ (/ (* p (- 1.0 p)) n)
                    (/ z2 (* 4.0 n n)))))
             denominator)]
      {:method :wilson-observed-rate-95
       :lower (clamp 0.0 1.0 (- centre margin))
       :upper (clamp 0.0 1.0 (+ centre margin))})))

(defn predict-outcome
  "Return a mission estimate or an explicit abstention for unseen/thin data."
  [model mission-id]
  (if-let [{:keys [observation-count success-count] :as estimate}
           (get-in model [:mission-estimates mission-id])]
    (assoc estimate
           :mission-id mission-id
           :uncertainty-interval
           (wilson-interval success-count observation-count)
           :abstain? (< observation-count (:min-observations model))
           :abstention-reason
           (when (< observation-count (:min-observations model))
             :insufficient-independent-outcomes))
    {:mission-id mission-id
     :observation-count 0
     :outcome-probability nil
     :uncertainty-interval nil
     :abstain? true
     :abstention-reason :unknown-mission}))

(defn- normalize-support
  [ranked]
  (let [maximum (reduce max 0.0 (map :score ranked))]
    (into {}
          (map (fn [{:keys [mission-id score contributions]}]
                 [mission-id
                  {:raw-score score
                   :score (if (pos? maximum) (/ score maximum) 0.0)
                   :relation-contributions contributions}]))
          ranked)))

(defn- rank-scores
  [control-order candidates score-fn]
  (let [control-index (zipmap control-order (range))]
    (->> candidates
         (mapv score-fn)
         (sort-by (fn [{:keys [mission-id score]}]
                    [(- score)
                     (get control-index mission-id)
                     mission-id]))
         vec)))

(defn- factor-row
  [factors mission-id]
  (let [{:keys [central strategic phase-doable] :as row}
        (get factors mission-id)]
    (when-not
     (and (= #{:central :strategic :phase-doable} (set (keys row)))
          (every? #(and (finite-number? %) (<= 0.0 % 1.0))
                  [central strategic phase-doable]))
      (throw (ex-info "invalid current additive factor row"
                      {:mission-id mission-id :factors row})))
    row))

(defn rank-judgement
  "Run all preregistered dark comparators for one judgement.

   Support is recalculated from the admissible projection. When the outcome
   model abstains, the outcome comparators fall back to support and expose that
   fact; missing probability is never coerced to zero."
  [projection outcome-model judgement]
  (let [candidate-ids (mapv :mission-id (:candidates projection))
        support-ranking
        (dynamic-queries/fixed-typed-ranking
         projection
         {:pattern-activation (:pattern-activation judgement)
          :relation-weights (:relation-weights judgement)
          :facet-resolution :phase6-held-out-judgement})
        support (normalize-support (:ranked-candidates support-ranking))
        outcomes (into {}
                       (map (fn [mission-id]
                              [mission-id
                               (predict-outcome outcome-model mission-id)]))
                       candidate-ids)
        factors (:current-additive-factors judgement)
        centrality (into {}
                         (map (fn [mission-id]
                                [mission-id
                                 (:central (factor-row factors mission-id))]))
                         candidate-ids)
        build-row
        (fn [model-id mission-id score components]
          {:mission-id mission-id
           :score score
           :model-id model-id
           :components components
           :memory-ids
           (->> (get-in support [mission-id :relation-contributions])
                (mapcat :memory-ids) distinct sort vec)
           :outcome-abstained?
           (:abstain? (get outcomes mission-id))
           :outcome-required?
           (contains? #{:support+outcome
                        :support+outcome+centrality}
                      model-id)})
        rank-model
        (fn [model-id score-and-components]
          (rank-scores
           candidate-ids candidate-ids
           (fn [mission-id]
             (let [[score components]
                   (score-and-components mission-id)]
               (build-row model-id mission-id score components)))))
        rankings
        {:current-additive
         (rank-model
          :current-additive
          (fn [mission-id]
            (let [{:keys [central strategic phase-doable]}
                  (factor-row factors mission-id)]
              [(+ (* 0.25 central)
                  (* 0.45 strategic)
                  (* 0.30 phase-doable))
               {:central {:value central :weight 0.25
                          :semantics :engineering-potential}
                :strategic {:value strategic :weight 0.45
                            :semantics :engineering-potential}
                :phase-doable {:value phase-doable :weight 0.30
                               :semantics :engineering-potential}}])))

         :direct-support
         (rank-model
          :direct-support
          (fn [mission-id]
            (let [value (get-in support [mission-id :score])]
              [value
               {:support {:value value :weight 1.0
                          :semantics :executable-support-potential}}])))

         :direct-support+centrality
         (rank-model
          :direct-support+centrality
          (fn [mission-id]
            (let [support-value (get-in support [mission-id :score])
                  central-value (get centrality mission-id)]
              [(+ (* 0.8 support-value) (* 0.2 central-value))
               {:support {:value support-value :weight 0.8
                          :semantics :executable-support-potential}
                :centrality {:value central-value :weight 0.2
                             :semantics :engineering-potential}}])))

         :support+outcome
         (rank-model
          :support+outcome
          (fn [mission-id]
            (let [support-value (get-in support [mission-id :score])
                  outcome (get outcomes mission-id)
                  probability (:outcome-probability outcome)
                  applied? (not (:abstain? outcome))]
              [(if applied?
                 (+ (* 0.5 support-value) (* 0.5 probability))
                 support-value)
               {:support {:value support-value
                          :weight (if applied? 0.5 1.0)
                          :semantics :executable-support-potential}
                :outcome {:value probability
                          :weight (if applied? 0.5 0.0)
                          :applied? applied?
                          :semantics (:semantics outcome-model)
                          :uncertainty-interval
                          (:uncertainty-interval outcome)
                          :abstention-reason
                          (:abstention-reason outcome)}}])))

         :support+outcome+centrality
         (rank-model
          :support+outcome+centrality
          (fn [mission-id]
            (let [support-value (get-in support [mission-id :score])
                  central-value (get centrality mission-id)
                  outcome (get outcomes mission-id)
                  probability (:outcome-probability outcome)
                  applied? (not (:abstain? outcome))]
              [(if applied?
                 (+ (* 0.4 support-value)
                    (* 0.4 probability)
                    (* 0.2 central-value))
                 (+ (* 0.8 support-value) (* 0.2 central-value)))
               {:support {:value support-value
                          :weight (if applied? 0.4 0.8)
                          :semantics :executable-support-potential}
                :outcome {:value probability
                          :weight (if applied? 0.4 0.0)
                          :applied? applied?
                          :semantics (:semantics outcome-model)
                          :uncertainty-interval
                          (:uncertainty-interval outcome)
                          :abstention-reason
                          (:abstention-reason outcome)}
                :centrality {:value central-value :weight 0.2
                             :semantics :engineering-potential}}])))}]
    {:judgement-id (:judgement-id judgement)
     :gold-mission-id (:gold-mission-id judgement)
     :gold-witness-id (:gold-witness-id judgement)
     :control-ranking candidate-ids
     :rankings rankings
     :counterfactual-additive-choice
     (get-in rankings [:current-additive 0 :mission-id])
     :selected-mission nil
     :live-ordering-changed? false}))

(defn outcome-conditioned-operator-update
  "Propose exactly one dark theta update from one witnessed outcome.

   The Phase-6 beta-binomial model is fitted before and after TRANSITION. For
   each pattern supporting that transition's admitted mission, theta is
   multiplied by the mission posterior-probability ratio. Other patterns are
   unchanged. The function then performs exactly one Rung-1 rerank and retains
   the pre-update typed ranking as counterfactual control.

   This is an operator-update proposal, never a live choice. Promotion requires
   both the explicit Phase-6 promotion gate and its minimum sample size."
  [projection training-transitions transition judgement
   {:keys [min-observations minimum-promotion-sample-size
           phase6-promotion phase6-outcome-evaluation]
    :or {min-observations 3
         minimum-promotion-sample-size 20}}]
  (let [transition (validate-transition transition)
        candidate-ids (set (map :mission-id (:candidates projection)))
        mission-id (:mission-id transition)
        _ (when-not (contains? candidate-ids mission-id)
            (throw (ex-info
                    "operator update mission is outside admissible projection"
                    {:mission-id mission-id
                     :admissible-candidate-ids candidate-ids})))
        before-model
        (fit-outcome-model
         training-transitions
         {:min-observations min-observations})
        after-model
        (fit-outcome-model
         (conj (vec training-transitions) transition)
         {:min-observations min-observations})
        before-outcome (predict-outcome before-model mission-id)
        after-outcome (predict-outcome after-model mission-id)
        candidate
        (first (filter #(= mission-id (:mission-id %))
                       (:candidates projection)))
        affected-patterns
        (->> (:support-relations candidate)
             (map :control-pattern-id)
             distinct sort vec)
        all-patterns
        (->> (:candidates projection)
             (mapcat :support-relations)
             (map :control-pattern-id)
             distinct sort vec)
        theta-before
        (into {}
              (map (fn [pattern-id]
                     [pattern-id
                      (double
                       (get (:pattern-activation judgement)
                            pattern-id 1.0))]))
              all-patterns)
        applicable?
        (and (seq affected-patterns)
             (number? (:outcome-probability before-outcome))
             (number? (:outcome-probability after-outcome))
             (not (:abstain? after-outcome)))
        posterior-ratio
        (when applicable?
          (/ (double (:outcome-probability after-outcome))
             (double (:outcome-probability before-outcome))))
        theta-after
        (if applicable?
          (reduce
           (fn [theta pattern-id]
             (update theta pattern-id * posterior-ratio))
           theta-before
           affected-patterns)
          theta-before)
        ranking-options
        {:relation-weights (:relation-weights judgement)
         :facet-resolution :rung2-outcome-update}
        before-ranking
        (dynamic-queries/fixed-typed-ranking
         projection
         (assoc ranking-options :pattern-activation theta-before))
        after-ranking
        (dynamic-queries/fixed-typed-ranking
         projection
         (assoc ranking-options :pattern-activation theta-after))
        before-top (first (:typed-ranking before-ranking))
        after-top (first (:typed-ranking after-ranking))
        after-top-outcome (predict-outcome after-model after-top)
        gold-mission-id (:gold-mission-id judgement)
        recovered?
        (and (string? gold-mission-id)
             (not= gold-mission-id before-top)
             (= gold-mission-id after-top))
        degraded?
        (and (string? gold-mission-id)
             (= gold-mission-id before-top)
             (not= gold-mission-id after-top))
        sample-count (:training-transition-count after-model)
        enough-data?
        (>= sample-count minimum-promotion-sample-size)
        phase6-advance? (true? (:advance? phase6-promotion))
        promotion-eligible? (and applicable?
                                 enough-data?
                                 phase6-advance?)
        memory-ids
        (->> (:support-relations candidate)
             (mapcat :memory-ids)
             distinct sort vec)
        update-rows
        (mapv
         (fn [pattern-id]
           {:pattern-id pattern-id
            :theta-before (get theta-before pattern-id)
            :posterior-ratio posterior-ratio
            :theta-after (get theta-after pattern-id)
            :mission-id mission-id
            :memory-ids memory-ids})
         affected-patterns)]
    {:status (if applicable? :dark-update-proposal :dark-abstention)
     :algorithm operator-update-algorithm
     :transition
     (select-keys transition
                  [:transition-id :mission-id :outcome
                   :witness-status :witness-id])
     :outcome-update
     {:semantics (:semantics before-model)
      :before (select-keys
               before-outcome
               [:mission-id :observation-count :success-count :failure-count
                :outcome-probability :uncertainty-interval :abstain?
                :abstention-reason])
      :after (select-keys
              after-outcome
              [:mission-id :observation-count :success-count :failure-count
               :outcome-probability :uncertainty-interval :abstain?
               :abstention-reason])
      :one-transition-consumed? true
      :rung3-entropy-consumed? false}
     :operator-update
     {:semantics
      :mission-posterior-ratio-rescaling-not-pattern-posterior
      :applied-to-dark-rerank? (boolean applicable?)
      :affected-pattern-ids affected-patterns
      :theta-before theta-before
      :theta-after theta-after
      :updates update-rows
      :abstention-reason
      (when-not applicable?
        (or (:abstention-reason after-outcome)
            :no-admitted-pattern-for-mission))}
     :rankings
     {:fixed-endpoint (:control-ranking before-ranking)
      :typed-before-outcome (:typed-ranking before-ranking)
      :typed-after-one-outcome (:typed-ranking after-ranking)}
     :candidate-set-preserved?
     (and (:candidate-set-preserved? before-ranking)
          (:candidate-set-preserved? after-ranking)
          (= (set (:typed-ranking before-ranking))
             (set (:typed-ranking after-ranking))))
     :evaluation
     {:gold-mission-id gold-mission-id
      :before-top before-top
      :after-top after-top
      :recovered-from-misleading-seed? (boolean recovered?)
      :degraded? (boolean degraded?)
      :unsupported-after-top?
      (boolean (:abstain? after-top-outcome))
      :phase6-outcome-evaluation phase6-outcome-evaluation}
     :promotion
     {:sample-count sample-count
      :minimum-sample-size minimum-promotion-sample-size
      :phase6-advance? phase6-advance?
      :promotion-eligible? promotion-eligible?
      :decision-reason
      (cond
        (not applicable?) :operator-update-abstained
        (not enough-data?) :exploratory-sample-too-small
        (not phase6-advance?) :phase6-calibration-gate-not-met
        :else :operator-review-required)}
     :counterfactual-ranking (:typed-ranking before-ranking)
     :selected-mission nil
     :live-ordering-changed? false}))

(defn- ordering
  [ranking]
  (mapv :mission-id ranking))

(defn- pairwise-agreement
  [left right]
  (let [left-index (zipmap left (range))
        right-index (zipmap right (range))
        ids (vec left)
        pairs (for [i (range (count ids))
                    j (range (inc i) (count ids))]
                [(ids i) (ids j)])
        agreements
        (count
         (filter
          (fn [[a b]]
            (= (< (left-index a) (left-index b))
               (< (right-index a) (right-index b))))
          pairs))]
    (if (seq pairs) (/ (double agreements) (count pairs)) 1.0)))

(defn- paired-normal-interval
  [differences]
  (let [n (count differences)
        mean (/ (reduce + 0.0 differences) n)
        variance
        (if (> n 1)
          (/ (reduce + 0.0
                     (map #(let [delta (- % mean)] (* delta delta))
                          differences))
             (dec n))
          0.0)
        margin (* 1.959963984540054
                  (/ (Math/sqrt variance) (Math/sqrt n)))]
    {:method :paired-normal-exploratory-95
     :estimate mean
     :lower (clamp -1.0 1.0 (- mean margin))
     :upper (clamp -1.0 1.0 (+ mean margin))
     :sample-size n}))

(defn- ranking-metrics
  [judgement-results model-id]
  (let [rows
        (mapv
         (fn [{:keys [gold-mission-id rankings]}]
           (let [baseline (ordering (:current-additive rankings))
                 ranking (ordering (get rankings model-id))]
             {:correct? (= gold-mission-id (first ranking))
              :baseline-correct?
              (= gold-mission-id (first baseline))
              :agreement (pairwise-agreement baseline ranking)
              :unsupported-top?
              (let [top (first (get rankings model-id))]
                (and (:outcome-required? top)
                     (:outcome-abstained? top)))}))
         judgement-results)
        n (count rows)
        accuracy (/ (count (filter :correct? rows)) (double n))
        baseline-accuracy
        (/ (count (filter :baseline-correct? rows)) (double n))
        differences
        (mapv #(- (if (:correct? %) 1.0 0.0)
                  (if (:baseline-correct? %) 1.0 0.0))
              rows)]
    {:top-choice-accuracy accuracy
     :accuracy-interval
     (assoc (wilson-interval (count (filter :correct? rows)) n)
            :sample-size n)
     :ranking-agreement-with-current-additive
     (/ (reduce + 0.0 (map :agreement rows)) n)
     :unsupported-answer-rate
     (/ (count (filter :unsupported-top? rows)) (double n))
     :paired-accuracy-difference-vs-current-additive
     (paired-normal-interval differences)
     :current-additive-accuracy baseline-accuracy}))

(defn- safe-log
  [probability]
  (Math/log (clamp 1.0e-12 (- 1.0 1.0e-12) probability)))

(defn- outcome-metrics
  [predictions]
  (let [scored (filterv (comp not :abstain?) predictions)
        n (count predictions)
        scored-n (count scored)
        losses
        (mapv
         (fn [{:keys [outcome probability]}]
           (let [y (if (= :success outcome) 1.0 0.0)
                 error (- probability y)]
             {:brier (* error error)
              :log-loss
              (- (+ (* y (safe-log probability))
                    (* (- 1.0 y) (safe-log (- 1.0 probability)))))}))
         scored)
        bins
        (->> scored
             (group-by #(if (< (:probability %) 0.5) :below-half
                            :at-least-half))
             (map
              (fn [[label rows]]
                (let [bin-count (count rows)
                      successes
                      (count (filter #(= :success (:outcome %)) rows))]
                  {:bin label
                   :count bin-count
                   :mean-prediction
                   (/ (reduce + 0.0 (map :probability rows)) bin-count)
                   :observed-success-rate
                   (/ (double successes) bin-count)
                   :observed-rate-interval
                   (wilson-interval successes bin-count)})))
             (sort-by :bin)
             vec)
        ece
        (if (pos? scored-n)
          (/ (reduce
              + 0.0
              (map #(* (:count %)
                       (Math/abs
                        (- (:mean-prediction %)
                           (:observed-success-rate %))))
                   bins))
             scored-n)
          nil)]
    {:sample-size n
     :scored-count scored-n
     :coverage (if (pos? n) (/ (double scored-n) n) 0.0)
     :abstention-rate (if (pos? n) (/ (- n scored-n) (double n)) 0.0)
     :brier-score
     (when (seq losses)
       (/ (reduce + 0.0 (map :brier losses)) scored-n))
     :log-loss
     (when (seq losses)
       (/ (reduce + 0.0 (map :log-loss losses)) scored-n))
     :calibration-error ece
     :calibration-bins bins}))

(defn- held-out-predictions
  [model rows]
  (mapv
   (fn [{:keys [mission-id outcome] :as row}]
     (let [prediction (predict-outcome model mission-id)]
       (assoc prediction
              :transition-id (:transition-id row)
              :outcome outcome
              :probability (:outcome-probability prediction))))
   rows))

(defn- baseline-predictions
  [model rows]
  (let [probability (get-in model [:global-estimate :outcome-probability])]
    (mapv #(assoc %
                  :probability probability
                  :abstain? false
                  :uncertainty-interval
                  (wilson-interval
                   (get-in model [:global-estimate :success-count])
                   (get-in model [:global-estimate :observation-count])))
          rows)))

(defn run-dark-ablation
  "Execute the frozen Phase-6 comparison without selecting a mission.

   FIXTURE contains preregistered training transitions, held-out outcomes, and
   held-out ranking judgements. The function refuses an unfrozen fixture and
   any overlap between training and held-out transition ids."
  [outer-result fixture]
  (let [started (System/nanoTime)
        {:keys [freeze-id frozen-at training-transitions held-out-outcomes
                judgements minimum-promotion-sample-size]} fixture
        training-transitions (mapv validate-transition training-transitions)
        held-out-outcomes (mapv validate-transition held-out-outcomes)
        train-ids (set (map :transition-id training-transitions))
        test-ids (set (map :transition-id held-out-outcomes))
        valid-judgements?
        (every?
         (fn [{:keys [judgement-id gold-mission-id gold-witness-id]}]
           (and (string? judgement-id) (not-empty judgement-id)
                (string? gold-mission-id) (not-empty gold-mission-id)
                (string? gold-witness-id) (not-empty gold-witness-id)))
         judgements)
        _ (when-not (and (string? freeze-id) (not-empty freeze-id)
                         (string? frozen-at) (not-empty frozen-at)
                         (vector? training-transitions)
                         (vector? held-out-outcomes)
                         (vector? judgements) (seq judgements)
                         valid-judgements?
                         (integer? minimum-promotion-sample-size)
                         (pos? minimum-promotion-sample-size)
                         (empty? (set/intersection train-ids test-ids)))
            (throw (ex-info "invalid or unfrozen Phase-6 fixture"
                            {:freeze-id freeze-id
                             :frozen-at frozen-at
                             :overlap (set/intersection train-ids test-ids)})))
        model (fit-outcome-model training-transitions
                                 {:min-observations
                                  (:min-outcome-observations fixture 3)})
        projection (:admissible-projection outer-result)
        candidate-ids (set (map :mission-id (:candidates projection)))
        blocked-ids (set (map :mission-id (:excluded-missions outer-result)))
        _ (when (seq (set/intersection candidate-ids blocked-ids))
            (throw (ex-info "blocked mission leaked into admissible projection"
                            {:leaked
                             (set/intersection candidate-ids blocked-ids)})))
        judgement-results
        (mapv #(rank-judgement projection model %) judgements)
        model-ids
        [:current-additive :direct-support :direct-support+centrality
         :support+outcome :support+outcome+centrality]
        ranking-evaluation
        (into {}
              (map (fn [model-id]
                     [model-id
                      (ranking-metrics judgement-results model-id)]))
              model-ids)
        predictions (held-out-predictions model held-out-outcomes)
        outcome-evaluation (outcome-metrics predictions)
        baseline-evaluation
        (outcome-metrics (baseline-predictions model held-out-outcomes))
        explained
        (for [judgement judgement-results
              model-id model-ids
              candidate (get-in judgement [:rankings model-id])]
          (and (seq (:components candidate))
               (vector? (:memory-ids candidate))))
        explanation-completeness
        (/ (count (filter true? explained)) (double (count explained)))
        without-centrality
        (get-in ranking-evaluation
                [:support+outcome :top-choice-accuracy])
        with-centrality
        (get-in ranking-evaluation
                [:support+outcome+centrality :top-choice-accuracy])
        paired-centrality
        (let [rows
              (mapv
               (fn [{:keys [gold-mission-id rankings]}]
                 (- (if (= gold-mission-id
                           (get-in rankings
                                   [:support+outcome 0 :mission-id]))
                      1.0 0.0)
                    (if (= gold-mission-id
                           (get-in rankings
                                   [:support+outcome+centrality
                                    0 :mission-id]))
                      1.0 0.0)))
               judgement-results)]
          (paired-normal-interval rows))
        misleading-seed-recovery
        (reduce
         (fn [summary {:keys [gold-mission-id rankings]}]
           (let [support-correct?
                 (= gold-mission-id
                    (get-in rankings [:direct-support 0 :mission-id]))
                 outcome-correct?
                 (= gold-mission-id
                    (get-in rankings [:support+outcome 0 :mission-id]))]
             (cond-> summary
               (and (not support-correct?) outcome-correct?)
               (update :recovered-count inc)

               (and support-correct? (not outcome-correct?))
               (update :degraded-count inc))))
         {:recovered-count 0 :degraded-count 0}
         judgement-results)
        enough-data?
        (>= (count held-out-outcomes) minimum-promotion-sample-size)
        outcome-better?
        (and (< (:brier-score outcome-evaluation)
                (:brier-score baseline-evaluation))
             (< (:log-loss outcome-evaluation)
                (:log-loss baseline-evaluation)))
        elapsed-ms (/ (- (System/nanoTime) started) 1000000.0)]
    {:status :dark
     :algorithm algorithm
     :fixture {:freeze-id freeze-id
               :frozen-at frozen-at
               :training-count (count training-transitions)
               :held-out-outcome-count (count held-out-outcomes)
               :judgement-count (count judgements)}
     :outcome-model model
     :factor-semantics
     {:admissible-support :typed-control-relation-potential
      :outcome (:semantics model)
      :centrality :engineering-potential
      :current-additive :engineering-potential
      :proposal-potential :not-modelled-in-phase6
      :habit :reserved-for-phase7
      :rung3-entropy :exploration-order-only}
     :held-out-predictions predictions
     :outcome-evaluation outcome-evaluation
     :global-rate-baseline-evaluation baseline-evaluation
     :ranking-evaluation ranking-evaluation
     :judgement-traces judgement-results
     :evidence-separation
     {:unknown-is-abstention?
      (= :unknown-mission
         (:abstention-reason
          (predict-outcome model "M-phase6-unseen")))
      :negative-is-observed-failure?
      (some pos? (map :failure-count
                      (vals (:mission-estimates model))))
      :admissible-candidate-ids candidate-ids
      :blocked-excluded-ids blocked-ids
      :blocked-disjoint-from-candidates?
      (empty? (set/intersection candidate-ids blocked-ids))
      :rung3-entropy-consumed-as-probability? false}
     :explanation-completeness explanation-completeness
     :latency-ms elapsed-ms
     :centrality-ablation
     {:without-centrality-top-choice-accuracy without-centrality
      :with-centrality-top-choice-accuracy with-centrality
      :paired-difference-without-minus-with paired-centrality
      :retire-centrality? false
      :decision-reason
      (if enough-data?
        :operator-review-required
        :exploratory-sample-too-small)}
     :misleading-seed-recovery misleading-seed-recovery
     :outcome-promotion
     {:beats-global-rate-baseline? outcome-better?
      :advance? false
      :decision-reason
      (cond
        (not enough-data?) :exploratory-sample-too-small
        (not outcome-better?) :does-not-beat-named-baseline
        :else :operator-review-required)}
     :rung3-trace
     {:reference (:rung3-trace-reference fixture)
      :entropy-role :exploration-order-only
      :entropy-consumed-as-calibrated-probability? false}
     :selected-mission nil
     :live-ordering-changed? false}))
