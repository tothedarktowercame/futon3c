(ns futon3c.peripheral.dynamic-queries
  "Pure dark ranking experiments over an already-admissible memory projection.

   This namespace is downstream of the Phase-4 domain/lifecycle/witness gates.
   It cannot add candidates: it only returns a control order and an explained
   typed order over the exact candidate set supplied by the projection."
  (:require [futon2.aif.mission-control-graph :as mission-graph]))

(def algorithm :dynamic-queries/fixed-typed-re-ranker-v1)
(def refinement-algorithm
  :dynamic-queries/budgeted-facet-refinement-v1)

(defn- finite-nonnegative-number?
  [value]
  (and (number? value)
       (Double/isFinite (double value))
       (not (neg? value))))

(defn- validate-weight-map
  [label weights allowed-keys]
  (when-not (and (map? weights)
                 (every? allowed-keys (keys weights))
                 (every? finite-nonnegative-number? (vals weights)))
    (throw (ex-info (str "invalid " label)
                    {:label label
                     :weights weights
                     :allowed-keys allowed-keys})))
  weights)

(defn- validate-projection
  [projection]
  (let [candidates (:candidates projection)
        mission-ids (mapv :mission-id candidates)]
    (when-not (and (map? projection)
                   (vector? candidates)
                   (every? string? mission-ids)
                   (= (count mission-ids) (count (set mission-ids)))
                   (every? #(vector? (:support-relations %)) candidates))
      (throw (ex-info "invalid admissible candidate projection"
                      {:projection projection})))
    projection))

(defn- relation-paths
  [candidate]
  (->> (:support-relations candidate)
       (map
        (fn [{:keys [control-pattern-id relation memory-ids] :as support}]
          (when-not (and (mission-graph/valid-control-pattern-id?
                          control-pattern-id)
                         (contains? mission-graph/relation-types relation)
                         (not= :blocked-by-control relation)
                         (vector? memory-ids)
                         (every? string? memory-ids))
            (throw (ex-info "invalid admitted support relation"
                            {:mission-id (:mission-id candidate)
                             :support-relation support})))
          {:control-pattern-id control-pattern-id
           :relation relation
           :memory-ids memory-ids}))
       (group-by (juxt :control-pattern-id :relation))
       (map
        (fn [[[pattern-id relation] paths]]
          {:control-pattern-id pattern-id
           :relation relation
           :memory-ids (->> paths (mapcat :memory-ids) distinct sort vec)}))
       (sort-by (juxt :control-pattern-id :relation))
       vec))

(defn- rank-candidate
  [candidate candidate-activation pattern-activation relation-weights]
  (let [mission-id (:mission-id candidate)
        prior (double (get candidate-activation mission-id 0.0))
        contributions
        (mapv
         (fn [{:keys [control-pattern-id relation] :as path}]
           (let [pattern-weight
                 (double (get pattern-activation control-pattern-id 1.0))
                 relation-weight
                 (double (get relation-weights relation 1.0))]
             (assoc path
                    :pattern-activation pattern-weight
                    :relation-weight relation-weight
                    :contribution (* pattern-weight relation-weight))))
         (relation-paths candidate))
        propagated (reduce + 0.0 (map :contribution contributions))]
    {:mission-id mission-id
     :prior-activation prior
     :propagated-activation propagated
     :score (+ prior propagated)
     :contributions contributions}))

(defn fixed-typed-ranking
  "Perform one fixed typed propagation step over an admissible projection.

   Options:
   - :candidate-activation — x_t, keyed by admitted mission id (default 0);
   - :pattern-activation — theta_t, keyed by p4ng pattern id (default 1);
   - :relation-weights — fixed weights keyed by the existing relation enum
     (default 1);
   - :facet-resolution — an audit label only.

   Each distinct [pattern relation] path contributes once. Attached memory
   multiplicity is reported but never multiplied into the score."
  ([projection]
   (fixed-typed-ranking projection {}))
  ([projection {:keys [candidate-activation pattern-activation
                       relation-weights facet-resolution]
                :or {candidate-activation {}
                     pattern-activation {}
                     relation-weights {}
                     facet-resolution :phase4-admissible-subgraph}}]
   (validate-projection projection)
   (let [candidates (:candidates projection)
         control-ranking (mapv :mission-id candidates)
         candidate-set (set control-ranking)
         _ (validate-weight-map
            "candidate activation" candidate-activation candidate-set)
         _ (validate-weight-map
            "relation weights" relation-weights mission-graph/relation-types)
         _ (when-not (and (map? pattern-activation)
                          (every? mission-graph/valid-control-pattern-id?
                                  (keys pattern-activation))
                          (every? finite-nonnegative-number?
                                  (vals pattern-activation)))
             (throw (ex-info "invalid pattern activation"
                             {:pattern-activation pattern-activation})))
         control-index (zipmap control-ranking (range))
         scored (mapv #(rank-candidate
                        % candidate-activation pattern-activation
                        relation-weights)
                      candidates)
         ranked
         (->> scored
              (sort-by
               (fn [{:keys [mission-id score]}]
                 [(- score)
                  (get control-index mission-id)
                  mission-id]))
              vec)
         typed-ranking (mapv :mission-id ranked)]
     {:status :dark
      :algorithm algorithm
      :state {:x-t candidate-activation
              :x-t+1 (into {} (map (juxt :mission-id :score)) ranked)
              :theta-t pattern-activation
              :facet-resolution facet-resolution
              :steps 1}
      :control-ranking control-ranking
      :typed-ranking typed-ranking
      :ranked-candidates ranked
      :candidate-set-preserved?
      (= (set control-ranking) (set typed-ranking))
      :live-ordering-changed? false})))

(defn- information-row
  [pattern-id {:keys [cost prior-entropy outcomes] :as model}]
  (when-not (and (map? model)
                 (integer? cost)
                 (pos? cost)
                 (finite-nonnegative-number? prior-entropy)
                 (vector? outcomes)
                 (seq outcomes)
                 (= (count outcomes)
                    (count (set (map :label outcomes))))
                 (every?
                  (fn [{:keys [label probability posterior-entropy]}]
                    (and (some? label)
                         (finite-nonnegative-number? probability)
                         (<= (double probability) 1.0)
                         (finite-nonnegative-number? posterior-entropy)))
                  outcomes))
    (throw (ex-info "invalid facet information model"
                    {:pattern-id pattern-id :model model})))
  (let [probability-sum (reduce + 0.0 (map :probability outcomes))
        _ (when (> (Math/abs (- probability-sum 1.0)) 1.0e-9)
            (throw (ex-info "facet outcome probabilities must sum to one"
                            {:pattern-id pattern-id
                             :probability-sum probability-sum})))
        expected-posterior
        (reduce + 0.0
                (map #(* (double (:probability %))
                         (double (:posterior-entropy %)))
                     outcomes))
        gain (- (double prior-entropy) expected-posterior)
        _ (when (< gain -1.0e-9)
            (throw (ex-info "facet model has negative expected information gain"
                            {:pattern-id pattern-id
                             :prior-entropy prior-entropy
                             :expected-posterior-entropy expected-posterior})))]
    {:pattern-id pattern-id
     :cost cost
     :prior-entropy (double prior-entropy)
     :expected-posterior-entropy expected-posterior
     :expected-information-gain (max 0.0 gain)
     :gain-per-cost (/ (max 0.0 gain) (double cost))
     :outcomes outcomes}))

(defn budgeted-facet-plan
  "Plan a coarse-to-fine traversal over a Phase-5 control-pattern cascade.

   CASCADE supplies the existing :shown and :semilattice/:descent structure.
   Children become eligible only after every parent was expanded and every
   parent edge has a witnessed transition warrant. INFORMATION-MODELS supplies
   explicit prior/outcome entropies and integer query costs per shown pattern;
   no memory count is used as evidence or as a score.

   The planner is greedy over expected-information-gain / cost. Its purpose is
   a deterministic, auditable dark policy, not a learned or live selector."
  [{:keys [cascade transition-warrants information-models budget]}]
  (let [shown (:shown cascade)
        descent (get-in cascade [:semilattice :descent])
        shown-set (set shown)
        _ (when-not (and (vector? shown)
                         (seq shown)
                         (= (count shown) (count shown-set))
                         (every? mission-graph/valid-control-pattern-id? shown)
                         (vector? descent)
                         (every? #(and (vector? %)
                                       (= 2 (count %))
                                       (every? shown-set %))
                                 descent)
                         (vector? transition-warrants)
                         (every?
                          (fn [{:keys [from to status provenance]}]
                            (and (contains? shown-set from)
                                 (contains? shown-set to)
                                 (contains?
                                  #{:witnessed :proposed
                                    :challenged :retracted}
                                  status)
                                 (vector? provenance)
                                 (seq provenance)))
                          transition-warrants)
                         (map? information-models)
                         (= shown-set (set (keys information-models)))
                         (integer? budget)
                         (not (neg? budget)))
            (throw (ex-info "invalid budgeted facet refinement input"
                            {:cascade cascade
                             :transition-warrants transition-warrants
                             :information-model-keys
                             (when (map? information-models)
                               (set (keys information-models)))
                             :budget budget})))
        information
        (into {} (map (fn [pattern-id]
                        [pattern-id
                         (information-row
                          pattern-id
                          (get information-models pattern-id))])
                      shown))
        shown-index (zipmap shown (range))
        parents
        (reduce (fn [by-child [parent child]]
                  (update by-child child (fnil conj []) parent))
                (zipmap shown (repeat []))
                descent)
        transition-by-edge
        (group-by (juxt :from :to) transition-warrants)
        witnessed-warrants
        (fn [parent child]
          (->> (get transition-by-edge [parent child])
               (filter #(= :witnessed (:status %)))
               (mapv #(select-keys
                       % [:from :to :status :provenance]))))
        eligible?
        (fn [selected pattern-id]
          (every? (fn [parent]
                    (and (contains? selected parent)
                         (seq (witnessed-warrants parent pattern-id))))
                  (get parents pattern-id)))
        choose
        (fn [selected remaining]
          (->> shown
               (remove selected)
               (filter #(eligible? selected %))
               (filter #(<= (:cost (get information %)) remaining))
               (sort-by
                (fn [pattern-id]
                  (let [{:keys [gain-per-cost expected-information-gain]}
                        (get information pattern-id)]
                    [(- gain-per-cost)
                     (- expected-information-gain)
                     (get shown-index pattern-id)])))
               first))]
    (loop [selected #{}
           trace []
           remaining budget]
      (if-let [pattern-id (choose selected remaining)]
        (let [{:keys [cost] :as info} (get information pattern-id)
              pattern-parents (vec (sort (get parents pattern-id)))
              warrants (->> pattern-parents
                            (mapcat #(witnessed-warrants % pattern-id))
                            vec)]
          (recur
           (conj selected pattern-id)
           (conj trace
                 (merge
                  info
                  {:step (count trace)
                   :parent-pattern-ids pattern-parents
                   :transition-warrants warrants
                   :remaining-budget-before remaining
                   :remaining-budget-after (- remaining cost)}))
           (- remaining cost)))
        (let [selected-order (mapv :pattern-id trace)
              unexpanded
              (->> shown
                   (remove selected)
                   (mapv
                    (fn [pattern-id]
                      (let [pattern-parents (get parents pattern-id)
                            missing-warrants
                            (->> pattern-parents
                                 (filter #(empty?
                                           (witnessed-warrants
                                            % pattern-id)))
                                 sort vec)
                            unexpanded-parents
                            (->> pattern-parents
                                 (remove selected)
                                 sort vec)
                            cost (:cost (get information pattern-id))]
                        {:pattern-id pattern-id
                         :cost cost
                         :reason
                         (cond
                           (seq missing-warrants)
                           :missing-transition-warrant

                           (seq unexpanded-parents)
                           :ancestor-not-expanded

                           (> cost remaining)
                           :budget-exhausted

                           :else
                           :not-selected)
                         :missing-warrant-parent-ids missing-warrants
                         :unexpanded-parent-ids unexpanded-parents}))))]
          {:status :dark-plan
           :algorithm refinement-algorithm
           :budget {:initial budget
                    :spent (- budget remaining)
                    :remaining remaining}
           :selected-patterns selected-order
           :selection-trace trace
           :unexpanded-patterns unexpanded
           :path-diversity
           {:distinct-pattern-count (count selected-order)
            :distinct-transition-count
            (count
             (set
              (mapcat
               (fn [row]
                 (map (juxt :from :to)
                      (:transition-warrants row)))
               trace)))
            :distinct-root-count
            (count (filter #(empty? (get parents %)) selected-order))}
           :evidence-counting
           :outcome-model-not-memory-multiplicity
           :selected-mission nil
           :live-ordering-changed? false})))))
