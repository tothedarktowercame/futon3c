(ns futon3c.peripheral.dynamic-queries
  "Pure dark ranking experiments over an already-admissible memory projection.

   This namespace is downstream of the Phase-4 domain/lifecycle/witness gates.
   It cannot add candidates: it only returns a control order and an explained
   typed order over the exact candidate set supplied by the projection."
  (:require [futon2.aif.mission-control-graph :as mission-graph]))

(def algorithm :dynamic-queries/fixed-typed-re-ranker-v1)

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
