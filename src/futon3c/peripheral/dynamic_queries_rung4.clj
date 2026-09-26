(ns futon3c.peripheral.dynamic-queries-rung4
  "Dark k-step coupled propagation over a frozen admissible projection.

   Theta is an auditable search heuristic over the relation types already in
   the projection. It is not a posterior and cannot add candidates or affect
   live ordering."
  (:require [futon2.aif.mission-control-graph :as mission-graph]
            [futon3c.peripheral.dynamic-queries :as dynamic-queries]))

(def algorithm :dynamic-queries/coupled-propagation-v1)

(defn- finite-nonnegative-number?
  [value]
  (and (number? value)
       (Double/isFinite (double value))
       (not (neg? value))))

(defn- admitted-relations
  [projection]
  (->> (:candidates projection)
       (mapcat :support-relations)
       (map :relation)
       set))

(defn- normalized
  [weights ordered-keys]
  (let [total (reduce + 0.0 (map #(double (get weights % 0.0))
                                 ordered-keys))]
    (if (pos? total)
      (into (sorted-map)
            (map (fn [key]
                   [key (/ (double (get weights key 0.0)) total)]))
            ordered-keys)
      (let [uniform (/ 1.0 (double (count ordered-keys)))]
        (into (sorted-map) (map #(vector % uniform)) ordered-keys)))))

(defn- lower-bounded-simplex
  [weights ordered-relations exploration-floor]
  (let [n (count ordered-relations)
        residual (- 1.0 (* n exploration-floor))
        base (normalized weights ordered-relations)]
    (into (sorted-map)
          (map (fn [relation]
                 [relation
                  (+ exploration-floor
                     (* residual (double (get base relation))))]))
          ordered-relations)))

(defn- entropy
  [weights]
  (- (reduce
      +
      0.0
      (keep (fn [value]
              (let [p (double value)]
                (when (pos? p)
                  (* p (Math/log p)))))
            (vals weights)))))

(defn- effective-relation-weights
  [theta initial-weight-total]
  (into {} (map (fn [[relation mass]]
                  [relation (* initial-weight-total mass)]))
        theta))

(defn- contribution-rows
  [ranking]
  (->> (:ranked-candidates ranking)
       (mapcat
        (fn [{:keys [mission-id contributions]}]
          (map #(assoc % :mission-id mission-id) contributions)))
       vec))

(defn- relation-reinforcement
  [ranking theta ordered-relations]
  (let [leader (first (:ranked-candidates ranking))
        signal (reduce
                (fn [by-relation {:keys [relation contribution]}]
                  (update by-relation relation
                          (fnil + 0.0)
                          (double contribution)))
                {}
                (:contributions leader))
        reinforced
        (into {}
              (map (fn [relation]
                     [relation
                      (* (double (get theta relation 0.0))
                         (double (get signal relation 0.0)))]))
              ordered-relations)]
    (if (some pos? (vals reinforced))
      reinforced
      theta)))

(defn- challenge-reachability
  [challenge-memories contributions]
  (let [reached-patterns (set (map :control-pattern-id contributions))]
    (mapv
     (fn [{:keys [memory-id control-pattern-id
                  independently-witnessed?]}]
       {:memory-id memory-id
        :control-pattern-id control-pattern-id
        :independently-witnessed? (true? independently-witnessed?)
        :reachable? (contains? reached-patterns control-pattern-id)})
     challenge-memories)))

(defn- path-diversity
  [contributions]
  {:distinct-path-count
   (count (set (map (juxt :mission-id :control-pattern-id :relation)
                    contributions)))
   :active-path-count (count (filter #(pos? (:contribution %)) contributions))
   :distinct-pattern-count (count (set (map :control-pattern-id contributions)))
   :distinct-relation-count (count (set (map :relation contributions)))})

(defn- state-signature
  [x theta typed-ranking]
  {:x x :theta theta :typed-ranking typed-ranking})

(defn- validate-input!
  [{:keys [projection candidate-activation pattern-activation
           relation-weights challenge-memories k exploration-floor]
    :or {candidate-activation {}
         pattern-activation {}
         relation-weights {}
         challenge-memories []}
    :as input}]
  (let [relations (admitted-relations projection)
        mission-ids (set (map :mission-id (:candidates projection)))]
    (when-not (and (map? projection)
                   (vector? (:candidates projection))
                   (seq (:candidates projection))
                   (= (count mission-ids) (count (:candidates projection)))
                   (every? string? mission-ids)
                   (seq relations)
                   (every? mission-graph/relation-types relations)
                   (not (contains? relations :blocked-by-control))
                   (map? candidate-activation)
                   (every? mission-ids (keys candidate-activation))
                   (every? finite-nonnegative-number?
                           (vals candidate-activation))
                   (map? pattern-activation)
                   (every? mission-graph/valid-control-pattern-id?
                           (keys pattern-activation))
                   (every? finite-nonnegative-number?
                           (vals pattern-activation))
                   (map? relation-weights)
                   (every? mission-graph/relation-types
                           (keys relation-weights))
                   (every? finite-nonnegative-number?
                           (vals relation-weights))
                   (vector? challenge-memories)
                   (every?
                    (fn [{:keys [memory-id control-pattern-id
                                independently-witnessed?]}]
                      (and (string? memory-id)
                           (mission-graph/valid-control-pattern-id?
                            control-pattern-id)
                           (boolean? independently-witnessed?)))
                    challenge-memories)
                   (pos-int? k)
                   (contains? input :exploration-floor)
                   (finite-nonnegative-number? exploration-floor)
                   (<= (* (count relations) (double exploration-floor))
                       1.0))
      (throw (ex-info "invalid coupled propagation input"
                      {:input input
                       :admitted-relations relations})))
    (when-not
     (every?
      (set (mapcat (fn [candidate]
                     (map :control-pattern-id
                          (:support-relations candidate)))
                   (:candidates projection)))
      (map :control-pattern-id challenge-memories))
      (throw (ex-info "challenge memory is outside the admitted subgraph"
                      {:challenge-memories challenge-memories})))
    relations))

(defn coupled-propagation
  "Iterate the coupled Phi/Psi dark-ranking updates for at most `k` steps.

   `exploration-floor` is mandatory. Zero is accepted only as the explicit
   floor-off ablation. `challenge-memories` names frozen audit memories and
   their admitted patterns; reachability is reported at every step."
  [{:keys [projection candidate-activation pattern-activation
           relation-weights challenge-memories k exploration-floor query]
    :or {candidate-activation {}
         pattern-activation {}
         relation-weights {}
         challenge-memories []}
    :as input}]
  (let [relations (validate-input! input)
        ordered-relations (vec (sort relations))
        effective-initial
        (into {}
              (map (fn [relation]
                     [relation
                      (double (get relation-weights relation 1.0))]))
              ordered-relations)
        initial-weight-total (reduce + 0.0 (vals effective-initial))
        _ (when-not (pos? initial-weight-total)
            (throw (ex-info "admitted relation weights have zero total"
                            {:relation-weights relation-weights
                             :admitted-relations relations})))
        theta-0
        (lower-bounded-simplex
         effective-initial ordered-relations (double exploration-floor))
        rung1
        (dynamic-queries/fixed-typed-ranking
         projection
         {:candidate-activation candidate-activation
          :pattern-activation pattern-activation
          :relation-weights relation-weights})
        control-rankings
        {:endpoint (:control-ranking rung1)
         :rung1-typed (:typed-ranking rung1)}
        candidate-set (set (:control-ranking rung1))]
    (loop [step 1
           x candidate-activation
           theta theta-0
           seen #{}
           trace []]
      (let [effective-weights
            (effective-relation-weights theta initial-weight-total)
            ranking
            (dynamic-queries/fixed-typed-ranking
             projection
             {:candidate-activation x
              :pattern-activation pattern-activation
              :relation-weights effective-weights})
            typed-ranking (:typed-ranking ranking)
            scores (get-in ranking [:state :x-t+1])
            x-next (normalized scores (:control-ranking rung1))
            reinforced
            (relation-reinforcement ranking theta ordered-relations)
            theta-next
            (lower-bounded-simplex
             reinforced ordered-relations (double exploration-floor))
            contributions (contribution-rows ranking)
            reachability
            (challenge-reachability challenge-memories contributions)
            signature (state-signature x-next theta-next typed-ranking)
            current-signature (state-signature x theta typed-ranking)
            termination
            (cond
              (= signature current-signature) :fixed-point
              (contains? seen signature) :cycle
              (= step k) :budget-exhausted
              :else nil)
            row
            {:step step
             :step-budget {:initial k
                           :spent step
                           :remaining (- k step)}
             :typed-ranking typed-ranking
             :contributions contributions
             :x x-next
             :theta theta
             :theta-next theta-next
             :x-entropy (entropy x-next)
             :theta-entropy (entropy theta-next)
             :path-diversity (path-diversity contributions)
             :challenge-memory-reachability reachability
             :challenge-reachable?
             (boolean
              (and (seq reachability)
                   (every? #(and (:independently-witnessed? %)
                                 (:reachable? %))
                           reachability)))
             :control-rankings control-rankings
             :termination termination}
            trace-next (conj trace row)]
        (if termination
          {:status :dark
           :algorithm algorithm
           :query query
           :typed-ranking typed-ranking
           :ranked-candidates (:ranked-candidates ranking)
           :per-step-trace trace-next
           :termination termination
           :steps-executed step
           :budget {:initial k
                    :spent step
                    :remaining (- k step)}
           :exploration-floor (double exploration-floor)
           :control-rankings control-rankings
           :candidate-set-preserved?
           (= candidate-set (set typed-ranking))
           :selected-mission nil
           :live-ordering-changed? false
           :theta-semantics :search-heuristic-not-posterior
           :calibration-gate {:minimum-live-transitions 20
                              :promoted? false
                              :unchanged? true}}
          (recur (inc step)
                 x-next
                 theta-next
                 (conj seen signature)
                 trace-next))))))
