(ns futon3c.peripheral.strategic-cascade
  "Phase-5 dark outer cascades over Phase-4 memory/control projections.

   The module constructs a reason-bearing frontier, blockers, dependencies,
   and explicit holes. It never scores or selects a live mission. Optional
   dynamic-query comparison is instrumentation over the finished admissible
   frontier, not a source of admission."
  (:require [clojure.string :as str]
            [futon2.aif.mission-control-graph :as mission-graph]
            [futon3c.peripheral.dynamic-queries :as dynamic-queries]
            [futon3c.peripheral.wm-memory :as wm-memory]))

(def algorithm :strategic-cascade/outer-frontier-v1)

(defn- nonblank-string?
  [value]
  (and (string? value) (not (str/blank? value))))

(defn- validate-cascade
  [{:keys [shown semilattice policy-holes] :as cascade}]
  (let [descent (or (:descent semilattice) [])
        shown-set (set shown)]
    (when-not
     (and (map? cascade)
          (vector? shown)
          (seq shown)
          (= (count shown) (count shown-set))
          (every? mission-graph/valid-control-pattern-id? shown)
          (map? semilattice)
          (vector? descent)
          (every? #(and (vector? %) (= 2 (count %))
                        (every? shown-set %))
                  descent)
          (vector? (or policy-holes [])))
      (throw (ex-info "invalid outer control-pattern cascade"
                      {:cascade cascade})))
    (assoc cascade :policy-holes (vec (or policy-holes [])))))

(defn- validate-dependency
  [{:keys [mission-id depends-on status provenance] :as dependency}]
  (when-not (and (nonblank-string? mission-id)
                 (nonblank-string? depends-on)
                 (contains? #{:witnessed :proposed :challenged :retracted}
                            status)
                 (vector? provenance)
                 (seq provenance))
    (throw (ex-info "invalid strategic mission dependency"
                    {:dependency dependency})))
  dependency)

(defn- validate-transition
  [{:keys [from to status provenance] :as transition}]
  (when-not (and (mission-graph/valid-control-pattern-id? from)
                 (mission-graph/valid-control-pattern-id? to)
                 (contains? #{:witnessed :proposed :challenged :retracted}
                            status)
                 (vector? provenance)
                 (seq provenance))
    (throw (ex-info "invalid control-pattern transition warrant"
                    {:transition transition})))
  transition)

(defn- merge-candidates
  [candidates]
  (->> candidates
       (group-by :mission-id)
       (map
        (fn [[mission-id rows]]
          {:mission-id mission-id
           :candidate-source :outer-control-pattern-cascade
           :control-pattern-ids
           (->> rows (mapcat :control-pattern-ids) distinct sort vec)
           :support-relations
           (->> rows
                (mapcat :support-relations)
                (distinct)
                (sort-by (juxt :control-pattern-id :relation))
                vec)
           :memory-ids
           (->> rows (mapcat :memory-ids) distinct sort vec)}))
       (sort-by :mission-id)
       vec))

(defn- dependency-state
  [candidate-ids excluded-ids dependencies mission-id]
  (let [relevant (filter #(and (= mission-id (:mission-id %))
                               (= :witnessed (:status %)))
                         dependencies)
        ids (->> relevant (map :depends-on) distinct sort vec)
        blocked (filterv excluded-ids ids)
        missing (filterv #(and (not (candidate-ids %))
                               (not (excluded-ids %)))
                         ids)]
    {:dependency-ids ids
     :blocked-dependency-ids blocked
     :missing-dependency-ids missing
     :dependency-status (cond
                          (seq blocked) :blocked
                          (seq missing) :missing
                          :else :ready)
     :dependency-relations (mapv #(select-keys
                                   % [:depends-on :provenance])
                                 relevant)}))

(defn- mint-proposals
  [holes]
  (->> holes
       (filter #(nonblank-string? (:hole-signature %)))
       (group-by :hole-signature)
       (keep
        (fn [[signature repeated]]
          (let [witness-ids (->> repeated
                                 (mapcat :witness-ids)
                                 (filter nonblank-string?)
                                 distinct sort vec)
                memory-ids (->> repeated
                                (mapcat :memory-ids)
                                (filter nonblank-string?)
                                distinct sort vec)]
            (when (and (>= (count repeated) 2)
                       (>= (count witness-ids) 2)
                       (>= (count memory-ids) 2))
              {:pattern-id
               (str "p4ng/proposed/"
                    (-> signature
                        str/lower-case
                        (str/replace #"[^a-z0-9]+" "-")
                        (str/replace #"(^-|-$)" "")))
               :status :proposed
               :prior :unearned
               :promotion-eligible? false
               :hole-signature signature
               :witness-ids witness-ids
               :memory-ids memory-ids
               :source-hole-count (count repeated)}))))
       (sort-by :pattern-id)
       vec))

(defn outer-frontier
  "Execute each bounded cascade step and construct a dark strategic frontier.

   QUERY-STEP-FN receives [pattern-id remaining-budget] and must return the
   Phase-4 WM adapter result. The fixed cascade order is execution order only;
   set semantics and output ordering are deterministic."
  [{:keys [cascade query-step-fn dependencies transition-warrants budget]
    :or {dependencies [] transition-warrants []}}]
  (let [cascade (validate-cascade cascade)
        shown (:shown cascade)
        budget (or budget (count shown))
        _ (when-not (and (integer? budget) (pos? budget))
            (throw (ex-info "outer cascade budget must be positive"
                            {:budget budget})))
        _ (when-not (fn? query-step-fn)
            (throw (ex-info "outer cascade requires query-step-fn" {})))
        dependencies (mapv validate-dependency dependencies)
        transitions (mapv validate-transition transition-warrants)
        executed-patterns (vec (take budget shown))
        skipped-patterns (vec (drop budget shown))
        step-results
        (mapv (fn [index pattern-id]
                {:step index
                 :pattern-id pattern-id
                 :remaining-budget (- budget index)
                 :result (query-step-fn pattern-id (- budget index))})
              (range)
              executed-patterns)
        projections (mapv #(get-in % [:result :projection]) step-results)
        candidates (merge-candidates (mapcat :candidates projections))
        excluded (->> projections
                      (mapcat :excluded-missions)
                      (group-by :mission-id)
                      (map (fn [[mission-id rows]]
                             {:mission-id mission-id
                              :exclusion :witnessed-block
                              :blocking-relations
                              (->> rows
                                   (mapcat :blocking-relations)
                                   distinct
                                   (sort-by (juxt :control-pattern-id
                                                  :relation))
                                   vec)}))
                      (sort-by :mission-id)
                      vec)
        candidate-ids (set (map :mission-id candidates))
        excluded-ids (set (map :mission-id excluded))
        frontier
        (mapv
         (fn [candidate]
           (let [dependency
                 (dependency-state candidate-ids excluded-ids dependencies
                                   (:mission-id candidate))
                 ready? (= :ready (:dependency-status dependency))]
             (merge candidate dependency
                    {:frontier-status (if ready? :ready :held)
                     :mission-cascade
                     (when ready?
                       (conj (:dependency-ids dependency)
                             (:mission-id candidate)))
                     :reasons
                     {:support (:support-relations candidate)
                      :dependencies (:dependency-relations dependency)}})))
         candidates)
        no-result-holes
        (->> step-results
             (keep
              (fn [{:keys [step pattern-id result]}]
                (let [projection (:projection result)]
                  (when (and (empty? (:candidates projection))
                             (empty? (:excluded-missions projection)))
                    {:hole/type :no-witnessed-mission
                     :step step
                     :control-pattern-id pattern-id
                     :why "No admitted mission relation survived Phase 4"}))))
             vec)
        transition-by-edge
        (group-by (juxt :from :to) transitions)
        transition-holes
        (->> (get-in cascade [:semilattice :descent])
             (keep
              (fn [[from to :as edge]]
                (when-not (some #(= :witnessed (:status %))
                                (get transition-by-edge edge))
                  {:hole/type :missing-pattern-transition
                   :from from
                   :to to
                   :why "Cascade edge has no witnessed transition warrant"})))
             vec)
        dependency-holes
        (->> frontier
             (mapcat
              (fn [{:keys [mission-id blocked-dependency-ids
                           missing-dependency-ids]}]
                (concat
                 (map (fn [dependency-id]
                        {:hole/type :blocked-mission-dependency
                         :mission-id mission-id
                         :dependency-id dependency-id})
                      blocked-dependency-ids)
                 (map (fn [dependency-id]
                        {:hole/type :missing-mission-dependency
                         :mission-id mission-id
                         :dependency-id dependency-id})
                      missing-dependency-ids))))
             vec)
        budget-holes
        (mapv (fn [pattern-id]
                {:hole/type :budget-exhausted
                 :control-pattern-id pattern-id
                 :why "Pattern step was outside the explicit query budget"})
              skipped-patterns)
        holes (vec (concat (:policy-holes cascade)
                           no-result-holes
                           transition-holes
                           dependency-holes
                           budget-holes))
        mint-proposals (mint-proposals holes)]
    {:status :dark
     :algorithm algorithm
     :cascade
     (select-keys cascade [:shown :semilattice :policy-holes])
     :budget {:initial budget
              :spent (count executed-patterns)
              :remaining (- budget (count executed-patterns))
              :skipped-patterns skipped-patterns}
     :steps step-results
     :admissible-projection
     {:candidates
      (mapv #(select-keys
              % [:mission-id :candidate-source :control-pattern-ids
                 :support-relations :memory-ids])
            frontier)}
     :frontier frontier
     :excluded-missions excluded
     :holes holes
     :mint-proposals mint-proposals
     :selected-mission nil
     :live-ordering-changed? false}))

(defn execute-outer-cascade
  "Production-shaped dark wrapper using the Phase-4 WM query at every step."
  [ctx cascade control-edges dependencies transition-warrants opts]
  (outer-frontier
   {:cascade cascade
    :dependencies dependencies
    :transition-warrants transition-warrants
    :budget (:budget opts)
    :query-step-fn
    (fn [pattern-id _remaining-budget]
      (wm-memory/dark-candidate-projection
       ctx [pattern-id] control-edges
       {:limit (or (:memory-limit opts) 10)}))}))

(defn checkpoint-ranking
  "Compare Rung-1 typed ranking with the fixed frontier order.

   Trace fields are carried only when the difference is non-trivial and every
   ranked candidate has an inspectable typed contribution. No winner is
   selected or fed back into the frontier."
  [outer-result ranking-options]
  (let [ranking
        (dynamic-queries/fixed-typed-ranking
         (:admissible-projection outer-result)
         ranking-options)
        contributions (mapcat :contributions
                              (:ranked-candidates ranking))
        paths (set (map (juxt :control-pattern-id :relation)
                        contributions))
        patterns (set (map :control-pattern-id contributions))
        relations (set (map :relation contributions))
        different? (not= (:control-ranking ranking)
                         (:typed-ranking ranking))
        inspectable?
        (every? (comp seq :contributions) (:ranked-candidates ranking))
        held-out-targets (set (:held-out-target-ids ranking-options))
        hit-k (or (:hit-k ranking-options) 1)
        _ (when-not (and (integer? hit-k) (pos? hit-k))
            (throw (ex-info "checkpoint hit-k must be positive"
                            {:hit-k hit-k})))
        hit-at-k
        (fn [ranked]
          (if (seq held-out-targets)
            (if (some held-out-targets (take hit-k ranked)) 1.0 0.0)
            nil))
        control-hit (hit-at-k (:control-ranking ranking))
        typed-hit (hit-at-k (:typed-ranking ranking))
        useful? (and different?
                     inspectable?
                     (:candidate-set-preserved? ranking)
                     (or (nil? control-hit)
                         (>= typed-hit control-hit)))]
    (assoc outer-result
           :retrieval-checkpoint
           (cond-> {:status :dark-comparison
                    :useful-inspectable-difference? useful?
                    :instrumentation-carried? useful?
                    :control-ranking (:control-ranking ranking)
                    :typed-ranking (:typed-ranking ranking)
                    :counterfactual-ranking (:control-ranking ranking)
                    :held-out
                    {:target-ids (vec (sort held-out-targets))
                     :k hit-k
                     :control-hit-at-k control-hit
                     :typed-hit-at-k typed-hit}
                    :budget (:budget outer-result)
                    :path-diversity
                    {:distinct-path-count (count paths)
                     :distinct-pattern-count (count patterns)
                     :distinct-relation-count (count relations)}
                    :selected-mission nil
                    :live-ordering-changed? false}
             useful?
             (assoc :relation-contributions
                    (:ranked-candidates ranking))))))

(defn budgeted-facet-frontier
  "Execute Rung-3 budgeted refinement through the Phase-5 frontier machinery.

   The plan chooses only existing cascade patterns and witnessed descent edges.
   The selected prefix is then executed by outer-frontier; no second cascade or
   candidate representation is introduced. Challenged memories remain
   non-admitting but are retained in the observation trace."
  [{:keys [cascade query-step-fn dependencies transition-warrants
           information-models budget]}]
  (let [plan
        (dynamic-queries/budgeted-facet-plan
         {:cascade cascade
          :transition-warrants transition-warrants
          :information-models information-models
          :budget budget})
        selected (:selected-patterns plan)
        _ (when-not (seq selected)
            (throw (ex-info "facet refinement selected no affordable root"
                            {:plan plan})))
        selected-set (set selected)
        reordered
        (assoc cascade :shown
               (into selected
                     (remove selected-set (:shown cascade))))
        trace-by-pattern
        (into {} (map (juxt :pattern-id identity))
              (:selection-trace plan))
        result
        (outer-frontier
         {:cascade reordered
          :query-step-fn
          (fn [pattern-id _step-budget]
            (query-step-fn
             pattern-id
             (:remaining-budget-before
              (get trace-by-pattern pattern-id))))
          :dependencies dependencies
          :transition-warrants transition-warrants
          :budget (count selected)})
        observations
        (mapv
         (fn [{:keys [pattern-id result]}]
           (let [memories (->> (:recalls result)
                               (mapcat :memories)
                               (reduce
                                (fn [by-id memory]
                                  (assoc by-id (:memory/id memory) memory))
                                {})
                               vals)
                 challenged
                 (->> memories
                      (filter #(or (= :challenged (:memory/state %))
                                   (= :challenged
                                      (:memory/witness-status %))))
                      (map :memory/id)
                      sort vec)]
             {:pattern-id pattern-id
              :trace-id (:trace-id result)
              :observed-memory-ids
              (->> memories (map :memory/id) sort vec)
              :observed-challenge-memory-ids challenged
              :admitted-mission-ids
              (->> (get-in result [:projection :candidates])
                   (map :mission-id) sort vec)
              :excluded-mission-ids
              (->> (get-in result [:projection :excluded-missions])
                   (map :mission-id) sort vec)}))
         (:steps result))
        evidence-paths
        (set (mapcat
              (fn [{:keys [pattern-id observed-memory-ids]}]
                (map #(vector pattern-id %) observed-memory-ids))
              observations))
        challenge-ids
        (set (mapcat :observed-challenge-memory-ids observations))]
    (assoc result
           :facet-refinement
           (-> plan
               (assoc :observations observations)
               (assoc-in [:path-diversity :distinct-evidence-path-count]
                         (count evidence-paths))
               (assoc-in [:path-diversity :distinct-challenge-memory-count]
                         (count challenge-ids))))))
