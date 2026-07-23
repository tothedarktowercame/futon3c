(ns futon3c.peripheral.strategic-policies
  "Phase-7 strategic policy identities, E_S habit, and shadow ranking.

   Strategic habit, predicted G_S, admissible support, proposal reasons, and
   memories remain separate trace fields. Tactical selection events are
   audited but cannot contribute to E_S. Nothing in this namespace selects or
   enacts a live mission."
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest]))

(def algorithm :strategic-policies/shadow-v1)
(def identity-version 1)

(defn- nonblank-string?
  [value]
  (and (string? value) (not-empty value)))

(defn- finite-number?
  [value]
  (and (number? value) (Double/isFinite (double value))))

(defn- sha256
  [value]
  (let [digest (.digest
                (MessageDigest/getInstance "SHA-256")
                (.getBytes (str value) StandardCharsets/UTF_8))]
    (apply str (map #(format "%02x" (bit-and (int %) 0xff)) digest))))

(defn strategic-policy-id
  "Return a stable identity for an ordered strategic mission cascade."
  [mission-ids]
  (when-not (and (vector? mission-ids)
                 (seq mission-ids)
                 (every? nonblank-string? mission-ids))
    (throw (ex-info "invalid strategic policy mission identity"
                    {:mission-ids mission-ids})))
  (str "pi-s-"
       (subs (sha256 (pr-str [identity-version :strategic mission-ids]))
             0 24)))

(defn- policy-from-cascade
  [frontier-by-id mission-ids]
  (let [members (mapv frontier-by-id mission-ids)
        _ (when-not (and (every? some? members)
                         (every? #(= :ready (:frontier-status %)) members))
            (throw (ex-info "strategic policy contains unsupported mission"
                            {:mission-ids mission-ids})))
        support-relations
        (->> members
             (mapcat :support-relations)
             (sort-by (juxt :control-pattern-id :relation))
             vec)
        memory-ids
        (->> support-relations (mapcat :memory-ids) distinct sort vec)
        provenance
        (->> support-relations (mapcat :provenance) distinct vec)]
    {:policy-id (strategic-policy-id mission-ids)
     :identity-version identity-version
     :grain :strategic
     :mission-ids mission-ids
     :length (count mission-ids)
     :hard-support
     {:admitted? true
      :frontier-statuses (mapv :frontier-status members)
      :dependency-statuses (mapv :dependency-status members)
      :support-relations support-relations}
     :proposal-reasons
     (mapv (fn [member]
             {:mission-id (:mission-id member)
              :reasons (:reasons member)})
           members)
     :memory-ids memory-ids
     :provenance provenance}))

(defn construct-strategic-policies
  "Construct length-1 and ready short-cascade policies from Phase 5."
  [outer-result]
  (let [frontier (:frontier outer-result)
        _ (when-not (and (vector? frontier)
                         (every? #(nonblank-string? (:mission-id %))
                                 frontier))
            (throw (ex-info "invalid Phase-5 frontier for strategic policies"
                            {:frontier frontier})))
        frontier-by-id (into {} (map (juxt :mission-id identity)) frontier)
        ready (filterv #(= :ready (:frontier-status %)) frontier)
        cascades
        (->> (concat (map #(vector (:mission-id %))
                          (filter #(empty? (:dependency-ids %)) ready))
                     (keep #(when (> (count (:mission-cascade %)) 1)
                              (:mission-cascade %))
                           ready))
             distinct
             (sort-by pr-str)
             vec)
        policies (mapv #(policy-from-cascade frontier-by-id %) cascades)
        held-candidates
        (filter #(or (not= :ready (:frontier-status %))
                     (seq (:dependency-ids %)))
                frontier)
        held
        (mapv
         (fn [candidate]
           {:mission-id (:mission-id candidate)
            :policy-id
            (strategic-policy-id [(:mission-id candidate)])
            :hard-support
            {:admitted? false
             :frontier-status (:frontier-status candidate)
             :dependency-status (:dependency-status candidate)
             :why
             (if (= :ready (:frontier-status candidate))
               :dependency-cascade-required
               :frontier-not-ready)
             :blocked-dependency-ids
             (:blocked-dependency-ids candidate)}
            :memory-ids (:memory-ids candidate)
            :reasons (:reasons candidate)})
         held-candidates)]
    {:identity-version identity-version
     :policies policies
     :held-policies held
     :policy-count (count policies)}))

(defn fit-strategic-habit
  "Fit a symmetric Dirichlet E_S from strategic selection frequency.

   Tactical E_T events are counted in the audit and excluded before fitting.
   Unknown grains and strategic events naming an unknown policy fail closed."
  [policies selection-events alpha]
  (let [policy-ids (mapv :policy-id policies)
        policy-set (set policy-ids)
        event-ids (mapv :selection-id selection-events)
        _ (when-not (and (seq policies)
                         (= (count policy-ids) (count policy-set))
                         (finite-number? alpha)
                         (pos? alpha)
                         (= (count event-ids) (count (set event-ids)))
                         (every?
                          #(and (nonblank-string? (:selection-id %))
                                (contains? #{:strategic :tactical}
                                           (:grain %))
                                (nonblank-string? (:policy-id %)))
                          selection-events))
            (throw (ex-info "invalid strategic habit input"
                            {:policy-ids policy-ids
                             :alpha alpha
                             :selection-events selection-events})))
        strategic-events (filterv #(= :strategic (:grain %))
                                  selection-events)
        tactical-events (filterv #(= :tactical (:grain %))
                                 selection-events)
        unknown-strategic
        (remove #(contains? policy-set (:policy-id %)) strategic-events)
        _ (when (seq unknown-strategic)
            (throw (ex-info "strategic habit event names unknown policy"
                            {:events (vec unknown-strategic)})))
        counts (frequencies (map :policy-id strategic-events))
        denominator (+ (count strategic-events)
                       (* (double alpha) (count policies)))
        estimates
        (into {}
              (map
               (fn [policy-id]
                 (let [count (get counts policy-id 0)
                       mass (+ (double alpha) count)
                       probability (/ mass denominator)]
                   [policy-id
                    {:policy-id policy-id
                     :selection-count count
                     :alpha (double alpha)
                     :posterior-mass mass
                     :probability probability
                     :log-probability (Math/log probability)}])))
              policy-ids)]
    {:algorithm :strategic-policies/dirichlet-habit-v1
     :grain :strategic
     :semantics :selection-frequency-habit-not-outcome-value
     :alpha (double alpha)
     :strategic-event-count (count strategic-events)
     :excluded-tactical-event-count (count tactical-events)
     :estimates estimates
     :normalization (reduce + 0.0 (map :probability (vals estimates)))}))

(defn- validate-g-row
  [policy-by-missions {:keys [mission-ids value uncertainty source] :as row}]
  (let [policy (get policy-by-missions mission-ids)
        lower (:lower uncertainty)
        upper (:upper uncertainty)]
    (when-not (and policy
                   (finite-number? value)
                   (not (neg? value))
                   (map? uncertainty)
                   (finite-number? lower)
                   (finite-number? upper)
                   (<= lower value upper)
                   (map? source)
                   (nonblank-string? (:model-id source))
                   (vector? (:evidence-ids source))
                   (every? nonblank-string? (:evidence-ids source)))
      (throw (ex-info "invalid predicted G_S row"
                      {:row row
                       :known-policy-missions
                       (set (keys policy-by-missions))})))
    [(:policy-id policy)
     {:value (double value)
      :uncertainty {:lower (double lower) :upper (double upper)}
      :source source
      :semantics :predicted-strategic-cost-not-habit}]))

(defn rank-shadow-policies
  "Rank admitted strategic policies with E_S * exp(-G_S / temperature)."
  [policies habit g-rows temperature]
  (when-not (and (finite-number? temperature) (pos? temperature))
    (throw (ex-info "strategic shadow temperature must be positive"
                    {:temperature temperature})))
  (let [policy-by-missions (into {} (map (juxt :mission-ids identity))
                                 policies)
        g-pairs (mapv #(validate-g-row policy-by-missions %) g-rows)
        g-by-policy (into {} g-pairs)
        policy-ids (set (map :policy-id policies))
        _ (when-not (and (= (count policies) (count g-pairs))
                         (= policy-ids (set (keys g-by-policy))))
            (throw (ex-info "predicted G_S must cover each policy exactly"
                            {:policy-ids policy-ids
                             :g-policy-ids (set (keys g-by-policy))})))
        unnormalized
        (mapv
         (fn [policy]
           (let [policy-id (:policy-id policy)
                 habit-row (get-in habit [:estimates policy-id])
                 g-row (get g-by-policy policy-id)
                 log-potential
                 (- (:log-probability habit-row)
                    (/ (:value g-row) (double temperature)))]
             (assoc policy
                    :e-s habit-row
                    :predicted-g-s g-row
                    :log-shadow-potential log-potential)))
         policies)
        max-log (reduce max (map :log-shadow-potential unnormalized))
        weights (mapv #(Math/exp (- (:log-shadow-potential %) max-log))
                      unnormalized)
        denominator (reduce + 0.0 weights)
        ranked
        (->> (mapv
              (fn [policy weight]
                (assoc policy
                       :shadow-probability (/ weight denominator)
                       :explanation-complete?
                       (and (seq (:proposal-reasons policy))
                            (seq (:memory-ids policy))
                            (seq (:provenance policy))
                            (map? (:e-s policy))
                            (map? (:predicted-g-s policy))
                            (true?
                             (get-in policy
                                     [:hard-support :admitted?])))))
              unnormalized weights)
             (sort-by (juxt (comp - :shadow-probability) :policy-id))
             vec)]
    {:status :shadow-ranking
     :algorithm :strategic-policies/e-s-g-s-shadow-v1
     :temperature (double temperature)
     :ranked-policies ranked
     :normalization (reduce + 0.0 (map :shadow-probability ranked))
     :model-winner-policy-id (:policy-id (first ranked))
     :model-winner-mission-ids (:mission-ids (first ranked))
     :selected-mission nil
     :live-ordering-changed? false}))

(defn- validate-shadow-case
  [{:keys [decision-id baseline-mission-ids reviewed-mission-ids
           reviewer-witness-id temperature predicted-g-s] :as shadow-case}]
  (when-not (and (nonblank-string? decision-id)
                 (vector? baseline-mission-ids)
                 (seq baseline-mission-ids)
                 (vector? reviewed-mission-ids)
                 (seq reviewed-mission-ids)
                 (nonblank-string? reviewer-witness-id)
                 (finite-number? temperature)
                 (pos? temperature)
                 (vector? predicted-g-s))
    (throw (ex-info "invalid strategic shadow-window case"
                    {:case shadow-case})))
  shadow-case)

(defn run-shadow-window
  "Run a frozen Phase-7 shadow window beside the live additive controller."
  [outer-result fixture]
  (let [{:keys [freeze-id frozen-at dirichlet-alpha selection-events
                shadow-cases minimum-review-window]} fixture
        _ (when-not (and (nonblank-string? freeze-id)
                         (nonblank-string? frozen-at)
                         (finite-number? dirichlet-alpha)
                         (pos? dirichlet-alpha)
                         (vector? selection-events)
                         (vector? shadow-cases)
                         (seq shadow-cases)
                         (integer? minimum-review-window)
                         (pos? minimum-review-window))
            (throw (ex-info "invalid or unfrozen Phase-7 fixture"
                            {:fixture fixture})))
        {:keys [policies held-policies] :as construction}
        (construct-strategic-policies outer-result)
        habit (fit-strategic-habit policies selection-events dirichlet-alpha)
        traces
        (mapv
         (fn [shadow-case]
           (let [{:keys [decision-id baseline-mission-ids
                         reviewed-mission-ids reviewer-witness-id
                         temperature predicted-g-s]}
                 (validate-shadow-case shadow-case)
                 ranking
                 (rank-shadow-policies policies habit
                                       predicted-g-s temperature)
                 model-missions (:model-winner-mission-ids ranking)]
             (assoc ranking
                    :decision-id decision-id
                    :baseline-winner-mission-ids baseline-mission-ids
                    :reviewed-winner-mission-ids reviewed-mission-ids
                    :reviewer-witness-id reviewer-witness-id
                    :model-baseline-disagreement?
                    (not= model-missions baseline-mission-ids)
                    :model-reviewed-agreement?
                    (= model-missions reviewed-mission-ids)
                    :counterfactual-baseline
                    {:controller :current-additive
                     :mission-ids baseline-mission-ids})))
         shadow-cases)
        all-ranked (mapcat :ranked-policies traces)
        identity-preserved?
        (every? #(= (:policy-id %)
                    (strategic-policy-id (:mission-ids %)))
                all-ranked)
        explanation-completeness
        (/ (count (filter :explanation-complete? all-ranked))
           (double (count all-ranked)))
        disagreement-traces
        (filterv :model-baseline-disagreement? traces)
        reviewed-disagreements?
        (every? #(nonblank-string? (:reviewer-witness-id %))
                disagreement-traces)
        window-size (count traces)
        enough-window? (>= window-size minimum-review-window)
        review-agreement
        (/ (count (filter :model-reviewed-agreement? traces))
           (double window-size))]
    {:status :shadow-complete
     :algorithm algorithm
     :fixture {:freeze-id freeze-id
               :frozen-at frozen-at
               :window-size window-size
               :minimum-review-window minimum-review-window}
     :policy-construction construction
     :e-s habit
     :shadow-traces traces
     :evaluation
     {:model-reviewed-agreement review-agreement
      :disagreement-count (count disagreement-traces)
      :all-disagreements-reviewed? reviewed-disagreements?
      :identity-preserved? identity-preserved?
      :provenance-preserved?
      (every? #(and (seq (:memory-ids %))
                    (seq (:provenance %)))
              all-ranked)
      :candidate-set-preserved?
      (every? #(= (set (map :policy-id policies))
                   (set (map :policy-id (:ranked-policies %))))
              traces)
      :explanation-completeness explanation-completeness
      :held-policy-count (count held-policies)}
     :promotion
     {:operator-decision-required? true
      :promotion-eligible-for-review?
      (and enough-window?
           reviewed-disagreements?
           (= 1.0 explanation-completeness))
      :promote? false
      :decision-reason
      (if enough-window?
        :operator-review-required
        :shadow-window-too-small)}
     :source-budget (:budget outer-result)
     :selected-mission nil
     :live-ordering-changed? false}))
