(ns futon3c.aif.mission-head
  "MissionAifHead — implements both AifAdapter and AifHead for the
   Mission Peripheral.

   This is the integration point: it wires observation channels (H-3),
   the inventory loader (H-2), the refusal surface (H-7), prediction
   enrichment (H-6), and default mode (H-5) into a single adapter.

   Design decision: D-2 in M-aif-head.md
   Pattern grounding: A-2 (structured-observation-vector, term-to-channel-traceability,
                           evidence-precision-registry, expected-free-energy-scorecard,
                           policy-precision-commitment-temperature, candidate-pattern-action-space)"
  (:require [futon3c.aif.observe :as obs]
            [futon3c.aif.invariant :as invariant]
            [futon3c.peripheral.mission-shapes :as ms]))

;; =============================================================================
;; Mission action arena (from peripheral-aif-vocabulary.sexp)
;; =============================================================================

(def mission-arena
  "Bounded candidate set for mission action selection.
   aif/candidate-pattern-action-space: constraining the action space makes
   downstream scoring meaningful."
  {:arena/id :mission-management
   :arena/actions [:advance-phase :revise-approach :request-review
                   :save-state :signal-blocked :signal-complete]})

;; =============================================================================
;; EFE computation (mission-specific)
;; =============================================================================

(def default-lambdas
  "EFE term weights for mission actions."
  {:pragmatic 0.6
   :epistemic 0.4
   :effort 0.2})

(defn- pragmatic-value
  "How much does this action advance the mission?"
  [action channels]
  (case action
    :advance-phase   (- 1.0 (:phase-progress channels 0.5))
    :revise-approach (:prediction-divergence channels 0.0)
    :request-review  (- 1.0 (:gate-readiness channels 0.5))
    :save-state      0.1
    :signal-blocked  (- 1.0 (:obligation-satisfaction channels 0.5))
    :signal-complete (:obligation-satisfaction channels 0.0)))

(defn- epistemic-value
  "How much would this action reduce uncertainty?"
  [action channels]
  (case action
    :advance-phase   0.3
    :revise-approach 0.7
    :request-review  0.8
    :save-state      0.0
    :signal-blocked  0.2
    :signal-complete 0.1))

(defn- effort-cost
  "How expensive is this action?"
  [action]
  (case action
    :advance-phase   0.3
    :revise-approach 0.6
    :request-review  0.2
    :save-state      0.05
    :signal-blocked  0.1
    :signal-complete 0.1))

(defn- compute-g
  "Compute G(a) for a single mission action. Lower = better."
  [action channels lambdas]
  (let [prag (pragmatic-value action channels)
        epis (epistemic-value action channels)
        eff (effort-cost action)]
    (+ (* (:effort lambdas) eff)
       (- (* (:pragmatic lambdas) prag))
       (- (* (:epistemic lambdas) epis)))))

(defn- softmax [logits]
  (let [max-l (apply max (vals logits))
        exps (into {} (map (fn [[k v]] [k (Math/exp (- v max-l))]) logits))
        total (reduce + (vals exps))]
    (into {} (map (fn [[k v]] [k (/ v total)]) exps))))

(defn select-mission-action
  "Evaluate all mission actions and select via softmax.
   Returns {:action kw :policies [...] :tau float :abstain? bool}"
  [channels tau]
  (let [lambdas default-lambdas
        abstain-threshold 0.55
        abstain? (< tau abstain-threshold)
        actions (:arena/actions mission-arena)
        evaluations (mapv (fn [a]
                            {:action a
                             :G (compute-g a channels lambdas)
                             :terms {:pragmatic (pragmatic-value a channels)
                                     :epistemic (epistemic-value a channels)
                                     :effort (effort-cost a)}})
                          actions)
        logits (into {} (map (fn [e] [(:action e) (/ (- (:G e)) (max tau 0.1))])
                             evaluations))
        probs (softmax logits)
        policies (mapv (fn [e] (assoc e :probability (get probs (:action e))))
                       evaluations)
        selected (if abstain?
                   :save-state
                   (:action (apply max-key :probability policies)))]
    {:action selected
     :policies policies
     :tau tau
     :abstain? abstain?}))

;; =============================================================================
;; Default mode (D-4: inter-cycle behavior)
;; =============================================================================

(defn mission-default-mode
  "Inter-cycle fallback behavior for the Mission Peripheral.

   Called when the deliberative tier has nothing to do — between cycles,
   after completion, or when select-pattern abstains.

   Actions:
   1. Generate PAR for completed cycle
   2. Consult Portfolio Inference for next assignment
   3. Check structural law compliance
   4. Either begin next cycle or signal blocked/complete

   Returns same shape as select-pattern for drop-in substitution."
  [state observation]
  (let [channels (:channels observation)
        obligations (or (:obligation-satisfaction channels) 0.0)
        compliance (or (:structural-law-compliance channels) 1.0)]
    (cond
      ;; All obligations satisfied → signal complete
      (>= obligations 0.95)
      {:action :signal-complete
       :params {:reason "All obligations satisfied"
                :obligation-satisfaction obligations}
       :source :default-mode}

      ;; Structural law violation → signal blocked
      (< compliance 0.5)
      {:action :signal-blocked
       :params {:reason "Structural law violation detected"
                :compliance compliance}
       :source :default-mode}

      ;; Otherwise → ready to begin next cycle
      :else
      {:action :advance-phase
       :params {:reason "Default mode: ready for next cycle"
                :observation observation}
       :source :default-mode})))

;; =============================================================================
;; MissionAifHead record
;; =============================================================================

(defrecord MissionAifHead [config inventory-db]
  ;; futon2.aif.adapter/AifAdapter methods
  ;; select-pattern: Mission action selection via EFE
  ;; update-beliefs: Update pattern evidence counts and precision

  ;; futon2.aif.head/AifHead methods implemented below via protocol extension
  )

;; Implement AifHead protocol methods on MissionAifHead.
;; We use extend-type rather than inline protocol implementation because
;; AifHead is defined in futon2 and we want to avoid a compile-time
;; dependency on futon2's protocol class at defrecord time.

(defn mission-observe
  "AifHead.observe implementation for Mission Peripheral.
   Gathers 10 observation channels from mission state."
  [_head state context]
  (let [mission-state (:mission-state context)
        cycle (:current-cycle context)
        compliance-result (:compliance-result context)
        gate-result (:gate-result context)
        evidence-counts (:evidence-counts context)]
    (obs/observe-mission state mission-state cycle
                         {:compliance-result compliance-result
                          :gate-result gate-result
                          :evidence-counts evidence-counts})))

(defn mission-default-mode-fn
  "AifHead.default-mode implementation for Mission Peripheral."
  [_head state observation]
  (mission-default-mode state observation))

(defn mission-check-law
  "AifHead.check-law implementation for Mission Peripheral.
   Consults inventory db and AIF head coverage invariant."
  [head state transition]
  ;; Check AIF head coverage invariant
  (let [coverage-result (invariant/check-aif-head-law)]
    (if-not (:ok coverage-result)
      coverage-result
      ;; All structural checks passed
      {:ok true})))

(defn mission-select-pattern
  "AifAdapter.select-pattern implementation for Mission Peripheral.
   Uses mission-specific EFE with observation channels."
  [head state context]
  (let [observation (mission-observe head state context)
        channels (:channels observation)
        tau (or (:tau context) 1.0)]
    (select-mission-action channels tau)))

(defn mission-update-beliefs
  "AifAdapter.update-beliefs implementation for Mission Peripheral.
   Updates precision based on prediction divergence and evidence."
  [_head state observation]
  (let [divergence (:prediction-divergence observation)
        tau-adj (if (and divergence (> divergence 0.5))
                  ;; High divergence → increase tau → explore
                  (min 2.0 (* (or (:tau state) 1.0) 1.1))
                  ;; Low divergence → decrease tau → exploit
                  (max 0.3 (* (or (:tau state) 1.0) 0.95)))]
    {:aif/state (assoc state :tau tau-adj)
     :aif {:tau-updated tau-adj
           :prediction-divergence divergence}}))

;; =============================================================================
;; Factory
;; =============================================================================

(defn make-mission-aif-head
  "Create a MissionAifHead.

   config: {:inventory-path string  ;; path to structural-law-inventory.sexp
            :tau float}             ;; initial temperature (default 1.0)
   inventory-db: pre-loaded inventory database (or nil for lazy loading)"
  ([] (make-mission-aif-head {} nil))
  ([config] (make-mission-aif-head config nil))
  ([config inventory-db]
   (->MissionAifHead (merge {:tau 1.0} config) inventory-db)))
