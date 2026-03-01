(ns futon3c.portfolio.affect
  "Portfolio affect layer — mode dynamics and precision modulation.

   Three modes: BUILD, MAINTAIN, CONSOLIDATE.
   Transitions require conditions to hold for 2 consecutive observations
   (hysteresis prevents oscillation at boundaries).

   Mode-conditioned precision sharpens channels relevant to the current
   behavioral stance.

   Source: D-5 in M-portfolio-inference.md
   Template: futon2/src/ants/aif/affect.clj"
  (:require [futon3c.portfolio.observe :as obs]))

;; =============================================================================
;; Mode transition conditions
;; =============================================================================

(defn- build->maintain?
  [observation]
  (and (> (:coverage-pct observation) 0.7)
       (< (:stall-count observation) 0.2)))

(defn- build->consolidate?
  [observation]
  (or (> (:spinoff-pressure observation) 0.6)
      (> (:review-age observation) 0.8)))

(defn- maintain->build?
  [observation]
  (or (> (:gap-count observation) 0.5)
      (< (:coverage-trajectory observation) 0.2))) ; rescaled: 0.2 in [0,1] = -0.6 in [-1,1]

(defn- maintain->consolidate?
  [observation]
  (or (> (:spinoff-pressure observation) 0.6)
      (> (:review-age observation) 0.8)))

(defn- consolidate->build?
  [observation]
  (and (> (:gap-count observation) 0.5)
       (< (:spinoff-pressure observation) 0.3)))

(defn- consolidate->maintain?
  [observation]
  (and (> (:coverage-pct observation) 0.7)
       (< (:review-age observation) 0.3)))

;; =============================================================================
;; Mode transition with hysteresis
;; =============================================================================

(defn candidate-mode
  "Compute what mode the system would transition to, given current mode
   and observation. Returns current mode if no transition condition holds."
  [current-mode observation]
  (case current-mode
    :BUILD (cond
             (build->maintain? observation) :MAINTAIN
             (build->consolidate? observation) :CONSOLIDATE
             :else :BUILD)
    :MAINTAIN (cond
                (maintain->build? observation) :BUILD
                (maintain->consolidate? observation) :CONSOLIDATE
                :else :MAINTAIN)
    :CONSOLIDATE (cond
                   (consolidate->build? observation) :BUILD
                   (consolidate->maintain? observation) :MAINTAIN
                   :else :CONSOLIDATE)
    ;; Default to BUILD for unknown modes
    :BUILD))

(defn next-mode
  "Determine next mode with hysteresis.
   Transitions require the candidate to match for 2 consecutive observations.

   Takes:
   - current-mode: keyword (:BUILD, :MAINTAIN, :CONSOLIDATE)
   - observation: normalized channel map
   - pending: {:candidate mode, :count n} or nil

   Returns:
   - {:mode new-mode, :pending new-pending}"
  [current-mode observation pending]
  (let [candidate (candidate-mode current-mode observation)]
    (if (= candidate current-mode)
      ;; No transition pressure — stay and clear pending
      {:mode current-mode :pending nil}
      ;; Transition pressure — check hysteresis
      (if (and pending
               (= candidate (:candidate pending))
               (>= (:count pending) 1))
        ;; Condition held for 2 consecutive observations → transition
        {:mode candidate :pending nil}
        ;; First observation of this candidate → record as pending
        {:mode current-mode
         :pending {:candidate candidate :count 1}}))))

;; =============================================================================
;; Urgency → tau coupling
;; =============================================================================

(defn urgency->tau
  "Map urgency [0,1] to commitment temperature tau.
   Low urgency (relaxed) → high tau (explore, 2.6 max)
   High urgency (pressured) → low tau (exploit, 0.35 min)

   Same formula as ant hunger→tau coupling."
  [urgency]
  (+ 0.35 (* 2.25 (- 1.0 (obs/clamp01 urgency)))))

;; =============================================================================
;; Mode-conditioned precision modulation
;; =============================================================================

(def mode-precision-boosts
  "Per-mode precision boosts. Each mode sharpens the channels it cares about.
   Heartbeat channels (T-7): BUILD cares about completion rate (are we doing what
   we planned?), CONSOLIDATE cares about effort error and unplanned work."
  {:BUILD        {:gap-count 0.3 :stall-count 0.2 :evidence-velocity 0.2
                  :dependency-depth 0.2 :bid-completion-rate 0.2}
   :MAINTAIN     {:coverage-pct 0.2 :coverage-trajectory 0.3
                  :mission-complete-ratio 0.2}
   :CONSOLIDATE  {:spinoff-pressure 0.3 :review-age 0.3
                  :pattern-reuse 0.2 :effort-prediction-error 0.2
                  :unplanned-work-ratio 0.2}})

(defn modulate-precisions
  "Apply mode-conditioned precision boosts and urgency→tau coupling.

   Takes:
   - precision: {:Pi-o {channel → weight}, :tau temperature}
   - mode: current behavioral mode
   - urgency: [0,1] urgency level

   Returns updated precision map."
  [precision mode urgency]
  (let [boosts (get mode-precision-boosts mode {})
        new-pi-o (reduce-kv (fn [m k boost]
                              (update m k (fn [v] (+ (or v 0.5) boost))))
                            (:Pi-o precision)
                            boosts)
        new-tau (urgency->tau urgency)]
    {:Pi-o new-pi-o
     :tau new-tau}))
