(ns futon3c.portfolio.perceive
  "Portfolio perception layer — predictive coding belief update.

   Computes prediction error (ε = observation - predicted) and updates
   belief state μ via: μ ← μ + κ·τ·ε

   The ant AIF loop runs 5 micro-steps per tick. Portfolio perception
   runs a configurable number of micro-steps (default 3, since portfolio
   channels change slower than ant sensors).

   Source: D-3, D-4 in M-portfolio-inference.md
   Template: futon2/src/ants/aif/perceive.clj"
  (:require [futon3c.portfolio.observe :as obs]))

;; =============================================================================
;; Default belief state (μ) and precision (τ)
;; =============================================================================

(def default-mu
  "Initial belief state. sens mirrors observation shape.
   All predictions start at 0.5 (maximum uncertainty / no prior knowledge)."
  {:sens (zipmap obs/channel-keys (repeat 0.5))
   :mode :BUILD
   :focus nil
   :urgency 0.5})

(def default-precision
  "Initial precision weights per channel.
   Higher = more precise measurement = stronger error signal.
   Source: D-4 in M-portfolio-inference.md"
  {:Pi-o {:mission-complete-ratio 1.0
          :coverage-pct           1.0
          :coverage-trajectory    0.6
          :mana-available         0.8
          :blocked-ratio          1.0
          :evidence-velocity      0.5
          :dependency-depth       0.7
          :gap-count              0.9
          :stall-count            0.7
          :spinoff-pressure       0.4
          :pattern-reuse          0.3
          :review-age             1.0}
   :tau 1.0})

;; =============================================================================
;; Prediction error
;; =============================================================================

(defn compute-errors
  "Compute per-channel prediction error: ε = observation - predicted.
   Precision-weighted: weighted_ε = precision × raw_ε.

   Returns map of {:raw {channel → ε}, :weighted {channel → Π·ε},
                   :free-energy FE}."
  [mu observation precision]
  (let [sens (:sens mu)
        pi-o (:Pi-o precision)
        raw (into {}
                  (map (fn [k]
                         (let [obs-val (get observation k 0.5)
                               pred-val (get sens k 0.5)]
                           [k (- obs-val pred-val)])))
                  obs/channel-keys)
        weighted (into {}
                       (map (fn [k]
                              [k (* (get pi-o k 1.0) (get raw k 0.0))]))
                       obs/channel-keys)
        ;; Free energy: 0.5 × mean(precision × raw²)
        fe (let [terms (map (fn [k]
                              (* (get pi-o k 1.0)
                                 (Math/pow (get raw k 0.0) 2)))
                            obs/channel-keys)]
             (* 0.5 (/ (reduce + terms) (count obs/channel-keys))))]
    {:raw raw
     :weighted weighted
     :free-energy fe}))

;; =============================================================================
;; Belief update (predictive coding micro-step)
;; =============================================================================

(defn update-sens
  "Single micro-step: update sensory predictions toward observation.
   sens[k] += α × weighted_error[k], clamped to [0,1]."
  [sens weighted-errors alpha]
  (into {}
        (map (fn [k]
               (let [current (get sens k 0.5)
                     update (* alpha (get weighted-errors k 0.0))]
                 [k (obs/clamp01 (+ current update))])))
        obs/channel-keys))

(defn update-urgency
  "Update urgency based on weighted error from key pressure channels.
   High gap-count, stall-count, review-age → urgency rises.
   High coverage-pct, mission-complete-ratio → urgency falls."
  [urgency weighted-errors beta]
  (let [pressure-channels [:gap-count :stall-count :review-age :blocked-ratio]
        relief-channels [:mission-complete-ratio :coverage-pct]
        pressure (reduce + (map #(Math/abs (double (get weighted-errors % 0.0)))
                                pressure-channels))
        relief (reduce + (map #(Math/abs (double (get weighted-errors % 0.0)))
                               relief-channels))
        delta (* beta (- pressure relief))]
    (obs/clamp01 (+ urgency delta))))

;; =============================================================================
;; Perceive — the main entry point
;; =============================================================================

(def default-opts
  "Default perception options."
  {:alpha 0.55     ; learning rate for sensory update (from ant perceive)
   :beta 0.30      ; learning rate for urgency update (from ant perceive)
   :micro-steps 3  ; number of prediction error minimization steps
   })

(defn perceive
  "Run predictive coding loop: compute errors, update beliefs, repeat.

   Takes:
   - mu: current belief state {:sens, :mode, :focus, :urgency}
   - observation: normalized [0,1] channel map from observe
   - precision: {:Pi-o {channel → weight}, :tau temperature}
   - opts: {:alpha, :beta, :micro-steps}

   Returns:
   - {:mu updated-mu, :prec precision, :errors final-errors,
      :free-energy accumulated-FE, :trace [{step errors}...]}"
  ([mu observation precision] (perceive mu observation precision default-opts))
  ([mu observation precision opts]
   (let [alpha (or (:alpha opts) 0.55)
         beta (or (:beta opts) 0.30)
         steps (or (:micro-steps opts) 3)]
     (loop [step 0
            current-mu mu
            total-fe 0.0
            trace []]
       (if (>= step steps)
         {:mu current-mu
          :prec precision
          :errors (last trace)
          :free-energy total-fe
          :trace trace}
         (let [errors (compute-errors current-mu observation precision)
               new-sens (update-sens (:sens current-mu) (:weighted errors) alpha)
               new-urgency (update-urgency (:urgency current-mu) (:weighted errors) beta)
               new-mu (assoc current-mu
                              :sens new-sens
                              :urgency new-urgency)]
           (recur (inc step)
                  new-mu
                  (+ total-fe (:free-energy errors))
                  (conj trace errors))))))))
