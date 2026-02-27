(ns futon3c.portfolio.core
  "Portfolio AIF core — the aif-step orchestrator.

   Wires observe → perceive → affect → policy into a single step.
   Two entry points:
   - portfolio-step!  — single AIF step, returns recommendation
   - portfolio-heartbeat! — weekly cycle with bid/clear discrepancy

   Source: D-8 in M-portfolio-inference.md
   Template: futon2/src/ants/aif/core.clj"
  (:require [futon3c.portfolio.observe :as observe]
            [futon3c.portfolio.perceive :as perceive]
            [futon3c.portfolio.affect :as affect]
            [futon3c.portfolio.adjacent :as adjacent]
            [futon3c.portfolio.policy :as policy]
            [futon3c.evidence.store :as estore]))

;; =============================================================================
;; State atom — persistent portfolio belief state
;; =============================================================================

(defonce !state
  (atom {:mu perceive/default-mu
         :prec perceive/default-precision
         :pending nil       ; hysteresis pending mode transition
         :recent []         ; sliding window of recent observations
         :step-count 0}))

(def recent-window-size 5)

;; =============================================================================
;; Evidence emission
;; =============================================================================

(defn- emit-evidence!
  "Emit portfolio inference evidence to the store."
  [evidence-store evidence-type body]
  (when evidence-store
    (estore/append* evidence-store
                    {:evidence/id (str "e-portfolio-" (name evidence-type) "-"
                                       (System/currentTimeMillis))
                     :evidence/subject {:ref/type :portfolio :ref/id "inference"}
                     :evidence/type :coordination
                     :evidence/claim-type :observation
                     :evidence/author "portfolio-inference"
                     :evidence/at (str (java.time.Instant/now))
                     :evidence/body body
                     :evidence/tags [:portfolio evidence-type]})))

;; =============================================================================
;; aif-step — the core loop
;; =============================================================================

(defn aif-step
  "Run one portfolio AIF step: observe → affect → perceive → policy.

   Takes:
   - state: {:mu, :prec, :pending, :recent, :step-count}
   - observation: normalized [0,1] channel map (from observe/observe)
   - adjacent-missions: from adjacent/compute-adjacent-set
   - opts: {:perceive-opts, :policy-opts}

   Returns:
   - {:state updated-state
      :action selected-action
      :observation observation
      :policy {:action :policies :tau :abstain?}
      :perception {:mu :prec :errors :free-energy :trace}
      :diagnostics {:mode :urgency :tau :free-energy}}"
  [state observation adjacent-missions opts]
  (let [mu (:mu state)
        prec (:prec state)
        ;; 1. Affect: mode transition + precision modulation
        mode-result (affect/next-mode (:mode mu) observation (:pending state))
        new-mode (:mode mode-result)
        new-pending (:pending mode-result)
        mu-with-mode (assoc mu :mode new-mode)
        ;; 2. Perceive: prediction error + belief update
        perception (perceive/perceive mu-with-mode observation prec
                                      (or (:perceive-opts opts) {}))
        updated-mu (:mu perception)
        ;; 3. Affect: precision modulation based on new urgency
        updated-prec (affect/modulate-precisions prec new-mode (:urgency updated-mu))
        ;; 4. Policy: action selection
        policy-result (policy/choose-action updated-mu updated-prec observation
                                            adjacent-missions
                                            (or (:policy-opts opts) {}))
        ;; 5. Update sliding window
        new-recent (vec (take-last recent-window-size
                                   (conj (:recent state) {:obs observation
                                                          :tau (:tau updated-prec)})))
        ;; 6. Assemble new state
        new-state {:mu updated-mu
                   :prec updated-prec
                   :pending new-pending
                   :recent new-recent
                   :step-count (inc (:step-count state))}]
    {:state new-state
     :action (:action policy-result)
     :observation observation
     :policy policy-result
     :perception perception
     :diagnostics {:mode new-mode
                   :urgency (:urgency updated-mu)
                   :tau (:tau updated-prec)
                   :free-energy (:free-energy perception)}}))

;; =============================================================================
;; portfolio-step! — the "what's the top priority?" query
;; =============================================================================

(defn portfolio-step!
  "Run one AIF step using live portfolio data. Mutates !state.

   Takes:
   - evidence-store: for evidence emission + observation gathering
   - opts: {:portfolio-review pre-built review, :emit-evidence? bool}

   Returns the aif-step result + updates !state."
  [evidence-store opts]
  (let [;; Gather observations
        mc-state (observe/gather-mc-state evidence-store (:portfolio-review opts))
        observation (observe/observe mc-state)
        ;; Get adjacent missions
        review (or (:portfolio-review opts)
                   (futon3c.peripheral.mission-control-backend/build-portfolio-review))
        missions (:portfolio/missions review)
        mana (:portfolio/mana review)
        adjacent-missions (adjacent/compute-adjacent-set missions mana)
        ;; Run the step
        current-state @!state
        result (aif-step current-state observation adjacent-missions opts)]
    ;; Update persistent state
    (reset! !state (:state result))
    ;; Emit evidence
    (when (:emit-evidence? opts true)
      (emit-evidence! evidence-store :observation
                      {:channels observation :mc-state mc-state})
      (emit-evidence! evidence-store :belief
                      {:mu (get-in result [:state :mu])
                       :prec (get-in result [:state :prec])})
      (emit-evidence! evidence-store :policy
                      {:action (:action result)
                       :policies (mapv #(select-keys % [:action :G :probability])
                                       (get-in result [:policy :policies]))
                       :abstain? (get-in result [:policy :abstain?])}))
    result))

;; =============================================================================
;; portfolio-heartbeat! — weekly cycle with bid/clear
;; =============================================================================

(defn portfolio-heartbeat!
  "Weekly heartbeat: run aif-step + compute bid/clear discrepancy.

   Takes:
   - evidence-store: for evidence emission
   - week-bid: map of {channel → predicted-value} (what we bid for this week)
   - opts: same as portfolio-step!

   Returns aif-step result + discrepancy analysis."
  [evidence-store week-bid opts]
  (let [result (portfolio-step! evidence-store (assoc opts :emit-evidence? false))
        observation (:observation result)
        ;; Compute discrepancy: bid - clear (predicted - observed)
        delta (into {}
                    (map (fn [k]
                           [k (- (get week-bid k 0.5) (get observation k 0.5))]))
                    observe/channel-keys)
        total-delta (reduce + (map #(Math/abs (double %)) (vals delta)))]
    ;; Emit heartbeat evidence
    (emit-evidence! evidence-store :heartbeat
                    {:bid week-bid
                     :clear observation
                     :delta delta
                     :total-delta total-delta
                     :mode (get-in result [:diagnostics :mode])
                     :focus (get-in result [:state :mu :focus])
                     :adjacent-count (count (filter :adjacent?
                                                    ;; re-derive from result
                                                    []))
                     :step (:step-count (:state result))})
    (assoc result
           :heartbeat {:bid week-bid
                       :clear observation
                       :delta delta
                       :total-delta total-delta})))

;; =============================================================================
;; Convenience: format recommendation
;; =============================================================================

(defn format-recommendation
  "Format an aif-step result as a human-readable recommendation."
  [result]
  (let [action (:action result)
        diag (:diagnostics result)
        policies (get-in result [:policy :policies])
        top-3 (->> policies
                   (sort-by :probability >)
                   (take 3))]
    (str "Portfolio Inference (step " (get-in result [:state :step-count]) ")\n"
         "Mode: " (:mode diag) " | Urgency: " (format "%.2f" (:urgency diag))
         " | τ: " (format "%.2f" (:tau diag))
         " | FE: " (format "%.4f" (:free-energy diag)) "\n"
         (if (get-in result [:policy :abstain?])
           "ABSTAIN: τ below threshold — not confident enough to recommend\n"
           (str "Recommendation: " (name action) "\n"))
         "Top actions:\n"
         (apply str
                (map (fn [p]
                       (format "  %s: G=%.3f  p=%.1f%%\n"
                               (name (:action p)) (:G p) (* 100 (:probability p))))
                     top-3)))))
