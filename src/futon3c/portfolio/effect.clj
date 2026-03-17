(ns futon3c.portfolio.effect
  "Portfolio effect sink — closes the AIF loop.

   observe -> perceive -> affect -> policy -> **effect** -> observe

   Translates portfolio inference actions into mission state changes,
   executes them against the mission backend, and emits evidence so
   the next observation cycle sees the consequences.

   Source: D-6 in M-portfolio-inference.md"
  (:require [clojure.string :as str]))

;; =============================================================================
;; Action -> Effect translation
;; =============================================================================

(defn apply-portfolio-action
  "Translate portfolio inference action into mission state change.
   Takes the action keyword, the full portfolio result map, and current
   mission state. Returns an effect map describing what should change."
  [action portfolio-result mission-state]
  (case action
    :work-on     {:effect :begin-cycle
                  :mission (:focus portfolio-result)}
    :review      {:effect :portfolio-review}
    :consolidate {:effect :consolidate
                  :targets (:consolidation-targets portfolio-result)}
    :upvote      {:effect :upvote
                  :target (:upvote-target portfolio-result)}
    :wait        {:effect :none}
    ;; unknown action — surface it rather than silently dropping
    {:effect :unknown :action action}))

;; =============================================================================
;; Effect execution (side-effecting)
;; =============================================================================

(defn execute-effect!
  "Execute a portfolio effect against the mission backend.
   Returns {:ok true :applied effect-map} or {:ok false :error ...}"
  [effect mission-backend mission-id]
  (try
    (case (:effect effect)
      :begin-cycle
      (let [focus-mission (or (:mission effect) mission-id)
            result (.execute-tool mission-backend
                                 :cycle-begin [focus-mission "portfolio-inference"])]
        (if (:ok result)
          {:ok true :applied effect :cycle (:result result)}
          {:ok false :error (:error result) :effect effect}))

      :portfolio-review
      ;; Review is an observation action — it triggers a fresh portfolio-step
      ;; rather than mutating mission state directly. The effect is recorded
      ;; but execution is a no-op; the caller runs the review.
      {:ok true :applied effect :note "review triggers re-observation"}

      :consolidate
      {:ok true :applied effect
       :note (str "consolidation targets: "
                  (str/join ", " (map str (or (:targets effect) []))))}

      :upvote
      {:ok true :applied effect
       :note (str "upvote target: " (:target effect))}

      :none
      {:ok true :applied effect :note "no-op (wait)"}

      ;; fallback
      {:ok false :error (str "Unknown effect type: " (:effect effect))
       :effect effect})
    (catch Exception e
      {:ok false :error (.getMessage e) :effect effect})))

;; =============================================================================
;; Evidence emission
;; =============================================================================

(defn portfolio-action->evidence
  "Create evidence metadata for a portfolio action.
   This makes portfolio decisions observable in the evidence landscape."
  [action portfolio-result effect]
  {:evidence/id (str "e-portfolio-effect-" (name (or action :unknown))
                     "-" (System/currentTimeMillis))
   :evidence/subject {:ref/type :portfolio :ref/id "effect"}
   :evidence/type :coordination
   :evidence/claim-type :action
   :evidence/author "portfolio-inference"
   :evidence/at (str (java.time.Instant/now))
   :evidence/body {:action action
                   :effect (:effect effect)
                   :mission (or (:mission effect) (:focus portfolio-result))
                   :tau (get-in portfolio-result [:policy :tau])
                   :abstain? (get-in portfolio-result [:policy :abstain?])
                   :applied? (:ok effect)}
   :evidence/tags [:portfolio :effect action]})
