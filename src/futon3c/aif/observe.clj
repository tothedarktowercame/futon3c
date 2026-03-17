(ns futon3c.aif.observe
  "Mission observation channels — 10 normalized [0,1] signals for the
   Mission Peripheral's AIF head.

   Each channel is a pure function from mission state to [0,1].
   Channel definitions match peripheral-aif-vocabulary.sexp :mission entry.

   6 channels read from existing functions. 3 need new code (structural
   law, prediction divergence, argument claims). 1 is partial (gate
   readiness).

   Design decision: D-2 in M-aif-head.md
   Pattern grounding: A-2 (structured-observation-vector, term-to-channel-traceability)"
  (:require [futon3c.peripheral.mission-shapes :as ms]))

;; =============================================================================
;; Utilities (same conventions as portfolio/observe)
;; =============================================================================

(defn clamp01
  "Clamp x to [0,1]."
  [x]
  (max 0.0 (min 1.0 (double x))))

;; =============================================================================
;; Individual channel functions
;; =============================================================================

(defn phase-progress
  "Ordinal progress through the 9-phase cycle. 0 = :observe, 1 = :completed.
   Returns 0.5 when no cycle is active."
  [state]
  (if-let [phase (:current-phase state)]
    (let [idx (.indexOf ^java.util.List (vec ms/phase-order) phase)
          total (dec (count ms/phase-order))]
      (if (neg? idx) 0.5
          (clamp01 (/ (double idx) (max 1.0 (double total))))))
    0.5))

(defn obligation-satisfaction
  "Fraction of obligations that are :done or :partial.
   1.0 = all obligations satisfied."
  [mission-state]
  (let [obligations (vals (:mission/obligations mission-state))]
    (if (empty? obligations) 0.5
        (let [satisfied (count (filter #(#{:done :partial} (:item/status %)) obligations))]
          (clamp01 (/ (double satisfied) (double (count obligations))))))))

(defn required-outputs-present
  "Fraction of required outputs present for the current phase.
   1.0 = all required outputs present."
  [state cycle]
  (if-let [phase (:cycle/phase cycle)]
    (let [required (get ms/phase-required-outputs phase #{})
          phase-data (get-in cycle [:cycle/phase-data phase] {})
          provided (set (keys phase-data))]
      (if (empty? required) 1.0
          (clamp01 (/ (double (count (clojure.set/intersection required provided)))
                      (double (count required))))))
    0.5))

(defn structural-law-compliance
  "Fraction of structural laws that are not violated.
   Takes an inventory check result from logic.inventory/check-compliance.
   1.0 = fully compliant."
  [compliance-result]
  (if compliance-result
    (if (:ok compliance-result) 1.0
        (let [{:keys [total violated]} (:coverage compliance-result)]
          (if (and total (pos? total))
            (clamp01 (- 1.0 (/ (double (or violated 0)) (double total))))
            0.5)))
    0.5))

(defn prediction-divergence
  "Cross-phase prediction error: how much did :execute diverge from :propose?
   0.0 = perfect match, 1.0 = total divergence.
   Returns 0.0 when no prediction data is available."
  [cycle]
  (let [propose-data (get-in cycle [:cycle/phase-data :propose])
        execute-data (get-in cycle [:cycle/phase-data :execute])]
    (or (ms/compute-prediction-divergence propose-data execute-data) 0.0)))

(defn evidence-for-completion
  "Fraction of completion criteria that have supporting evidence.
   Takes mission spec and evidence counts per criterion.
   1.0 = all criteria have evidence."
  [mission-spec evidence-counts]
  (let [criteria (:success-criteria mission-spec)]
    (if (empty? criteria) 0.5
        (let [supported (count (filter (fn [c]
                                          (pos? (get evidence-counts c 0)))
                                        criteria))]
          (clamp01 (/ (double supported) (double (count criteria))))))))

(defn gate-readiness
  "Fraction of gate checks that pass.
   Takes a gate check result from mission-backend/tool-gate-check.
   1.0 = all gates pass."
  [gate-result]
  (if gate-result
    (let [gates (get-in gate-result [:result :gates])]
      (if (seq gates)
        (clamp01 (/ (double (count (filter :passed? gates)))
                    (double (count gates))))
        0.5))
    0.5))

(defn argument-claim-coverage
  "Fraction of ARGUE claims that have supporting evidence.
   Placeholder: returns 0.5 until argument parsing is implemented.
   Will read claim count from mission doc and evidence landscape."
  [_mission-state _evidence-store]
  0.5)

(defn cycle-count-signal
  "Normalized cycle count. More cycles = more attempts.
   Rescaled with cap of 20 cycles."
  [mission-state]
  (let [n (count (:mission/cycles mission-state))]
    (clamp01 (/ (double n) 20.0))))

(defn days-since-last-activity
  "Normalized days since last mission activity.
   0.0 = just now, 1.0 = 30+ days stale."
  [mission-state]
  (if-let [updated (:mission/updated-at mission-state)]
    (try
      (let [then (.toEpochMilli (java.time.Instant/parse updated))
            now (System/currentTimeMillis)
            days (/ (- now then) (* 24.0 60 60 1000))]
        (clamp01 (/ days 30.0)))
      (catch Exception _ 0.5))
    0.5))

;; =============================================================================
;; Composite observation
;; =============================================================================

(def channel-keys
  "Ordered list of mission observation channel keys.
   Matches peripheral-aif-vocabulary.sexp :mission entry."
  [:phase-progress
   :obligation-satisfaction
   :required-outputs-present
   :structural-law-compliance
   :prediction-divergence
   :evidence-for-completion-criteria
   :gate-readiness
   :argument-claim-coverage
   :cycle-count
   :days-since-last-activity])

(defn observe-mission
  "Gather all 10 mission observation channels into a normalized map.

   Args:
     state           — cycle engine state (contains :current-phase, etc.)
     mission-state   — full mission state (obligations, cycles, spec, etc.)
     cycle           — current cycle record (or nil)
     opts            — {:compliance-result, :gate-result, :evidence-counts}

   Returns {:channels {kw → [0,1]}, :raw {kw → any}}"
  [state mission-state cycle opts]
  (let [channels {:phase-progress               (phase-progress state)
                  :obligation-satisfaction       (obligation-satisfaction mission-state)
                  :required-outputs-present      (required-outputs-present state cycle)
                  :structural-law-compliance     (structural-law-compliance (:compliance-result opts))
                  :prediction-divergence         (prediction-divergence cycle)
                  :evidence-for-completion-criteria (evidence-for-completion
                                                     (:mission/spec mission-state)
                                                     (:evidence-counts opts))
                  :gate-readiness                (gate-readiness (:gate-result opts))
                  :argument-claim-coverage       (argument-claim-coverage mission-state nil)
                  :cycle-count                   (cycle-count-signal mission-state)
                  :days-since-last-activity      (days-since-last-activity mission-state)}]
    {:channels channels
     :raw {:phase (:current-phase state)
           :cycle-id (:current-cycle-id state)
           :obligations-total (count (:mission/obligations mission-state))
           :cycles-completed (:cycles-completed state 0)}}))

(defn obs->vector
  "Convert observation channel map to ordered vector."
  [obs]
  (mapv #(get (:channels obs) % 0.0) channel-keys))
