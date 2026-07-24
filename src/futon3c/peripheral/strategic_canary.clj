(ns futon3c.peripheral.strategic-canary
  "Phase-8 bounded-autonomy gate for reason-bearing strategic selection.

   Operator decision evidence 6e6f56a1-b9d7-4f83-928f-3a211ef890a0
   retires confirm-to-enact.  The machine may authorize an admissible policy
   only when every machine gate below passes.  Delivery still requires an
   Arxana Field Desk QA note through the port-7070 API."
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def algorithm :strategic-canary/bounded-autonomy-v2)
(def operator-decision-evidence-id
  "6e6f56a1-b9d7-4f83-928f-3a211ef890a0")
(def rollback-boundary "e74c7e7")
(def armed-tripwires
  (set (map #(keyword (str "T" %)) (range 1 14))))
(def delivery-qa-path "/api/alpha/morning-brief/addendum")

(defn- nonblank-string?
  [value]
  (and (string? value) (not (str/blank? value))))

(defn- fallback
  [reason details]
  {:status :actuation-withheld
   :algorithm algorithm
   :effective-rung :bounded-autonomy
   :operator-decision-evidence-id operator-decision-evidence-id
   :fallback-controller :current-additive
   :fallback-mode :explicit-rollback-only
   :rollback-boundary rollback-boundary
   :rollback-reason reason
   :rollback-details details
   :enactment {:authorized? false :executed? false}
   :selected-mission nil
   :live-ordering-changed? false})

(defn- warm-cache-gate?
  [{:keys [maximum-endpoint-ms accepted-endpoint-latencies]}]
  (and (number? maximum-endpoint-ms)
       (<= maximum-endpoint-ms 1000)
       (seq accepted-endpoint-latencies)
       (every? #(and (number? (:elapsed-ms %))
                     (<= (:elapsed-ms %) maximum-endpoint-ms))
               accepted-endpoint-latencies)))

(defn- port-7070-delivery-gate?
  [{:keys [required? endpoint]}]
  (and (true? required?)
       (nonblank-string? endpoint)
       (try
         (let [uri (java.net.URI. endpoint)]
           (and (= 7070 (.getPort uri))
                (= delivery-qa-path (.getPath uri))))
         (catch Exception _ false))))

(defn bounded-autonomy
  "Authorize the frozen strategic recommendation under machine gates.

   This function does not execute an action.  It proves that the policy may
   proceed to actuation; the click runner must still emit delivery QA."
  [shadow-result fixture]
  (let [{:keys [freeze-id requested-rung decision-id
                operator-decision-evidence-id admissible-mission-ids
                tripwire-clear? armed-tripwire-ids
                query-limit maximum-query-limit serving-cache-gate
                rollback-boundary delivery-qa calibration
                observed-outcome]} fixture
        trace (first (filter #(= decision-id (:decision-id %))
                             (:shadow-traces shadow-result)))
        recommendation (first (:ranked-policies trace))
        budget (:source-budget shadow-result)
        observed-memory-ids (set (:memory-ids-used observed-outcome))
        recommendation-memory-ids (set (:memory-ids recommendation))
        admissible-set (set admissible-mission-ids)
        recommendation-set (set (:mission-ids recommendation))
        basic-valid?
        (and (nonblank-string? freeze-id)
             (= :bounded-autonomy requested-rung)
             (nonblank-string? decision-id)
             (= futon3c.peripheral.strategic-canary/operator-decision-evidence-id
                operator-decision-evidence-id)
             (= futon3c.peripheral.strategic-canary/rollback-boundary
                rollback-boundary)
             (vector? admissible-mission-ids)
             (seq admissible-mission-ids)
             (= armed-tripwires (set armed-tripwire-ids))
             (boolean? tripwire-clear?)
             (integer? query-limit)
             (pos? query-limit)
             (integer? maximum-query-limit)
             (pos? maximum-query-limit))
        explanation-valid?
        (and trace recommendation
             (true? (:explanation-complete? recommendation))
             (seq (:proposal-reasons recommendation))
             (seq (:provenance recommendation))
             (seq (:memory-ids recommendation))
             (true? (get-in recommendation [:hard-support :admitted?]))
             (set/subset? recommendation-set admissible-set))
        budget-valid?
        (and (map? budget)
             (<= (:spent budget) (:initial budget))
             (<= query-limit maximum-query-limit))
        outcome-valid?
        (and (map? observed-outcome)
             (contains? #{:useful-progress :no-useful-progress}
                        (:outcome observed-outcome))
             (= :independently-witnessed (:witness-status observed-outcome))
             (nonblank-string? (:witness-id observed-outcome))
             (seq (:memory-ids-used observed-outcome))
             (set/subset? observed-memory-ids recommendation-memory-ids))
        calibration-honest?
        (and (= 13 (:sample-count calibration))
             (= 20 (:minimum calibration))
             (false? (:advance? calibration))
             (false? (:calibrated-probabilities? calibration)))]
    (cond
      (not basic-valid?)
      (fallback :invalid-bounded-autonomy-fixture {:decision-id decision-id})

      (not tripwire-clear?)
      (fallback :tripwire-fired {:decision-id decision-id})

      (not budget-valid?)
      (fallback :query-or-resource-bound-failed
                {:budget budget :query-limit query-limit
                 :maximum-query-limit maximum-query-limit})

      (not (warm-cache-gate? serving-cache-gate))
      (fallback :block-unwarmed-click {:serving-cache-gate serving-cache-gate})

      (not explanation-valid?)
      (fallback :admissibility-explanation-or-provenance-incomplete
                {:decision-id decision-id})

      (not outcome-valid?)
      (fallback :independent-outcome-incomplete
                {:observed-outcome observed-outcome})

      (not calibration-honest?)
      (fallback :calibration-status-incomplete {:calibration calibration})

      (not (port-7070-delivery-gate? delivery-qa))
      (fallback :delivery-qa-gate-invalid {:delivery-qa delivery-qa})

      :else
      {:status :bounded-autonomy-authorized
       :algorithm algorithm
       :effective-rung :bounded-autonomy
       :operator-decision-evidence-id
       futon3c.peripheral.strategic-canary/operator-decision-evidence-id
       :recommendation
       (select-keys recommendation
                    [:policy-id :mission-ids :shadow-probability :e-s
                     :predicted-g-s :hard-support :proposal-reasons
                     :memory-ids :provenance])
       :counterfactual-baseline (:counterfactual-baseline trace)
       :observed-outcome observed-outcome
       :memory-use {:surfaced-ids (:memory-ids recommendation)
                    :used-ids (:memory-ids-used observed-outcome)}
       :resource-audit {:outer-budget budget
                        :query-limit query-limit
                        :maximum-query-limit maximum-query-limit
                        :within-bounds? true}
       :machine-gates
       {:admissible-mission-ids admissible-mission-ids
        :armed-tripwire-ids (vec (sort armed-tripwires))
        :tripwire-clear? true
        :serving-cache serving-cache-gate
        :witness-and-provenance? true
        :query-bounds? true}
       :calibration calibration
       :delivery-qa delivery-qa
       :fallback-controller :current-additive
       :fallback-mode :explicit-rollback-only
       :rollback-boundary
       futon3c.peripheral.strategic-canary/rollback-boundary
       :rollback-reason nil
       :enactment
       {:authorized? true
        :executed? false
        :authority :machine-determined-bounded-autonomy
        :operator-confirmation-required? false}
       :selected-mission (first (:mission-ids recommendation))
       :selected-mission-ids (:mission-ids recommendation)
       :live-ordering-changed? true})))

(defn advice-only
  "Compatibility entry point.  Operator gating has been retired; evaluate the
   bounded-autonomy contract instead."
  [shadow-result fixture]
  (bounded-autonomy shadow-result fixture))
