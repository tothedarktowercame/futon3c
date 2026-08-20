(ns futon3c.apm.campaign-qualification
  "Derive qualification facts from concrete frame-18 observation routes.

  This adapter never fills missing evidence with optimistic defaults."
  (:require [clojure.edn :as edn]
            [futon3c.apm.campaign-gates :as gates]))

(def required-roles #{:solver :student :guide :scribe :proctor :analyst})

(defn- ms->minutes [x]
  (when (and (integer? x) (not (neg? x))) (quot x 60000)))

(defn- all-role-value [seat-configs key]
  (let [values (map #(get-in seat-configs [% key]) required-roles)]
    (when (every? integer? values) (apply min values))))

(defn seat-configs-from-roster
  "Extract effective timeout metadata for ROLE->AGENT-ID from an agents response."
  [response role-seat-ids]
  (let [agents (or (:agents response) (get response "agents"))]
    (into {}
          (map (fn [[role agent-id]]
                 (let [agent (or (get agents agent-id) (get agents (str agent-id)))
                       metadata (or (:metadata agent) (get agent "metadata"))
                       policy (or (:effective-timeouts metadata)
                                  (get metadata "effective-timeouts"))]
                   [role policy])))
          role-seat-ids)))

(defn- complete-role-map [roles]
  (into {} (map (fn [role] [role (contains? roles role)]) required-roles)))

(defn derive-facts
  "Normalize already-observed routes into the frame-18 gate vocabulary.

  Inputs are transport-neutral values obtained by the caller from the serving
  roster, registration validator, campaign certificate, cycle trace, and
  durable receipts. Missing routes remain nil and therefore fail closed."
  [{:keys [seat-configs registration-check cast-check continuation-check
           projection-check trace-check separation-check receipt-check
           apparatus-check problem-check specification-check]}]
  (let [roles (set (keys seat-configs))
        applicable-request-values
        (keep (fn [[_ config]]
                (let [value (:request-timeout-ms config)]
                  (when (integer? value) value)))
              seat-configs)
        request-ms (when (seq applicable-request-values)
                     (apply min applicable-request-values))
        turn-ms (all-role-value seat-configs :turn-timeout-ms)]
    {:specification {:valid? (:valid? specification-check)
                     :digest (:digest specification-check)
                     :frame-matches? (:frame-matches? specification-check)
                     :registration-matches?
                     (:registration-matches? specification-check)}
     :timeouts
     {:explicit? (and (= required-roles roles)
                      (seq applicable-request-values)
                      (every? #(and (or (integer? (:request-timeout-ms %))
                                        (= :not-applicable
                                           (:request-timeout-ms %)))
                                    (integer? (:turn-timeout-ms %))
                                    (keyword? (:request/source %))
                                    (keyword? (:turn/source %)))
                              (vals seat-configs)))
      :request-minutes (ms->minutes request-ms)
      :turn-minutes (ms->minutes turn-ms)
      :solver-minutes (ms->minutes
                       (get-in seat-configs [:solver :turn-timeout-ms]))
      :student-minutes (ms->minutes
                        (get-in seat-configs [:student :turn-timeout-ms]))
      :frame-minutes (ms->minutes (:frame-timeout-ms registration-check))}
     :pins {:complete? (:complete? registration-check)
            :coherent? (:coherent? registration-check)
            :branch (:branch registration-check)
            :commit (:commit registration-check)
            :worktree (:worktree registration-check)
            :worktree-clean? (:worktree-clean? registration-check)
            :head-matches? (:head-matches? registration-check)
            :dedicated-worktree? (:dedicated-worktree? registration-check)}
     :problem {:topology? (:topology? problem-check)
               :classification-source (:classification-source problem-check)}
     :cast {:ready? (and (= required-roles roles) (:ready? cast-check))
            :attributed? (:attributed? cast-check)}
     :continuations {:durable? (:durable? continuation-check)
                     :wake-tested? (:wake-tested? continuation-check)}
     :projection {:ledger-derived? (:ledger-derived? projection-check)
                  :frame-matches? (:frame-matches? projection-check)}
     :legs {:required (complete-role-map (set (:completed-roles trace-check)))}
     :memory {:recall-invoked? (:recall-invoked? trace-check)
              :terrain-measured? (:terrain-measured? trace-check)
              :dispositions-complete? (:dispositions-complete? trace-check)
              :promotion-reviewed? (:promotion-reviewed? trace-check)}
     :separation {:author-reviewer-distinct?
                  (:author-reviewer-distinct? separation-check)
                  :arms-isolated? (:arms-isolated? separation-check)}
     :receipts {:durable? (:durable? receipt-check)
                :replayable? (:replayable? receipt-check)}
     :apparatus {:unchanged-since-open?
                 (:unchanged-since-open? apparatus-check)}}))

(defn read-plan [path]
  (try
    (let [plan (edn/read-string (slurp path))]
      (if (vector? (:qualification/gates plan))
        {:ok true :plan plan}
        {:ok false :error/code :campaign-qualification-gates-required}))
    (catch Throwable t
      {:ok false :error/code :campaign-qualification-plan-unreadable
       :finding {:message (.getMessage t)}})))

(defn gate-provider
  "Create the campaign-stepper provider for one pinned plan and observation fn.
  OBSERVATION-FN is called on every inspection, never cached across a step."
  [plan observation-fn]
  (fn [{:keys [certificate obligation]}]
    (let [observations (observation-fn {:certificate certificate
                                        :obligation obligation})
          facts (derive-facts observations)]
      (gates/evaluate-obligation (:qualification/gates plan)
                                 facts obligation))))
