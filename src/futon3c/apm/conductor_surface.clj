(ns futon3c.apm.conductor-surface
  "Typed transport boundary for an already-bound problem conductor."
  (:require [futon3c.agency.registry :as agency]
            [futon3c.apm.conductor :as conductor]
            [futon3c.apm.conductor-binding :as binding]))

(def ^:private operations
  {:dispatch-solver #'conductor/dispatch-solver!
   :guide-solver #'conductor/guide-solver!
   :dispatch-student #'conductor/dispatch-student!
   :dispatch-scribe #'conductor/dispatch-scribe!
   :promote-artifact #'conductor/promote-artifact!
   :record-scribe-lanes #'conductor/record-scribe-lanes!
   :record-solver-attempt #'conductor/record-solver-attempt!
   :deposit #'conductor/deposit!
   :record-students #'conductor/record-students!
   :write-use #'conductor/write-uses!
   :adjudicate #'conductor/adjudicate!
   :close #'conductor/close!})

(def ^:private promotion-verdicts #{:approve :challenge :reject})

(defn- normalize-operation [operation]
  (cond
    (keyword? operation) operation
    (string? operation) (keyword operation)
    :else nil))

(defn- authenticated-session? [agent-id session-id]
  (= session-id (:agent/session-id (agency/get-agent agent-id))))

(defn- authenticate-promotion-reviewers [agent-id disposition]
  (if (map? disposition)
    (update disposition :promotion-result
            (fn [promotions]
              (mapv #(assoc % :acting-identity (str agent-id))
                    (or promotions []))))
    disposition))

(defn- reviewer-mismatch [agent-id operation args]
  (case operation
    :adjudicate
    (some (fn [promotion]
            (let [reviewer (:reviewer promotion)]
              (when (and (string? reviewer) (not= (str agent-id) reviewer))
                {:reviewer reviewer :acting-identity (str agent-id)})))
          (:promotion-result (first args)))

    :promote-artifact
    (let [reviewer (:reviewer (first args))]
      (when (and (string? reviewer) (not= (str agent-id) reviewer))
        {:reviewer reviewer :acting-identity (str agent-id)}))

    nil))

(defn- transport-args [agent-id operation args]
  ;; JSON has no keyword value type. Decode only closed enums in the public
  ;; conductor API at this transport boundary; arbitrary payload values remain
  ;; byte-for-byte data rather than being guessed into keywords.
  (cond-> (vec args)
    (and (= :adjudicate operation) (map? (first args)))
    (update-in [0 :outcome] #(if (string? %) (keyword %) %))

    (and (= :adjudicate operation) (map? (first args)))
    (update 0 #(authenticate-promotion-reviewers agent-id %))

    (and (= :promote-artifact operation) (map? (first args)))
    (update 0 assoc :acting-identity (str agent-id))

    (and (= :promote-artifact operation) (map? (first args))
         (string? (:verdict (first args))))
    (update-in [0 :verdict] keyword)

    (and (= :record-scribe-lanes operation) (map? (first args)))
    (update-in [0 :lane] #(if (string? %) (keyword %) %))))

(defn- invalid-promotion-verdict [operation args]
  (when (and (= :promote-artifact operation)
             (map? (first args))
             (contains? (first args) :verdict))
    (let [verdict (:verdict (first args))
          decoded (if (string? verdict) (keyword verdict) verdict)]
      (when-not (contains? promotion-verdicts decoded)
        {:verdict verdict}))))

(defn execute-action!
  "Execute a closed-vocabulary conductor action for an authenticated session."
  [agent-id session-id action]
  (let [operation (normalize-operation (:operation action))
        mismatch (reviewer-mismatch agent-id operation (:args action))
        invalid-verdict (invalid-promotion-verdict operation (:args action))]
    (cond
      (not (authenticated-session? agent-id session-id))
      {:ok false :error/code :conductor-session-unauthenticated}

      (not (contains? operations operation))
      {:ok false :error/code :conductor-operation-unknown
       :operation operation}

      mismatch
      {:ok false :error/code :reviewer-not-actor
       :finding (assoc mismatch :failure :reviewer-not-actor)}

      invalid-verdict
      {:ok false :error/code :promotion-verdict-invalid
       :finding (assoc invalid-verdict
                       :failure :promotion-verdict-invalid
                       :allowed (vec (sort promotion-verdicts)))}

      :else
      (binding/execute!
       agent-id session-id (assoc action
                                  :operation operation
                                  :args (transport-args agent-id operation
                                                        (:args action)))
       (fn [handle op args]
         (apply (get operations op) handle args))
       conductor/record-action-refusal!))))

(defn status [agent-id session-id]
  (binding/status agent-id session-id))

(defn abandon!
  "Authenticate and abandon the caller's explicitly versioned live binding."
  [agent-id session-id cycle-id version]
  (if-not (authenticated-session? agent-id session-id)
    {:ok false :error/code :conductor-session-unauthenticated}
    (binding/abandon! agent-id session-id cycle-id version)))

(defn takeover!
  "Authenticate and transfer an explicitly named saved cycle to this session."
  [agent-id session-id cycle-id version]
  (if-not (authenticated-session? agent-id session-id)
    {:ok false :error/code :conductor-session-unauthenticated}
    (binding/takeover! agent-id session-id cycle-id version
                       (fn [source named-cycle named-version]
                         (conductor/resume-fresh
                          source named-cycle named-version
                          {:agent agent-id :session session-id})))))

(defn resume-parked!
  "Authenticate a parked wake-up without mutating the conductor handle."
  [agent-id session-id cycle-id version]
  (if-not (authenticated-session? agent-id session-id)
    {:ok false :error/code :conductor-session-unauthenticated}
    (binding/check-continuation agent-id session-id cycle-id version)))
