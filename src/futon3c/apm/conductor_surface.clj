(ns futon3c.apm.conductor-surface
  "Typed transport boundary for an already-bound problem conductor."
  (:require [futon3c.agency.registry :as agency]
            [futon3c.apm.conductor :as conductor]
            [futon3c.apm.conductor-binding :as binding]))

(def ^:private operations
  {:dispatch-solver conductor/dispatch-solver!
   :guide-solver conductor/guide-solver!
   :dispatch-student conductor/dispatch-student!
   :dispatch-scribe conductor/dispatch-scribe!
   :record-solver-attempt conductor/record-solver-attempt!
   :deposit conductor/deposit!
   :record-students conductor/record-students!
   :adjudicate conductor/adjudicate!
   :close conductor/close!})

(defn- normalize-operation [operation]
  (cond
    (keyword? operation) operation
    (string? operation) (keyword operation)
    :else nil))

(defn- authenticated-session? [agent-id session-id]
  (= session-id (:agent/session-id (agency/get-agent agent-id))))

(defn- transport-args [operation args]
  ;; JSON has no keyword value type. Decode the one closed enum in the public
  ;; conductor API at this transport boundary; arbitrary payload values remain
  ;; byte-for-byte data rather than being guessed into keywords.
  (if (and (= :adjudicate operation) (map? (first args)))
    (update-in (vec args) [0 :outcome]
               #(if (string? %) (keyword %) %))
    args))

(defn execute-action!
  "Execute a closed-vocabulary conductor action for an authenticated session."
  [agent-id session-id action]
  (let [operation (normalize-operation (:operation action))]
    (cond
      (not (authenticated-session? agent-id session-id))
      {:ok false :error/code :conductor-session-unauthenticated}

      (not (contains? operations operation))
      {:ok false :error/code :conductor-operation-unknown
       :operation operation}

      :else
      (binding/execute!
       agent-id session-id (assoc action
                                  :operation operation
                                  :args (transport-args operation (:args action)))
       (fn [handle op args]
         (apply (get operations op) handle args))))))

(defn status [agent-id session-id]
  (binding/status agent-id session-id))

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
