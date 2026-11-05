(ns futon3c.agency.promise-state
  "Pure promise-history fold and its current-state hyperedge projection."
  (:require [clojure.edn :as edn]
            [futon3c.agency.promise-log :as promise-log]))

(def creation-types
  #{:promise/park-made :promise/followup-enqueued
    :promise/park-store-changed :promise/followup-store-changed})

(def terminal-types #{:promise/released :promise/followup-terminal})

(defn state-id [promise-id] (str "promise-state:" promise-id))

(defn- incomplete [promise-id reason & [detail]]
  (merge {:promise/state :incomplete
          :promise-id promise-id
          :reason reason}
         detail))

(defn- creation-record [row]
  (try
    (:record (edn/read-string (get-in row [:evidence/body :history/payload-edn])))
    (catch Throwable _ nil)))

(defn advance-state
  "Apply one evidence-shaped history ROW to PREVIOUS. Returns a typed
   :incomplete marker when continuity or the creation payload is unavailable."
  [previous row]
  (let [body (:evidence/body row)
        promise-id (:history/promise-id body)
        sequence (:history/promise-sequence body)
        predecessor (:history/predecessor body)
        transition (:evidence/type row)
        evidence-id (:evidence/id row)
        event-time (:evidence/at row)]
    (cond
      (= :incomplete (:promise/state previous)) previous

      (nil? previous)
      (cond
        (not= 1 sequence)
        (incomplete promise-id :missing-origin {:sequence sequence})

        (not (contains? creation-types transition))
        (incomplete promise-id :invalid-origin {:transition transition})

        predecessor
        (incomplete promise-id :invalid-predecessor {:sequence sequence})

        :else
        (if-let [record (creation-record row)]
          {:promise-id promise-id
           :status (if (contains? terminal-types transition) :closed :open)
           :head-sequence sequence
           :head-evidence-id evidence-id
           :terminal-transition (when (contains? terminal-types transition) transition)
           :debtor (:agent record)
           :creditor (:beneficiary record)
           :fulfilment-criterion (:fulfilment-criterion record)
           :creation-event-time event-time
           :last-event-time event-time}
          (incomplete promise-id :invalid-creation-payload
                      {:evidence-id evidence-id})))

      (not= promise-id (:promise-id previous))
      (incomplete promise-id :promise-id-mismatch
                  {:expected (:promise-id previous)})

      (not= sequence (inc (:head-sequence previous)))
      (incomplete promise-id :missing-transition
                  {:expected-sequence (inc (:head-sequence previous))
                   :sequence sequence})

      (or (not= (:sequence predecessor) (:head-sequence previous))
          (not= (:id predecessor) (:head-evidence-id previous)))
      (incomplete promise-id :predecessor-mismatch {:sequence sequence})

      :else
      (cond-> (assoc previous
                     :head-sequence sequence
                     :head-evidence-id evidence-id
                     :last-event-time event-time)
        (contains? terminal-types transition)
        (assoc :status :closed :terminal-transition transition)))))

(defn fold-state
  "Fold one promise's complete history. Rows are ordered by promise sequence;
   a gap or malformed origin returns a typed :incomplete marker."
  [rows]
  (let [ordered (sort-by #(get-in % [:evidence/body :history/promise-sequence]) rows)]
    (if (seq ordered)
      (reduce advance-state nil ordered)
      (incomplete nil :empty-history))))

(defn- present-values [m]
  (into {} (remove (comp nil? val)) m))

(defn state->hyperedge
  "Project STATE to the stable bitemporal serving edge."
  [state]
  (when (= :incomplete (:promise/state state))
    (throw (ex-info "Incomplete promise history cannot produce state"
                    {:reason :incomplete-promise-history :state state})))
  (let [promise-id (:promise-id state)
        debtor (:debtor state)
        creditor (:creditor state)]
    {:hx/id (state-id promise-id)
     :hx/type :promise/state
     :hx/endpoints (cond-> [promise-id]
                     debtor (conj (str "agent:" debtor))
                     creditor (conj (str "agent:" creditor))
                     true (conj (str "promise-status:" (name (:status state)))))
     :hx/valid-time (:last-event-time state)
     :hx/props (present-values state)}))

(defn- prior-state [promise-id]
  (try
    (some-> (promise-log/*get-fn* (state-id promise-id)) :hx/props)
    (catch clojure.lang.ExceptionInfo error
      (if (= 404 (:status (ex-data error))) nil (throw error)))))

(defn write!
  "Increment the current state with ENTRY, write it, and require exact readback."
  [entry]
  (let [promise-id (get-in entry [:evidence/body :history/promise-id])
        sequence (get-in entry [:evidence/body :history/promise-sequence])
        previous (prior-state promise-id)]
    ;; A state version can commit immediately before the outbox persist fails.
    ;; Since this writer only advances contiguous rows, a later/equal head proves
    ;; this row was already included and makes replay safe without rewinding.
    (if (and previous (>= (:head-sequence previous) sequence))
      {:ok true :state previous :replayed? true}
      (let [state (advance-state previous entry)]
        (when (= :incomplete (:promise/state state))
          (throw (ex-info "Promise state projection is incomplete"
                          {:reason :incomplete-promise-state :state state})))
        (let [edge (state->hyperedge state)
              post-error (try
                           (promise-log/*post-fn* edge)
                           nil
                           (catch Throwable error error))
              stored (try
                       (promise-log/*get-fn* (:hx/id edge))
                       (catch Throwable read-error
                         (throw (ex-info "Promise state write/readback failed"
                                         {:reason :promise-state-unavailable
                                          :hx/id (:hx/id edge)
                                          :post-error (some-> post-error ex-message)}
                                         read-error))))]
          (when-not (promise-log/same-edge? edge stored)
            (throw (ex-info "Promise state exact readback mismatch"
                            {:reason :promise-state-readback-mismatch
                             :hx/id (:hx/id edge)})))
          {:ok true :state state :hyperedge stored
           :replayed? (boolean post-error)})))))
