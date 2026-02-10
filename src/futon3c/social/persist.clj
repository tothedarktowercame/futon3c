(ns futon3c.social.persist
  "S-persist: in-memory session continuity with authoritative transcript.

   R8 (authoritative transcript): sessions keep an append-only structured event
   history (:events). Derived state (:data) is computed from events and is not
   a separate source of truth.

   R9 (structured events): events and state updates are maps; no free-text blobs.
   R4 (loud failure): never return nil; return SessionRecord or SocialError."
  (:require [futon3c.social.shapes :as shapes])
  (:import [java.time Instant]
           [java.util UUID]))

(defonce ^{:doc "In-memory session store: {session-id -> SessionRecord}"} !sessions
  (atom {}))

(defn reset-sessions!
  "Reset all sessions (testing only)."
  []
  (reset! !sessions {}))

(defn- now-str []
  (str (Instant/now)))

(defn- social-error
  [code message & {:as context}]
  (cond-> {:error/component :S-persist
           :error/code code
           :error/message message
           :error/at (now-str)}
    (seq context) (assoc :error/context context)))

(defn- keyword-map?
  [m]
  (and (map? m) (every? keyword? (keys m))))

(defn- ensure-session-record
  [session]
  (if (shapes/valid? shapes/SessionRecord session)
    session
    (social-error :invalid-session-record
                  "Internal error: SessionRecord did not conform to shape"
                  :session session
                  :validation (or (:error (shapes/validate shapes/SessionRecord session)) {}))))

(defn- new-session-id []
  (str "sess-" (UUID/randomUUID)))

(defn persist-session!
  "Persist session state after a successful dispatch.
   Takes a dispatch receipt and session context, produces a SessionRecord.
   R8: the persisted record is the authoritative transcript.
   R9: all persisted events are structured, machine-parseable maps."
  [dispatch-receipt session-context]
  (cond
    (not (shapes/valid? shapes/DispatchReceipt dispatch-receipt))
    (social-error :invalid-input
                  "Invalid DispatchReceipt input"
                  :dispatch-receipt dispatch-receipt
                  :validation (or (:error (shapes/validate shapes/DispatchReceipt dispatch-receipt)) {}))

    (not (true? (:receipt/delivered? dispatch-receipt)))
    (social-error :dispatch-not-delivered
                  "Cannot persist session: dispatch not delivered"
                  :dispatch-receipt dispatch-receipt)

    (and (some? session-context) (not (keyword-map? session-context)))
    (social-error :invalid-input
                  "Session context must be a map with keyword keys"
                  :session-context session-context)

    :else
    (let [ctx (or session-context {})
          sid (or (:session/id ctx) (new-session-id))
          agent-id (:receipt/to dispatch-receipt)
          at (now-str)
          event {:event/type :dispatch
                 :event/at at
                 :dispatch/receipt dispatch-receipt
                 :session/context (dissoc ctx :session/id)}
          state {:events [event]
                 :data (dissoc ctx :session/id)}
          session {:session/id sid
                   :session/agent-id agent-id
                   :session/state state
                   :session/at at}]
      (if (contains? @!sessions sid)
        (social-error :session-already-exists
                      "Session already exists"
                      :session-id sid)
        (let [validated (ensure-session-record session)]
          (if (shapes/valid? shapes/SocialError validated)
            validated
            (do
              (swap! !sessions assoc sid validated)
              {:ok true :session validated})))))))

(defn get-session
  "Retrieve a session record by session ID.
   Returns SessionRecord or SocialError."
  [session-id]
  (cond
    (not (string? session-id))
    (social-error :invalid-input "Session id must be a string" :session-id session-id)

    :else
    (if-let [session (get @!sessions session-id)]
      session
      (social-error :session-not-found "Session not found" :session-id session-id))))

(defn update-session!
  "Update an existing session's state.
   R8: updates are appended to the authoritative transcript, not overwrites."
  [session-id state-update]
  (cond
    (not (string? session-id))
    (social-error :invalid-input "Session id must be a string" :session-id session-id)

    (not (keyword-map? state-update))
    (social-error :invalid-input "State update must be a map with keyword keys" :state-update state-update)

    :else
    (if-let [session (get @!sessions session-id)]
      (let [at (now-str)
            event {:event/type :state-update
                   :event/at at
                   :state/update state-update}
            old-state (:session/state session)
            events (vec (conj (vec (or (:events old-state) [])) event))
            ;; Derived snapshot. Transcript is authoritative; snapshot is derived.
            data (merge (or (:data old-state) {}) state-update)
            updated (assoc session
                           :session/state {:events events :data data}
                           :session/at at)
            validated (ensure-session-record updated)]
        (if (shapes/valid? shapes/SocialError validated)
          validated
          (do
            (swap! !sessions assoc session-id validated)
            {:ok true :session validated})))
      (social-error :session-not-found "Session not found" :session-id session-id))))

(defn list-sessions
  "List all active sessions, optionally filtered by agent-id."
  [{:keys [agent-id]}]
  (let [sessions (vals @!sessions)
        sessions (if (nil? agent-id)
                   sessions
                   (if (shapes/valid? shapes/TypedAgentId agent-id)
                     (filter #(= agent-id (:session/agent-id %)) sessions)
                     []))]
    (->> sessions
         (sort-by :session/id)
         vec)))

