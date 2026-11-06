(ns futon3c.arxana.journal
  "D3 subscription substrate: an ordered event journal derived from authority
  mutations (INSTANTIATE increment 6, per D3 subscription model and D7
  authority table — journal/head persistence futon1b, projection/retention
  Arxana).

  Events carry a monotonically ordered cursor, event ID, cause message ID,
  topic, affected IDs, before/after revision IDs and a semantic payload
  digest. Delivery is at-least-once; event IDs deduplicate. The journal is
  backed by the graph store: every event is persisted as an immutable
  :arxana/journal-event entity, so durability is exactly the store's
  durability (the mock store is memory-durable; the futon1b conditional route
  is XTDB-durable). Retention is an Arxana-side projection concern: cursors
  older than the retained window are expired and subscribers must resync —
  replay is never guessed."
  (:require [futon3c.arxana.digest :as digest]
            [futon3c.arxana.envelope :as env]
            [futon3c.arxana.store :as store]))

(def topics
  "Journal topics a client may subscribe to in v1."
  #{:revision/ingested :assertion/promoted :correspondence/asserted})

(defn event-id
  "Deterministic event ID from cursor + semantic fields (dedupe key; crash
  republish of the same applied command produces the same ID)."
  ^String [cursor topic cause-message-id payload-digest]
  (str "arxana:journal-event:sha256:"
       (digest/canonical-digest {:cursor cursor
                                 :topic topic
                                 :cause/message-id cause-message-id
                                 :payload/digest payload-digest})))

(defrecord Journal [graph-store state retention]
  ;; state: {:head n :events {cursor event} :min-retained n}
  )

(defn journal
  "A store-backed journal. retention is the number of events kept replayable
  (default 1000); older cursors expire."
  ([graph-store] (journal graph-store 1000))
  ([graph-store retention]
   (->Journal graph-store (atom {:head 0 :events (sorted-map)}) retention)))

(defn head-cursor
  "The current journal head cursor (0 when empty)."
  [j]
  (:head @(:state j)))

(defn append!
  "Persist and publish one event. Returns {:ok event} with :cursor and
  :event/id assigned. topic must be known; the event is written to the graph
  store before the in-memory index advances, so a crash between apply and
  publish can only republish (same event ID), never reapply."
  [j event]
  (let [{topic :topic
         message-id :cause/message-id
         ids :affected/ids
         before :before/revision-id
         after :after/revision-id
         payload :payload} event]
    (if-not (contains? topics topic)
      {:error (env/error :invalid :arxana.journal/unknown-topic
                         {:topic topic :known (vec (sort topics))} nil)}
      (let [payload-digest (digest/canonical-digest (or payload {}))
            cursor (inc (head-cursor j))
            eid (event-id cursor topic message-id payload-digest)
            event* {:event/id eid
                    :cursor cursor
                    :topic topic
                    :cause/message-id message-id
                    :affected/ids (vec ids)
                    :before/revision-id before
                    :after/revision-id after
                    :payload/digest payload-digest
                    :payload payload}
            entity {:entity/id eid
                    :type :arxana/journal-event
                    :external-id eid
                    :props (dissoc event* :payload)}]
        (store/put-entity! (:graph-store j) entity)
        (swap! (:state j)
               (fn [s]
                 (let [s' (-> s
                              (assoc :head cursor)
                              (update :events assoc cursor event*))
                       floor (- cursor (:retention j))]
                   (if (pos? floor)
                     (update s' :events
                             (fn [evs] (into (sorted-map)
                                             (filter (fn [[c _]] (> c floor)) evs))))
                     s'))))
        {:ok event*}))))

(defn- min-retained [j]
  (if-let [c (first (keys (:events @(:state j))))]
    c
    (inc (head-cursor j))))

(defn events-after
  "Ordered events strictly after cursor. A cursor older than the retained
  window is expired: {:error ... :resync-required true} — the caller takes a
  fresh snapshot rather than guessed replay (D3)."
  [j cursor]
  (cond
    (not (and (integer? cursor) (<= 0 cursor)))
    {:error (env/error :invalid :arxana.journal/malformed-cursor
                       {:cursor cursor} nil)}

    (> cursor (head-cursor j))
    {:error (env/error :invalid :arxana.journal/cursor-beyond-head
                       {:cursor cursor :head (head-cursor j)} nil)}

    (< cursor (dec (min-retained j)))
    {:error (assoc (env/error :stale :arxana.journal/cursor-expired
                              {:cursor cursor
                               :min-retained (min-retained j)
                               :head (head-cursor j)}
                              [:subscription/open])
                   :resync-required true)}

    :else
    {:ok {:events (vec (vals (filter (fn [[c _]] (> c cursor))
                                     (:events @(:state j)))))
          :head (head-cursor j)}}))
