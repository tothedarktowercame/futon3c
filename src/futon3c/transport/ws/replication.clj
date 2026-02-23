(ns futon3c.transport.ws.replication
  "Laptop-side evidence replication sender.

   Polls a local evidence store and sends non-replicated entries over WS
   using frames of the form:
     {\"type\" \"evidence\" \"entry\" <EvidenceEntry-map>}

   Tracks pending/acked state so entries are not resent in steady-state.
   High-water mark advances on evidence_ack (or duplicate-id fallback)."
  (:require [clojure.string :as str]
            [futon3c.evidence.store :as estore])
  (:import [java.time Instant]))

(def ^:const default-interval-ms 30000)

(defn make-state
  "Create replication state atom.

   Keys:
   - :last-acked-at  ISO timestamp or nil
   - :acked-ids      ids acked at :last-acked-at (ties at same timestamp)
   - :pending        {evidence-id evidence-at} sent but not yet acked"
  []
  (atom {:last-acked-at nil
         :acked-ids #{}
         :pending {}}))

(defn- parse-instant-safe [t]
  (try
    (cond
      (instance? Instant t) t
      (string? t) (Instant/parse t)
      :else (Instant/ofEpochMilli 0))
    (catch Exception _
      (Instant/ofEpochMilli 0))))

(defn- entry-at-inst [entry]
  (parse-instant-safe (:evidence/at entry)))

(defn- replicated-entry? [entry]
  (let [tags (or (:evidence/tags entry) [])]
    (boolean (some #{:replicated "replicated"} tags))))

(defn- acked-or-before? [state entry]
  (let [last (:last-acked-at state)
        last-inst (when last (parse-instant-safe last))
        entry-inst (entry-at-inst entry)
        evidence-id (:evidence/id entry)]
    (cond
      (nil? last-inst) false
      (.isBefore entry-inst last-inst) true
      (.isAfter entry-inst last-inst) false
      :else (contains? (:acked-ids state) evidence-id))))

(defn- advance-ack*
  [state evidence-id evidence-at]
  (let [at-inst (parse-instant-safe evidence-at)
        last (:last-acked-at state)
        last-inst (when last (parse-instant-safe last))
        next-state
        (cond
          (nil? last-inst)
          (-> state
              (assoc :last-acked-at evidence-at)
              (assoc :acked-ids #{evidence-id}))

          (.isAfter at-inst last-inst)
          (-> state
              (assoc :last-acked-at evidence-at)
              (assoc :acked-ids #{evidence-id}))

          (.equals at-inst last-inst)
          (update state :acked-ids (fnil conj #{}) evidence-id)

          :else state)]
    (update next-state :pending dissoc evidence-id)))

(defn ack!
  "Mark EVIDENCE-ID acknowledged when present in :pending.

   Returns {:ok true :evidence-id ...} on success,
   {:ok false :reason :unknown-evidence-id} otherwise."
  [state* evidence-id]
  (if-let [at (get-in @state* [:pending evidence-id])]
    (do
      (swap! state* advance-ack* evidence-id at)
      {:ok true :evidence-id evidence-id})
    {:ok false :reason :unknown-evidence-id :evidence-id evidence-id}))

(defn reset-connection!
  "Drop pending (unacked) sends so a reconnect can resume from last acked point."
  [state*]
  (swap! state* assoc :pending {}))

(defn poll-once!
  "Poll EVIDENCE-STORE and send pending entries via SEND-FN.

   SEND-FN is called with a Clojure map payload suitable for JSON encoding.
   Returns {:sent <n> :ids [..]}.

   Filtering:
   - skips entries tagged :replicated / \"replicated\"
   - skips entries already acked by high-water mark
   - skips entries currently pending ack"
  [state* evidence-store send-fn]
  (let [state @state*
        q (cond-> {:query/include-ephemeral? false}
            (:last-acked-at state) (assoc :query/since (:last-acked-at state)))
        pending-ids (set (keys (:pending state)))
        entries (estore/query* evidence-store q)
        candidates (->> entries
                        (remove replicated-entry?)
                        (remove #(acked-or-before? state %))
                        (remove #(contains? pending-ids (:evidence/id %)))
                        (sort-by (fn [entry]
                                   [(entry-at-inst entry)
                                    (str (:evidence/id entry))]))
                        vec)]
    (doseq [entry candidates]
      (send-fn {"type" "evidence"
                "entry" entry})
      (swap! state* assoc-in [:pending (:evidence/id entry)] (:evidence/at entry)))
    {:sent (count candidates)
     :ids (mapv :evidence/id candidates)}))

(defn- ack-oldest-pending!
  [state*]
  (if-let [[evidence-id evidence-at]
           (first
            (sort-by (fn [[id at]] [(parse-instant-safe at) (str id)])
                     (seq (:pending @state*))))]
    (do
      (swap! state* advance-ack* evidence-id evidence-at)
      {:ok true :evidence-id evidence-id :synthetic? true})
    {:ok false :reason :no-pending}))

(defn handle-frame!
  "Handle inbound WS frames relevant to replication state.

   Supported frames:
   - evidence_ack: {\"type\" \"evidence_ack\", \"evidence_id\" \"...\", \"ok\" true}
   - duplicate-id errors: {\"type\" \"error\", \"code\" \"duplicate-id\", ...}

   duplicate-id is treated as idempotent success. If no evidence_id is provided,
   the oldest pending entry is acknowledged to keep forward progress."
  [state* frame]
  (let [frame-type (:type frame)]
    (cond
      (and (= "evidence_ack" frame-type)
           (true? (:ok frame))
           (string? (:evidence_id frame)))
      (assoc (ack! state* (:evidence_id frame)) :handled :evidence-ack)

      (and (= "error" frame-type)
           (= "duplicate-id" (:code frame)))
      (let [evidence-id (:evidence_id frame)
            result (if (and (string? evidence-id) (not (str/blank? evidence-id)))
                     (ack! state* evidence-id)
                     (ack-oldest-pending! state*))]
        (assoc result :handled :duplicate-id))

      :else
      {:ok true :handled :ignored})))

(defn start!
  "Start periodic replication.

   opts:
   - :evidence-store (required)
   - :send-fn (optional, can be set later via :set-send-fn!)
   - :interval-ms (default 30000)
   - :state (optional state atom from make-state)

   Returns handle map with:
   - :state
   - :poll!               ; manual single poll
   - :handle-frame!       ; call with parsed inbound WS frame
   - :set-send-fn!        ; update send function (e.g. on reconnect)
   - :reset-connection!   ; clear pending queue on reconnect
   - :stop-fn"
  [{:keys [evidence-store send-fn interval-ms state]}]
  (let [state* (or state (make-state))
        send* (atom send-fn)
        running* (atom true)
        interval-ms (long (max 1 (or interval-ms default-interval-ms)))
        worker (future
                 (while @running*
                   (try
                     (when-let [f @send*]
                       (poll-once! state* evidence-store f))
                     (catch Throwable _))
                   (Thread/sleep interval-ms)))]
    {:state state*
     :poll! (fn []
              (if-let [f @send*]
                (poll-once! state* evidence-store f)
                {:sent 0 :ids [] :reason :no-send-fn}))
     :handle-frame! (fn [frame] (handle-frame! state* frame))
     :set-send-fn! (fn [f] (reset! send* f))
     :reset-connection! (fn [] (reset-connection! state*))
     :stop-fn (fn []
                (reset! running* false)
                (future-cancel worker))}))
