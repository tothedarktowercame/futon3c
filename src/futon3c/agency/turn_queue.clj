(ns futon3c.agency.turn-queue
  "Flag-gated per-agent durable FIFO turn queue.

   The queue is intentionally independent of registry invocation. Callers
   accept a turn, then drain the recipient queue with their existing invoke
   function. Queue entries persist the routing metadata that reply delivery
   must use: from, to, surface, msg-id, seq, accepted-at."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.dev.config :as config])
  (:import [java.security MessageDigest]
           [java.time Instant]
           [java.util UUID]))

(def ^:private max-history 200)

(defn enabled?
  "True when the load-dark Car-3 queue path is explicitly enabled.
   Default is false; loading this namespace does not change invoke behavior."
  []
  (let [prop (System/getProperty "FUTON3C_DURABLE_QUEUE")]
    (if (some? prop)
      (not (#{"0" "false" "no" "off"} (str/lower-case (str/trim prop))))
      (config/env-bool "FUTON3C_DURABLE_QUEUE" false))))

(defn queue-store-path []
  (or (config/env "FUTON3C_DURABLE_QUEUE_PATH")
      "/tmp/futon3c-durable-turn-queue.edn"))

(defn- empty-state []
  {:queues {}
   :entries {}
   :seqs {}
   :msg-index {}
   :draining #{}
   :drained-frontier {}
   :history []})

(defonce ^:private !queue
  (atom nil))

(defonce ^:private !waiters
  (atom {}))

(defonce ^:private !processors
  (atom {}))

(defn- load-state []
  (let [f (io/file (queue-store-path))]
    (if (.exists f)
      (try
        (let [parsed (edn/read-string (slurp f))]
          (merge (empty-state) (select-keys parsed (keys (empty-state)))))
        (catch Throwable _
          (empty-state)))
      (empty-state))))

(defn- ensure-state! []
  (when (nil? @!queue)
    (reset! !queue (load-state)))
  @!queue)

(defn- persist! [state]
  (try
    (spit (queue-store-path) (pr-str state))
    (catch Throwable t
      (println (str "[turn-queue] persist failed: " (.getMessage t)))))
  state)

(defn- swap-state! [f & args]
  (ensure-state!)
  (persist! (apply swap! !queue f args)))

(defn clear!
  "Reset queue state. Intended for tests/dev only."
  []
  (clojure.core/reset! !waiters {})
  (clojure.core/reset! !processors {})
  (persist! (clojure.core/reset! !queue (empty-state))))

(defn snapshot []
  (ensure-state!))

(defn- now [] (str (Instant/now)))

(defn- sha256-hex [s]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes (str s) "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn- clean-str [x]
  (some-> x str str/trim not-empty))

(defn prompt-field
  "Best-effort field extraction from a prompt map or the standard wrapped
   prompt headers used by the transport layer."
  [prompt field]
  (cond
    (map? prompt)
    (or (get prompt field) (get prompt (name field)))

    (string? prompt)
    (let [label (case field
                  :from "(?:Caller|From)"
                  :surface "Surface"
                  :msg-id "(?:Msg-ID|Message-ID|Message Id|Message)"
                  (name field))
          re (re-pattern (str "(?im)^\\s*(?:[-*]\\s*)?" label "\\s*:\\s*(.+?)\\s*$"))]
      (some-> (re-find re prompt) second clean-str))

    :else nil))

(defn reply-route
  "Return the persisted origin route for a queued turn."
  [entry]
  {:from (:from entry)
   :surface (:surface entry)
   :msg-id (:msg-id entry)
   :seq (:seq entry)})

(defn- normalized-entry [entry]
  (let [prompt (:prompt entry)
        from (or (clean-str (:from entry))
                 (clean-str (prompt-field prompt :from))
                 "unknown")
        to (or (clean-str (:to entry)) "unknown")
        surface (or (clean-str (:surface entry))
                    (clean-str (prompt-field prompt :surface))
                    "unknown")
        supplied-msg-id (or (clean-str (:msg-id entry))
                            (clean-str (prompt-field prompt :msg-id)))]
    (assoc entry
           :from from
           :to to
           :surface surface
           :msg-id (or supplied-msg-id
                       (str "generated-" (subs (sha256-hex (str to ":" surface ":" (UUID/randomUUID)))
                                               0 16)))
           :msg-id-source (if supplied-msg-id :supplied :generated))))

(defn accept!
  "Accept and enqueue a turn. Assigns a per-recipient monotone :seq.
   Duplicate :msg-id for the recipient is recorded as :deduped and never
   enters the work queue."
  [entry]
  (let [entry* (normalized-entry entry)
        id (or (clean-str (:id entry*)) (str "turn-" (UUID/randomUUID)))
        waiter (promise)
        result (atom nil)]
    (swap! !waiters assoc id waiter)
    (when-let [process-fn (:process-fn entry*)]
      (swap! !processors assoc id process-fn))
    (let [entry* (dissoc entry* :process-fn)]
      (swap-state!
       (fn [state]
         (let [to (:to entry*)
               msg-id (:msg-id entry*)
               duplicate-id (get-in state [:msg-index to msg-id])
               accepted-at (now)]
           (if duplicate-id
             (let [original (get-in state [:entries duplicate-id])
                   deduped (-> entry*
                               (assoc :id id
                                      :seq (:seq original)
                                      :accepted-at accepted-at
                                      :status :deduped
                                      :original-id duplicate-id))]
               (reset! result {:status :deduped :entry deduped :waiter waiter})
               (-> state
                   (assoc-in [:entries id] deduped)
                   (update :history #(vec (take-last max-history (conj (or % []) deduped))))))
             (let [seq* (inc (long (get-in state [:seqs to] 0)))
                   queued (-> entry*
                              (assoc :id id
                                     :seq seq*
                                     :accepted-at accepted-at
                                     :status :queued))]
               (reset! result {:status :queued :entry queued :waiter waiter})
               (-> state
                   (assoc-in [:seqs to] seq*)
                   (assoc-in [:msg-index to msg-id] id)
                   (assoc-in [:entries id] queued)
                   (update-in [:queues to] (fnil conj []) id)))))))
      (when (= :deduped (:status @result))
        (deliver waiter {:result "[deduped turn: msg-id already accepted]"
                         :turn-queue/status :deduped
                         :turn-queue/entry (:entry @result)
                         :turn-queue/reply-route (reply-route (:entry @result))}))
      @result)))

(defn- acquire-drain! [agent-id]
  (let [acquired? (atom false)]
    (swap-state!
     (fn [state]
       (if (contains? (:draining state) agent-id)
         state
         (do (reset! acquired? true)
             (update state :draining conj agent-id)))))
    @acquired?))

(defn- release-drain! [agent-id]
  (swap-state! update :draining disj agent-id)
  true)

(defn- pop-next! [agent-id]
  (let [entry (atom nil)]
    (swap-state!
     (fn [state]
       (if-let [id (first (get-in state [:queues agent-id]))]
         (do
           (reset! entry (get-in state [:entries id]))
           (update-in state [:queues agent-id] (comp vec rest)))
         state)))
    @entry))

(defn- mark-terminal! [entry status result]
  (let [entry* (assoc entry
                      :status status
                      :finished-at (now)
                      :reply-route (reply-route entry))]
    (swap-state!
     (fn [state]
       (cond-> (-> state
                   (assoc-in [:entries (:id entry*)] entry*)
                   (update :history #(vec (take-last max-history (conj (or % []) entry*)))))
         (and (:seq entry*) (not= :deduped status))
         (update-in [:drained-frontier (:to entry*)] (fnil max 0) (:seq entry*)))))
    (when-let [p (get @!waiters (:id entry*))]
      (deliver p (cond-> result
                   (map? result)
                   (assoc :turn-queue/status status
                          :turn-queue/entry entry*
                          :turn-queue/reply-route (reply-route entry*))))
      (swap! !waiters dissoc (:id entry*)))
    entry*))

(defn drain!
  "Drain AGENT-ID's FIFO queue using PROCESS-FN, one caller at a time.
   PROCESS-FN receives the persisted entry and returns the invoke result map.
   Entries accepted through accept-and-drain! carry their own processor; the
   argument is a fallback for direct tests/dev drains."
  [agent-id process-fn]
  (let [aid (clean-str agent-id)]
    (when (acquire-drain! aid)
      (try
        (loop [processed []]
          (if-let [entry (pop-next! aid)]
            (let [frontier (get-in (snapshot) [:drained-frontier aid] 0)
                  stale? (and (:seq entry) (< (:seq entry) frontier))
                  entry-process-fn (or (get @!processors (:id entry)) process-fn)
                  result (if stale?
                           {:result "[stale turn reconciled]"
                            :turn-queue/status :stale}
                           (try
                             (entry-process-fn entry)
                             (catch Throwable t
                               {:error (.getMessage t)
                                :exception-class (.getName (class t))})))
                  status (cond
                           stale? :stale
                           (:error result) :failed
                           :else :processed)
                  entry* (mark-terminal! entry status result)]
              (swap! !processors dissoc (:id entry))
              (recur (conj processed entry*)))
            processed))
        (finally
          (release-drain! aid))))))

(defn accept-and-drain!
  "Accept ENTRY, drain its recipient queue, and return this turn's terminal
   invoke result. Duplicate msg-ids return immediately with :deduped."
  [entry process-fn]
  (let [{:keys [status entry waiter]} (accept! (assoc entry :process-fn process-fn))]
    (when-not (= :deduped status)
      (drain! (:to entry) process-fn))
    @waiter))
