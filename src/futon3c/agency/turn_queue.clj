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
  (:import [java.nio.file Files StandardCopyOption]
           [java.security MessageDigest]
           [java.time Instant]
           [java.util UUID]))

(def ^:private max-history 200)

(defn enabled?
  "True when the durable Car-3 queue path is enabled.  Default TRUE (2026-06-14):
   the durable queue is proven + was already activated via dev-laptop-env, so it
   defaults on to survive a stale-env OOM-resume that doesn't carry the flag.  Set
   FUTON3C_DURABLE_QUEUE=false to force the legacy lock-then-timeout path."
  []
  (let [prop (System/getProperty "FUTON3C_DURABLE_QUEUE")]
    (if (some? prop)
      (not (#{"0" "false" "no" "off"} (str/lower-case (str/trim prop))))
      (config/env-bool "FUTON3C_DURABLE_QUEUE" true))))

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

(declare run-finalizer!)
(declare stop-all-drainers!)

(defn- load-state []
  (let [f (io/file (queue-store-path))]
    (if (.exists f)
      (try
        (let [parsed (edn/read-string (slurp f))]
          ;; Clear :draining on load. It is an IN-FLIGHT drain lock, not durable
          ;; state: a fresh JVM has no drain in progress, so any persisted :draining
          ;; is stale. A crash mid-drain (e.g. OOM) otherwise restores every agent
          ;; stuck in :draining, making acquire-drain! fail forever and ALL restored
          ;; agents permanently un-drainable (queued bells never run). Pending
          ;; queues/entries/frontier are kept; only the drain lock is reset.
          ;; (M-agency-hardening, OOM-restore self-recovery, 2026-06-12.)
          (-> (merge (empty-state) (select-keys parsed (keys (empty-state))))
              (assoc :draining #{})))
        (catch Throwable _
          (empty-state)))
      (empty-state))))

(defn- ensure-state! []
  (when (nil? @!queue)
    (reset! !queue (load-state)))
  @!queue)

(defn- persist! [state]
  (try
    (let [target (.toPath (io/file (queue-store-path)))
          tmp (.resolveSibling target (str (.getFileName target) ".tmp"))]
      (io/make-parents (.toFile target))
      (spit (.toFile tmp) (pr-str state))
      (Files/move tmp target
                  (into-array java.nio.file.CopyOption
                              [StandardCopyOption/ATOMIC_MOVE
                               StandardCopyOption/REPLACE_EXISTING])))
    (catch Throwable t
      (println (str "[turn-queue] persist failed: " (.getMessage t)))))
  state)

(defn- swap-state! [f & args]
  ;; Atom mutation and disk replacement must have the same total order. Without
  ;; this lock, two drainers can persist snapshots in the opposite order from
  ;; their successful swaps, restoring stale queue state after a crash.
  (locking !queue
    (ensure-state!)
    (let [changed? (atom false)
          state (swap! !queue
                       (fn [old]
                         (let [new (apply f old args)]
                           (when-not (identical? old new)
                             (reset! changed? true))
                           new)))]
      (when @changed?
        (persist! state))
      state)))

(defn clear!
  "Reset queue state. Intended for tests/dev only."
  []
  (when (bound? #'stop-all-drainers!)
    (stop-all-drainers!))
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
      (loop [processed-total []]
        (let [processed-batch
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
                      (run-finalizer! (:id entry) result)        ;; no-op unless a v2 finalizer is registered
                      (recur (conj processed entry*)))
                    processed))
                (finally
                  (release-drain! aid)))
              processed* (into processed-total processed-batch)]
          (if (and (seq (get-in (snapshot) [:queues aid]))
                   (acquire-drain! aid))
            (recur processed*)
            processed*))))))

(defn accept-and-drain!
  "Accept ENTRY, drain its recipient queue, and return this turn's terminal
   invoke result. Duplicate msg-ids return immediately with :deduped."
  [entry process-fn]
  (let [{:keys [status entry waiter]} (accept! (assoc entry :process-fn process-fn))]
    (when-not (= :deduped status)
      (drain! (:to entry) process-fn))
    @waiter))

;; =============================================================================
;; Drainer v2 — dedicated per-agent drainer thread (no shared invoke-lane park)
;; =============================================================================
;;
;; accept-and-drain! parks the CALLING thread on @waiter when the agent is already
;; draining. For bells that calling thread is one of the 4 shared invoke-executor
;; lanes, so turns queued behind a slow agent burn lanes (M-agency-hardening
;; "drainer car"). Drainer v2 gives each agent its own dedicated daemon drainer
;; thread that owns drain + finalization, so a bell can enqueue and return without
;; holding a shared lane. Validated by futon3c.agency.drainer-model-test BEFORE
;; implementation. Flag-gated, load-dark; the v2 path is only reached when callers
;; opt in via accept-async!/accept-block!, so OFF behavior is byte-for-byte the
;; durable-queue path above.

(defn drainer-v2-enabled?
  "True when the per-agent drainer-thread path (no shared-lane parking) is enabled.
   Default TRUE (2026-06-14): proven + previously activated, defaults on so a stale-env
   OOM-resume keeps it. Layered on the durable queue. Set FUTON3C_DRAINER_V2=false to disable."
  []
  (let [prop (System/getProperty "FUTON3C_DRAINER_V2")]
    (if (some? prop)
      (not (#{"0" "false" "no" "off"} (str/lower-case (str/trim prop))))
      (config/env-bool "FUTON3C_DRAINER_V2" true))))

(def ^:dynamic *drained-by-outer*
  "Bound true while a turn runs under an outer per-agent drainer (v2), so the inner
   invoke-fn accept-and-drain! does not re-queue the same turn."
  false)

(defonce ^:private !finalizers (atom {}))   ;; id -> (fn [result] ...)
(defonce ^:private !drainers (atom {}))      ;; agent-id -> {:lock Object :running atom :thread Thread}

(def ^:private drainer-wait-ms 500)

(defn run-finalizer!
  "Invoke and clear the registered finalizer for ID with the turn RESULT.
   No-op when none is registered (the durable-queue/old path never registers one)."
  [id result]
  (when-let [f (get @!finalizers id)]
    (swap! !finalizers dissoc id)
    (try (f result)
         (catch Throwable t
           (println (str "[turn-queue] finalizer " id " failed: " (.getMessage t)))))))

(defn- spawn-drainer! [agent-id]
  (let [lock (Object.)
        running (atom true)
        thread (Thread.
                ^Runnable
                (fn []
                  (while @running
                    (locking lock
                      (when (and @running
                                 (empty? (get-in (snapshot) [:queues agent-id])))
                        (try (.wait lock (long drainer-wait-ms))
                             (catch InterruptedException _))))
                    ;; A timeout or spurious wake with no work is genuinely
                    ;; idle: do not acquire a durable drain lock or rewrite the
                    ;; queue store. A signal cannot be lost because the queue is
                    ;; checked while holding the same monitor used by notify.
                    (when (and @running
                               (seq (get-in (snapshot) [:queues agent-id])))
                      (try
                        (drain! agent-id nil)   ;; each entry carries its own processor
                        (catch Throwable t
                          (println (str "[turn-queue] drainer " agent-id " error: "
                                        (.getMessage t))))))))
                (str "turn-drainer-" agent-id))]
    (.setDaemon thread true)
    (.start thread)
    {:lock lock :running running :thread thread}))

(defn- ensure-drainer! [agent-id]
  (let [aid (clean-str agent-id)]
    (when (and aid (not (get @!drainers aid)))
      (locking !drainers
        (when-not (get @!drainers aid)
          (swap! !drainers assoc aid (spawn-drainer! aid)))))
    (get @!drainers aid)))

(defn- signal-drainer! [agent-id]
  (when-let [{:keys [lock]} (get @!drainers (clean-str agent-id))]
    (locking lock (.notifyAll lock))))

(defn resume-pending-drainers!
  "Boot-time recovery: spawn a drainer for every agent that has a NON-EMPTY persisted
   queue. Drainers are otherwise spawned lazily by accept-async! when a bell arrives;
   after a restart, restored turns already sit in the queue but no new bell triggers
   that spawn, so those turns — and the agent — are stuck (un-drainable) until knocked.
   Restored turns have no in-memory processor, so the drainer reconciles them (clearing
   the false 'invoking' state) and stays alive for new bells. No-op unless drainer-v2 is
   on. Call once after the registry is restored. Pairs with load-state clearing :draining.
   (M-agency-hardening, OOM-restore self-recovery, 2026-06-12.)"
  []
  (when (drainer-v2-enabled?)
    (let [agents (->> (:queues (snapshot))
                      (keep (fn [[aid q]] (when (seq q) aid)))
                      vec)]
      (doseq [aid agents]
        (ensure-drainer! aid)
        (signal-drainer! aid))
      (when (seq agents)
        (println (str "[turn-queue] resume-pending-drainers! — spawned drainers for "
                      (count agents) " restored agent(s) with queued turns: " agents)))
      agents)))

(defn accept-async!
  "Enqueue ENTRY (carrying :process-fn and optional :finalize-fn) for its recipient
   and ensure a dedicated per-agent drainer thread processes it. Returns the accept
   result map IMMEDIATELY without blocking. finalize-fn (if present) runs with the
   turn's terminal result on the drainer thread. Bell-style fire-and-forget dispatch
   that never holds a shared invoke lane."
  [entry]
  (let [finalize-fn (:finalize-fn entry)
        {:keys [status entry] :as r} (accept! (dissoc entry :finalize-fn))]
    (when (not= :deduped status)
      (when finalize-fn
        (swap! !finalizers assoc (:id entry) finalize-fn))
      (ensure-drainer! (:to entry))
      (signal-drainer! (:to entry)))
    r))

(defn accept-block!
  "Like accept-async! but blocks the CALLING thread on the turn's waiter and returns
   the terminal result. Use only on a non-scarce thread (e.g. an HTTP request thread),
   never on a shared invoke-executor lane."
  [entry]
  (let [{:keys [status waiter]} (accept-async! entry)]
    (if (= :deduped status)
      {:result "[deduped turn]" :turn-queue/status :deduped}
      @waiter)))

(defn stop-all-drainers!
  "Stop and clear every per-agent drainer thread + pending finalizers.
   Tests/dev only."
  []
  (doseq [[_ {:keys [running lock thread]}] @!drainers]
    (when running (clojure.core/reset! running false))
    (when lock (locking lock (.notifyAll lock)))
    (when thread (.interrupt ^Thread thread)))
  (clojure.core/reset! !drainers {})
  (clojure.core/reset! !finalizers {}))
