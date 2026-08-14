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
   :held {}
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
(declare drainer-v2-enabled?)
(declare ensure-drainer!)
(declare signal-drainer!)

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

(def ^:private persist-coalesce-ms
  "Minimum interval between durable-store writes. persist! pr-strs the WHOLE
   state (~0.5MB); a single drain cycle mutates state 4-5 times per turn, and
   writing synchronously on every mutation was the chronic half of the
   2026-07-18 CPU alert (five drainer threads averaging 15% CPU each over the
   JVM's life). The flusher writes immediately on the first change after an
   idle stretch, then coalesces further changes into at most one write per
   this interval — so the crash-loss window under sustained churn equals this
   interval, and a lone change still hits disk right away.
   Override: FUTON3C_DURABLE_QUEUE_PERSIST_MS."
  (or (some-> (config/env "FUTON3C_DURABLE_QUEUE_PERSIST_MS") parse-long)
      2000))

(defonce ^:private !dirty? (atom false))
(defonce ^:private !flusher (atom nil))   ;; {:lock Object :running atom :thread Thread}

(defn- flusher-loop [lock running]
  (while @running
    (locking lock
      (when (and @running (not @!dirty?))
        (try (.wait lock) (catch InterruptedException _))))
    (when (and @running @!dirty?)
      (reset! !dirty? false)
      (persist! @!queue)
      ;; Coalesce window: changes landing during this sleep fold into the
      ;; next single write instead of one write each.
      (try (Thread/sleep (long persist-coalesce-ms))
           (catch InterruptedException _)))))

(defn- ensure-flusher! []
  (or @!flusher
      (locking !flusher
        (or @!flusher
            (let [lock (Object.)
                  running (atom true)
                  thread (doto (Thread. ^Runnable #(flusher-loop lock running)
                                        "turn-queue-persist-flusher")
                           (.setDaemon true)
                           (.start))]
              (.addShutdownHook (Runtime/getRuntime)
                                (Thread. ^Runnable
                                         (fn []
                                           (when @!dirty?
                                             (reset! !dirty? false)
                                             (persist! @!queue)))
                                         "turn-queue-persist-final"))
              (reset! !flusher {:lock lock :running running :thread thread}))))))

(defn- mark-dirty! []
  (let [{:keys [lock]} (ensure-flusher!)]
    (reset! !dirty? true)
    (locking lock (.notifyAll lock))))

(defn- swap-state! [f & args]
  ;; Persistence is asynchronous: the single flusher thread snapshots @!queue
  ;; at write time, so the file always holds a state at least as new as any
  ;; acknowledged mutation from the last coalesce window — write order cannot
  ;; invert (there is only one writer), which is what the old
  ;; persist-under-lock scheme guaranteed at ~5 full-store writes per turn.
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
        (mark-dirty!))
      state)))

(defn clear!
  "Reset queue state. Intended for tests/dev only."
  []
  (when (bound? #'stop-all-drainers!)
    (stop-all-drainers!))
  (clojure.core/reset! !waiters {})
  (clojure.core/reset! !processors {})
  ;; Consume any pending async flush, then write synchronously: tests redef
  ;; queue-store-path around clear!, and a flush left pending here could fire
  ;; after the redef unwinds and write test state to the real store path.
  (clojure.core/reset! !dirty? false)
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
        waiter-installed? (atom false)
        processor-installed? (atom false)
        result (atom nil)]
    ;; An exact-id retry must not steal the original turn's waiter/processor.
    (swap! !waiters
           (fn [m]
             (if (contains? m id)
               m
               (do (reset! waiter-installed? true) (assoc m id waiter)))))
    (when-let [process-fn (:process-fn entry*)]
      (swap! !processors
             (fn [m]
               (if (contains? m id)
                 m
                 (do (reset! processor-installed? true)
                     (assoc m id process-fn))))))
    (let [entry* (dissoc entry* :process-fn)]
      (swap-state!
       (fn [state]
         (let [to (:to entry*)
               msg-id (:msg-id entry*)
               duplicate-id (or (get-in state [:msg-index to msg-id])
                                (when (contains? (:entries state) id) id))
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
                         :turn-queue/reply-route (reply-route (:entry @result))})
        (when @waiter-installed? (swap! !waiters dissoc id))
        (when @processor-installed? (swap! !processors dissoc id)))
      @result)))

;; =============================================================================
;; Operator hold — pause a recipient's queue without touching the live turn
;; =============================================================================
;;
;; A hold stops the drain loop BETWEEN turns: the entry already being processed
;; runs to completion and finalizes normally, and nothing new is popped until
;; release!. Bells keep being ACCEPTED while held — they pile up in FIFO order
;; rather than being rejected — which is the point: the operator wants to
;; interpose a word, not lose the backlog (Joe, 2026-08-11: "a way to hold the
;; queue while I have a word with claude-1", with the work in progress left
;; alone).
;;
;; Unlike :draining, a hold is durable across a JVM restart. :draining is an
;; in-flight lock and is cleared on load; a hold is operator intent, and a
;; restart silently resuming a queue the operator paused is the failure mode
;; worth avoiding. :expires-at is the safety valve for the opposite failure —
;; a hold set and forgotten, with bells accumulating unseen behind it.

(def ^:private hold-backoff-ms
  "Sleep applied when drain! is entered for a held agent. A drainer's .wait
   only covers the EMPTY-queue case, so a held NON-EMPTY queue would otherwise
   spin the drainer thread at full CPU on the 500 ms poll — including a drainer
   thread compiled before this change and still running after a hot reload."
  500)

(defn- expired? [hold now-ms]
  (boolean (when-let [exp (:expires-at hold)]
             (<= (long exp) (long now-ms)))))

(defn held?
  "True when AGENT-ID's queue is currently held. Clears an expired hold."
  [agent-id]
  (let [aid (clean-str agent-id)
        hold (get-in (snapshot) [:held aid])]
    (cond
      (nil? hold) false
      (expired? hold (System/currentTimeMillis))
      (do (swap-state! update :held dissoc aid) false)
      :else true)))

(defn holds
  "Map of agent-id -> hold for every queue currently held (expired ones dropped)."
  []
  (let [now-ms (System/currentTimeMillis)]
    (into {} (remove (fn [[_ h]] (expired? h now-ms))) (:held (snapshot)))))

(defn hold!
  "Hold AGENT-ID's queue. The turn already in flight finishes; nothing new is
   popped until release!. OPTS: :reason, :by, :ttl-ms (auto-release after this
   many ms; omit or 0 for an indefinite hold). Returns the hold map."
  ([agent-id] (hold! agent-id nil))
  ([agent-id {:keys [reason by ttl-ms]}]
   (when-let [aid (clean-str agent-id)]
     (let [ttl (some-> ttl-ms long)
           hold (cond-> {:agent-id aid
                         :held-at (now)
                         :by (or (clean-str by) "operator")}
                  (clean-str reason) (assoc :reason (clean-str reason))
                  (and ttl (pos? ttl)) (assoc :expires-at (+ (System/currentTimeMillis) ttl)
                                              :ttl-ms ttl))]
       (swap-state! assoc-in [:held aid] hold)
       hold))))

(defn release!
  "Lift AGENT-ID's hold and wake its drainer so queued turns resume at once."
  [agent-id]
  (when-let [aid (clean-str agent-id)]
    (let [had? (contains? (:held (snapshot)) aid)]
      (swap-state! update :held dissoc aid)
      (let [pending (count (get-in (snapshot) [:queues aid]))]
        (when (and (pos? pending) (drainer-v2-enabled?))
          (ensure-drainer! aid)
          (signal-drainer! aid))
        {:agent-id aid :released had? :pending pending}))))

(defn- entry-view [entry]
  (let [prompt (:prompt entry)
        text (cond
               (string? prompt) prompt
               (map? prompt) (str (or (:prompt prompt) (get prompt "prompt") prompt))
               :else (str prompt))]
    {:id (:id entry)
     :seq (:seq entry)
     :from (:from entry)
     :surface (:surface entry)
     :msg-id (:msg-id entry)
     :accepted-at (:accepted-at entry)
     :preview (let [t (str/replace (str/trim (str text)) #"\s+" " ")]
                (if (> (count t) 200) (str (subs t 0 197) "...") t))}))

(defn queue-view
  "Operator view of the queues: who has pending turns, what they are, who is
   mid-drain, and who is held. Agents with nothing pending, no drain in flight
   and no hold are omitted unless ALL? is true."
  ([] (queue-view false))
  ([all?]
   (let [state (snapshot)
         held (holds)
         ;; Union, not just (:queues state): an agent can be held (or mid-drain)
         ;; before it has ever had a queue key, and a hold that is invisible in
         ;; the operator view is the whole failure this endpoint exists to fix.
         aids (into #{} cat [(keys (:queues state)) (keys held) (:draining state)])
         rows (for [aid aids
                    :let [ids (get-in state [:queues aid] [])
                          pending (count ids)
                          draining? (contains? (:draining state) aid)
                          hold (get held aid)]
                    :when (or all? (pos? pending) draining? hold)]
                (cond-> {:agent-id aid
                         :pending pending
                         :draining draining?
                         :queued (mapv #(entry-view (get-in state [:entries %])) ids)}
                  hold (assoc :held hold)))]
     {:agents (vec (sort-by (juxt (comp - :pending) :agent-id) rows))
      :held held
      :total-pending (reduce + 0 (map (comp count val) (:queues state)))})))

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

(defn- prune-terminal [state]
  ;; Only :history is capped; without pruning, :entries and :msg-index keep a
  ;; record of every turn ever accepted (the persisted store grows without
  ;; bound — 490KB after two days when the 2026-07-18 CPU alert hit). Retain
  ;; entries that are still queued, still in flight (popped by a drainer but
  ;; not yet terminal, status :queued), or within the :history window; dedup
  ;; memory (:msg-index) follows the same window, so a msg-id repeated more
  ;; than max-history turns later re-processes instead of deduping — retries
  ;; arrive within seconds, so that window is ample.
  (let [keep-ids (-> #{}
                     (into (keep (fn [[id e]] (when (= :queued (:status e)) id)))
                           (:entries state))
                     (into (map :id) (:history state))
                     (into cat (vals (:queues state))))]
    (if (= (count keep-ids) (count (:entries state)))
      state
      (assoc state
             :entries (select-keys (:entries state) keep-ids)
             :msg-index (into {}
                              (keep (fn [[to m]]
                                      (let [m* (into {}
                                                     (filter (fn [[_ id]] (contains? keep-ids id)))
                                                     m)]
                                        (when (seq m*) [to m*]))))
                              (:msg-index state))))))

(defn- mark-terminal! [entry status result]
  (let [entry* (assoc entry
                      :status status
                      :finished-at (now)
                      :reply-route (reply-route entry))]
    (swap-state!
     (fn [state]
       (cond-> (-> state
                   (assoc-in [:entries (:id entry*)] entry*)
                   (update :history #(vec (take-last max-history (conj (or % []) entry*))))
                   prune-terminal)
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
    (cond
      ;; Held: do not acquire the drain lock at all, and back off so a poll-loop
      ;; drainer cannot hot-spin on the held non-empty queue (hold-backoff-ms).
      (held? aid)
      (do (try (Thread/sleep (long hold-backoff-ms))
               (catch InterruptedException _))
          nil)

      (not (acquire-drain! aid))
      nil

      :else
      (loop [processed-total []]
        (let [processed-batch
              (try
                (loop [processed []]
                  ;; Checked BEFORE each pop, never mid-turn: a hold placed
                  ;; while an entry is in flight lets that entry finish and
                  ;; finalize, then stops the drain.
                  (if-let [entry (when-not (held? aid) (pop-next! aid))]
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
          (if (and (not (held? aid))
                   (seq (get-in (snapshot) [:queues aid]))
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

(def ^:dynamic *turn-id*
  "Canonical id of the queue entry currently being processed. Transport
   adapters bind this while invoking the agent so lower layers (notably the
   warm-pouch waiter) can retain the same turn identity without changing the
   agent-facing prompt. nil outside a queued turn."
  nil)

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
                                 (or (held? agent-id)
                                     (empty? (get-in (snapshot) [:queues agent-id]))))
                        (try (.wait lock (long drainer-wait-ms))
                             (catch InterruptedException _))))
                    ;; A timeout or spurious wake with no work is genuinely
                    ;; idle: do not acquire a durable drain lock or rewrite the
                    ;; queue store. A signal cannot be lost because the queue is
                    ;; checked while holding the same monitor used by notify.
                    (when (and @running
                               (not (held? agent-id))
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
        ;; Finalization must exist BEFORE accept! publishes the queue entry.
        ;; A live drainer polls every 500 ms and can otherwise pop a fast turn,
        ;; run it, and observe no finalizer in the gap between accept! returning
        ;; and the old post-accept registration. That loses the SSE `done` event
        ;; after all content has already streamed.
        turn-id (or (clean-str (:id entry)) (str "turn-" (UUID/randomUUID)))
        entry* (assoc (dissoc entry :finalize-fn) :id turn-id)
        installed? (atom false)]
    (when finalize-fn
      ;; Do not overwrite an in-flight finalizer if an exact turn-id retry is
      ;; deduped. Its original channel still owns that terminal event.
      (swap! !finalizers
             (fn [m]
               (if (contains? m turn-id)
                 m
                 (do (reset! installed? true)
                     (assoc m turn-id finalize-fn))))))
    (try
      (let [{:keys [status entry] :as r} (accept! entry*)]
        (if (and (= :deduped status) @installed?)
          (swap! !finalizers dissoc turn-id)
          (when-not (= :deduped status)
            (ensure-drainer! (:to entry))
            (signal-drainer! (:to entry))))
        r)
      (catch Throwable t
        (when @installed?
          (swap! !finalizers dissoc turn-id))
        (throw t)))))

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
