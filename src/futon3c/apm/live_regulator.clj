(ns futon3c.apm.live-regulator
  "Single-flight, non-agentic scheduler for the idempotent live supervisor."
  (:import [java.time Instant]
           [java.util.concurrent Executors ScheduledExecutorService ThreadFactory
            TimeUnit]
           [java.util.concurrent.atomic AtomicLong]))

(def default-period-ms 2000)
(def terminal-statuses #{:complete :failed})
(defonce ^:private runners (atom {}))
(defonce ^:private thread-seq (AtomicLong. 0))

(defn- now [] (str (Instant/now)))

(defn initial-state [regulator-id]
  {:state/type :live-regulator
   :regulator/id regulator-id
   :regulator/status :running
   :regulator/ticks 0
   :regulator/updated-at (now)})

(defn terminal? [state]
  (contains? terminal-statuses (:regulator/status state)))

(defn tick!
  "Execute one machine-owned supervisor tick and durably classify its result."
  [{:keys [state tick-fn tick-state-fn persist-fn]}]
  (cond
    (not (and (map? state) (= :live-regulator (:state/type state))))
    {:ok false :error/code :live-regulator-state-invalid}
    (not (and (or (fn? tick-fn) (fn? tick-state-fn)) (fn? persist-fn)))
    {:ok false :error/code :live-regulator-provider-missing}
    (terminal? state)
    {:ok true :status (:regulator/status state) :state state}
    :else
    (let [result (try (if tick-state-fn
                        (tick-state-fn state)
                        (tick-fn))
                      (catch Throwable t
                        {:ok false :error/code :live-regulator-tick-threw
                         :exception/class (.getName (class t))
                         :exception/message (.getMessage t)}))
          status (cond
                   (and (:ok result) (= :frame-complete (:status result))) :complete
                   (:ok result) :running
                   :else :failed)
          next-state (-> state
                         (merge (:regulator/state-updates result))
                         (assoc :regulator/status status
                                :regulator/ticks (inc (:regulator/ticks state))
                                :regulator/updated-at (now)
                                :regulator/last-result
                                (dissoc result :regulator/state-updates)))
          persisted (persist-fn next-state)]
      (if (:ok persisted)
        {:ok (not= :failed status) :status status :state next-state
         :result result}
        {:ok false :error/code :live-regulator-persistence-failed
         :finding persisted}))))

(defn status [regulator-id]
  (some-> (get @runners regulator-id) :state deref))

(defn stop! [regulator-id]
  (if-let [{:keys [^ScheduledExecutorService executor]} (get @runners regulator-id)]
    (do (swap! runners dissoc regulator-id)
        (.shutdown executor)
        {:ok true :status :stopped :regulator/id regulator-id})
    {:ok true :status :not-running :regulator/id regulator-id}))

(defn repair-resume!
  "Durably reopen a failed regulator after an explicit code repair.

   The failed observation is retained in :regulator/failures. Completed runs
   and running regulators cannot be reopened through this boundary."
  [{:keys [state reason persist-fn]}]
  (cond
    (not= :failed (:regulator/status state))
    {:ok false :error/code :live-regulator-not-repairable}
    (not (and (string? reason) (not-empty reason) (fn? persist-fn)))
    {:ok false :error/code :live-regulator-repair-evidence-invalid}
    :else
    (let [failure {:failed-at (:regulator/updated-at state)
                   :ticks (:regulator/ticks state)
                   :result (:regulator/last-result state)
                   :repair/reason reason}
          resumed (-> state
                      (update :regulator/failures (fnil conj []) failure)
                      (assoc :regulator/status :running
                             :regulator/updated-at (now)
                             :regulator/last-result nil))
          persisted (persist-fn resumed)]
      (if (:ok persisted)
        {:ok true :status :running :state resumed}
        {:ok false :error/code :live-regulator-repair-persistence-failed
         :finding persisted}))))

(defn start!
  "Start an idempotent single-thread regulator, recovering durable state."
  [{:keys [regulator-id read-fn persist-fn tick-fn tick-state-fn period-ms]
    :or {period-ms default-period-ms}}]
  (cond
    (not (and (string? regulator-id) (not-empty regulator-id)))
    {:ok false :error/code :live-regulator-id-invalid}
    (not (and (fn? read-fn) (fn? persist-fn)
              (or (fn? tick-fn) (fn? tick-state-fn))))
    {:ok false :error/code :live-regulator-provider-missing}
    (not (pos-int? period-ms))
    {:ok false :error/code :live-regulator-period-invalid}
    (contains? @runners regulator-id)
    {:ok true :status :already-running :state (status regulator-id)}
    :else
    (let [recovered (or (read-fn) (initial-state regulator-id))]
      (cond
        (not= regulator-id (:regulator/id recovered))
        {:ok false :error/code :live-regulator-id-mismatch
         :finding {:expected regulator-id :actual (:regulator/id recovered)}}
        (terminal? recovered)
        {:ok true :status (:regulator/status recovered) :state recovered}
        :else
        (let [state (atom recovered)
              executor (Executors/newSingleThreadScheduledExecutor
                        (reify ThreadFactory
                          (newThread [_ runnable]
                            (doto (Thread. runnable
                                           (str "apm-regulator-"
                                                (.incrementAndGet thread-seq)))
                              (.setDaemon true)))))
              run-one (fn []
                        (let [result (tick! {:state @state :tick-fn tick-fn
                                            :tick-state-fn tick-state-fn
                                            :persist-fn persist-fn})]
                          (when-let [next-state (:state result)]
                            (reset! state next-state))
                          (when (or (not (:ok result)) (terminal? @state))
                            (stop! regulator-id))))]
          (swap! runners assoc regulator-id {:executor executor :state state})
          (.scheduleWithFixedDelay ^ScheduledExecutorService executor
                                   ^Runnable run-one 0 period-ms
                                   TimeUnit/MILLISECONDS)
          {:ok true :status :started :state recovered})))))
