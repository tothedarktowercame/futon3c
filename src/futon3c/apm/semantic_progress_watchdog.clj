(ns futon3c.apm.semantic-progress-watchdog
  "Independent liveness observer for one durable APM coordinator."
  (:require [futon3c.apm.campaign-trace :as campaign-trace])
  (:import [java.time Instant]
           [java.util.concurrent Executors ScheduledExecutorService
            ThreadFactory TimeUnit]
           [java.util.concurrent.atomic AtomicLong]))

(def scheduler-claim-max-ms 30000)
(def internal-progress-max-ms (* 5 60 1000))
(def external-deadline-grace-ms (* 2 60 1000))
(def default-period-ms 10000)

(def cursor-keys
  [:frame-id :phase :attempt-ordinal :obligation/status :active-job-id
   :last-committed-event-id])

(defonce ^:private thread-seq (AtomicLong. 0))
(defonce ^:private runners (atom {}))

(defn- new-watchdog-executor []
  (Executors/newSingleThreadScheduledExecutor
   (reify ThreadFactory
     (newThread [_ runnable]
       (doto (Thread. runnable
                      (str "apm-semantic-watchdog-"
                           (.incrementAndGet thread-seq)))
         (.setDaemon true))))))

(defn progress-cursor [observation]
  (select-keys (:cursor observation) cursor-keys))

(defn- instant-ms [value]
  (cond
    (integer? value) value
    (string? value) (.toEpochMilli (Instant/parse value))
    :else nil))

(defn- immediate-reason [observation now-ms]
  (let [claimed-at (or (get-in observation [:tick-claim :tick/claimed-at])
                       (get-in observation [:tick-claim :claimed-at]))]
    (cond
    (= :failed (get-in observation [:regulator :regulator/status]))
    {:code :regulator-failed}

    (:invalid-state? observation)
    {:code :invalid-state :finding (:invalid-state observation)}

    (:failed-launch-audit? observation)
    {:code :failed-launch-audit :finding (:launch-audit observation)}

    (:impossible-transition? observation)
    {:code :impossible-transition :finding (:transition observation)}

    (and (some? claimed-at)
         (let [claimed-ms (instant-ms claimed-at)]
           (or (nil? claimed-ms)
               (> (- now-ms claimed-ms) scheduler-claim-max-ms))))
    {:code :scheduler-claim-stale
     :claimed-at claimed-at}

    :else nil)))

(defn evaluate
  "Pure watchdog transition. NOW-MS is injected by the caller.

   OBSERVATION contains :cursor, :regulator, :supervisor/status, optional
   :tick-claim, and optional :awaiting-job {:job-id ... :deadline ...}.
   WATCH-STATE is the previously persisted watchdog state."
  [watch-state observation now-ms]
  (let [cursor (progress-cursor observation)
        previous-cursor (:watchdog/cursor watch-state)
        cursor-changed? (not= cursor previous-cursor)
        last-progress-ms (if (or (nil? previous-cursor) cursor-changed?)
                           now-ms
                           (:watchdog/last-progress-ms watch-state))
        base {:state/type :semantic-progress-watchdog
              :watchdog/status :watching
              :watchdog/cursor cursor
              :watchdog/last-progress-ms last-progress-ms
              :watchdog/observed-at-ms now-ms}
        immediate (immediate-reason observation now-ms)
        awaiting (:awaiting-job observation)
        deadline-ms (some-> awaiting :deadline instant-ms)
        ready? (= :ready (:supervisor/status observation))
        elapsed-ms (max 0 (- now-ms last-progress-ms))
        valid-external-wait? (and (some? awaiting) (some? deadline-ms)
                                  (<= now-ms (+ deadline-ms
                                                external-deadline-grace-ms)))
        reason (cond
                 immediate immediate
                 (and awaiting (nil? deadline-ms))
                 {:code :external-job-deadline-missing
                  :job-id (:job-id awaiting)}
                 (and awaiting
                      (> now-ms (+ deadline-ms external-deadline-grace-ms)))
                 {:code :external-job-deadline-exceeded
                  :job-id (:job-id awaiting)
                  :deadline-ms deadline-ms}
                 (and ready? (nil? awaiting)
                      (>= (- now-ms last-progress-ms)
                          internal-progress-max-ms))
                 {:code :internal-semantic-progress-stalled
                  :last-progress-ms last-progress-ms}
                 :else nil)]
    (if reason
      {:status :halt
       :reason reason
       :state (assoc base :watchdog/status :halted
                     :watchdog/halt-reason reason
                     :watchdog/trace-observation
                     (campaign-trace/validate-authoritative-observation
                      :progress
                     {:coordinator-enabled?
                      (boolean (:coordinator-enabled? observation))
                      :elapsed-ms elapsed-ms
                      :valid-external-wait? (boolean valid-external-wait?)
                      :semantic-cursor-advanced? cursor-changed?
                      :coordinator-disabled? false
                      :first-violation-recorded? true}))}
      {:status :watching
       :state (assoc base :watchdog/trace-observation
                     (campaign-trace/validate-authoritative-observation
                      :progress
                     {:coordinator-enabled?
                      (boolean (:coordinator-enabled? observation))
                      :elapsed-ms elapsed-ms
                      :valid-external-wait? (boolean valid-external-wait?)
                      :semantic-cursor-advanced? cursor-changed?
                      :coordinator-disabled? false
                      :first-violation-recorded? false}))})))

(defn check!
  "Observe once. A halt first disables the durable coordinator, then persists
   the watchdog reason. STOP-FN and PERSIST-FN are injectable for hermetic
   tests."
  [{:keys [watch-state observation now-ms registry-path coordinator-id
           stop-fn persist-fn]
    :or {stop-fn (fn [path id]
                   ((requiring-resolve
                     'futon3c.apm.durable-coordinator/stop!) path id))}}]
  (let [decision (evaluate watch-state observation now-ms)]
    (if (= :halt (:status decision))
      (let [stopped (stop-fn registry-path coordinator-id)
            final-state (-> (:state decision)
                            (assoc :watchdog/durable-stop stopped)
                            (assoc-in [:watchdog/trace-observation
                                       :coordinator-disabled?]
                                      (boolean (:ok stopped))))
            persisted (persist-fn final-state)]
        {:ok (and (:ok stopped) (:ok persisted))
         :status :halted
         :reason (:reason decision)
         :stop stopped
         :persisted persisted
         :state final-state})
      (let [persisted (persist-fn (:state decision))]
        {:ok (:ok persisted)
         :status :watching
         :persisted persisted
         :state (:state decision)}))))

(defn stop! [watchdog-id]
  (if-let [^ScheduledExecutorService executor (get @runners watchdog-id)]
    (do
      (swap! runners dissoc watchdog-id)
      (.shutdown executor)
      {:ok true :status :stopped :watchdog/id watchdog-id})
    {:ok true :status :not-running :watchdog/id watchdog-id}))

(defn running? [watchdog-id]
  (when-let [^ScheduledExecutorService executor (get @runners watchdog-id)]
    (and (not (.isShutdown executor))
         (not (.isTerminated executor)))))

(defn start!
  "Schedule WATCH-FN on a dedicated executor. The watched runner/executor is
   deliberately not accepted, so its death cannot stop this observer."
  [{:keys [watchdog-id watch-fn period-ms executor-fn]
    :or {period-ms default-period-ms
         executor-fn new-watchdog-executor}}]
  (cond
    (not (and (string? watchdog-id) (not-empty watchdog-id)))
    {:ok false :error/code :semantic-progress-watchdog-id-invalid}
    (not (fn? watch-fn))
    {:ok false :error/code :semantic-progress-watchdog-provider-missing}
    (not (pos-int? period-ms))
    {:ok false :error/code :semantic-progress-watchdog-period-invalid}
    (and (contains? @runners watchdog-id) (running? watchdog-id))
    {:ok true :status :already-running :watchdog/id watchdog-id}
    (contains? @runners watchdog-id)
    (do
      (stop! watchdog-id)
      (start! {:watchdog-id watchdog-id :watch-fn watch-fn
               :period-ms period-ms :executor-fn executor-fn}))
    :else
    (let [^ScheduledExecutorService executor (executor-fn)
          run-one (fn []
                    (try
                      (watch-fn)
                      (catch Throwable _
                        ;; A failed observation is retried by this independent
                        ;; scheduler; WATCH-FN owns durable error recording.
                        nil)))]
      (swap! runners assoc watchdog-id executor)
      ;; Registration is visible before the watched coordinator can run.  The
      ;; first observation occurs after one period so restart reconciliation
      ;; gets the same grace as every later interval.
      (.scheduleWithFixedDelay executor run-one period-ms period-ms
                               TimeUnit/MILLISECONDS)
      {:ok true :status :started :watchdog/id watchdog-id
       :executor executor})))
