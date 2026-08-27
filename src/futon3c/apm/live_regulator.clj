(ns futon3c.apm.live-regulator
  "Single-flight, non-agentic scheduler for the idempotent live supervisor."
  (:import [java.time Instant]
           [java.nio.channels FileChannel]
           [java.nio.file Files Path StandardOpenOption]
           [java.util.concurrent Executors ScheduledExecutorService ThreadFactory
            TimeUnit]
           [java.util.concurrent.atomic AtomicLong]))

(def default-period-ms 2000)
(def terminal-statuses #{:complete :failed :stopped})
(defonce ^:private runners (atom {}))
(defonce ^:private thread-seq (AtomicLong. 0))

(defn- now [] (str (Instant/now)))

(defn with-file-tick-lock
  "Return a cross-process single-flight wrapper backed by `claim-path`."
  [claim-path]
  (fn [f]
    (let [^Path path (if (instance? Path claim-path)
                       claim-path
                       (Path/of (str claim-path) (make-array String 0)))]
      (when-let [parent (.getParent path)]
        (Files/createDirectories
         parent (make-array java.nio.file.attribute.FileAttribute 0)))
      (with-open [channel (FileChannel/open
                           path
                           (into-array StandardOpenOption
                                       [StandardOpenOption/CREATE
                                        StandardOpenOption/WRITE]))
                  _lock (.lock channel)]
        (f)))))

(defn initial-state
  ([regulator-id] (initial-state regulator-id now))
  ([regulator-id now-fn]
   {:state/type :live-regulator
    :regulator/id regulator-id
    :regulator/status :running
    :regulator/epoch 0
    :regulator/ticks 0
    :regulator/updated-at (now-fn)}))

(defn terminal? [state]
  (contains? terminal-statuses (:regulator/status state)))

(defn- valid-claim? [regulator-id claim]
  (and (map? claim)
       (= :live-regulator-tick-claim (:state/type claim))
       (= regulator-id (:regulator/id claim))
       (nat-int? (:tick/epoch claim))
       (pos-int? (:tick/ordinal claim))
       (string? (:tick/id claim))
       (string? (:tick/claimed-at claim))))

(defn tick!
  "Execute one supervisor tick inside a claim persisted before any effect."
  [{:keys [state tick-fn tick-state-fn persist-fn now-fn expected-epoch
           claim-allowed-fn]
    :or {now-fn now}}]
  (cond
    (not (and (map? state) (= :live-regulator (:state/type state))))
    {:ok false :error/code :live-regulator-state-invalid}
    (not (and (or (fn? tick-fn) (fn? tick-state-fn)) (fn? persist-fn)))
    {:ok false :error/code :live-regulator-provider-missing}
    (terminal? state)
    {:ok true :status (:regulator/status state) :state state}
    (and (some? expected-epoch)
         (not= expected-epoch (:regulator/epoch state)))
    {:ok false :error/code :live-regulator-epoch-superseded
     :finding {:expected expected-epoch :actual (:regulator/epoch state)}}
    (and claim-allowed-fn (not (claim-allowed-fn)))
    {:ok false :error/code :live-regulator-draining
     :status :draining :state state}
    :else
    (let [regulator-id (:regulator/id state)
          prior-claim (:regulator/tick-claim state)
          invalid-claim? (and prior-claim
                              (not (valid-claim? regulator-id prior-claim)))
          claim (or prior-claim
                    {:state/type :live-regulator-tick-claim
                     :regulator/id regulator-id
                     :tick/epoch (or (:regulator/epoch state) 0)
                     :tick/ordinal (inc (or (:regulator/ticks state) 0))
                     :tick/id (str regulator-id ":"
                                   (or (:regulator/epoch state) 0) ":"
                                   (inc (or (:regulator/ticks state) 0)))
                     :tick/claimed-at (now-fn)})
          claimed-state (assoc state
                               :regulator/tick-claim claim
                               :regulator/reconciliation
                               (if prior-claim :reconciling :claimed))
          claimed (when-not invalid-claim?
                    (if prior-claim {:ok true} (persist-fn claimed-state)))]
      (cond
        invalid-claim?
        {:ok false :error/code :live-regulator-tick-claim-invalid
         :finding prior-claim}
        (not (:ok claimed))
        {:ok false :error/code :live-regulator-claim-persistence-failed
         :finding claimed}
        :else
        (let [result (try (if tick-state-fn
                            (tick-state-fn claimed-state)
                            (tick-fn))
                      (catch Throwable t
                        {:ok false :error/code :live-regulator-tick-threw
                         :exception/class (.getName (class t))
                         :exception/message (.getMessage t)}))
          status (cond
                   (and (:ok result) (= :frame-complete (:status result))) :complete
                   (:ok result) :running
                   :else :failed)
          next-state (-> claimed-state
                         (merge (:regulator/state-updates result))
                         (dissoc :regulator/tick-claim)
                         (assoc :regulator/status status
                                :regulator/ticks (:tick/ordinal claim)
                                :regulator/updated-at (now-fn)
                                :regulator/reconciliation :settled
                                :regulator/last-completed-tick claim
                                :regulator/last-result
                                (dissoc result :regulator/state-updates)))
          persisted (persist-fn next-state)]
          (if (:ok persisted)
            {:ok (not= :failed status) :status status :state next-state
             :result result}
            {:ok false :error/code :live-regulator-persistence-failed
             :claim claim :finding persisted}))))))

(defn status [regulator-id]
  (some-> (get @runners regulator-id) :state deref))

(defn- live-runner? [{:keys [^ScheduledExecutorService executor state]}]
  (and executor state
       (not (.isShutdown executor))
       (not (.isTerminated executor))
       (not (terminal? @state))))

(defn cancel-scheduler!
  "Cancel future scheduled ticks. This is not an operator-visible stop: it
  writes no durable lifecycle state and supplies no quiescence witness."
  [regulator-id]
  (if-let [{:keys [^ScheduledExecutorService executor]} (get @runners regulator-id)]
    (do (swap! runners dissoc regulator-id)
        (.shutdown executor)
        {:ok true :status :stopped :regulator/id regulator-id})
    {:ok true :status :not-running :regulator/id regulator-id}))

(defn stop!
  "Refuse the former scheduler-only stop API. Operator stop requires the
  durable coordinator registry so draining and quiescence can be witnessed."
  [regulator-id]
  {:ok false :error/code :live-regulator-durable-stop-required
   :regulator/id regulator-id
   :required-call 'futon3c.apm.durable-coordinator/stop!})

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

(defn continue-complete!
  "Durably reopen a completed regulator for an explicitly continued workflow.

  This is not failure repair: the completed observation is retained separately
  and the caller must supply operator-visible continuation evidence."
  [{:keys [state reason persist-fn]}]
  (cond
    (not= :complete (:regulator/status state))
    {:ok false :error/code :live-regulator-not-complete}
    (not (and (string? reason) (not-empty reason) (fn? persist-fn)))
    {:ok false :error/code :live-regulator-continuation-evidence-invalid}
    :else
    (let [completion {:completed-at (:regulator/updated-at state)
                      :ticks (:regulator/ticks state)
                      :result (:regulator/last-result state)
                      :continuation/reason reason}
          resumed (-> state
                      (update :regulator/completions (fnil conj []) completion)
                      (assoc :regulator/status :running
                             :regulator/updated-at (now)
                             :regulator/last-result nil))
          persisted (persist-fn resumed)]
      (if (:ok persisted)
        {:ok true :status :running :state resumed}
        {:ok false :error/code :live-regulator-continuation-persistence-failed
         :finding persisted}))))

(defn resume-stopped!
  "Reopen a durably quiescent regulator through an explicit lifecycle resume."
  [{:keys [state persist-fn now-fn] :or {now-fn now}}]
  (cond
    (not= :stopped (:regulator/status state))
    {:ok false :error/code :live-regulator-not-stopped}
    (not (and (map? (:regulator/quiescence-witness state))
              (nil? (:regulator/tick-claim state))
              (fn? persist-fn)))
    {:ok false :error/code :live-regulator-quiescence-witness-invalid}
    :else
    (let [resumed (-> state
                      (update :regulator/quiescence-history (fnil conj [])
                              (:regulator/quiescence-witness state))
                      (dissoc :regulator/quiescence-witness)
                      (assoc :regulator/status :running
                             :regulator/reconciliation :ready
                             :regulator/updated-at (now-fn)))
          persisted (persist-fn resumed)]
      (if (:ok persisted)
        {:ok true :status :running :state resumed}
        {:ok false :error/code :live-regulator-resume-persistence-failed
         :finding persisted}))))

(defn start!
  "Start an idempotent single-thread regulator, recovering durable state."
  [{:keys [regulator-id read-fn persist-fn tick-fn tick-state-fn period-ms
           with-tick-lock-fn now-fn claim-allowed-fn]
    :or {period-ms default-period-ms}}]
  (cond
    (not (and (string? regulator-id) (not-empty regulator-id)))
    {:ok false :error/code :live-regulator-id-invalid}
    (not (and (fn? read-fn) (fn? persist-fn) (fn? with-tick-lock-fn)
              (or (fn? tick-fn) (fn? tick-state-fn))))
    {:ok false :error/code :live-regulator-provider-missing}
    (not (pos-int? period-ms))
    {:ok false :error/code :live-regulator-period-invalid}
    (live-runner? (get @runners regulator-id))
    {:ok true :status :already-running :state (status regulator-id)}

    (contains? @runners regulator-id)
    (do
      ;; A failed scheduled tick normally removes its runner. Namespace reloads
      ;; and repair calls can race that cleanup, leaving a shutdown executor in
      ;; the defonce table. It is not a running coordinator and must not satisfy
      ;; idempotent start.
      (cancel-scheduler! regulator-id)
      (start! {:regulator-id regulator-id :read-fn read-fn
               :persist-fn persist-fn :tick-fn tick-fn
               :tick-state-fn tick-state-fn :period-ms period-ms
               :with-tick-lock-fn with-tick-lock-fn :now-fn now-fn
               :claim-allowed-fn claim-allowed-fn}))
    :else
    (let [prepared
          (with-tick-lock-fn
            (fn []
              (let [recovered (or (read-fn)
                                  (initial-state regulator-id (or now-fn now)))]
                (cond
                  (not= regulator-id (:regulator/id recovered))
                  {:ok false :error/code :live-regulator-id-mismatch
                   :finding {:expected regulator-id
                             :actual (:regulator/id recovered)}}
                  (terminal? recovered) {:ok true :state recovered}
                  :else
                  (let [next-state
                        (if (:regulator/tick-claim recovered)
                          (assoc recovered :regulator/reconciliation :required)
                          (-> recovered
                              (update :regulator/epoch (fnil inc 0))
                              (assoc :regulator/reconciliation :ready
                                     :regulator/updated-at ((or now-fn now)))))
                        persisted (persist-fn next-state)]
                    (if (:ok persisted)
                      {:ok true :state next-state}
                      {:ok false
                       :error/code :live-regulator-start-persistence-failed
                       :finding persisted}))))))
          recovered (:state prepared)]
      (cond
        (not (:ok prepared)) prepared
        (not= regulator-id (:regulator/id recovered))
        {:ok false :error/code :live-regulator-id-mismatch
         :finding {:expected regulator-id :actual (:regulator/id recovered)}}
        (terminal? recovered)
        {:ok true :status (:regulator/status recovered) :state recovered}
        :else
        (let [state (atom recovered)
              first-tick (promise)
              executor (Executors/newSingleThreadScheduledExecutor
                        (reify ThreadFactory
                          (newThread [_ runnable]
                            (doto (Thread. runnable
                                           (str "apm-regulator-"
                                                (.incrementAndGet thread-seq)))
                              (.setDaemon true)))))
              run-one (fn []
                        (let [result
                              (with-tick-lock-fn
                                #(tick! {:state (or (read-fn) @state)
                                         :tick-fn tick-fn
                                         :tick-state-fn tick-state-fn
                                         :persist-fn persist-fn
                                         :expected-epoch
                                         (:regulator/epoch recovered)
                                         :now-fn (or now-fn now)
                                         :claim-allowed-fn claim-allowed-fn}))]
                          (when-let [next-state (:state result)]
                            (reset! state next-state))
                          (deliver first-tick result)
                          (when (or (not (:ok result)) (terminal? @state))
                            (cancel-scheduler! regulator-id))))]
          (swap! runners assoc regulator-id {:executor executor :state state})
          (.scheduleWithFixedDelay ^ScheduledExecutorService executor
                                   ^Runnable run-one 0 period-ms
                                   TimeUnit/MILLISECONDS)
          {:ok true :status :started :state recovered
           :first-tick first-tick})))))
