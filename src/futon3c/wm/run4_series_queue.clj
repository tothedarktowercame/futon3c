(ns futon3c.wm.run4-series-queue
  "Explicit, disabled-by-default lifecycle for an ordered queue of already
  frozen RUN4 manifests. Execution remains exclusively in run4-series-service."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.apm.library-loop-runner :as durable]
            [futon3c.wm.run4-run-visibility :as run-visibility]
            [futon3c.wm.run4-series-service :as series]
            [futon3c.wm.runner-service :as runner])
  (:import (java.nio.channels FileChannel)
           (java.nio.file StandardOpenOption)
           (java.util.concurrent Executors ScheduledExecutorService TimeUnit)))

(def schema :wm/run4-series-queue-state-v1)
(def visibility-schema "wm/run4-series-queue-visibility-v1")
(def config-keys #{:queue-id :state-root :visibility-file :interval-ms
                   :await-timeout-ms :entries})
(def entry-keys #{:entry-id :series-id :server-config :headers :request
                  :manifest-sha256 :cohort-id :cohort-sha256})
(defonce ^:private !runtimes (atom {}))
(defonce ^:private queue-monitor (Object.))

(defn- refuse! [reason & [data]]
  (throw (ex-info "RUN4 series queue refused"
                  (merge {:error :run4-series-queue-refused :reason reason} data))))
(defn- nonblank? [x] (and (string? x) (not (str/blank? x))))
(defn- sha? [x] (and (string? x) (boolean (re-matches #"[0-9a-f]{64}" x))))
(defn- exact-file [root path reason]
  (let [base (.getCanonicalFile (io/file root))
        file (.getCanonicalFile (io/file path))]
    (when-not (and (.isDirectory base) (.isFile file)
                   (.startsWith (.toPath file) (.toPath base)))
      (refuse! reason))
    file))
(defn- parse-one [text reason]
  (try
    (with-open [r (java.io.PushbackReader. (java.io.StringReader. text))]
      (let [v (edn/read {:eof ::empty} r)]
        (when (or (= ::empty v) (not= ::end (edn/read {:eof ::end} r)))
          (refuse! reason))
        v))
    (catch clojure.lang.ExceptionInfo e (throw e))
    (catch Throwable _ (refuse! reason))))

(defn- public-entry [entry]
  (select-keys entry [:entry-id :series-id :manifest-sha256
                      :cohort-id :cohort-sha256]))

(defn validate-config!
  "Validate server-owned runtime configuration without retaining secrets in
  durable state. There is deliberately no request/HTTP constructor."
  [config]
  (when-not (and (map? config) (= config-keys (set (keys config)))
                 (nonblank? (:queue-id config))
                 (integer? (:interval-ms config)) (pos? (:interval-ms config))
                 (integer? (:await-timeout-ms config)) (pos? (:await-timeout-ms config))
                 (vector? (:entries config)) (seq (:entries config))
                 (= (count (:entries config))
                    (count (distinct (map :entry-id (:entries config))))))
    (refuse! :invalid-config))
  (let [state-root (.getCanonicalFile (io/file (:state-root config)))
        visibility-file (.getCanonicalFile (io/file (:visibility-file config)))]
    (when-not (and (.isDirectory state-root)
                   (.isDirectory (.getParentFile visibility-file)))
      (refuse! :invalid-output-authority)))
  (doseq [entry (:entries config)]
    (let [server (:server-config entry)
          run4 (:run4 server)
          configured (:series run4)
          cohort (:execution-cohort run4)
          manifest (exact-file (:manifest-root configured)
                               (io/file (:manifest-root configured)
                                        (:manifest-ref configured))
                               :manifest-authority-invalid)
          text (slurp manifest)
          parsed (parse-one text :manifest-invalid)]
      (when-not
       (and (= entry-keys (set (keys entry)))
            (nonblank? (:entry-id entry)) (nonblank? (:series-id entry))
            (map? server) (map? (:headers entry))
            (= {:run4-series-ref (:manifest-ref configured)} (:request entry))
            (= (:manifest-sha256 configured) (:manifest-sha256 entry)
               (digest/sha256 text))
            (= (:series-id parsed) (:series-id entry))
            (= #{(:manifest-ref configured)} (:manifest-allowlist configured))
            (= (:cohort-id cohort) (:cohort-id entry))
            (= (:sha256 cohort) (:cohort-sha256 entry))
            (sha? (:manifest-sha256 entry)) (sha? (:cohort-sha256 entry))
            (true? (:enabled? run4)) (true? (:enabled? configured)))
        (refuse! :entry-authority-mismatch {:entry-id (:entry-id entry)}))))
  config)

(defn- config-digest [config]
  (digest/sha256
   (pr-str {:queue-id (:queue-id config)
            :interval-ms (:interval-ms config)
            :await-timeout-ms (:await-timeout-ms config)
            :entries (mapv public-entry (:entries config))})))
(defn- state-file [config] (io/file (:state-root config) "queue-state.edn"))
(defn- lock-file [config] (io/file (:state-root config) ".queue.lock"))
(defn- initial-state [config]
  {:schema schema :queue-id (:queue-id config) :config-sha256 (config-digest config)
   :status :stopped :cursor 0 :in-flight nil :reason :not-started
   :updated-at (str (java.time.Instant/now))})
(defn- valid-state? [config x]
  (and (map? x) (= schema (:schema x)) (= (:queue-id config) (:queue-id x))
       (= (config-digest config) (:config-sha256 x))
       (contains? #{:running :held :stopped} (:status x))
       (nat-int? (:cursor x)) (<= (:cursor x) (count (:entries config)))
       (or (nil? (:in-flight x)) (map? (:in-flight x)))
       (or (nil? (:reason x)) (keyword? (:reason x)))
       (nonblank? (:updated-at x))))
(defn read-state! [config]
  (validate-config! config)
  (let [f (state-file config)
        state (if (.isFile f) (parse-one (slurp f) :state-corrupt)
                  (initial-state config))]
    (when-not (valid-state? config state) (refuse! :state-invalid))
    state))

(defn- visibility [config state]
  (let [entry (get (:entries config) (:cursor state))
        casting (get-in entry [:server-config :run4 :casting])]
    {:schema visibility-schema :queue_id (:queue-id config)
     :controller_state (name (:status state)) :updated_at (:updated-at state)
     :hold_reason (some-> (:reason state) name)
     :target (when entry {:entry_id (:entry-id entry) :series_id (:series-id entry)
                          :manifest_sha256 (:manifest-sha256 entry)})
     :assigned_roles (when entry {:author (:author casting) :reviewer (:reviewer casting)
                                  :repair_reviewer (:repair-reviewer casting)})
     :active_actors []
     :in_flight (:in-flight state)}))
(defn- persist! [config state]
  (let [state (assoc state :updated-at (str (java.time.Instant/now)))]
    (durable/atomic-write-edn! (state-file config) state)
    (run-visibility/publish! (:visibility-file config) (visibility config state))
    state))
(defn status [config] (visibility config (read-state! config)))
(defn- hold! [config state reason details]
  (persist! config (assoc state :status :held :reason reason
                          :in-flight (merge (:in-flight state) details))))

(defn tick!
  "Perform at most one queue entry through the existing series service. A
  started click is awaited once; missing/unknown evidence holds the queue."
  [config]
  (validate-config! config)
  (locking queue-monitor
    (with-open [ch (FileChannel/open (.toPath (lock-file config))
                                    (into-array StandardOpenOption
                                                [StandardOpenOption/CREATE
                                                 StandardOpenOption/WRITE]))
                _lock (.lock ch)]
      (let [state (read-state! config)]
        (if (not= :running (:status state))
          state
          (if-let [entry (get (:entries config) (:cursor state))]
            (try
              (let [response (series/step! (:server-config entry) (:headers entry)
                                           (:request entry))
                    response (if (= :trial-started (:status response))
                               (let [started (persist! config
                                                       (assoc state :in-flight
                                                              {:entry-id (:entry-id entry)
                                                               :click-id (:click-id response)}))
                                     awaited (runner/await-click! (:click-id response)
                                                                 (:await-timeout-ms config))]
                                 (if (= :completed (:status awaited))
                                   (series/step! (:server-config entry) (:headers entry)
                                                 (:request entry))
                                   (reduced (hold! config started :click-incomplete
                                                   {:await-status (:status awaited)}))))
                               response)]
                (if (reduced? response)
                  @response
                  (case (:status response)
                    :series-terminal
                    (persist! config (assoc state :cursor (inc (:cursor state))
                                            :in-flight nil :reason nil))
                    :trial-terminal
                    (persist! config (assoc state :in-flight nil :reason nil))
                    :awaiting-terminal-evidence
                    (hold! config state :terminal-evidence-incomplete
                           {:entry-id (:entry-id entry) :click-id (:click-id response)})
                    :infrastructure-stopped
                    (hold! config state :infrastructure-unsafe {:entry-id (:entry-id entry)})
                    :reconciliation-required
                    (hold! config state :reconciliation-required {:entry-id (:entry-id entry)})
                    (hold! config state :unknown-series-state {:entry-id (:entry-id entry)}))))
              (catch Throwable e
                (hold! config state :series-step-refused
                       {:entry-id (:entry-id entry)
                        :refusal (or (:reason (ex-data e)) :exception)})))
            (persist! config (assoc state :status :stopped :reason :queue-complete
                                    :in-flight nil))))))))

(defn- schedule! [config]
  (let [id (:queue-id config)
        executor (Executors/newSingleThreadScheduledExecutor)
        handle (.scheduleWithFixedDelay
                ^ScheduledExecutorService executor
                ^Runnable (fn [] (try (tick! config) (catch Throwable _ nil)))
                (:interval-ms config) (:interval-ms config) TimeUnit/MILLISECONDS)]
    (swap! !runtimes assoc id {:executor executor :handle handle})
    (status config)))

(defn start!
  "Explicitly start a validated queue. The first tick occurs after INTERVAL-MS."
  [config]
  (validate-config! config)
  (locking !runtimes
    (let [id (:queue-id config)]
      (when (get @!runtimes id) (refuse! :already-started))
      (let [state (read-state! config)]
        (when (= :held (:status state)) (refuse! :held-requires-explicit-resume))
        (persist! config (assoc state :status :running :reason nil))
        (schedule! config)))))

(defn recover!
  "Recover process-local scheduling from durable state. Only a durable
  :running queue is rescheduled; held and stopped queues remain passive."
  [config]
  (validate-config! config)
  (locking !runtimes
    (if (get @!runtimes (:queue-id config))
      (status config)
      (let [state (read-state! config)]
        (if (= :running (:status state))
          (schedule! config)
          (visibility config state))))))

(defn stop!
  "Stop without invoking the series boundary or dispatching new work."
  [config]
  (validate-config! config)
  (let [runtime (get @!runtimes (:queue-id config))]
    (swap! !runtimes dissoc (:queue-id config))
    (when-let [{:keys [^ScheduledExecutorService executor handle]} runtime]
      (.cancel ^java.util.concurrent.ScheduledFuture handle false)
      (.shutdown executor)))
  ;; Serialize behind an already-running tick so it cannot later overwrite the
  ;; durable stop with a stale :running state.
  (locking queue-monitor
    (with-open [ch (FileChannel/open (.toPath (lock-file config))
                                    (into-array StandardOpenOption
                                                [StandardOpenOption/CREATE
                                                 StandardOpenOption/WRITE]))
                _lock (.lock ch)]
      (persist! config (assoc (read-state! config) :status :stopped
                              :reason :operator-stopped)))))

(defn resume!
  "Explicit recovery from a held durable state; never retries by itself."
  [config]
  (validate-config! config)
  (let [state (read-state! config)]
    (when-not (= :held (:status state)) (refuse! :queue-not-held))
    (persist! config (assoc state :status :stopped :reason :operator-resume-required))
    (start! config)))
