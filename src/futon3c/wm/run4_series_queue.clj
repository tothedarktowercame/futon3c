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
(def state-keys #{:schema :queue-id :config-sha256 :status :cursor
                  :completed-entry-ids :in-flight :reason :updated-at})
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

(def ^:private secret-keys
  #{:headers :authorization :credential :credentials :secret :token})
(defn- authority-form [x]
  (cond
    (fn? x) :server-owned-port
    (instance? java.io.File x) (.getCanonicalPath ^java.io.File x)
    (map? x) (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
                   (keep (fn [[k v]] (when-not (secret-keys k)
                                      [k (authority-form v)]))) x)
    (set? x) [:set (vec (sort-by pr-str (map authority-form x)))]
    (sequential? x) (mapv authority-form x)
    :else x))
(defn- entry-authority [entry]
  (assoc (public-entry entry)
         :request (authority-form (:request entry))
         :server-config (authority-form (:server-config entry))))

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
            :state-root (.getCanonicalPath (io/file (:state-root config)))
            :visibility-file (.getCanonicalPath (io/file (:visibility-file config)))
            :entries (mapv entry-authority (:entries config))})))
(defn- state-file [config] (io/file (:state-root config) "queue-state.edn"))
(defn- lock-file [config] (io/file (:state-root config) ".queue.lock"))
(defn- initial-state [config]
  {:schema schema :queue-id (:queue-id config) :config-sha256 (config-digest config)
   :status :stopped :cursor 0 :completed-entry-ids [] :in-flight nil :reason :not-started
   :updated-at (str (java.time.Instant/now))})
(defn- instant? [x]
  (and (nonblank? x) (try (java.time.Instant/parse x) true (catch Throwable _ false))))
(defn- valid-in-flight? [config cursor x]
  (or (nil? x)
      (and (map? x) (= #{:entry-id :click-id} (set (keys x)))
           (= (:entry-id (get (:entries config) cursor)) (:entry-id x))
           (nonblank? (:click-id x)))))
(defn- valid-state? [config x]
  (and (map? x) (= state-keys (set (keys x)))
       (= schema (:schema x)) (= (:queue-id config) (:queue-id x))
       (= (config-digest config) (:config-sha256 x))
       (contains? #{:running :held :stopped} (:status x))
       (nat-int? (:cursor x)) (<= (:cursor x) (count (:entries config)))
       (= (:completed-entry-ids x)
          (mapv :entry-id (take (:cursor x) (:entries config))))
       (valid-in-flight? config (:cursor x) (:in-flight x))
       (or (nil? (:reason x)) (keyword? (:reason x)))
       (instant? (:updated-at x))))
(defn- read-state* [config validate-sources?]
  (when validate-sources? (validate-config! config))
  (let [f (state-file config)
        state (cond (.isFile f) (parse-one (slurp f) :state-corrupt)
                    (.exists f) (refuse! :state-not-regular-file)
                    :else (initial-state config))]
    (when-not (valid-state? config state) (refuse! :state-invalid))
    state))
(defn read-state! [config] (read-state* config true))

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
  (let [new-flight (when (and (nil? (:in-flight state))
                              (nonblank? (:click-id details))
                              (nonblank? (:entry-id details)))
                     (select-keys details [:entry-id :click-id]))]
    (persist! config (assoc state :status :held :reason reason
                            :in-flight (or (:in-flight state) new-flight)))))

(defn- with-lifecycle-lock [config f]
  (locking queue-monitor
    (with-open [ch (FileChannel/open (.toPath (lock-file config))
                                    (into-array StandardOpenOption
                                                [StandardOpenOption/CREATE
                                                 StandardOpenOption/WRITE]))
                _lock (.lock ch)]
      (f))))

(defn- terminal-response [config entry state]
  (if-let [click-id (get-in state [:in-flight :click-id])]
    (let [awaited (runner/await-click! click-id (:await-timeout-ms config))]
      (if (= :completed (:status awaited))
        (series/step! (:server-config entry) (:headers entry) (:request entry))
        (reduced (hold! config state :click-incomplete
                        {:await-status (:status awaited)}))))
    (let [response (series/step! (:server-config entry) (:headers entry) (:request entry))]
      (if (= :trial-started (:status response))
        (let [started (persist! config
                                (assoc state :in-flight {:entry-id (:entry-id entry)
                                                        :click-id (:click-id response)}))]
          (terminal-response config entry started))
        response))))

(defn tick!
  "Perform at most one queue entry through the existing series service. A
  started click is awaited once; missing/unknown evidence holds the queue."
  [config]
  (with-lifecycle-lock
   config
   (fn []
     (validate-config! config)
     (let [state (read-state! config)
           entry (get (:entries config) (:cursor state))]
       (cond
         (not= :running (:status state)) state
         (nil? entry) (persist! config (assoc state :status :stopped
                                              :reason :queue-complete :in-flight nil))
         :else
         (try
           (let [response (terminal-response config entry state)]
             (if (reduced? response)
               @response
               (case (:status response)
                 :series-terminal
                 (persist! config
                           (assoc state :cursor (inc (:cursor state))
                                  :completed-entry-ids
                                  (conj (:completed-entry-ids state) (:entry-id entry))
                                  :in-flight nil :reason nil))
                 :trial-terminal
                 (persist! config (assoc state :in-flight nil :reason nil))
                 :awaiting-terminal-evidence
                 (hold! config state :terminal-evidence-incomplete
                        {:entry-id (:entry-id entry) :click-id (:click-id response)})
                 :infrastructure-stopped
                 (hold! config state :infrastructure-unsafe
                        {:entry-id (:entry-id entry)})
                 :reconciliation-required
                 (hold! config state :reconciliation-required
                        {:entry-id (:entry-id entry)})
                 (hold! config state :unknown-series-state
                        {:entry-id (:entry-id entry)}))))
           (catch Throwable e
             (let [latest (try (read-state* config false) (catch Throwable _ state))]
               (hold! config latest :series-step-refused
                      {:entry-id (:entry-id entry)
                       :refusal (or (:reason (ex-data e)) :exception)})))))))))

(defn- schedule! [config]
  (let [id (:queue-id config)
        executor (Executors/newSingleThreadScheduledExecutor)
        task (fn []
               (try (tick! config)
                    (catch Throwable e
                      (try
                        (with-lifecycle-lock
                          config
                          #(hold! config (read-state* config false) :scheduler-failed
                                  {:refusal (or (:reason (ex-data e)) :exception)}))
                        (catch Throwable _ nil)))))]
    (try
      (let [handle (.scheduleWithFixedDelay
                    ^ScheduledExecutorService executor ^Runnable task
                    (:interval-ms config) (:interval-ms config) TimeUnit/MILLISECONDS)]
        (swap! !runtimes assoc id {:executor executor :handle handle})
        (status config))
      (catch Throwable e
        (.shutdownNow executor)
        (swap! !runtimes dissoc id)
        (hold! config (read-state* config false) :scheduler-failed
               {:refusal (or (:reason (ex-data e)) :exception)})))))

(defn start!
  "Explicitly start a validated queue. The first tick occurs after INTERVAL-MS."
  [config]
  (with-lifecycle-lock
   config
   (fn []
    (validate-config! config)
    (let [id (:queue-id config)]
      (when (get @!runtimes id) (refuse! :already-started))
      (let [state (read-state! config)]
        (when (= :held (:status state)) (refuse! :held-requires-explicit-resume))
        (persist! config (assoc state :status :running :reason nil))
        (schedule! config))))))

(defn recover!
  "Recover process-local scheduling from durable state. Only a durable
  :running queue is rescheduled; held and stopped queues remain passive."
  [config]
  (with-lifecycle-lock
   config
   (fn []
    (validate-config! config)
    (if (get @!runtimes (:queue-id config))
      (status config)
      (let [state (read-state! config)]
        (if (= :running (:status state))
          (schedule! config)
          (visibility config state)))))))

(defn stop!
  "Stop without invoking the series boundary or dispatching new work."
  [config]
  (with-lifecycle-lock
   config
   (fn []
     (let [runtime (get @!runtimes (:queue-id config))]
       (swap! !runtimes dissoc (:queue-id config))
       (when-let [{:keys [^ScheduledExecutorService executor handle]} runtime]
         (.cancel ^java.util.concurrent.ScheduledFuture handle false)
         (.shutdown executor)))
     ;; Deliberately avoids mutable source revalidation so an operator can stop
     ;; a locally scheduled queue after authority drift.
     (persist! config (assoc (read-state* config false) :status :stopped
                             :reason :operator-stopped)))))

(defn resume!
  "Explicit recovery from a held durable state; never retries by itself."
  [config]
  (with-lifecycle-lock
   config
   (fn []
     (validate-config! config)
     (let [state (read-state! config)
           id (:queue-id config)
           runtime (get @!runtimes id)]
       (when-not (= :held (:status state)) (refuse! :queue-not-held))
       (when-let [{:keys [^ScheduledExecutorService executor handle]} runtime]
         (.cancel ^java.util.concurrent.ScheduledFuture handle false)
         (.shutdown executor)
         (swap! !runtimes dissoc id))
       (persist! config (assoc state :status :running :reason nil))
       (schedule! config)))))
