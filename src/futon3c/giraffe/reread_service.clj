(ns futon3c.giraffe.reread-service
  "Event-driven re-reading in the serving JVM. No periodic census or child JVM.
   One-shot debounce/mint deadlines share one reload-safe executor. Loading this
   namespace does nothing; the operator explicitly calls start! after cutover."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.mission-registry :as registry]
            [futon2.aif.task-requisition :as requisition]
            [futon3c.giraffe.missions :as missions]
            [futon3c.giraffe.readings :as readings]
            [futon3c.giraffe.reread-cli :as cli]
            [futon3c.giraffe.reread-mint-batch :as mint-batch]
            [futon3c.giraffe.reread-queue :as queue]
            [futon3c.giraffe.run :as giraffe-run])
  (:import [java.util.concurrent ScheduledThreadPoolExecutor ScheduledFuture TimeUnit ThreadFactory]))

(defonce ^:private state (atom {:status :stopped}))
(defonce ^:private lock (Object.))

(defn load-config
  "Read EnvironmentFile assignments without evaluating shell code. Process
   environment overrides the file; explicit options override both."
  [{:keys [env-file environment] :as opts}]
  (let [file (when-not (= false env-file)
               (io/file (or env-file (str (System/getProperty "user.home")
                                         "/.config/futon3c/giraffe-reread.env"))))
        assignments
        (if (and file (.isFile file))
          (into {} (keep (fn [line]
                           (when-not (or (str/blank? line) (str/starts-with? (str/trim line) "#"))
                             (if-let [[_ k v] (re-matches #"\s*([A-Z_0-9]+)=(.*?)\s*" line)]
                               [k (if (or (and (str/starts-with? v "\"") (str/ends-with? v "\""))
                                          (and (str/starts-with? v "'") (str/ends-with? v "'")))
                                    (subs v 1 (dec (count v))) v)]
                               (throw (ex-info "Unsupported EnvironmentFile assignment"
                                               {:file (str file)})))))
                         (str/split-lines (slurp file)))) {})
        env (merge assignments (or environment (into {} (System/getenv))))
        number (fn [k] (when-let [v (get env k)] (Long/parseLong v)))
        config (merge {:reader-seats (some-> (get env "FUTON3C_GIRAFFE_REREAD_READERS")
                                             (str/split #",") (->> (map str/trim) (remove str/blank?) vec))
                       :new-item-cap (number "FUTON3C_GIRAFFE_REREAD_NEW_ITEM_CAP")
                       :mint-window-ms (number "FUTON3C_GIRAFFE_REREAD_MINT_WINDOW_MS")
                       :kimi-task-repo (get env "KIMI_TASK_REPO")
                       :pointer (or (get env "GIRAFFE_READINGS_ROOT") cli/default-pointer)
                       :giraffe-root (or (get env "GIRAFFE_ROOT") cli/default-root)
                       :edges (or (get env "GIRAFFE_EDGES") cli/default-edges)
                       :mint-state-path mint-batch/default-state-path :debounce-ms 4000}
                      (dissoc opts :env-file :environment))]
    (when-not (and (seq (:reader-seats config))
                   (every? #(and (string? %) (not (str/blank? %))) (:reader-seats config))
                   (integer? (:new-item-cap config)) (<= 0 (:new-item-cap config))
                   (pos-int? (:mint-window-ms config)) (pos-int? (:debounce-ms config)))
      (throw (ex-info "Readers, nonnegative new-item cap and positive mint/debounce windows required"
                      {:kind :giraffe-reread/invalid-config})))
    config))

(defn item-snapshot
  "Read one document with the census parser and check only its reading. The
   corpus's population and JSONL bytes are read, but unrelated readings do not
   launch checker processes. Never infer coverage from members.json alone."
  [path {:keys [pointer]}]
  (if-let [entry (registry/task-document-entry path)]
    (let [live? (case (:kind entry)
                  :mission (registry/live-mission? entry)
                  :excursion (and (registry/live-excursion? entry)
                                  (not (requisition/requisition-record? entry)))
                  :ticket (registry/live-ticket? entry))
          root (.getCanonicalFile (io/file pointer))
          coverage (when live?
                     (:coverage
                      (readings/consume
                       {:checker (str (io/file root "check_edges.py"))
                        :members-file (str (io/file root "members.json"))
                        :batches (->> (file-seq root) (filter #(.isFile %))
                                      (filter #(re-matches #"batch-.*\.jsonl" (.getName %)))
                                      (sort-by str) (mapv str))
                        :repos missions/default-repos :target-id (:id entry)})))]
      {:id (:id entry) :source (:source entry)
       :item (when live? {:id (keyword (:id entry)) :source-pin (:source entry)})
       :coverage coverage})
    (throw (ex-info "Document is not admitted by the task census"
                    {:kind :giraffe-reread/path-not-admitted :path path}))))

(defn- active? [token] (and (= token (:token @state)) (:running? @state)))

(defn- failure! [token key exc]
  (when (= token (:token @state))
    (swap! state assoc-in [:errors key] {:message (.getMessage exc) :at-ms (System/currentTimeMillis)})))

(declare drain! schedule-mint! schedule-lease!)

(defn- mint-step! [config landed queue-idle?]
  (mint-batch/step! {:path (:mint-state-path config) :window-ms (:mint-window-ms config)
                     :now-ms (System/currentTimeMillis) :landed-count landed
                     :queue-idle? queue-idle? :mint-fn (:mint-fn config)}))

(defn- schedule-mint! [token result]
  ;; A single outstanding batch gets one deadline, not a polling loop. Failed
  ;; mints remain visible until another event or an explicit restart retries.
  (locking lock
    (when-let [^ScheduledFuture task (:mint-task @state)]
      (.cancel task false)
      (swap! state dissoc :mint-task))
    (when (and (active? token) (= :deferred (:status result))
               (pos? (:pending-landings result)) (nil? (:mint-task @state)))
      (let [{:keys [config executor]} @state
            delay (max 0 (- (+ (or (:last-mint-at-ms result) 0) (:mint-window-ms config))
                            (System/currentTimeMillis)))]
        (swap! state assoc :mint-task
               (.schedule ^ScheduledThreadPoolExecutor executor
                          ^Runnable (fn []
                                      (swap! state dissoc :mint-task)
                                      (when (active? token)
                                        (try
                                          (let [result (mint-step! config 0 false)]
                                            (swap! state assoc :last-mint result)
                                            (swap! state update :errors dissoc :mint)
                                            (schedule-mint! token result))
                                          (catch Exception e (failure! token :mint e)))))
                          (long delay) TimeUnit/MILLISECONDS))))))

(defn- schedule-lease! [token]
  ;; A claim left by a prior process needs one expiry event; it must not sit
  ;; forever merely because no document changes after startup.
  (locking lock
    (when-let [^ScheduledFuture task (:lease-task @state)]
      (.cancel task false)
      (swap! state dissoc :lease-task))
    (when (active? token)
      (let [{:keys [config executor]} @state
            deadline (first (sort (keep :lease_until_ms
                                         (queue/requests (:store config) {:state :claimed}))))]
        (when deadline
          (swap! state assoc :lease-task
                 (.schedule ^ScheduledThreadPoolExecutor executor
                            ^Runnable #(do (swap! state dissoc :lease-task) (drain! token))
                            (long (max 0 (- deadline (System/currentTimeMillis))))
                            TimeUnit/MILLISECONDS)))))))

(defn- record-round! [token result]
  (swap! state assoc :last-round result :last-mint (:mint result))
  (schedule-mint! token (:mint result))
  (schedule-lease! token)
  (when (and (active? token) (some #(not= :idle (:status %)) (:workers result)))
    ;; Yield between rounds so due document events can reconcile before more
    ;; claims. Failed attempts are bounded by the existing durable worker cap.
    (.execute ^ScheduledThreadPoolExecutor (:executor @state) ^Runnable #(drain! token))))

(defn- drain! [token]
  (when (active? token)
    (try
      (record-round! token (cli/drain-once! (:config @state)))
      (swap! state update :errors dissoc :drain)
      (catch Exception e (failure! token :drain e)))))

(defn notify-doc-changed!
  "Coalesce rapid saves per item. Stopped services never start implicitly.
   The watcher receives immediately; all validation and IO run on the worker."
  [path]
  (locking lock
    (if-not (:running? @state)
      {:status :stopped}
      (if-not (registry/task-document-kind path)
        {:status :ignored :reason :not-a-task-document}
        (let [path (.getAbsolutePath (io/file path))
              id (str/replace (.getName (io/file path)) #"\.md$" "")
              {:keys [executor token config]} @state
              prior (get-in @state [:pending id])
              serial (Object.)]
          (when (and prior (not= path (:path prior)))
            (throw (ex-info "Conflicting paths for one task identity" {:id id :paths [path (:path prior)]})))
          (when-let [^ScheduledFuture task (:task prior)] (.cancel task false))
          (let [task (.schedule ^ScheduledThreadPoolExecutor executor
                                ^Runnable
                                (fn []
                                  (let [current? (locking lock
                                                   (when (= serial (get-in @state [:pending id :serial]))
                                                     (swap! state update :pending dissoc id)
                                                     true))]
                                    (when (and current? (active? token))
                                      (try
                                        (let [{:keys [source item coverage]} (item-snapshot path config)
                                              known (get-in @state [:identities id])]
                                          (when (and known (not= known (:path source)))
                                            (throw (ex-info "Document conflicts with census identity"
                                                            {:id id :census-path known :path path})))
                                          (swap! state assoc-in [:identities id] (:path source))
                                          (queue/reconcile-item! (:store config) id item coverage
                                                                 (System/currentTimeMillis))
                                          (swap! state update :errors dissoc id)
                                          (drain! token))
                                        (catch Exception e (failure! token id e))))))
                                (long (:debounce-ms config)) TimeUnit/MILLISECONDS)]
            (swap! state assoc-in [:pending id] {:path path :serial serial :task task})
            {:status :scheduled :item-id id}))))))

(defn start!
  "Start once, asynchronously catching up through the full CLI reconciliation.
   Config is read at start, not namespace load. No systemd units are changed."
  ([] (start! {}))
  ([opts]
   (locking lock
     (let [old-executor (:executor @state)]
       (if (and old-executor (not (.isTerminated ^ScheduledThreadPoolExecutor old-executor)))
         {:status :already-started :service-status (:status @state)}
         (let [config (load-config opts)
               config (assoc config :store (or (:store config) (queue/open-store)) :parallel? false
                             :mint-fn (or (:mint-fn config)
                                          #(giraffe-run/mint! {:edges (:edges config)
                                                              :readings-root (:pointer config)
                                                              :root (:giraffe-root config)})))
               executor (doto (ScheduledThreadPoolExecutor.
                               1 (reify ThreadFactory
                                   (newThread [_ r] (doto (Thread. r "giraffe-reread") (.setDaemon true)))))
                          (.setRemoveOnCancelPolicy true))
               token (Object.)]
           (reset! state {:status :starting :running? true :token token :executor executor
                          :config config :pending {} :errors {} :identities {}})
           (.execute executor
                     ^Runnable
                     (fn []
                       (when (active? token)
                         (try
                           (let [snapshot-fn (or (:snapshot-fn config) cli/authority-snapshot)
                                 result (cli/run-once!
                                         (assoc config :snapshot-fn
                                                (fn [opts]
                                                  (let [snapshot (snapshot-fn opts)]
                                                    (swap! state assoc :identities
                                                           (into {} (map (fn [item]
                                                                           [(name (:id item)) (get-in item [:source-pin :path])]))
                                                                 (or (:all-items snapshot) (:open-items snapshot))))
                                                    snapshot))))]
                             (when (active? token) (swap! state assoc :status :running))
                             (record-round! token result))
                           (catch Exception e
                             (failure! token :startup e)
                             (swap! state assoc :status :failed :running? false))))))
           {:status :starting}))))))

(defn stop!
  "Stop accepting events and cancel pending deadlines. An active reading/mint
   may finish; start! refuses another executor until that one terminates."
  []
  (locking lock
    (doseq [task (concat (keep :task (vals (:pending @state))) [(:mint-task @state) (:lease-task @state)])
            :when task]
      (.cancel ^ScheduledFuture task false))
    (swap! state assoc :running? false :status :stopped :pending {})
    (when-let [executor (:executor @state)] (.shutdown ^ScheduledThreadPoolExecutor executor))
    {:status :stopped}))

(defn health
  "Read-only status, debounced items, durable requests and latest mint."
  []
  (let [{:keys [status config pending errors executor last-round last-mint]} @state
        queue-health (when config
                       (try (cli/health config)
                            (catch Exception e
                              {:status :unavailable :health-error (.getMessage e)
                               :requests (try (queue/requests (:store config))
                                              (catch Exception q
                                                {:unavailable (.getMessage q)}))})))]
    (merge queue-health
           {:status (cond (or (seq errors) (= :failed (:status last-mint))
                              (= :unavailable (:status queue-health))) :attention
                          (= :stopped status) (if (and executor (not (.isTerminated ^ScheduledThreadPoolExecutor executor)))
                                                :stopping :stopped)
                          :else status)
            :queue-status (:status queue-health) :pending-items (vec (keys pending))
            :errors errors :last-round last-round :mint-batch last-mint})))
