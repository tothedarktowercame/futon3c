(ns futon3c.giraffe.reread-service
  "Event-driven re-reading with a periodic authority-reconciliation backstop.
   Reconciliation, reader rounds and deadlines share one reload-safe executor. Loading this
   namespace does nothing; the operator explicitly calls start! after cutover."
  (:require [cheshire.core :as json]
            [futon3c.giraffe.corpus-generation :as generation]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.mission-registry :as registry]
            [futon2.aif.task-requisition :as requisition]
            [futon3c.giraffe.missions :as missions]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.giraffe-needs :as giraffe-needs]
            [futon3c.giraffe.preview-refresh :as preview]
            [futon3c.giraffe.readings :as readings]
            [futon3c.giraffe.reread-cli :as cli]
            [futon3c.giraffe.reread-mint-batch :as mint-batch]
            [futon3c.giraffe.reread-queue :as queue]
            [futon3c.giraffe.run :as giraffe-run]
            [futon3c.arxana.item-inventory :as item-inventory]
            [futon3c.arxana.turn-mention-writer :as mention-writer]
            [futon3c.circus.hou-store :as hou-store]
            [futon3c.xiang.turn-service :as turn-service]
            [futon3c.xiang.turn-store :as ts])
  (:import [java.sql DriverManager]
           [java.util.concurrent ScheduledThreadPoolExecutor ScheduledFuture TimeUnit ThreadFactory]))

(defonce ^:private state (atom {:status :stopped}))
(defonce ^:private lock (Object.))

(def default-entity-sidecar
  (str (System/getProperty "user.home") "/code/futon1b/migration-store-21/fts5-evidence.db"))

(defn- live-turn-store []
  (when-let [http (find-ns 'futon3c.transport.http)]
    (when-let [service (ns-resolve http 'xiang-bell-service)]
      (get-in (service) [:config :store]))))

(defn items-touched-by-turn
  "Resolve stored typed mentions to document entities, never scanning prose or
   selecting legacy workplan items. A Hou header is the completion barrier for
   the asynchronous source/reply mention writes; absence remains :pending."
  ([record] (items-touched-by-turn record {:entity-sidecar default-entity-sidecar}))
  ([record {:keys [entity-sidecar mention-config]}]
   (let [mention-config (or mention-config (mention-writer/default-config))
         header-read (hou-store/headers-for-turn mention-config record)
         mention-read (when (seq (:headers header-read))
                        (mention-writer/read-turn-mentions mention-config record))
         pending (or (:pending header-read) (:pending mention-read))
         ids (:item-ids mention-read)
         empty-result {:items [] :unknown-ids [] :ambiguous-ids []}]
     (if pending
       (assoc empty-result :pending pending)
       (with-open [db (DriverManager/getConnection
                       (str "jdbc:sqlite:" (.toASCIIString (.toURI (io/file entity-sidecar))) "?mode=ro"))
                   query (.prepareStatement db "SELECT id FROM ent_node WHERE external_id=? AND type IN (?,?)")]
         (reduce
          (fn [result id]
            (let [kind (case (first id) \M "mission/doc" \E "excursion/doc" \T "ticket/doc")]
              (.setString query 1 id)
              (.setString query 2 kind)
              (.setString query 3 (str ":" kind))
              (with-open [rows (.executeQuery query)]
                (let [matches (loop [found []]
                                (if (.next rows) (recur (conj found (.getString rows "id"))) found))]
                  (case (count matches)
                    0 (update result :unknown-ids conj id)
                    1 (update result :items conj {:external-id id :entity-id (first matches) :type kind})
                    (update result :ambiguous-ids conj id))))))
          empty-result ids))))))

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
                       :mint-state-path mint-batch/default-state-path :debounce-ms 4000
                       :event-retry-ms 60000 :event-retry-limit 3
                       :failed-retry-after-ms (or (number "FUTON3C_GIRAFFE_REREAD_FAILED_RETRY_AFTER_MS") 86400000)
                       :failed-retry-limit (or (number "FUTON3C_GIRAFFE_REREAD_FAILED_RETRY_LIMIT") 3)
                       :retire-cap (or (number "FUTON3C_GIRAFFE_REREAD_RETIRE_CAP") 200)
                       :reconcile-interval-ms (or (number "FUTON3C_GIRAFFE_REREAD_RECONCILE_INTERVAL_MS")
                                                  600000)
                       :preview (merge preview/default-config (:preview opts))
                       :entity-sidecar default-entity-sidecar
                       :mention-config (mention-writer/default-config)
                       :inventory-config (mention-writer/default-config)
                       :inventory-items-fn item-inventory/current-items
                       :inventory-sync-fn item-inventory/sync-items!
                       :needs-writer! giraffe-needs/write-run!
                       :turn-store live-turn-store}
                      (dissoc opts :env-file :environment :preview))]
    (when-not (and (seq (:reader-seats config))
                   (every? #(and (string? %) (not (str/blank? %))) (:reader-seats config))
                   (integer? (:new-item-cap config)) (<= 0 (:new-item-cap config))
                   (pos-int? (:mint-window-ms config)) (pos-int? (:debounce-ms config))
                   (pos-int? (:reconcile-interval-ms config))
                   (pos-int? (:event-retry-ms config)) (pos-int? (:event-retry-limit config))
                   (pos-int? (:retire-cap config))
                   (pos-int? (:failed-retry-after-ms config))
                   (nat-int? (:failed-retry-limit config)))
      (throw (ex-info "Readers, nonnegative new-item cap and positive mint/debounce/reconcile windows required"
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

(declare drain! schedule-mint! schedule-lease! schedule-preview-refresh! refresh-preview!)

(defn- failure! [token key exc]
  (when (= token (:token @state))
    (swap! state assoc-in [:errors key] {:message (.getMessage exc) :at-ms (System/currentTimeMillis)})))

(defn- after-mint!
  "A completed mint (new pass/priorities run) refreshes Mojira's GCC pages.
   Scheduled on the preview executor, never blocking the mint or turn path."
  [token mint-result]
  (when (= :minted (:status mint-result))
    (schedule-preview-refresh! token (get-in mint-result [:last-result :run-id]))))

(defn- schedule-preview-refresh!
  "At most one refresh in flight; a mint during a refresh queues one more.
   Stop! clears :running? so a stopped service never refreshes again."
  [token run-id]
  (locking lock
    (when (and (active? token) run-id)
      (swap! state assoc :refresh-requested? true :refresh-run-id run-id)
      (when-not (:refresh-running? @state)
        (swap! state assoc :refresh-running? true)
        (let [executor (:preview-executor @state)]
          (.execute ^ScheduledThreadPoolExecutor executor ^Runnable #(refresh-preview! token)))))))

(defn- refresh-preview!
  "One or more coalesced page refreshes. Each pass takes the newest queued
   run id and items; a mint landing mid-pass queues exactly one follow-up.
   A failing script is recorded in :last-refresh and never escapes."
  [token]
  (loop []
    (let [{:keys [config refresh-run-id refresh-items]} @state]
      (swap! state assoc :refresh-requested? false :refresh-items #{})
      ;; The minted run's needs go to the GCC before the party is repointed
      ;; at it, so 猴's GCC needs reader never sees a party run it lacks.
      (when-let [w (and refresh-run-id (:needs-writer! config))]
        (let [r (try (w refresh-run-id)
                     (catch Exception e {:failed :needs-write-exception :detail (.getMessage e)}))]
          (when (active? token)
            (swap! state assoc :last-needs-write
                   (assoc (select-keys r [:run :written :skipped :failed :detail])
                          :run refresh-run-id :at-ms (System/currentTimeMillis))))))
      (let [summary (try
                      (preview/refresh! (:preview config)
                                        (when refresh-run-id
                                          (str (io/file (:giraffe-root config) refresh-run-id)))
                                        refresh-items)
                      (catch Exception e
                        {:ok? false :items [] :error (.getMessage e)}))]
        (when (active? token)
          (swap! state assoc :last-refresh (assoc summary :at-ms (System/currentTimeMillis))))))
    ;; Check and release under the same lock schedule-preview-refresh! takes
    ;; (claude-13 review): checking outside it let a mint landing between the
    ;; check and the release be dropped.
    (when (locking lock
            (if (and (active? token) (:refresh-requested? @state))
              true
              (do (swap! state dissoc :refresh-running?) false)))
      (recur))))

(defn- mint-step! [config landed queue-idle?]
  (mint-batch/step! {:path (:mint-state-path config) :window-ms (:mint-window-ms config)
                     :now-ms (System/currentTimeMillis) :landed-count landed
                     :queue-idle? queue-idle? :mint-fn (:mint-fn config)}))

(defn- schedule-mint! [token result]
  ;; A single outstanding batch gets one deadline, not a polling loop. Failed
  ;; mints remain visible until another event or an explicit restart retries.
  (locking lock
    (when (active? token)
      (when-let [^ScheduledFuture task (:mint-task @state)]
        (.cancel task false)
        (swap! state dissoc :mint-task)))
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
                                            (after-mint! token result)
                                            (swap! state update :errors dissoc :mint)
                                            (schedule-mint! token result))
                                          (catch Exception e (failure! token :mint e)))))
                          (long delay) TimeUnit/MILLISECONDS))))))

(defn- schedule-lease! [token]
  ;; A claim left by a prior process needs one expiry event; it must not sit
  ;; forever merely because no document changes after startup.
  ;; Only the live token may touch the task handle: an old executor finishing
  ;; after a restart must not cancel the new service's deadline (codex-7 review).
  (locking lock
    (when (active? token)
      (when-let [^ScheduledFuture task (:lease-task @state)]
        (.cancel task false)
        (swap! state dissoc :lease-task))
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
  ;; A round finished by a stopped service's executor writes nothing into the
  ;; new service's state (codex-7 review, 2026-11-16).
  (when (active? token)
    (swap! state assoc :last-round result :last-mint (:mint result)))
  (after-mint! token (:mint result))
  (schedule-mint! token (:mint result))
  (schedule-lease! token)
  (when (and (active? token) (some #(not= :idle (:status %)) (:workers result)))
    ;; Yield between rounds so due document events can reconcile before more
    ;; claims. Failed attempts are bounded by the existing durable worker cap.
    (.execute ^ScheduledThreadPoolExecutor (:executor @state) ^Runnable #(drain! token))))

(defn- round! [token f]
  ;; Every round runs on the single worker; queued ticks run after it finishes.
  (when (active? token)
    (swap! state assoc :round-running? true)
    (try (f)
         (finally
           (when (= token (:token @state))
             (swap! state assoc :round-running? false :round-ended-ns (System/nanoTime)))))))

(defn- drain! [token]
  (round! token
          #(try
             (record-round! token (cli/drain-once! (:config @state)))
             (swap! state update :errors dissoc :drain)
             (catch Exception e (failure! token :drain e)))))

(defn- full-round! [token config]
  (round! token
          #(let [snapshot-fn (or (:snapshot-fn config) cli/authority-snapshot)
                 result (cli/run-once!
                         (assoc config :snapshot-fn
                                (fn [opts]
                                  (let [snapshot (snapshot-fn opts)]
                                    (when (active? token)
                                      (swap! state assoc :identities
                                             (into {} (map (fn [item]
                                                             [(name (:id item))
                                                              (get-in item [:source-pin :path])]))
                                                   (or (:all-items snapshot) (:open-items snapshot)))))
                                    snapshot))))]
             (record-round! token result))))

(defn- retire-missing! [token]
  (when (active? token)
    (let [at-ms (System/currentTimeMillis)]
      (try
        (let [{:keys [config identities]} @state
              {:keys [path]} (generation/resolve-generation (:pointer config))
              members (json/parse-string (slurp (io/file path "members.json")) true)
              queued (into #{} (comp (filter #(#{:pending :claimed} (:state %)))
                                    (map :target_id)) (queue/requests (:store config)))
              targets (->> members
                           (remove :retired)
                           (remove #(contains? identities (:id %)))
                           (remove #(contains? queued (:id %)))
                           (filter #(and (string? (:path %))
                                         (not (str/blank? (:path %)))
                                         (not (.isFile (io/file (:path %))))))
                           (map :id) distinct sort (take (:retire-cap config)) vec)]
          (when (active? token)
            (let [result (when (seq targets)
                           (generation/retire-readings!
                            {:pointer (:pointer config) :generations-dir (:generations-dir config)
                             :targets targets :reason :source-missing :now-ms at-ms}))]
              (when (and (active? token) (= :published (:status result)))
                ;; The corpus changed, so the pinned run must follow: count the
                ;; retirement as one landing, under the same rule as a landed
                ;; reading: mint now if no reread is queued, otherwise the
                ;; window spaces mints during a burst (claude-13 review).
                (let [mint (mint-step! config 1 (empty? queued))]
                  (swap! state assoc :last-mint mint)
                  (after-mint! token mint)
                  (schedule-mint! token mint)))
              (when (active? token)
                (swap! state assoc :last-retirement
                       {:at-ms at-ms :retired (count (:retired result)) :refused nil})
                (swap! state update :errors dissoc :retirement)))))
        (catch Exception e
          (when (active? token)
            (swap! state assoc :last-retirement
                   {:at-ms at-ms :retired 0 :refused (or (ex-data e) {:message (.getMessage e)})})
            (failure! token :retirement e)))))))

(defn- retry-failed! [token]
  (when (active? token)
    (try
      (let [{:keys [config identities]} @state
            {:keys [store failed-retry-after-ms failed-retry-limit]} config
            now-ms ((or (:clock config) #(System/currentTimeMillis)))
            failed (queue/requests store {:state :failed})
            exhausted (->> failed (filter #(>= (:retries %) failed-retry-limit))
                           (map :target_id) distinct sort vec)
            retried (reduce
                     (fn [n request]
                       (let [path (get identities (:target_id request))]
                         (if (and (active? token) path
                                  ;; an operator's withdrawal is a decision,
                                  ;; not a failure to retry (claude-13 review)
                                  (not (re-find #"operator-withdrawn"
                                                (str (:last_error request))))
                                  (< (:retries request) failed-retry-limit)
                                  (< (:updated_at_ms request) (- now-ms failed-retry-after-ms))
                                  (.isFile (io/file path))
                                  (= (:source_sha request) (generation/file-sha path))
                                  (queue/retry-failed! store request now-ms
                                                       failed-retry-after-ms failed-retry-limit))
                           (inc n) n))) 0 failed)]
        (when (active? token)
          (swap! state assoc :last-failed-retry {:retried retried :exhausted (count exhausted)}
                 :failed-exhausted exhausted)
          (swap! state update :errors dissoc :failed-retry)))
      (catch Exception e (failure! token :failed-retry e)))))

(defn- refresh-if-behind!
  "If party.json was built from an older run than the last successful mint,
   refresh now. A stop!/start! drops a refresh queued on the old preview
   executor (claude-13, 2026-11-16: the 00:46 mint's refresh was lost that
   way and the pages stayed on the 00:35 run). Cheap: two small file reads."
  [token]
  (when (active? token)
    (try
      (let [{:keys [config]} @state
            minted (get-in (mint-batch/read-state (:mint-state-path config))
                           [:last-result :run-id])
            party-file (io/file (get-in config [:preview :out-dir]) "party.json")
            shown (when (.isFile party-file)
                    (get-in (json/parse-string (slurp party-file) true)
                            [:sources :giraffe_run]))]
        (when (and minted (not= minted shown))
          (schedule-preview-refresh! token minted)))
      (catch Exception e (failure! token :refresh-catch-up e)))))

(declare schedule-backstop!)

(defn- sync-item-inventory! [token]
  (when (active? token)
    (try
      (let [config (:config @state)
            items ((:inventory-items-fn config) (:inventory-config config))
            fingerprint #(digest/canonical-digest
                          (select-keys % [:id :path :status-line :source]))
            confirmed (or (:inventory-confirmed-items @state) {})
            unchecked (filterv #(not= (fingerprint %) (confirmed (:id %))) items)
            result (if (empty? unchecked)
                     {:checked [] :written [] :batch-count 0}
                     ((:inventory-sync-fn config)
                      (assoc (:inventory-config config) :inventory-items items)
                      unchecked))]
        (when (active? token)
          (swap! state assoc
                 :inventory-confirmed-ids (set (map :id items))
                 :inventory-confirmed-items (into {} (map (juxt :id fingerprint)) items)
                 :last-inventory-sync (assoc result :at-ms (System/currentTimeMillis)))
          (swap! state update :errors dissoc :item-inventory)))
      (catch Exception e
        ;; This independent port must not prevent the rest of the backstop.
        (when (active? token)
          (swap! state assoc :last-inventory-sync
                 {:error (.getMessage e) :at-ms (System/currentTimeMillis)}))
        (failure! token :item-inventory e)))))

(defn- backstop-tick! [token]
  (when (active? token)
    (try
      ;; Ticks and rounds share one worker. A delayed tick runs once after
      ;; the round finishes, then schedules its successor without a backlog.
      (sync-item-inventory! token)
      (full-round! token (:config @state))
      (retire-missing! token)
      (retry-failed! token)
      (refresh-if-behind! token)
      (when (active? token)
        (swap! state assoc :last-backstop-at-ms (System/currentTimeMillis))
        (swap! state update :errors dissoc :backstop))
      (catch Exception e (failure! token :backstop e))
      (finally (schedule-backstop! token)))))

(defn- schedule-backstop! [token]
  ;; One outstanding tick, scheduled after the last one completes; no backlog
  ;; or catch-up burst, including when a reader round delays the tick.
  (locking lock
    (when (active? token)
      (let [{:keys [config executor]} @state
            delay (:reconcile-interval-ms config)]
        (swap! state assoc :backstop-task
               (.schedule ^ScheduledThreadPoolExecutor executor
                          ^Runnable #(backstop-tick! token)
                          (long delay) TimeUnit/MILLISECONDS))))))

(defn- schedule-doc-event!
  ;; Caller holds lock. Keep the serial while executing so a newer notify can
  ;; supersede an in-flight failure as well as a queued retry. Limit includes
  ;; the initial attempt; fixed backoff never creates concurrent item work.
  [token id path serial attempt delay]
  (when (active? token)
    (let [{:keys [executor config]} @state
          task (.schedule ^ScheduledThreadPoolExecutor executor
                 ^Runnable
                 (fn []
                   (when (locking lock
                           (and (active? token)
                                (= serial (get-in @state [:pending id :serial]))))
                     (try
                       (let [{:keys [source item coverage]} (item-snapshot path config)
                             known (get-in @state [:identities id])]
                         (when (and known (not= known (:path source)))
                           (throw (ex-info "Document conflicts with census identity"
                                           {:id id :census-path known :path path})))
                         (swap! state assoc-in [:identities id] (:path source))
                         (queue/reconcile-item! (:store config) id item coverage
                                                (System/currentTimeMillis))
                         (swap! state update :refresh-items (fnil conj #{}) id)
                         (locking lock
                           (when (and (active? token)
                                      (= serial (get-in @state [:pending id :serial])))
                             (swap! state update :pending dissoc id)
                             (swap! state update :errors dissoc id)))
                         (drain! token))
                       (catch Exception e
                         (locking lock
                           (when (and (= token (:token @state))
                                      (or (not (:running? @state))
                                          (= serial (get-in @state [:pending id :serial]))))
                             ;; Stop cancels retries, not the diagnostic of a
                             ;; parse that was already running when stopped.
                             (failure! token id e)
                             (when (active? token)
                               (if (< attempt (:event-retry-limit config))
                                 (schedule-doc-event! token id path serial (inc attempt)
                                                      (:event-retry-ms config))
                                 (swap! state update :pending dissoc id)))))))))
                 (long delay) TimeUnit/MILLISECONDS)]
      (swap! state assoc-in [:pending id]
             {:path path :serial serial :task task :attempt attempt}))))

(defn notify-doc-changed!
  "Coalesce saves per item. Failed events retry with bounded backoff; a new
   notification supersedes that retry budget. Stopped services never retry."
  [path]
  (locking lock
    (if-not (:running? @state)
      {:status :stopped}
      (if-not (registry/task-document-kind path)
        {:status :ignored :reason :not-a-task-document}
        (let [path (.getAbsolutePath (io/file path))
              id (str/replace (.getName (io/file path)) #"\.md$" "")
              {:keys [token config]} @state
              prior (get-in @state [:pending id])]
          (when (and prior (not= path (:path prior)))
            (throw (ex-info "Conflicting paths for one task identity" {:id id :paths [path (:path prior)]})))
          (when-let [^ScheduledFuture task (:task prior)] (.cancel task false))
          (schedule-doc-event! token id path (Object.) 1 (:debounce-ms config))
          {:status :scheduled :item-id id})))))

(defn- schedule-turn-mint! [token]
  (locking lock
    (when (active? token)
      (when-let [^ScheduledFuture task (:turn-mint-task @state)] (.cancel task false))
      (let [{:keys [config executor]} @state]
        (swap! state assoc :turn-mint-task
               (.schedule ^ScheduledThreadPoolExecutor executor
                          ^Runnable
                          (fn []
                            (swap! state dissoc :turn-mint-task)
                            (when (active? token)
                              (try
                                (let [result (mint-batch/step!
                                              {:path (:mint-state-path config)
                                               :window-ms (:mint-window-ms config)
                                               :now-ms (System/currentTimeMillis) :refresh? true
                                               :landed-count 0 :mint-fn (:mint-fn config)})]
                                  (swap! state assoc :last-mint result :last-turn-mint result)
                                  (after-mint! token result)
                                  (swap! state update :errors dissoc :turn-mint)
                                  (schedule-mint! token result))
                                (catch Exception e (failure! token :turn-mint e)))))
                          (long (:debounce-ms config)) TimeUnit/MILLISECONDS))))))

(defn- handle-turn! [token {:keys [id]}]
  (when (and (active? token) (not (contains? (:seen-turns @state) id)))
    (try
      (let [config (:config @state)
            store (:turn-store config)
            store (if (fn? store) (store) store)
            record (some-> (when store (ts/read-record store id)) (assoc :id id))]
        (when-not record
          (throw (ex-info "Settled turn record unavailable" {:turn-id id})))
        (let [analysis (ts/read-analysis store id)
              draft (ts/read-draft store id)
              record (cond-> record analysis (assoc :analysis analysis) draft (assoc :draft draft))
              {:keys [items unknown-ids ambiguous-ids pending]} (items-touched-by-turn record config)
              now (System/currentTimeMillis)]
          (when-not pending
            (swap! state
                 (fn [s]
                   (-> (reduce (fn [s item]
                                 (assoc-in s [:discussed (:external-id item)]
                                           (assoc item :turn-id id :at-ms now))) s items)
                       (update :seen-turns conj id)
                       (update :unknown-turn-item-count + (count unknown-ids))
                       (update :ambiguous-turn-item-count + (count ambiguous-ids)))))
            (swap! state update :refresh-items (fnil into #{}) (map :external-id items))
            (when (seq items) (schedule-turn-mint! token))
            (swap! state update :errors dissoc [:turn id]))))
      (catch Exception e (failure! token [:turn id] e)))))

(defn- listen-for-turns! [token]
  (turn-service/add-turn-listener!
   ::giraffe
   (fn [{:keys [event id] :as turn}]
     (when (and (= :settled event) (ts/record-id? id))
       (locking lock
         (when (active? token)
           (.execute ^ScheduledThreadPoolExecutor (:executor @state)
                     ^Runnable #(handle-turn! token turn))))))))

(defn start!
  "Start once, asynchronously catching up through the full CLI reconciliation.
   Config is read at start, not namespace load. No systemd units are changed."
  ([] (start! {}))
  ([opts]
   (locking lock
     (let [old-executor (:executor @state)
           old-preview (:preview-executor @state)]
       ;; Refuse only a RUNNING executor. One that stop! has shut down may
       ;; still be finishing an in-flight reading (an Agency job can take many
       ;; minutes); start! no longer waits for it: the new executor's first
       ;; task does, so rounds stay serial and the in-flight reading still
       ;; settles through the queue (claude-13, 2026-11-15: restart after
       ;; stop! returned :already-started for ~5 minutes).
       (if (and old-executor (not (.isShutdown ^ScheduledThreadPoolExecutor old-executor)))
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
               preview-executor (doto (ScheduledThreadPoolExecutor.
                                       1 (reify ThreadFactory
                                           (newThread [_ r] (doto (Thread. r "giraffe-preview") (.setDaemon true)))))
                                  (.setRemoveOnCancelPolicy true))
               token (Object.)]
           (reset! state {:status :starting :running? true :token token :executor executor
                          :preview-executor preview-executor
                          :config config :pending {} :errors {} :identities {}
                          :inventory-confirmed-ids nil :last-inventory-sync nil
                          :discussed {} :seen-turns #{}
                          :refresh-items #{} :refresh-run-id nil
                          :refresh-running? false :refresh-requested? false :last-refresh nil
                          :unknown-turn-item-count 0 :ambiguous-turn-item-count 0
                          :draining-previous? (boolean
                                               (some #(and % (not (.isTerminated ^ScheduledThreadPoolExecutor %)))
                                                     [old-executor old-preview]))})
           (.execute executor
                     ^Runnable
                     (fn []
                       ;; wait out the previous executors' in-flight work
                       (doseq [^ScheduledThreadPoolExecutor ex [old-executor old-preview]
                               :when ex]
                         (while (and (active? token)
                                     (not (.awaitTermination ex 5 TimeUnit/SECONDS)))))
                       (swap! state #(if (identical? token (:token %))
                                       (assoc % :draining-previous? false) %))
                       (when (active? token)
                         (try
                           (full-round! token config)
                           (when (active? token)
                             (swap! state assoc :status :running)
                             (refresh-if-behind! token)
                             (schedule-backstop! token))
                           (catch Exception e
                             (failure! token :startup e)
                             (swap! state assoc :status :failed :running? false))))))
           (listen-for-turns! token)
           {:status :starting}))))))

(defn stop!
  "Stop accepting events and cancel pending deadlines. An active reading/mint
   may finish; start! refuses another executor until that one terminates."
  []
  (locking lock
    (turn-service/remove-turn-listener! ::giraffe)
    (doseq [task (concat (keep :task (vals (:pending @state))) [(:mint-task @state) (:lease-task @state) (:turn-mint-task @state) (:backstop-task @state)])
            :when task]
      (.cancel ^ScheduledFuture task false))
    (swap! state assoc :running? false :status :stopped :pending {} :refresh-requested? false)
    (when-let [executor (:executor @state)] (.shutdown ^ScheduledThreadPoolExecutor executor))
    (when-let [executor (:preview-executor @state)] (.shutdown ^ScheduledThreadPoolExecutor executor))
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
                                                {:unavailable (.getMessage q)}))})))
        reader-health (when config
                        (try
                          (let [now-ms (System/currentTimeMillis)
                                window-ms (* 60 60 1000)]
                            ;; only configured reader seats: a maintenance pass
                            ;; (e.g. zai-5-recovery) is not a seat to stop
                            (select-keys
                             (merge
                              (queue/seat-health (queue/recent-outcomes (:store config) now-ms window-ms)
                                                 now-ms window-ms)
                              (cli/usage-health (or (:seat-pauses config) cli/usage-pauses) now-ms))
                             (:reader-seats config)))
                          (catch Exception _ nil)))]
    (merge queue-health
             {:reader-health reader-health
              :seats-stopped (vec (sort (keep (fn [[seat h]] (when (:stop-dispatch? h) seat))
                                              (or reader-health {}))))
              :status (cond (or (seq errors) (= :failed (:status last-mint))
                                (= :unavailable (:status queue-health))
                                (some :stop-dispatch? (vals (or reader-health {})))) :attention
                          (= :stopped status) (if (and executor (not (.isTerminated ^ScheduledThreadPoolExecutor executor)))
                                                :stopping :stopped)
                          :else status)
            :queue-status (:status queue-health) :pending-items (vec (keys pending))
            :errors errors :last-round last-round :mint-batch last-mint
            :discussed (:discussed @state)
            :unknown-turn-item-count (:unknown-turn-item-count @state)
            :ambiguous-turn-item-count (:ambiguous-turn-item-count @state)
            :last-failed-retry (:last-failed-retry @state)
            :failed-exhausted (:failed-exhausted @state)
            :last-retirement (:last-retirement @state)
            :last-backstop-at-ms (:last-backstop-at-ms @state)
            :last-turn-mint (:last-turn-mint @state)
            :last-refresh (:last-refresh @state)
            :last-inventory-sync (:last-inventory-sync @state)})))
