(ns futon3c.process-watchdog
  "Infrastructure watchdog for long-running CYDER processes.

   Watches a bounded set of heartbeat-capable daemons and emits
   notifications/evidence when they become missing, stale, stuck, or
   erroring. Initial scope is intentionally narrow: the multi-watcher is
   the first monitored process because a single blocked child path can
   freeze incremental substrate ingestion silently.

   Design notes:

   - Runs in its own ScheduledExecutorService so it can still observe a
     wedged watcher thread.
   - Alerts are edge-triggered: notify on problem transition, not every
     scan, and emit a low-urgency recovery notification once the problem
     clears.
   - Uses CYDER `:process/last-active` as the coarse liveness surface,
     then process-specific state fields for a sharper diagnosis."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [futon3c.cyder :as cyder]
            [futon3c.evidence.boundary :as boundary])
  (:import [java.time Duration Instant]
           [java.util.concurrent Executors ScheduledExecutorService TimeUnit]))

(def default-interval-ms 10000)

(def default-profiles
  {"multi-watcher"
   {:expect-present? true
    :min-stale-ms 30000
    :stale-factor 3
    :error-key :last-error
    :label "multi-watcher"}})

(defonce !state
  (atom {:executor nil
         :interval-ms default-interval-ms
         :profiles default-profiles
         :evidence-store nil
         :started-at nil
         :cycles-completed 0
         :last-cycle-at nil
         :last-cycle nil
         :last-error nil
         :active-alerts {}
         :recent-history []}))

(declare status)

(defn- now [] (Instant/now))

(defn- parse-instant [x]
  (cond
    (instance? Instant x) x
    (string? x) (try
                  (Instant/parse x)
                  (catch Exception _ nil))
    :else nil))

(defn- millis-since [^Instant then ^Instant current]
  (when then
    (.toMillis (Duration/between then current))))

(defn default-notify!
  [{:keys [title body urgency]
    :or {urgency "normal"}}]
  (try
    (-> (ProcessBuilder. ["notify-send" "--urgency" urgency title body])
        .start
        (.waitFor 2000 TimeUnit/MILLISECONDS))
    true
    (catch Throwable _ false)))

(defn- process-state [process-record]
  (when-let [state-fn (:process/state-fn process-record)]
    (try
      (state-fn)
      (catch Throwable t
        {:state-fn-error (.getMessage t)}))))

(defn- process-snapshot [process-id process-record]
  {:process-id process-id
   :last-active (:process/last-active process-record)
   :metadata (:process/metadata process-record)
   :state (process-state process-record)})

(defn- watcher-stale-threshold-ms [profile state]
  (let [interval-ms (long (or (:interval-ms state) 5000))
        stale-factor (long (or (:stale-factor profile) 3))
        min-stale-ms (long (or (:min-stale-ms profile) 15000))]
    (max min-stale-ms (* stale-factor interval-ms))))

(defn- watcher-problem [snapshot profile current]
  (let [state (:state snapshot)
        threshold-ms (watcher-stale-threshold-ms profile state)
        last-active (parse-instant (:last-active snapshot))
        last-active-age-ms (millis-since last-active current)
        last-progress (or (parse-instant (:last-progress-at state))
                          last-active)
        last-progress-age-ms (millis-since last-progress current)
        cycle-started-at (parse-instant (:last-cycle-started-at state))
        cycle-finished-at (parse-instant (:last-cycle-finished-at state))
        cycle-open? (and cycle-started-at
                         (or (nil? cycle-finished-at)
                             (.isBefore ^Instant cycle-finished-at ^Instant cycle-started-at)))
        cycle-age-ms (millis-since cycle-started-at current)
        cycle-n (:cycle-n state)
        event-n (:event-n state)
        last-subtask (:last-subtask state)
        running? (not= false (:running? state))
        last-error (some-> (get state (or (:error-key profile) :last-error))
                           str
                           str/trim
                           not-empty)]
    (cond
      (not running?)
      {:process-id (:process-id snapshot)
       :kind :not-running
       :severity :critical
       :message "multi-watcher reports running? false"
       :details {:cycle-n cycle-n
                 :event-n event-n}}

      last-error
      {:process-id (:process-id snapshot)
       :kind :state-error
       :severity :critical
       :message last-error
       :details {:cycle-n cycle-n
                 :event-n event-n
                 :last-subtask last-subtask}}

      (and cycle-open?
           last-progress-age-ms
           (> last-progress-age-ms threshold-ms))
      {:process-id (:process-id snapshot)
       :kind :cycle-stuck
       :severity :critical
       :message (str "no watcher progress for " last-progress-age-ms "ms")
       :details {:cycle-n cycle-n
                 :event-n event-n
                 :cycle-age-ms cycle-age-ms
                 :last-progress-age-ms last-progress-age-ms
                 :threshold-ms threshold-ms
                 :last-subtask last-subtask
                 :started-at (str cycle-started-at)
                 :last-progress-at (some-> last-progress str)}}

      (and last-active-age-ms (> last-active-age-ms threshold-ms))
      {:process-id (:process-id snapshot)
       :kind :stale
       :severity :warning
       :message (str "last-active age " last-active-age-ms "ms")
       :details {:cycle-n cycle-n
                 :event-n event-n
                 :threshold-ms threshold-ms
                 :last-subtask last-subtask
                 :last-active (some-> last-active str)}}

      :else
      nil)))

(defn- generic-problem [snapshot profile current]
  (let [state (:state snapshot)
        threshold-ms (long (or (:stale-after-ms profile) 60000))
        last-active (parse-instant (:last-active snapshot))
        last-active-age-ms (millis-since last-active current)
        last-error (some-> (get state (or (:error-key profile) :last-error))
                           str
                           str/trim
                           not-empty)]
    (cond
      last-error
      {:process-id (:process-id snapshot)
       :kind :state-error
       :severity :critical
       :message last-error
       :details {}}

      (and last-active-age-ms (> last-active-age-ms threshold-ms))
      {:process-id (:process-id snapshot)
       :kind :stale
       :severity :warning
       :message (str "last-active age " last-active-age-ms "ms")
       :details {:threshold-ms threshold-ms
                 :last-active (some-> last-active str)}}

      :else
      nil)))

(defn process-problems
  "Scan monitored processes and return any active problems."
  ([] (process-problems default-profiles (now)))
  ([profiles] (process-problems profiles (now)))
  ([profiles current]
   (->> profiles
        (keep (fn [[process-id profile]]
                (if-let [process-record (get @cyder/!processes process-id)]
                  (let [snapshot (process-snapshot process-id process-record)]
                    (case process-id
                      "multi-watcher" (watcher-problem snapshot profile current)
                      (generic-problem snapshot profile current)))
                  (when (:expect-present? profile)
                    {:process-id process-id
                     :kind :missing
                     :severity :critical
                     :message "process not registered in CYDER"
                     :details {}}))))
        vec)))

(defn- alert-key [{:keys [process-id kind]}]
  [process-id kind])

(defn- emit-evidence!
  [evidence-store event issue]
  (when evidence-store
    (boundary/append! evidence-store
                      {:subject {:ref/type :session
                                 :ref/id "process-watchdog"}
                       :type :coordination
                       :claim-type :observation
                       :author "process-watchdog"
                       :tags [:process-watchdog :infrastructure]
                       :session-id "process-watchdog"
                       :body {:event event
                              :process-id (:process-id issue)
                              :kind (name (:kind issue))
                              :severity (name (:severity issue))
                              :message (:message issue)
                              :details (:details issue)
                              :at (str (now))}})))

(defn- notify-problem!
  [notify-fn issue]
  (let [process-id (:process-id issue)
        title (str "futon3c watchdog · " process-id)
        body (str (name (:kind issue)) ": " (:message issue)
                  (when-let [subtask (:last-subtask (:details issue))]
                    (str "\nsubtask: " subtask))
                  (when-let [cycle-n (:cycle-n (:details issue))]
                    (str "\ncycle: " cycle-n)))]
    (notify-fn {:title title
                :body body
                :urgency (if (= :critical (:severity issue))
                           "critical"
                           "normal")})))

(defn- notify-recovery!
  [notify-fn process-id]
  (notify-fn {:title (str "futon3c watchdog · " process-id)
              :body "recovered"
              :urgency "low"}))

(defn run-cycle!
  [{:keys [profiles evidence-store notify-fn]}]
  (let [current (now)
        problems (process-problems profiles current)
        active (into {} (map (juxt alert-key identity)) problems)
        prev-active (:active-alerts @!state)
        new-issues (remove #(contains? prev-active (alert-key %)) problems)
        resolved-keys (set/difference (set (keys prev-active))
                                      (set (keys active)))
        cycle-result {:at (str current)
                      :problems (mapv #(select-keys % [:process-id :kind :severity :message]) problems)
                      :new-problems (mapv #(select-keys % [:process-id :kind :severity :message]) new-issues)
                      :resolved (mapv (fn [[process-id kind]]
                                        {:process-id process-id :kind kind})
                                      resolved-keys)}]
    (doseq [issue new-issues]
      (notify-problem! notify-fn issue)
      (emit-evidence! evidence-store "process-alert" issue))
    (doseq [[process-id _kind] resolved-keys]
      (notify-recovery! notify-fn process-id)
      (emit-evidence! evidence-store "process-recovery"
                      {:process-id process-id
                       :kind :recovered
                       :severity :info
                       :message "recovered"
                       :details {}}))
    (swap! !state
           (fn [s]
             (-> s
                 (update :cycles-completed inc)
                 (assoc :last-cycle-at current
                        :last-cycle cycle-result
                        :last-error nil
                        :active-alerts active)
                 (update :recent-history #(vec (take-last 20 (conj % cycle-result)))))))
    (cyder/touch! "process-watchdog")
    cycle-result))

(defn start!
  [{:keys [interval-ms profiles evidence-store notify-fn]
    :or {interval-ms default-interval-ms
         profiles default-profiles
         notify-fn default-notify!}}]
  (when-let [s @!state]
    (when (:executor s)
      (throw (ex-info "process-watchdog already running; call stop! first or use status"
                      {:running true}))))
  (let [executor (Executors/newSingleThreadScheduledExecutor
                  (reify java.util.concurrent.ThreadFactory
                    (newThread [_ r]
                      (doto (Thread. r "futon3c-process-watchdog")
                        (.setDaemon true)))))
        cycle-fn #(try
                    (run-cycle! {:profiles profiles
                                 :evidence-store evidence-store
                                 :notify-fn notify-fn})
                    (catch Throwable t
                      (swap! !state assoc :last-error (.getMessage t))))]
    (.scheduleWithFixedDelay executor cycle-fn 0 interval-ms TimeUnit/MILLISECONDS)
    (reset! !state {:executor executor
                    :interval-ms interval-ms
                    :profiles profiles
                    :evidence-store evidence-store
                    :started-at (now)
                    :cycles-completed 0
                    :last-cycle-at nil
                    :last-cycle nil
                    :last-error nil
                    :active-alerts {}
                    :recent-history []})
    (status)))

(defn stop! []
  (when-let [s @!state]
    (when-let [^ScheduledExecutorService ex (:executor s)]
      (.shutdownNow ex)
      (.awaitTermination ex 2 TimeUnit/SECONDS))
    (reset! !state {:executor nil
                    :interval-ms default-interval-ms
                    :profiles default-profiles
                    :evidence-store nil
                    :started-at nil
                    :cycles-completed 0
                    :last-cycle-at nil
                    :last-cycle nil
                    :last-error nil
                    :active-alerts {}
                    :recent-history []}))
  nil)

(defn status []
  (let [s @!state
        running? (and (some? (:executor s))
                      (not (.isShutdown ^ScheduledExecutorService (:executor s))))]
    {:running? running?
     :interval-ms (:interval-ms s)
     :started-at (some-> (:started-at s) str)
     :cycles-completed (:cycles-completed s)
     :last-cycle-at (some-> (:last-cycle-at s) str)
     :last-cycle (:last-cycle s)
     :last-error (:last-error s)
     :active-alerts (->> (:active-alerts s)
                         vals
                         (mapv #(select-keys % [:process-id :kind :severity :message])))}))

(defn tick! []
  (run-cycle! {:profiles (:profiles @!state)
               :evidence-store (:evidence-store @!state)
               :notify-fn default-notify!}))
