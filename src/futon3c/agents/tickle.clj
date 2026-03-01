(ns futon3c.agents.tickle
  "Tickle watchdog agent — detects stalled agents and pages/escalates.

   Tickle stays in umwelt darkness: it infers liveness from shared evidence
   timestamps instead of reading private agent internals."
  (:require [clojure.string :as str]
            [futon3c.agency.registry :as reg]
            [futon3c.evidence.store :as estore]
            [futon3c.social.bells :as bells])
  (:import [java.time Duration Instant]))

(defn- parse-instant
  [x]
  (cond
    (instance? Instant x) x
    (string? x) (try
                  (Instant/parse x)
                  (catch Exception _ nil))
    :else nil))

(defn- agent-id->string
  [x]
  (cond
    (string? x) x
    (keyword? x) (name x)
    (map? x)
    (or (:id/value x)
        (get-in x [:agent/id :id/value])
        (:agent-id x))
    :else (str x)))

(defn- registry-agent-ids
  [registry-snapshot]
  (let [raw (cond
              (nil? registry-snapshot) []
              (and (map? registry-snapshot) (map? (:agents registry-snapshot)))
              (keys (:agents registry-snapshot))
              (sequential? registry-snapshot) registry-snapshot
              :else [])]
    (->> raw
         (map agent-id->string)
         (remove #(or (nil? %) (str/blank? %)))
         distinct
         vec)))

(defn- latest-activity-by-author
  [entries]
  (reduce
   (fn [acc entry]
     (let [author (:evidence/author entry)
           at (parse-instant (:evidence/at entry))]
       (if (and (string? author) at)
         (update acc author
                 (fn [prev]
                   (if (or (nil? prev) (.isAfter ^Instant at ^Instant prev))
                     at
                     prev)))
         acc)))
   {}
   entries))

(defn scan-activity
  "Check evidence store for recent activity per registered agent.
   Returns map of {agent-id {:last-active Instant :stale? bool :stale-seconds long}}.
   Queries evidence store (not agent internals) — respects Tickle's umwelt darkness."
  [evidence-store registry-snapshot threshold-seconds & {:keys [self-id]
                                                         :or {self-id "tickle-1"}}]
  (let [threshold-seconds (long (or threshold-seconds 300))
        now (Instant/now)
        entries (estore/query* evidence-store {})
        latest-by-author (latest-activity-by-author entries)
        result (into {}
                     (map (fn [agent-id]
                            (let [last-active (get latest-by-author agent-id)
                                  stale-seconds (if last-active
                                                  (max 0 (.getSeconds (Duration/between last-active now)))
                                                  Long/MAX_VALUE)
                                  stale? (or (nil? last-active)
                                             (> stale-seconds threshold-seconds))]
                              [agent-id {:last-active last-active
                                         :stale? stale?
                                         :stale-seconds stale-seconds}])))
                     (registry-agent-ids registry-snapshot))]
    (dissoc result self-id)))

(defn detect-stalls
  "Given scan-activity result, return seq of stalled agent-ids."
  [activity-map]
  (->> activity-map
       (keep (fn [[agent-id {:keys [stale?]}]]
               (when stale?
                 agent-id)))
       vec))

(defn- bell-paged?
  [result]
  (and (map? result)
       (not (:error result))
       (or (= :test-bell (:bell/type result))
           (true? (:ok result))
           (true? (:paged? result)))))

(defn page-agent!
  "Attempt to wake a stalled agent. Uses test-bell for liveness check,
   IRC message for nudge. Returns {:paged? bool :agent-id string :method :bell|:irc}."
  [agent-id config]
  (let [agent-id (agent-id->string agent-id)
        {:keys [ring-test-bell! send-to-channel! room evidence-store
                make-page-message]} config
        ring-test-bell! (or ring-test-bell! bells/ring-test-bell!)
        bell-result (try
                      (ring-test-bell! {:agent-id agent-id
                                        :evidence-store evidence-store})
                      (catch Throwable _ {:ok false :error :bell-exception}))]
    (cond
      (bell-paged? bell-result)
      {:paged? true :agent-id agent-id :method :bell}

      (fn? send-to-channel!)
      (try
        (let [msg (if (fn? make-page-message)
                    (make-page-message agent-id)
                    (str "@" agent-id " tickle paging you: no recent activity observed"))]
          (send-to-channel! (or room "#ops") "tickle-1" msg))
        {:paged? true :agent-id agent-id :method :irc}
        (catch Throwable _
          {:paged? false :agent-id agent-id :method :irc}))

      :else
      {:paged? false :agent-id agent-id :method :bell})))

(defn escalate!
  "Page failed — escalate to Joe. Returns {:escalated? bool :agent-id string}."
  [agent-id config]
  (let [agent-id (agent-id->string agent-id)
        {:keys [notify-fn]} config]
    (if-not (fn? notify-fn)
      {:escalated? false :agent-id agent-id}
      (try
        (notify-fn agent-id :page-failed)
        {:escalated? true :agent-id agent-id}
        (catch Throwable _
          {:escalated? false :agent-id agent-id})))))

(defn- emit-scan-evidence!
  [evidence-store cycle-result threshold-seconds]
  (when evidence-store
    (estore/append* evidence-store
                    {:subject {:ref/type :session
                               :ref/id "tickle/watchdog"}
                     :type :coordination
                     :claim-type :observation
                     :author "tickle-1"
                     :tags [:tickle :scan]
                     :session-id "tickle-watchdog"
                     :body (assoc cycle-result
                                  :threshold-seconds (int threshold-seconds)
                                  :cycle-at (str (Instant/now)))})))

(defn run-scan-cycle!
  "Single scan-page-escalate cycle. Pure orchestration.
   Returns {:scanned int :stalled [id ...] :paged [id ...] :escalated [id ...]}."
  [config]
  (let [threshold-seconds (long (or (:threshold-seconds config) 300))
        evidence-store (:evidence-store config)
        self-id (or (:self-id config) "tickle-1")
        registry-snapshot (or (:registry-snapshot config)
                              (reg/registered-agents))
        activity-map (scan-activity evidence-store registry-snapshot threshold-seconds
                                    :self-id self-id)
        stalled (detect-stalls activity-map)
        page-config (assoc (:page-config config) :evidence-store evidence-store)
        page-results (mapv #(page-agent! % page-config) stalled)
        paged (->> page-results
                   (filter :paged?)
                   (mapv :agent-id))
        failed-page-ids (->> page-results
                             (remove :paged?)
                             (mapv :agent-id))
        escalated-results (mapv #(escalate! % (:escalate-config config))
                                failed-page-ids)
        escalated (->> escalated-results
                       (filter :escalated?)
                       (mapv :agent-id))
        cycle-result {:scanned (count activity-map)
                      :stalled stalled
                      :paged paged
                      :escalated escalated}]
    (emit-scan-evidence! evidence-store cycle-result threshold-seconds)
    cycle-result))

(defn start-watchdog!
  "Start the Tickle watchdog loop. Runs scan cycles every interval-ms.
   Returns {:stop-fn (fn [] ...) :started-at Instant}.

   config keys:
     :evidence-store    — atom or EvidenceBackend
     :interval-ms       — scan interval (default 60000)
     :threshold-seconds — stale after this many seconds (default 300)
     :page-config       — {:ring-test-bell! fn, :send-to-channel! fn, :room string}
     :escalate-config   — {:notify-fn (fn [agent-id reason] ...)}
     :on-cycle          — optional callback (fn [cycle-result] ...) for testing"
  [config]
  (let [interval-ms (max 1 (long (or (:interval-ms config) 60000)))
        started-at (Instant/now)
        running? (atom true)
        worker
        (future
          (while @running?
            (let [cycle-result (try
                                 (run-scan-cycle! config)
                                 (catch Throwable _
                                   {:scanned 0 :stalled [] :paged [] :escalated []}))]
              (when-let [on-cycle (:on-cycle config)]
                (try
                  (on-cycle cycle-result)
                  (catch Throwable _)))
              (when @running?
                (try
                  (Thread/sleep interval-ms)
                  (catch InterruptedException _))))))]
    {:stop-fn (fn []
                (reset! running? false)
                (future-cancel worker)
                (try
                  (deref worker 1000 nil)
                  (catch java.util.concurrent.CancellationException _ nil))
                true)
     :started-at started-at}))
