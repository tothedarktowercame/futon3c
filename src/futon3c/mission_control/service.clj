(ns futon3c.mission-control.service
  "Persistent mission-control sessions for Drawbridge/agent use.

   Purpose:
   - keep mission-control state hot inside the long-running dev JVM
   - provide session-style entry points (start/list/resume/step/stop)
   - avoid per-query Clojure cold-start cost
   - expose convenience review workflows for humans and agents"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.peripheral.mission-control :as mc]
            [futon3c.peripheral.mission-control-backend :as mcb]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools])
  (:import [java.time Instant]
           [java.util UUID]))

(defn- now-str [] (str (Instant/now)))

(defn- social-error?
  [x]
  (and (map? x) (contains? x :error/code)))

(defn- default-snapshot-path []
  (or (System/getenv "FUTON3C_MISSION_CONTROL_SNAPSHOT_PATH")
      (str (or (System/getenv "FUTON_STORAGE_ROOT")
               (str (System/getProperty "user.home") "/code/storage"))
           "/futon3c/mission-control/sessions.edn")))

(defonce !config
  (atom {:repos mcb/default-repo-roots
         :evidence-store nil
         :snapshot-path (default-snapshot-path)}))

(defonce !sessions (atom {}))
(defonce !session-order (atom []))
(defonce !loaded? (atom false))

(defn- make-peripheral []
  (mc/make-mission-control (tools/make-mock-backend)))

(defn- summarize-session
  [session]
  {:session-id (:session/id session)
   :author (:session/author session)
   :status (:session/status session)
   :started-at (:session/started-at session)
   :updated-at (:session/updated-at session)
   :stopped-at (:session/stopped-at session)
   :step-count (count (get-in session [:session/state :steps]))
   :latest-review (get-in session [:session/state :latest-review :portfolio/summary])})

(defn- snapshot-view
  [session]
  (-> session
      (dissoc :session/peripheral)
      (update :session/state dissoc :evidence-store)))

(defn- hydrate-state
  [state]
  (assoc (or state {}) :evidence-store (:evidence-store @!config)))

(defn- persist-snapshots! []
  (let [path (:snapshot-path @!config)
        file (io/file path)
        payload {:version 1
                 :saved-at (now-str)
                 :order @!session-order
                 :sessions (into {}
                                 (map (fn [[sid s]]
                                        [sid (snapshot-view s)]))
                                 @!sessions)}]
    (.mkdirs (.getParentFile file))
    (spit file (pr-str payload))))

(defn- load-snapshots! []
  (let [path (:snapshot-path @!config)
        file (io/file path)]
    (when (.exists file)
      (try
        (let [payload (edn/read-string (slurp file))
              sessions (:sessions payload)
              order (:order payload)
              hydrated (into {}
                            (map (fn [[sid s]]
                                   [sid (-> s
                                            (assoc :session/peripheral (make-peripheral))
                                            (update :session/state hydrate-state))]))
                            sessions)]
          (reset! !sessions hydrated)
          (reset! !session-order (vec order)))
        (catch Exception e
          (println "[mission-control] failed to load snapshots:" (.getMessage e)))))))

(defn- ensure-loaded! []
  (when (compare-and-set! !loaded? false true)
    (load-snapshots!)))

(defn configure!
  "Configure mission-control service defaults.

   opts:
   - :repos map of repo roots
   - :evidence-store evidence backend/atom used by peripheral evidence emission
   - :snapshot-path where session snapshots are persisted"
  [{:keys [repos evidence-store snapshot-path]}]
  (when snapshot-path
    (swap! !config assoc :snapshot-path snapshot-path))
  (ensure-loaded!)
  (swap! !config merge
         (cond-> {}
           repos (assoc :repos repos)
           (some? evidence-store) (assoc :evidence-store evidence-store)
           snapshot-path (assoc :snapshot-path snapshot-path)))
  ;; Keep in-memory states wired to latest evidence store.
  (swap! !sessions
         (fn [sessions]
           (into {}
                 (map (fn [[sid s]]
                        [sid (update s :session/state hydrate-state)]))
                 sessions)))
  (persist-snapshots!)
  {:ok true :config @!config})

(defn status []
  (ensure-loaded!)
  {:ok true
   :session-count (count @!sessions)
   :active-count (count (filter #(= :active (:session/status %)) (vals @!sessions)))
   :config (select-keys @!config [:repos :snapshot-path])})

(defn reset-service!
  "Reset in-memory mission-control service state.
   Intended for tests."
  []
  (reset! !sessions {})
  (reset! !session-order [])
  (reset! !loaded? false)
  {:ok true})

(defn list-sessions []
  (ensure-loaded!)
  (->> @!session-order
       (keep #(get @!sessions %))
       (mapv summarize-session)))

(defn get-session
  [session-id]
  (ensure-loaded!)
  (when-let [s (get @!sessions session-id)]
    (snapshot-view s)))

(defn start-session!
  "Start a mission-control session.

   opts:
   - :session-id optional explicit id
   - :author optional (default \"system\")
   - :repos optional override"
  ([] (start-session! {}))
  ([{:keys [session-id author repos]}]
   (ensure-loaded!)
   (let [sid (or session-id (str "mc-" (UUID/randomUUID)))
         existing (get @!sessions sid)]
     (if existing
       {:ok true
        :session-id sid
        :session (snapshot-view existing)
        :note "session already exists"}
       (let [p (make-peripheral)
             start (runner/start p {:session-id sid
                                    :author (or author "system")
                                    :repos (or repos (:repos @!config))
                                    :evidence-store (:evidence-store @!config)})]
         (if (social-error? start)
           {:ok false :error start}
           (let [session {:session/id sid
                          :session/author (or author "system")
                          :session/status :active
                          :session/peripheral p
                          :session/state (:state start)
                          :session/started-at (now-str)
                          :session/updated-at (now-str)
                          :session/stopped-at nil}]
             (swap! !sessions assoc sid session)
             (swap! !session-order
                    (fn [order]
                      (vec (cons sid (remove #{sid} order)))))
             (persist-snapshots!)
             {:ok true
              :session-id sid
              :evidence (:evidence start)
              :session (snapshot-view session)})))))))

(defn resume-session!
  "Mark an existing session as active and return it."
  [session-id]
  (ensure-loaded!)
  (if-let [s (get @!sessions session-id)]
    (let [resumed (-> s
                      (assoc :session/status :active
                             :session/updated-at (now-str))
                      (update :session/state hydrate-state))]
      (swap! !sessions assoc session-id resumed)
      (swap! !session-order
             (fn [order]
               (vec (cons session-id (remove #{session-id} order)))))
      (persist-snapshots!)
      {:ok true :session-id session-id :session (snapshot-view resumed)})
    {:ok false
     :error {:error/code :session-not-found
             :error/message (str "Unknown mission-control session: " session-id)}}))

(defn step!
  "Run one mission-control tool step in an active session.

   Examples:
     (step! \"mc-...\" :mc-inventory)
     (step! \"mc-...\" :mc-bulletin [\"note\"])"
  ([session-id tool] (step! session-id tool []))
  ([session-id tool args]
   (ensure-loaded!)
   (if-let [{:keys [session/peripheral session/state] :as session}
            (get @!sessions session-id)]
     (if (not= :active (:session/status session))
       {:ok false
        :error {:error/code :session-not-active
                :error/message (str "Session is not active: " session-id)}}
       (let [step-result (runner/step peripheral state {:tool tool :args (vec args)})]
         (if (social-error? step-result)
           {:ok false :error step-result}
           (let [updated (-> session
                             (assoc :session/state (:state step-result)
                                    :session/updated-at (now-str)))]
             (swap! !sessions assoc session-id updated)
             (persist-snapshots!)
             {:ok true
              :session-id session-id
              :result (:result step-result)
              :evidence (:evidence step-result)
              :session (snapshot-view updated)}))))
     {:ok false
      :error {:error/code :session-not-found
              :error/message (str "Unknown mission-control session: " session-id)}})))

(defn stop-session!
  "Stop a mission-control session and emit stop evidence."
  ([session-id] (stop-session! session-id "session closed"))
  ([session-id reason]
   (ensure-loaded!)
   (if-let [{:keys [session/peripheral session/state] :as session}
            (get @!sessions session-id)]
     (let [stop-result (runner/stop peripheral state reason)]
       (if (social-error? stop-result)
         {:ok false :error stop-result}
         (let [updated (-> session
                           (assoc :session/status :stopped
                                  :session/stopped-at (now-str)
                                  :session/updated-at (now-str)
                                  :session/state (-> state
                                                     (assoc :latest-fruit (:fruit stop-result))
                                                     (assoc :stop-context (:context stop-result))))
                           (dissoc :session/peripheral))]
           ;; We store a fresh peripheral so the session can be resumed later.
           (swap! !sessions assoc session-id
                  (assoc updated :session/peripheral (make-peripheral)))
           (persist-snapshots!)
           {:ok true
            :session-id session-id
            :fruit (:fruit stop-result)
            :context (:context stop-result)
            :evidence (:evidence stop-result)
            :session (snapshot-view updated)})))
     {:ok false
      :error {:error/code :session-not-found
              :error/message (str "Unknown mission-control session: " session-id)}})))

(def review-actions
  [{:tool :mc-inventory}
   {:tool :mc-devmaps}
   {:tool :mc-coverage}
   {:tool :mc-mana}
   {:tool :mc-review}])

(defn run-review!
  "Run the standard mission-control review sequence in one session.

   opts:
   - :session-id optional existing session id (or auto-created)
   - :author optional when creating a new session
   - :close? when true, stop the session after steps (default false)"
  ([] (run-review! {}))
  ([{:keys [session-id author close?]
     :or {close? false}}]
   (ensure-loaded!)
   (let [sid (or session-id (str "mc-" (UUID/randomUUID)))
         session-result (if (get @!sessions sid)
                          (resume-session! sid)
                          (start-session! {:session-id sid :author author}))]
     (if-not (:ok session-result)
       session-result
       (loop [remaining review-actions
              last-result nil]
         (if (empty? remaining)
           (let [summary {:ok true
                          :session-id sid
                          :last-result last-result
                          :session (get-session sid)}]
             (if close?
               (let [stopped (stop-session! sid "review complete")]
                 (merge summary {:stopped stopped}))
               summary))
           (let [{:keys [tool args]} (first remaining)
                 step-result (step! sid tool args)]
             (if (:ok step-result)
               (recur (rest remaining) step-result)
               step-result))))))))
