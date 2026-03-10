(ns futon3c.dev.bootstrap
  "Bootstrap helpers and top-level startup orchestration for futon3c.dev."
  (:require [futon1a.system :as f1]
            [futon3c.agency.federation :as federation]
            [futon3c.agency.registry :as reg]
            [futon3c.blackboard :as bb]
            [futon3c.cyder :as cyder]
            [futon3c.dev.config :as config]
            [futon3c.mission-control.service :as mcs]
            [futon3c.peripheral.mission-control-backend :as mcb]
            [futon3c.transport.http :as http]
            [futon3c.transport.irc :as irc]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [org.httpkit.server :as hk]
            [repl.http :as drawbridge]))

(defn start-futon1a!
  "Start futon1a (XTDB + HTTP). Returns system map with :node, :store, :stop!, etc."
  [direct-xtdb?]
  (let [port (config/env-int "FUTON1A_PORT" 7071)
        data-dir (config/env "FUTON1A_DATA_DIR"
                             (str (System/getProperty "user.home")
                                  "/code/storage/futon1a/default"))
        static-dir (config/env "FUTON1A_STATIC_DIR" nil)
        allowed-penholders (->> (config/env-list "FUTON1A_ALLOWED_PENHOLDERS" ["api" "joe"])
                                (remove str/blank?)
                                set)]
    (println (str "[dev] Starting futon1a (XTDB: " data-dir ")..."))
    (let [sys (f1/start! (cond-> {:data-dir data-dir
                                  :port port
                                  :allowed-penholders allowed-penholders
                                  :expose-internals? direct-xtdb?}
                           static-dir (assoc :static-dir static-dir)))]
      (println (str "[dev] futon1a: http://localhost:" (:http/port sys)))
      (println (str "[dev] futon1a allowed penholders: "
                    (if (seq allowed-penholders)
                      (str/join "," (sort allowed-penholders))
                      "<none>")))
      (when static-dir
        (println (str "[dev] futon1a static: " static-dir)))
      sys)))

(defn start-futon5!
  "Start futon5 nonstarter heartbeat API. Returns system map or nil if disabled."
  [nonstarter-fn]
  (let [port (config/env-int "FUTON5_PORT" 7072)
        db (config/env "FUTON5_DB"
                       (str (System/getProperty "user.home")
                            "/code/futon5/data/nonstarter.db"))]
    (when (pos? port)
      (if-let [start! (nonstarter-fn 'start!)]
        (try
          (let [sys (start! {:port port :db db})]
            (println (str "[dev] futon5 heartbeat API: http://localhost:" port))
            sys)
          (catch Exception e
            (println (str "[dev] futon5 heartbeat API failed: " (.getMessage e)))
            nil))
        (do
          (println "[dev] futon5 heartbeat API disabled: nonstarter.api not on classpath")
          nil)))))

(defn start-futon3c!
  "Start futon3c transport HTTP+WS. Returns system map or nil when disabled."
  [{:keys [xtdb-node evidence-store irc-interceptor irc-send-fn irc-send-base make-http-handler make-ws-handler]}]
  (let [port (config/env-int "FUTON3C_PORT" 7070)]
    (when (pos? port)
      (let [pattern-ids (if-let [s (config/env "FUTON3C_PATTERNS")]
                          (mapv keyword (remove empty? (.split s ",")))
                          [])
            opts (cond-> {:patterns {:patterns/ids pattern-ids}
                          :irc-send-fn irc-send-fn
                          :irc-send-base irc-send-base}
                   xtdb-node (assoc :xtdb-node xtdb-node)
                   evidence-store (assoc :evidence-store evidence-store))
            http-handler (make-http-handler opts)
            ws-opts (cond-> opts
                      irc-interceptor (assoc :irc-interceptor irc-interceptor))
            {:keys [handler connections]} (make-ws-handler ws-opts)
            app (fn [request]
                  (if (:websocket? request)
                    (handler request)
                    (http-handler request)))
            result (http/start-server! app port)]
        (println (str "[dev] futon3c: http://localhost:" (:port result)
                      " (patterns: " (if (seq pattern-ids) pattern-ids "none") ")"))
        (assoc result :ws-connections connections)))))

(defn start-irc!
  "Start IRC server + WS relay bridge. Returns system map or nil when disabled."
  ([evidence-store]
   (start-irc! evidence-store (config/deployment-role)))
  ([evidence-store role]
   (let [{:keys [irc-port irc-bind-host]} (config/role-defaults role)
         irc-port (config/env-int "FUTON3C_IRC_PORT" irc-port)]
     (when (pos? irc-port)
       (let [bind-host (config/env "FUTON3C_BIND_HOST" irc-bind-host)
             relay-bridge (irc/make-relay-bridge {:evidence-store evidence-store})
             server (irc/start-irc-server!
                     {:port irc-port
                      :bind-host bind-host
                      :relay-fn (:relay-fn relay-bridge)
                      :evidence-store evidence-store})]
         ((:set-irc-send-fn! relay-bridge) (:send-to-channel! server))
         (println (str "[dev] IRC: localhost:" irc-port
                       " (channel: #futon, auto-join on WS connect)"))
         {:server server
          :relay-bridge relay-bridge
          :port irc-port})))))

(defn install-irc-auto-join!
  "Watch WS connections atom; auto-join agents to #futon when they connect."
  [ws-connections relay-bridge irc-server]
  (add-watch ws-connections :irc-auto-join
             (fn [_ _ old-conns new-conns]
               (doseq [[ch conn] new-conns
                       :when (and (:connected? conn)
                                  (not (:connected? (get old-conns ch))))]
                 (let [agent-id (:agent-id conn)
                       agent-record (reg/get-agent agent-id)
                       agent-metadata (:agent/metadata agent-record)
                       irc-auto-join? (cond
                                        (contains? agent-metadata :irc-auto-join?)
                                        (boolean (:irc-auto-join? agent-metadata))
                                        (:ws-bridge? agent-metadata)
                                        false
                                        :else true)
                       nick (let [aid (str agent-id)]
                              (or (some-> aid
                                          (str/replace #"-\d+$" "")
                                          str/trim
                                          not-empty)
                                  aid))
                       send-fn (fn [msg] (hk/send! ch msg))]
                   (when irc-auto-join?
                     (try
                       ((:join-agent! relay-bridge) agent-id nick "#futon" send-fn {:overwrite? false})
                       (when-let [part-virtual-nick! (:part-virtual-nick! irc-server)]
                         (part-virtual-nick! "#futon" (str agent-id)))
                       ((:join-virtual-nick! irc-server) "#futon" nick)
                       (catch Exception e
                         (println (str "[dev] IRC auto-join failed for " agent-id ": "
                                       (.getMessage e)))))))))))

(defn start-drawbridge!
  "Start Drawbridge endpoint used by fubar/portal style tooling."
  []
  (let [port (config/env-int "FUTON3C_DRAWBRIDGE_PORT" 6768)]
    (when (pos? port)
      (let [bind (config/env "FUTON3C_DRAWBRIDGE_BIND" "127.0.0.1")
            allow (config/env-list "FUTON3C_DRAWBRIDGE_ALLOW" ["127.0.0.1" "::1"])
            token (config/read-admin-token)
            stop-fn (drawbridge/start! {:port port
                                        :bind bind
                                        :allow allow
                                        :token token})]
        {:stop stop-fn
         :port port
         :bind bind}))))

(defn run-main!
  [{:keys [!f1-sys !evidence-store !irc-sys
           direct-xtdb-enabled? make-evidence-store
           start-futon1a! start-futon5! start-irc! start-agents!
           start-tickle! start-fm-conductor! start-drawbridge!
           start-agents-blackboard-ticker! nonstarter-fn stop-agents!
           on-agent-invoke-complete!]
    :as _deps}]
  (let [role-info (config/deployment-role-info)
        role (:role role-info)
        role-cfg (config/role-defaults role)
        _ (when-not (System/getenv "MEME_DB_PATH")
            (let [candidates ["/home/joe/code/futon3a/meme.db"
                              (str (System/getProperty "user.dir") "/meme.db")]]
              (when-let [found (first (filter #(.exists (io/file %)) candidates))]
                (alter-var-root (requiring-resolve 'meme.schema/db-path)
                                (constantly (fn [] found)))
                (println (str "  meme.db → " found " (auto-detected)")))))
        direct-xtdb? (direct-xtdb-enabled? role-cfg)
        f1-sys (start-futon1a! direct-xtdb?)
        evidence-store (make-evidence-store f1-sys direct-xtdb?)
        _ (reset! !f1-sys f1-sys)
        _ (reset! !evidence-store evidence-store)
        _ (mcs/configure! {:evidence-store evidence-store
                           :repos mcb/default-repo-roots})
        _ (cyder/register!
           {:id "futon1a"
            :type :server
            :stop-fn (:stop! f1-sys)
            :state-fn #(let [s @!f1-sys]
                         {:port (:http/port s)
                          :direct-xtdb? direct-xtdb?})})
        f5-sys (start-futon5!)
        _ (when f5-sys
            (cyder/register!
             {:id "futon5"
              :type :server
              :stop-fn (or (nonstarter-fn 'stop!) (fn []))
              :state-fn #(do {:port (config/env-int "FUTON5_PORT" 7072)})}))
        irc-sys (start-irc! evidence-store role)
        _ (reset! !irc-sys irc-sys)
        _ (when irc-sys
            (cyder/register!
             {:id "irc-server"
              :type :server
              :stop-fn (or (get-in irc-sys [:server :stop-fn])
                           (fn []))
              :state-fn #(let [s @!irc-sys]
                           {:port (:port s)
                            :relay-bridge? (boolean (:relay-bridge s))})}))
        _ (reg/set-on-invoke-complete! on-agent-invoke-complete!)
        _ (start-agents!)
        _ (when (config/env-bool "FUTON3C_TICKLE_AUTOSTART" false)
            (start-tickle! {:auto-restart? true}))
        _ (when (config/env-bool "FUTON3C_FM_CONDUCTOR_AUTOSTART" true)
            (start-fm-conductor!))
        bridge-sys (start-drawbridge!)
        _ (when bridge-sys
            (cyder/register!
             {:id "drawbridge"
              :type :server
              :stop-fn (or (:stop bridge-sys) (fn []))
              :state-fn #(do {:port (:port bridge-sys)
                              :bind (:bind bridge-sys)})}))
        _ (federation/configure-from-env!)
        _ (federation/install-hook!)
        _ (doseq [typed-id (reg/registered-agents)]
            (when-let [agent-record (reg/get-agent typed-id)]
              (federation/announce! agent-record)))
        fed-sync-results (federation/sync-peers!)
        fed-peers (federation/peers)
        fed-self (federation/self-url)
        mission-count (cyder/register-missions!)
        _ (add-watch cyder/!processes :blackboard
                     (fn [_ _ _ new-val]
                       (bb/project-processes!
                        (sort-by :process/id (vals new-val)))))
        _ (start-agents-blackboard-ticker! 5000)
        _ (bb/project-agents! (reg/registry-status))
        _ (bb/project-processes! (sort-by :process/id (vals @cyder/!processes)))]
    (println)
    (println (str "[dev] Role: " (name role)
                  " (" (:source role-info) ")"
                  " | agents: " (count (reg/registered-agents)) " registered"
                  " | CYDER: " (count (cyder/list-processes)) " processes"
                  " (" mission-count " missions)"))
    (println)
    (println "[dev] Evidence API (futon3c transport → XTDB backend)")
    (println "[dev]   POST /api/alpha/invoke             — invoke registered agent")
    (println "[dev]   POST /api/alpha/mission-control    — portfolio review, sessions, step")
    (println "[dev]   GET  /api/alpha/missions           — cross-repo mission inventory")
    (println "[dev]   GET  /api/alpha/missions/:id       — mission detail + wiring")
    (println "[dev]   GET  /api/alpha/missions/:id/wiring — per-mission wiring diagram")
    (println "[dev]   GET  /api/alpha/evidence          — query entries")
    (println "[dev]   GET  /api/alpha/evidence/:id       — single entry")
    (println "[dev]   GET  /api/alpha/evidence/:id/chain — reply chain")
    (println "[dev]   POST /api/alpha/evidence          — append entry")
    (println "[dev]   POST /api/alpha/agents            — register agent")
    (println "[dev]   GET  /api/alpha/agents            — list agents")
    (println "[dev]   GET  /api/alpha/processes         — CYDER: list processes")
    (println "[dev]   GET  /api/alpha/processes/:id     — CYDER: inspect process")
    (println "[dev]   DELETE /api/alpha/processes/:id   — CYDER: stop process")
    (println "[dev]   POST /api/alpha/portfolio/step    — AIF portfolio step")
    (println "[dev]   POST /api/alpha/portfolio/heartbeat — weekly heartbeat")
    (println "[dev]   GET  /api/alpha/portfolio/state   — portfolio belief state")
    (when f5-sys
      (println)
      (println "[dev] futon5 Heartbeat API (portfolio bid/clear persistence)")
      (println "[dev]   GET  /api/heartbeat              — current week heartbeat")
      (println "[dev]   POST /api/heartbeat/bid          — record intended actions")
      (println "[dev]   POST /api/heartbeat/clear        — record actual actions"))
    (println)
    (when irc-sys
      (println (str "[dev]   Connect IRC: irssi -c localhost -p " (:port irc-sys) " -n joe"))
      (println "[dev]   Agents auto-join #futon on WS connect")
      (println))
    (if (seq fed-peers)
      (do (println (str "[dev] Federation: self=" fed-self " peers=" fed-peers))
          (when (seq fed-sync-results)
            (println (str "[dev]   Peer sync results: " fed-sync-results)))
          (println "[dev]   Agents registered locally will be announced to peers."))
      (println "[dev] Federation: no peers configured (set FUTON3C_PEERS, FUTON3C_SELF_URL)"))
    (println)
    (println "[dev] Invoke: curl -X POST http://localhost:7070/api/alpha/invoke \\")
    (println "[dev]   -H 'Content-Type: application/json' \\")
    (println "[dev]   -d '{\"agent-id\":\"claude-1\",\"prompt\":\"hello\"}'")
    (println "[dev]   -d '{\"agent-id\":\"codex-1\",\"prompt\":\"hello\"}'")
    (println)
    (println "[dev] REPL helpers (Drawbridge or nREPL):")
    (println "[dev]   (require '[futon3c.dev :as dev])")
    (println "[dev]   (dev/restart-agents!)                  — restart WS+agents (IRC stays up)")
    (println "[dev]   (dev/stop-agents!)                     — stop WS+agents only")
    (println "[dev]   (dev/start-agents!)                    — start WS+agents only")
    (println "[dev]   (dev/start-tickle!)                    — start watchdog (auto-restart on)")
    (println "[dev]   (dev/start-tickle! {:auto-restart? false}) — watchdog without restart")
    (println "[dev]   (dev/stop-tickle!)                     — stop watchdog")
    (println "[dev]   (dev/status)                           — runtime summary")
    (println)
    (println "[dev] CT work queue (PlanetMath wiring extraction):")
    (println "[dev]   (dev/ct-progress!)                       — queue status (N/313)")
    (println "[dev]   (dev/run-ct-entry!)                      — process next entry")
    (println "[dev]   (dev/run-ct-entry! :entity-id \"pm-ct-FunctorCategory\")")
    (println "[dev]   (dev/run-ct-batch! :n 50)                — overnight batch (50 entries)")
    (println "[dev]   (dev/run-ct-batch! :n 313)               — full corpus")
    (println)
    (println "[dev] Mission control service:")
    (println "[dev]   (require '[futon3c.mission-control.service :as mcs])")
    (println "[dev]   (mcs/list-sessions)")
    (println "[dev]   (mcs/run-review! {:author \"joe\"})")
    (println)
    (when-let [port (config/env-int "FUTON3C_DRAWBRIDGE_PORT" 6768)]
      (when (pos? port)
        (println (str "[dev] drawbridge: http://"
                      (config/env "FUTON3C_DRAWBRIDGE_BIND" "127.0.0.1")
                      ":" port "/repl"
                      " (allow: " (config/env-list "FUTON3C_DRAWBRIDGE_ALLOW" ["127.0.0.1" "::1"]) ")"))))
    (println)
    (println "[dev] Press Ctrl-C to stop.")
    (.addShutdownHook
     (Runtime/getRuntime)
     (Thread.
      ^Runnable
      (fn []
        (println "\n[dev] Shutting down...")
        (remove-watch cyder/!processes :blackboard)
        (reset! cyder/!processes {})
        (stop-agents!)
        (when-let [stop (:stop bridge-sys)] (stop))
        (when-let [irc @!irc-sys]
          (when-let [stop (:stop-fn (:server irc))] (stop)))
        (when f5-sys
          (when-let [stop! (nonstarter-fn 'stop!)]
            (stop! f5-sys)))
        (when-let [f1 @!f1-sys]
          ((:stop! f1)))
        (println "[dev] Stopped."))))
    @(promise)))
