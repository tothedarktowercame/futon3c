(ns futon3c.dev
  "Dev server: boots futon1a (XTDB), futon3c (HTTP+WS), IRC, and Drawbridge.

   Claude and Codex are registered at startup with real invoke-fns backed by
   their CLIs. Transports route through the same registry invoke path.

   Environment variables:
     FUTON3C_ROLE       — deployment role (linode|laptop|default)
     FUTON1A_PORT       — HTTP port for futon1a (default 7071)
     FUTON1A_DATA_DIR   — XTDB storage directory
     FUTON3C_PORT       — futon3c transport HTTP+WS port (default 7070, 0 = disable)
     FUTON3C_IRC_PORT   — IRC server port (default 6667, 0 = disable)
     FUTON3C_BIND_HOST  — bind address for IRC server (default 0.0.0.0)
     FUTON3C_DRAWBRIDGE_PORT  — Drawbridge HTTP port (default 6768, 0 = disable)
     FUTON3C_DRAWBRIDGE_BIND  — Drawbridge bind interface (default 127.0.0.1)
     FUTON3C_DRAWBRIDGE_ALLOW — Drawbridge allowlist CSV (default 127.0.0.1,::1)
     FUTON3C_ADMIN_TOKEN / ADMIN_TOKEN / .admintoken — Drawbridge auth token
     FUTON3C_PATTERNS   — comma-separated pattern IDs (default: none)
     FUTON3C_PEERS      — comma-separated peer Agency URLs for federation
     FUTON3C_SELF_URL   — this Agency's externally reachable URL
     CLAUDE_BIN         — path to claude CLI binary (default: claude)
     CLAUDE_SESSION_FILE — path to session ID file (default: /tmp/futon-session-id)
     CODEX_BIN          — path to codex CLI binary (default: codex)
     CODEX_MODEL        — codex model (default: gpt-5-codex)
     CODEX_SANDBOX      — codex sandbox (default: workspace-write)
     CODEX_APPROVAL_POLICY / CODEX_APPROVAL
                        — codex approval policy (default: never)
     CODEX_SESSION_FILE — path to codex session ID file (default: /tmp/futon-codex-session-id)
     FUTON3C_REGISTER_CLAUDE — whether to register claude-1 on this host
     FUTON3C_REGISTER_CODEX  — whether to register codex-1 on this host"
  (:require [futon1a.system :as f1]
            [futon3c.agents.codex-cli :as codex-cli]
            [futon3c.agents.tickle :as tickle]
            [futon3c.blackboard :as bb]
            [futon3c.evidence.xtdb-backend :as xb]
            [futon3c.mission-control.service :as mcs]
            [futon3c.agency.federation :as federation]
            [futon3c.agency.registry :as reg]
            [futon3c.runtime.agents :as rt]
            [futon3c.transport.http :as http]
            [futon3c.transport.irc :as irc]
            [repl.http :as drawbridge]
            [cheshire.core :as json]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [org.httpkit.server :as hk])
  (:import [java.time Instant]
           [java.util UUID]))

(defn env
  "Read an env var with optional default."
  ([k] (System/getenv k))
  ([k default] (or (System/getenv k) default)))

(defn env-int [k default]
  (if-let [s (env k)]
    (parse-long s)
    default))

(defn read-admin-token []
  (or (some-> (env "FUTON3C_ADMIN_TOKEN") str/trim not-empty)
      (some-> (env "ADMIN_TOKEN") str/trim not-empty)
      (let [f (io/file ".admintoken")]
        (when (.exists f)
          (some-> (slurp f) str/trim not-empty)))
      "change-me"))

(defn env-list [k default]
  (if-let [s (env k)]
    (->> (str/split s #",")
         (map str/trim)
         (remove str/blank?)
         vec)
    default))

(defn env-bool
  "Read a boolean env var. Returns DEFAULT when unset.
   Falsey values: 0,false,no,off (case-insensitive)."
  [k default]
  (if-let [s (env k)]
    (let [v (-> s str str/trim str/lower-case)]
      (not (contains? #{"0" "false" "no" "off"} v)))
    default))

(defn deployment-role
  "Resolve deployment role from FUTON3C_ROLE.
   Supported: linode, laptop. Anything else => :default."
  []
  (case (some-> (env "FUTON3C_ROLE") str/trim str/lower-case)
    "linode" :linode
    "laptop" :laptop
    :default))

(defn role-defaults
  "Role-driven defaults. Explicit env vars still override."
  [role]
  (case role
    :linode {:irc-port 6667
             :irc-bind-host "0.0.0.0"
             :register-claude? true
             :register-codex? false}
    :laptop {:irc-port 0
             :irc-bind-host "127.0.0.1"
             :register-claude? false
             :register-codex? true}
    ;; Legacy behavior when role is not set.
    {:irc-port 6667
     :irc-bind-host "0.0.0.0"
     :register-claude? true
     :register-codex? true}))

(defn- read-session-id
  [f]
  (when (.exists f)
    (let [s (str/trim (slurp f))]
      (when-not (str/blank? s) s))))

(defn- persist-session-id!
  [f sid]
  (when (and sid (not (str/blank? sid)))
    (try (spit f sid)
         (catch Exception e
           (println (str "[dev] session-id persist warning: " (.getMessage e)))))))

;; =============================================================================
;; Runtime atoms — populated by -main, accessible from Drawbridge REPL
;; =============================================================================

(defonce !f1-sys (atom nil))
(defonce !evidence-store (atom nil))
(defonce !irc-sys (atom nil))
(defonce !f3c-sys (atom nil))
(defonce !tickle (atom nil))

;; =============================================================================
;; REPL helpers — use from Drawbridge or nREPL after boot
;; =============================================================================

(defn start-tickle!
  "Start the Tickle watchdog. Pages stalled agents via IRC (which triggers
   the dispatch relay → invoke-agent!).

   Options:
     :interval-ms       — scan interval (default 60000 = 1 min)
     :threshold-seconds — stale after this many seconds (default 300 = 5 min)
     :room              — IRC room for nudges (default \"#futon\")

   Returns the watchdog handle, or nil if IRC is not running."
  ([] (start-tickle! {}))
  ([opts]
   (when-let [irc-sys @!irc-sys]
     (when-let [evidence-store @!evidence-store]
       (when-let [old @!tickle]
         (println "[dev] Stopping previous tickle watchdog...")
         ((:stop-fn old)))
       (let [send-fn (:send-to-channel! (:server irc-sys))
             config {:evidence-store evidence-store
                     :interval-ms (or (:interval-ms opts) 60000)
                     :threshold-seconds (or (:threshold-seconds opts) 300)
                     :page-config {;; Skip bell (always returns "paged" without actually
                                   ;; reaching the agent). Go straight to IRC, where the
                                   ;; dispatch relay invokes the agent for real.
                                   :ring-test-bell! (constantly {:ok false :error :skip-to-irc})
                                   :send-to-channel! send-fn
                                   :room (or (:room opts) "#futon")}
                     :escalate-config {:notify-fn
                                       (fn [agent-id reason]
                                         ;; Escalate to Joe via blackboard
                                         (bb/blackboard!
                                          "*Tickle*"
                                          (str "ESCALATION\n"
                                               "Agent: " agent-id "\n"
                                               "Reason: " reason "\n"
                                               "Time: " (Instant/now))))}
                     :on-cycle (fn [{:keys [stalled paged]}]
                                 (when (seq stalled)
                                   (println (str "[tickle] stalled: " stalled
                                                 " paged: " paged))))}
             handle (tickle/start-watchdog! config)]
         (reset! !tickle handle)
         (println (str "[dev] Tickle started: interval="
                       (or (:interval-ms opts) 60000) "ms"
                       " threshold=" (or (:threshold-seconds opts) 300) "s"))
         handle)))))

(defn stop-tickle!
  "Stop the Tickle watchdog."
  []
  (when-let [handle @!tickle]
    ((:stop-fn handle))
    (reset! !tickle nil)
    (println "[dev] Tickle stopped.")))

(defn status
  "Quick runtime status for the REPL."
  []
  {:agents (reg/registered-agents)
   :tickle (when @!tickle {:running true :started-at (:started-at @!tickle)})
   :irc (when @!irc-sys {:port (:port @!irc-sys)})
   :evidence-count (when @!evidence-store
                     (count (futon3c.evidence.store/query* @!evidence-store {})))})

;; =============================================================================
;; System boot
;; =============================================================================

(defn start-futon1a!
  "Start futon1a (XTDB + HTTP). Returns system map with :node, :store, :stop!, etc."
  []
  (let [port (env-int "FUTON1A_PORT" 7071)
        data-dir (env "FUTON1A_DATA_DIR"
                      (str (System/getProperty "user.home")
                           "/code/storage/futon1a/default"))]
    (println (str "[dev] Starting futon1a (XTDB: " data-dir ")..."))
    (let [sys (f1/start! {:data-dir data-dir :port port})]
      (println (str "[dev] futon1a: http://localhost:" (:http/port sys)))
      sys)))

(defn start-futon3c!
  "Start futon3c transport HTTP+WS. Returns system map or nil if disabled.

   opts:
     :xtdb-node        — XTDB node for persistent peripheral config
     :irc-interceptor  — (fn [ch conn parsed]) for IRC relay (optional)"
  [{:keys [xtdb-node irc-interceptor]}]
  (let [port (env-int "FUTON3C_PORT" 7070)]
    (when (pos? port)
      (let [pattern-ids (if-let [s (env "FUTON3C_PATTERNS")]
                          (mapv keyword (remove empty? (.split s ",")))
                          [])
            opts {:patterns {:patterns/ids pattern-ids}
                  :xtdb-node xtdb-node}
            http-handler (rt/make-http-handler opts)
            ws-opts (cond-> opts
                      irc-interceptor (assoc :irc-interceptor irc-interceptor))
            {:keys [handler connections]} (rt/make-ws-handler ws-opts)
            app (fn [request]
                  (if (:websocket? request)
                    (handler request)
                    (http-handler request)))
            result (http/start-server! app port)]
        (println (str "[dev] futon3c: http://localhost:" (:port result)
                      " (patterns: " (if (seq pattern-ids) pattern-ids "none") ")"))
        (assoc result :ws-connections connections)))))

(defn start-irc!
  "Start IRC server + WS relay bridge. Returns system map or nil if disabled.

   The relay bridge connects IRC ↔ WS agents. When a human sends a PRIVMSG,
   it's relayed to all WS-connected agents in that channel. When an agent
   sends an irc_response WS frame, it's broadcast back to the IRC channel.
   Agents are auto-joined to #futon when they connect via WS."
  ([evidence-store]
   (start-irc! evidence-store (deployment-role)))
  ([evidence-store role]
  (let [{:keys [irc-port irc-bind-host]} (role-defaults role)
        irc-port (env-int "FUTON3C_IRC_PORT" irc-port)]
    (when (pos? irc-port)
      (let [bind-host (env "FUTON3C_BIND_HOST" irc-bind-host)
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

(defn- install-irc-auto-join!
  "Watch WS connections atom; auto-join agents to #futon when they connect."
  [ws-connections relay-bridge irc-server]
  (add-watch ws-connections :irc-auto-join
    (fn [_ _ old-conns new-conns]
      (doseq [[ch conn] new-conns
              :when (and (:connected? conn)
                         (not (:connected? (get old-conns ch))))]
        (let [agent-id (:agent-id conn)
              send-fn (fn [msg] (hk/send! ch msg))]
          (try
            ((:join-agent! relay-bridge) agent-id agent-id "#futon" send-fn)
            ((:join-virtual-nick! irc-server) "#futon" agent-id)
            (catch Exception e
              (println (str "[dev] IRC auto-join failed for " agent-id ": " (.getMessage e))))))))))

;; =============================================================================
;; Claude invoke-fn — real CLI invocation via `claude -p`
;; =============================================================================

(defn- extract-text-from-json-output
  "Extract text content from `claude -p --output-format json` response.

   JSON shape: {\"type\":\"result\", \"result\":\"text here\", \"session_id\":\"uuid\", ...}
   The `result` field is a string with the text output.
   Falls back to the raw string if JSON parsing fails."
  [raw]
  (try
    (let [parsed (json/parse-string raw true)
          result (:result parsed)
          text (if (string? result) result (str result))
          session-id (:session_id parsed)]
      {:text (str/trim (or text ""))
       :session-id session-id})
    (catch Exception _
      {:text (str/trim (or raw ""))
       :session-id nil})))

(defn make-claude-invoke-fn
  "Create an invoke-fn that calls `claude -p` for real Claude interaction.

   invoke-fn contract: (fn [prompt session-id] -> {:result str :session-id str})

   Uses --output-format json to capture tool-use responses that would
   otherwise produce empty text output.

   Serialized via locking — only one `claude -p` process at a time (I-1).
   First call with nil session-id generates a new UUID via --session-id.
   Subsequent calls use --resume."
  [{:keys [claude-bin permission-mode]
    :or {claude-bin "claude" permission-mode "bypassPermissions"}}]
  (let [!lock (Object.)]
    (fn [prompt session-id]
      (locking !lock
        (let [prompt-str (cond
                           (string? prompt) prompt
                           (map? prompt)    (or (:prompt prompt) (:text prompt)
                                                (json/generate-string prompt))
                           :else            (str prompt))
              new-sid (when-not session-id (str (UUID/randomUUID)))
              args (cond-> [claude-bin "-p" prompt-str
                            "--permission-mode" permission-mode
                            "--output-format" "json"]
                     session-id (into ["--resume" (str session-id)])
                     new-sid    (into ["--session-id" new-sid]))
              used-sid (or session-id new-sid)
              _ (println (str "[invoke] claude -p " (subs (pr-str prompt-str) 0
                                                          (min 60 (count (pr-str prompt-str))))
                              "... (session: " (when used-sid (subs used-sid 0 8)) ")"))
              _ (flush)
              {:keys [exit out err]} (apply shell/sh args)
              {:keys [text session-id]} (extract-text-from-json-output (or out ""))
              ;; Prefer session-id from JSON output (canonical), fall back to our tracked one
              final-sid (or session-id used-sid)]
          (println (str "[invoke] exit=" exit " out-len=" (count (or out ""))
                        " text-len=" (count (or text ""))
                        " err-len=" (count (or err ""))))
          (flush)
          (if (zero? exit)
            {:result (if (str/blank? text)
                       "[Claude used tools but produced no text response]"
                       text)
             :session-id final-sid}
            {:result nil :session-id final-sid
             :error (str "Exit " exit ": " (str/trim (or err out "")))}))))))

;; =============================================================================
;; Dispatch-based IRC relay — routes through invoke-agent! (I-1, I-2 compliant)
;; =============================================================================

(defn start-dispatch-relay!
  "Wire IRC messages to agent dispatch via invoke-agent!.

   When an IRC message arrives, invoke the registered agent through the
   registry and send the response back to IRC. Uses future for non-blocking.

   Returns {:agent-id str :nick str} or nil if IRC is not running."
  [{:keys [relay-bridge irc-server agent-id nick]
    :or {agent-id "claude-1" nick "claude"}}]
  (when (and relay-bridge irc-server)
    ((:join-agent! relay-bridge) agent-id nick "#futon"
     (fn [data]
       (let [parsed (try (json/parse-string data true) (catch Exception _ nil))]
         (when (and parsed (= "irc_message" (:type parsed)))
           (future
             (try
               (let [resp (reg/invoke-agent! agent-id (:text parsed))]
                 (if (and (:ok resp) (string? (:result resp)))
                   (do
                     ((:send-to-channel! irc-server)
                      (or (:channel parsed) "#futon") nick (:result resp))
                     (println (str "[" nick " → " (or (:channel parsed) "#futon") "] "
                                   (subs (:result resp) 0 (min 80 (count (:result resp))))))
                     (flush))
                   (do
                     (println (str "[dev] IRC invoke failed: " (:error resp)))
                     (flush))))
               (catch Exception e
                 (println (str "[dev] IRC dispatch error: " (.getMessage e)))
                 (flush))))))))
    ((:join-virtual-nick! irc-server) "#futon" nick)
    (println (str "[dev] Dispatch relay: " nick " → invoke-agent! → #futon"))
    {:agent-id agent-id :nick nick}))

(defn start-drawbridge!
  "Start Drawbridge endpoint used by fubar/portal style tooling.
   Returns {:stop stop-fn :port p} or nil when disabled."
  []
  (let [port (env-int "FUTON3C_DRAWBRIDGE_PORT" 6768)]
    (when (pos? port)
      (let [bind (env "FUTON3C_DRAWBRIDGE_BIND" "127.0.0.1")
            allow (env-list "FUTON3C_DRAWBRIDGE_ALLOW" ["127.0.0.1" "::1"])
            token (read-admin-token)
            stop-fn (drawbridge/start! {:port port
                                        :bind bind
                                        :allow allow
                                        :token token})]
        {:stop stop-fn
         :port port
         :bind bind}))))

(defn -main [& _args]
  (let [role (deployment-role)
        role-cfg (role-defaults role)
        register-claude? (env-bool "FUTON3C_REGISTER_CLAUDE" (:register-claude? role-cfg))
        register-codex? (env-bool "FUTON3C_REGISTER_CODEX" (:register-codex? role-cfg))
        relay-claude? (env-bool "FUTON3C_RELAY_CLAUDE" (or register-claude? (= role :linode)))
        relay-codex? (env-bool "FUTON3C_RELAY_CODEX" (or register-codex? (= role :linode)))
        f1-sys (start-futon1a!)
        evidence-store (xb/make-xtdb-backend (:node f1-sys))
        _ (reset! !f1-sys f1-sys)
        _ (reset! !evidence-store evidence-store)
        _ (mcs/configure! {:evidence-store evidence-store})
        ;; IRC relay bridge + server (before futon3c so interceptor is ready)
        irc-sys (start-irc! evidence-store role)
        _ (reset! !irc-sys irc-sys)
        ;; futon3c HTTP + WS (with IRC interceptor if IRC is running)
        f3c-sys (start-futon3c!
                 {:xtdb-node (:node f1-sys)
                  :irc-interceptor (when irc-sys
                                     (:irc-interceptor (:relay-bridge irc-sys)))})
        _ (reset! !f3c-sys f3c-sys)
        ;; Auto-join agents to #futon when they complete WS handshake
        _ (when (and irc-sys (:ws-connections f3c-sys))
            (install-irc-auto-join!
             (:ws-connections f3c-sys)
             (:relay-bridge irc-sys)
             (:server irc-sys)))
        ;; Register Claude + Codex with real invoke-fns.
        ;; Each wrapper persists session-id to disk after each call so all
        ;; transports share durable continuity.
        _ (when register-claude?
            (let [raw-invoke-fn (make-claude-invoke-fn
                                 {:claude-bin (or (env "CLAUDE_BIN") "claude")})
                  session-file (io/file (or (env "CLAUDE_SESSION_FILE")
                                            "/tmp/futon-session-id"))
                  initial-sid (read-session-id session-file)
                  invoke-fn (fn [prompt session-id]
                              (let [result (raw-invoke-fn prompt session-id)
                                    sid (:session-id result)]
                                (persist-session-id! session-file sid)
                                result))]
              (rt/register-claude! {:agent-id "claude-1"
                                    :invoke-fn invoke-fn})
              (when initial-sid
                (reg/update-agent! "claude-1" :agent/session-id initial-sid))
              (println (str "[dev] Claude agent registered: claude-1"
                            (when initial-sid
                              (str " (session: " (subs initial-sid 0
                                                       (min 8 (count initial-sid))) ")"))))))
        _ (when register-codex?
            (let [raw-invoke-fn (codex-cli/make-invoke-fn
                                 {:codex-bin (or (env "CODEX_BIN") "codex")
                                  :model (or (env "CODEX_MODEL") "gpt-5-codex")
                                  :sandbox (or (env "CODEX_SANDBOX") "workspace-write")
                                  :approval-policy (or (env "CODEX_APPROVAL_POLICY")
                                                       (env "CODEX_APPROVAL")
                                                       "never")
                                  :cwd (System/getProperty "user.dir")})
                  session-file (io/file (or (env "CODEX_SESSION_FILE")
                                            "/tmp/futon-codex-session-id"))
                  initial-sid (read-session-id session-file)
                  invoke-fn (fn [prompt session-id]
                              (let [result (raw-invoke-fn prompt session-id)
                                    sid (:session-id result)]
                                (persist-session-id! session-file sid)
                                result))]
              (rt/register-codex! {:agent-id "codex-1"
                                   :invoke-fn invoke-fn})
              (when initial-sid
                (reg/update-agent! "codex-1" :agent/session-id initial-sid))
              (println (str "[dev] Codex agent registered: codex-1"
                            (when initial-sid
                              (str " (session: " (subs initial-sid 0
                                                       (min 8 (count initial-sid))) ")"))))))
        ;; Dispatch relays: route IRC messages through invoke-agent!
        _dispatch-relay-claude (when (and irc-sys relay-claude?)
                                 (start-dispatch-relay!
                                  {:relay-bridge (:relay-bridge irc-sys)
                                   :irc-server (:server irc-sys)
                                   :agent-id "claude-1"
                                   :nick "claude"}))
        _dispatch-relay-codex (when (and irc-sys relay-codex?)
                                (start-dispatch-relay!
                                 {:relay-bridge (:relay-bridge irc-sys)
                                  :irc-server (:server irc-sys)
                                  :agent-id "codex-1"
                                  :nick "codex"}))
        bridge-sys (start-drawbridge!)
        ;; Federation: configure peers and install announcement hook
        _ (federation/configure-from-env!)
        _ (federation/install-hook!)
        fed-peers (federation/peers)
        fed-self (federation/self-url)]
    (println)
    (println (str "[dev] Role: " (name role)
                  " | register-claude=" register-claude?
                  " register-codex=" register-codex?
                  " | relay-claude=" relay-claude?
                  " relay-codex=" relay-codex?))
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
    (println)
    (when irc-sys
      (println (str "[dev]   Connect IRC: irssi -c localhost -p " (:port irc-sys) " -n joe"))
      (println "[dev]   Agents auto-join #futon on WS connect")
      (println))
    (if (seq fed-peers)
      (do (println (str "[dev] Federation: self=" fed-self " peers=" fed-peers))
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
    (println "[dev]   (dev/start-tickle!)                    — start watchdog")
    (println "[dev]   (dev/start-tickle! {:interval-ms 30000}) — fast scan")
    (println "[dev]   (dev/stop-tickle!)                     — stop watchdog")
    (println "[dev]   (dev/status)                           — runtime summary")
    (println)
    (println "[dev] Mission control service:")
    (println "[dev]   (require '[futon3c.mission-control.service :as mcs])")
    (println "[dev]   (mcs/list-sessions)")
    (println "[dev]   (mcs/run-review! {:author \"joe\"})")
    (println)
    (when-let [port (env-int "FUTON3C_DRAWBRIDGE_PORT" 6768)]
      (when (pos? port)
        (println (str "[dev] drawbridge: http://"
                      (env "FUTON3C_DRAWBRIDGE_BIND" "127.0.0.1")
                      ":" port "/repl"
                      " (allow: " (env-list "FUTON3C_DRAWBRIDGE_ALLOW" ["127.0.0.1" "::1"]) ")"))))
    (println)
    (println "[dev] Press Ctrl-C to stop.")

    (.addShutdownHook
     (Runtime/getRuntime)
     (Thread.
      ^Runnable
      (fn []
        (println "\n[dev] Shutting down...")
        (stop-tickle!)
        (when-let [stop (:stop bridge-sys)] (stop))
        (when-let [stop (:stop-fn (:server irc-sys))] (stop))
        (when-let [stop (:server f3c-sys)] (stop))
        ((:stop! f1-sys))
        (println "[dev] Stopped."))))

    ;; Block forever
    @(promise)))
