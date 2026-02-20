(ns futon3c.dev
  "Dev server: boots futon1a (XTDB), futon3c (HTTP+WS), IRC, and Drawbridge.

   Claude is registered at startup with a real invoke-fn that calls `claude -p`.
   Both Emacs chat and IRC route through the registry's invoke-fn — one agent,
   one session, serialized invocation.

   Environment variables:
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
     CLAUDE_SESSION_FILE — path to session ID file (default: /tmp/futon-session-id)"
  (:require [futon1a.system :as f1]
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
  [evidence-store]
  (let [irc-port (env-int "FUTON3C_IRC_PORT" 6667)]
    (when (pos? irc-port)
      (let [bind-host (env "FUTON3C_BIND_HOST" "0.0.0.0")
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
         :port irc-port}))))

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

(defn make-claude-invoke-fn
  "Create an invoke-fn that calls `claude -p` for real Claude interaction.

   invoke-fn contract: (fn [prompt session-id] -> {:result str :session-id str})

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
                            "--permission-mode" permission-mode]
                     session-id (into ["--resume" (str session-id)])
                     new-sid    (into ["--session-id" new-sid]))
              used-sid (or session-id new-sid)
              _ (println (str "[invoke] claude -p " (subs (pr-str prompt-str) 0
                                                          (min 60 (count (pr-str prompt-str))))
                              "... (session: " (when used-sid (subs used-sid 0 8)) ")"))
              _ (flush)
              {:keys [exit out err]} (apply shell/sh args)]
          (println (str "[invoke] exit=" exit " out-len=" (count (or out ""))
                        " err-len=" (count (or err ""))))
          (flush)
          (if (zero? exit)
            {:result (str/trim (or out "")) :session-id used-sid}
            {:result nil :session-id used-sid
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
  (let [f1-sys (start-futon1a!)
        evidence-store (xb/make-xtdb-backend (:node f1-sys))
        _ (mcs/configure! {:evidence-store evidence-store})
        ;; IRC relay bridge + server (before futon3c so interceptor is ready)
        irc-sys (start-irc! evidence-store)
        ;; futon3c HTTP + WS (with IRC interceptor if IRC is running)
        f3c-sys (start-futon3c!
                 {:xtdb-node (:node f1-sys)
                  :irc-interceptor (when irc-sys
                                     (:irc-interceptor (:relay-bridge irc-sys)))})
        ;; Auto-join agents to #futon when they complete WS handshake
        _ (when (and irc-sys (:ws-connections f3c-sys))
            (install-irc-auto-join!
             (:ws-connections f3c-sys)
             (:relay-bridge irc-sys)
             (:server irc-sys)))
        ;; Register Claude agent with real invoke-fn
        _ (let [invoke-fn (make-claude-invoke-fn
                           {:claude-bin (or (env "CLAUDE_BIN") "claude")})
                session-file (io/file (or (env "CLAUDE_SESSION_FILE")
                                          "/tmp/futon-session-id"))
                initial-sid (when (.exists session-file)
                              (let [s (str/trim (slurp session-file))]
                                (when-not (str/blank? s) s)))]
            (rt/register-claude! {:agent-id "claude-1"
                                  :invoke-fn invoke-fn})
            (when initial-sid
              (reg/update-agent! "claude-1" :agent/session-id initial-sid))
            (println (str "[dev] Claude agent registered: claude-1"
                          (when initial-sid
                            (str " (session: " (subs initial-sid 0
                                                     (min 8 (count initial-sid))) ")")))))
        ;; Dispatch relay: routes IRC messages through invoke-agent!
        dispatch-relay (when irc-sys
                         (start-dispatch-relay!
                          {:relay-bridge (:relay-bridge irc-sys)
                           :irc-server (:server irc-sys)}))
        bridge-sys (start-drawbridge!)
        ;; Federation: configure peers and install announcement hook
        _ (federation/configure-from-env!)
        _ (federation/install-hook!)
        fed-peers (federation/peers)
        fed-self (federation/self-url)]
    (println)
    (println "[dev] Evidence API (futon3c transport → XTDB backend)")
    (println "[dev]   POST /api/alpha/invoke             — invoke registered agent")
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
    (println)
    (println "[dev] Register more agents via REPL:")
    (println "[dev]   (require '[futon3c.runtime.agents :as rt])")
    (println "[dev]   (rt/register-codex!  {:agent-id \"codex-1\"  :invoke-fn ...})")
    (println "[dev]   (rt/register-tickle! {:agent-id \"tickle-1\" :invoke-fn ...})")
    (println)
    (println "[dev] Mission control service (Drawbridge, no cold-start per query)")
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
        (when-let [stop (:stop bridge-sys)] (stop))
        (when-let [stop (:stop-fn (:server irc-sys))] (stop))
        (when-let [stop (:server f3c-sys)] (stop))
        ((:stop! f1-sys))
        (println "[dev] Stopped."))))

    ;; Block forever
    @(promise)))
