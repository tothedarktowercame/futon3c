(ns futon3c.dev.agents
  "Agent-layer startup for futon3c.dev.

   Extracted from futon3c.dev so the top-level dev namespace can remain a
   facade/orchestrator while boot logic for Claude/Codex registration lives in
   one place."
  (:require [futon3c.agency.federation :as federation]
            [futon3c.agency.registry :as reg]
            [futon3c.cyder :as cyder]
            [futon3c.dev.config :as config]
            [futon3c.dev.peripheral-agents :as peripheral-agents]
            [futon3c.runtime.agents :as rt]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.util.concurrent TimeUnit]))

(defn- short-session
  [session-id]
  (when session-id
    (subs session-id 0 (min 8 (count session-id)))))

(defn- binary-on-path?
  [bin-name]
  (try
    (let [which (.start (ProcessBuilder. ^java.util.List ["which" bin-name]))]
      (.waitFor which 5000 TimeUnit/MILLISECONDS)
      (zero? (.exitValue which)))
    (catch Exception _
      false)))

(defn- register-claude-agent!
  [{:keys [agent-id session-file socket metadata make-claude-invoke-fn read-session-id]}]
  (let [initial-sid (read-session-id session-file)
        sid-atom (atom initial-sid)
        invoke-fn (make-claude-invoke-fn
                   {:claude-bin (config/env "CLAUDE_BIN" "claude")
                    :permission-mode (config/env "CLAUDE_PERMISSION" "bypassPermissions")
                    :agent-id agent-id
                    :session-file session-file
                    :session-id-atom sid-atom
                    :emacs-socket socket})]
    (rt/register-claude! {:agent-id agent-id
                          :invoke-fn invoke-fn})
    (reg/update-agent! agent-id
                       :agent/type :claude
                       :agent/invoke-fn invoke-fn
                       :agent/metadata metadata
                       :agent/capabilities [:explore :edit :test :coordination/execute])
    (when initial-sid
      (reg/update-agent! agent-id :agent/session-id initial-sid))
    (println (str "[dev] Claude agent registered: " agent-id " (inline invoke)"
                  (when-let [emacs-socket (:emacs-socket metadata)]
                    (str " (emacs: " emacs-socket ")"))
                  (when initial-sid
                    (str " (session: " (short-session initial-sid) ")"))))
    {:invoke-fn invoke-fn
     :session-id initial-sid}))

(defn- register-corpus-agent!
  []
  (reg/register-agent! {:agent-id "corpus-1"
                        :type :corpus
                        :invoke-fn nil
                        :capabilities []
                        :metadata {:role "corpus-bot"}})
  (println "[dev] Corpus agent registered: corpus-1 (ws-only)"))

(defn- register-inline-codex-agent!
  [{:keys [agent-id session-file metadata make-codex-invoke-fn read-session-id]}]
  (let [initial-sid (read-session-id session-file)
        sid-atom (atom initial-sid)
        invoke-fn (make-codex-invoke-fn
                   {:codex-bin (config/env "CODEX_BIN" "codex")
                    :model (config/env "CODEX_MODEL" "gpt-5-codex")
                    :sandbox (config/env "CODEX_SANDBOX" "danger-full-access")
                    :approval-policy (or (config/env "CODEX_APPROVAL_POLICY")
                                         (config/env "CODEX_APPROVAL" "never"))
                    :reasoning-effort (config/env "CODEX_REASONING_EFFORT")
                    :timeout-ms (or (config/env-int "CODEX_INVOKE_TIMEOUT_MS" 1800000) 1800000)
                    :cwd (config/configured-codex-cwd)
                    :agent-id agent-id
                    :session-file session-file
                    :session-id-atom sid-atom})]
    (rt/register-codex! {:agent-id agent-id
                         :invoke-fn invoke-fn
                         :metadata metadata})
    (reg/update-agent! agent-id
                       :agent/type :codex
                       :agent/invoke-fn invoke-fn
                       :agent/capabilities [:edit :test :coordination/execute]
                       :agent/metadata metadata)
    (when initial-sid
      (reg/update-agent! agent-id :agent/session-id initial-sid))
    (println (str "[dev] Codex agent registered: " agent-id " (inline invoke)"
                  (when initial-sid
                    (str " (session: " (short-session initial-sid) ")"))))
    {:invoke-fn invoke-fn
     :session-id initial-sid}))

(defn- codex-registration-config
  [{:keys [f3c-sys role codex-ws-bridge?]}]
  (let [ws-port (or (:port f3c-sys) (config/env-int "FUTON3C_PORT" 7070))
        peer-base (or (some-> (config/env "FUTON3C_LINODE_URL") str/trim not-empty)
                      (config/first-peer-url))
        peer-ws-base (config/normalize-ws-base peer-base)
        explicit-ws-base (some-> (config/env "FUTON3C_CODEX_WS_BASE") str/trim not-empty)
        ws-base (or explicit-ws-base
                    (when (= role :laptop) peer-ws-base)
                    (str "ws://127.0.0.1:" ws-port))
        remote-ws-target? (and (pos? (long ws-port))
                               (not (config/local-ws-target? ws-base ws-port)))
        register-http-base (or (some-> (config/env "FUTON3C_CODEX_WS_HTTP_BASE") str/trim not-empty)
                               (when remote-ws-target?
                                 (config/normalize-http-base (or peer-base ws-base))))
        evidence-replication? (config/env-bool "FUTON3C_CODEX_WS_REPLICATE_EVIDENCE"
                                               remote-ws-target?)
        replication-interval-ms (or (config/env-int "EVIDENCE_REPLICATION_INTERVAL_MS" 30000)
                                    30000)
        ws-bridge-enabled? (and codex-ws-bridge? (pos? (long ws-port)))]
    {:ws-port ws-port
     :peer-base peer-base
     :peer-ws-base peer-ws-base
     :ws-base ws-base
     :remote-ws-target? remote-ws-target?
     :register-http-base register-http-base
     :evidence-replication? evidence-replication?
     :replication-interval-ms replication-interval-ms
     :ws-bridge-enabled? ws-bridge-enabled?}))

(defn start-agents!
  [{:keys [irc-sys evidence-store f1-sys !f3c-sys !codex-ws-bridge
           start-futon3c! install-irc-auto-join! make-claude-invoke-fn
           make-codex-invoke-fn make-tickle-invoke-fn start-codex-ws-bridge!
           start-dispatch-relay! read-session-id direct-xtdb-enabled?
           make-bridge-irc-send-fn]
    :as _deps}]
  (let [role-info (config/deployment-role-info)
        role (:role role-info)
        role-cfg (config/role-defaults role)
        register-claude? (config/env-bool "FUTON3C_REGISTER_CLAUDE" (:register-claude? role-cfg))
        register-codex? (config/env-bool "FUTON3C_REGISTER_CODEX" (:register-codex? role-cfg))
        register-corpus? (config/env-bool "FUTON3C_REGISTER_CORPUS" false)
        register-vscode-codex? (config/env-bool "FUTON3C_REGISTER_VSCODE_CODEX" true)
        vscode-codex-agent-id (or (some-> (config/env "FUTON3C_VSCODE_AGENT_ID") str/trim not-empty)
                                  "codex-vscode")
        relay-claude? (config/env-bool "FUTON3C_RELAY_CLAUDE" (or register-claude? (= role :linode)))
        relay-codex? (config/env-bool "FUTON3C_RELAY_CODEX" (or register-codex? (= role :linode)))
        relay-channel (if (config/env-bool "MATH_IRC" false) "#math" "#futon")
        codex-agent-id (config/configured-codex-agent-id)
        codex-relay-nick (config/configured-codex-relay-nick)
        relay-invoke-timeout-ms (or (config/env-int "FUTON3C_RELAY_INVOKE_TIMEOUT_MS" 600000) 600000)
        relay-invoke-hard-timeout-ms (or (config/env-int "FUTON3C_RELAY_INVOKE_HARD_TIMEOUT_MS" 1800000) 1800000)
        codex-ws-bridge? (config/env-bool "FUTON3C_CODEX_WS_BRIDGE" (= role :laptop))
        codex-remote-origin (or (some-> (config/env "FUTON3C_CODEX_REMOTE_BASE") config/normalize-http-base)
                                (some-> (config/env "FUTON3C_LAPTOP_URL") config/normalize-http-base)
                                (some-> (config/env "FUTON3C_IRC_SEND_BASE") config/normalize-http-base)
                                (some-> (config/first-peer-url) config/normalize-http-base))
        irc-send-base-hint (or (some-> (config/env "FUTON3C_IRC_SEND_BASE") config/normalize-http-base)
                               (some-> (config/env "FUTON3C_LINODE_URL") config/normalize-http-base)
                               (some-> (config/first-peer-url) config/normalize-http-base))
        direct-xtdb? (direct-xtdb-enabled? role-cfg)
        f3c-sys (start-futon3c!
                 {:xtdb-node (when direct-xtdb? (:node f1-sys))
                  :evidence-store evidence-store
                  :irc-send-base irc-send-base-hint
                  :irc-send-fn (or (when irc-sys
                                     (:send-to-channel! (:server irc-sys)))
                                   (make-bridge-irc-send-fn))
                  :irc-interceptor (when irc-sys
                                     (:irc-interceptor (:relay-bridge irc-sys)))})]
    (reset! !f3c-sys f3c-sys)
    (when f3c-sys
      (cyder/register!
       {:id "futon3c-http"
        :type :server
        :stop-fn (or (:server f3c-sys) (fn []))
        :state-fn #(let [s @!f3c-sys]
                     {:port (:port s)
                      :ws-connections (count (some-> s :ws-connections deref))})}))
    (when (and irc-sys (:ws-connections f3c-sys))
      (install-irc-auto-join!
       (:ws-connections f3c-sys)
       (:relay-bridge irc-sys)
       (:server irc-sys)))
    (when register-claude?
      (register-claude-agent!
       {:agent-id "claude-1"
        :session-file (io/file (or (config/env "CLAUDE_SESSION_FILE")
                                   "/tmp/futon-session-id"))
        :socket (or (config/env "CLAUDE_EMACS_SOCKET")
                    (config/env "FUTON3C_EMACS_SOCKET"))
        :metadata (cond-> {}
                    (or (config/env "CLAUDE_EMACS_SOCKET")
                        (config/env "FUTON3C_EMACS_SOCKET"))
                    (assoc :emacs-socket (or (config/env "CLAUDE_EMACS_SOCKET")
                                             (config/env "FUTON3C_EMACS_SOCKET"))))
        :make-claude-invoke-fn make-claude-invoke-fn
        :read-session-id read-session-id}))
    (when (config/env-bool "FUTON3C_REGISTER_CLAUDE2" (:register-claude2? role-cfg))
      (peripheral-agents/register-mentor-claude-agent!
       {:agent-id "claude-2"
        :session-file (io/file (or (config/env "CLAUDE2_SESSION_FILE")
                                   "/tmp/futon-session-id-claude-2"))
        :socket (or (config/env "CLAUDE2_EMACS_SOCKET") "workspace2")
        :metadata {:role "mentor"
                   :emacs-socket (or (config/env "CLAUDE2_EMACS_SOCKET") "workspace2")}
        :make-claude-invoke-fn make-claude-invoke-fn
        :read-session-id read-session-id}))
    (when register-corpus?
      (register-corpus-agent!))
    (peripheral-agents/register-tickle-agent!
     {:make-claude-invoke-fn make-claude-invoke-fn
      :make-tickle-invoke-fn make-tickle-invoke-fn
      :read-session-id read-session-id})
    (let [codex-bin-name (config/env "CODEX_BIN" "codex")
          codex-bin-exists? (binary-on-path? codex-bin-name)
          register-codex? (if (and register-codex? (not codex-bin-exists?))
                            (do
                              (println (str "[dev][WARN] FUTON3C_REGISTER_CODEX=true but `"
                                            codex-bin-name "` not found on PATH."))
                              (println "[dev][WARN] Falling back to relay mode. Remove FUTON3C_REGISTER_CODEX override.")
                              false)
                            register-codex?)
          register-vscode-codex? (if (and register-vscode-codex? (not codex-bin-exists?))
                                   (do
                                     (println (str "[dev][WARN] FUTON3C_REGISTER_VSCODE_CODEX=true but `"
                                                   codex-bin-name "` not found on PATH."))
                                     (println "[dev][WARN] Skipping separate VS Code codex lane registration."))
                                   register-vscode-codex?)]
      (when register-codex?
        (let [session-file (io/file (or (config/env "CODEX_SESSION_FILE")
                                        "/tmp/futon-codex-session-id"))
              initial-sid (read-session-id session-file)
              sid-atom (atom initial-sid)
              invoke-fn (make-codex-invoke-fn
                         {:codex-bin (config/env "CODEX_BIN" "codex")
                          :model (config/env "CODEX_MODEL" "gpt-5-codex")
                          :sandbox (config/env "CODEX_SANDBOX" "danger-full-access")
                          :approval-policy (or (config/env "CODEX_APPROVAL_POLICY")
                                               (config/env "CODEX_APPROVAL" "never"))
                          :reasoning-effort (config/env "CODEX_REASONING_EFFORT")
                          :timeout-ms (or (config/env-int "CODEX_INVOKE_TIMEOUT_MS" 1800000) 1800000)
                          :cwd (config/configured-codex-cwd)
                          :agent-id codex-agent-id
                          :session-file session-file
                          :session-id-atom sid-atom})
              {:keys [ws-base remote-ws-target? register-http-base
                      evidence-replication? replication-interval-ms
                      ws-bridge-enabled?]}
              (codex-registration-config {:f3c-sys f3c-sys
                                          :role role
                                          :codex-ws-bridge? codex-ws-bridge?})]
          (when-let [old @!codex-ws-bridge]
            ((:stop-fn old))
            (reset! !codex-ws-bridge nil))
          (if ws-bridge-enabled?
            (let [ws-path (or (some-> (config/env "FUTON3C_CODEX_WS_PATH") str/trim not-empty)
                              "/agency/ws")
                  codex-invoke-fn (when remote-ws-target? invoke-fn)
                  codex-metadata (cond-> {:ws-bridge? true}
                                   remote-ws-target? (assoc :skip-federation-proxy? true)
                                   remote-ws-target? (assoc :ws-remote? true))
                  bridge (start-codex-ws-bridge!
                          {:agent-id codex-agent-id
                           :invoke-fn invoke-fn
                           :initial-sid initial-sid
                           :session-file session-file
                           :ws-base ws-base
                           :ws-path ws-path
                           :register-http-base register-http-base
                           :evidence-store evidence-store
                           :evidence-replication? evidence-replication?
                           :replication-interval-ms replication-interval-ms})]
              (rt/register-codex! {:agent-id codex-agent-id
                                   :invoke-fn codex-invoke-fn
                                   :metadata codex-metadata})
              (reg/update-agent! codex-agent-id
                                 :agent/type :codex
                                 :agent/invoke-fn codex-invoke-fn
                                 :agent/capabilities [:edit :test :coordination/execute]
                                 :agent/metadata codex-metadata)
              (when initial-sid
                (reg/update-agent! codex-agent-id :agent/session-id initial-sid))
              (reset! !codex-ws-bridge bridge)
              (println (str "[dev] Codex agent registered: " codex-agent-id " (ws-bridge mode"
                            (when remote-ws-target? ", remote")
                            ")"
                            (when initial-sid
                              (str " (session: " (short-session initial-sid) ")")))))
            (do
              (when codex-ws-bridge?
                (println "[dev] codex ws bridge requested but FUTON3C_PORT is disabled; falling back to inline invoke"))
              (rt/register-codex! {:agent-id codex-agent-id
                                   :invoke-fn invoke-fn})
              (reg/update-agent! codex-agent-id
                                 :agent/type :codex
                                 :agent/invoke-fn invoke-fn
                                 :agent/capabilities [:edit :test :coordination/execute])
              (when initial-sid
                (reg/update-agent! codex-agent-id :agent/session-id initial-sid))
              (println (str "[dev] Codex agent registered: " codex-agent-id " (inline invoke)"
                            (when initial-sid
                              (str " (session: " (short-session initial-sid) ")"))))))))
      (when (and register-vscode-codex?
                 (or (not register-codex?)
                     (not= vscode-codex-agent-id codex-agent-id)))
        (register-inline-codex-agent!
         {:agent-id vscode-codex-agent-id
          :session-file (io/file (or (config/env "FUTON3C_VSCODE_CODEX_SESSION_FILE")
                                     "/tmp/futon-vscode-codex-session-id"))
          :metadata {:surface "VS Code"
                     :lane "vscode"}
          :make-codex-invoke-fn make-codex-invoke-fn
          :read-session-id read-session-id}))
      (when (and (not register-codex?) relay-codex?)
        (let [proxy-invoke-fn (when codex-remote-origin
                                (federation/make-proxy-invoke-fn codex-remote-origin codex-agent-id))
              note (if codex-remote-origin
                     (str "Remote proxy origin configured: " codex-remote-origin)
                     "Awaiting WS bridge from laptop; set FUTON3C_LAPTOP_URL, FUTON3C_IRC_SEND_BASE, FUTON3C_CODEX_REMOTE_BASE, or FUTON3C_PEERS for HTTP proxy fallback")
              metadata (cond-> {:remote? true
                                :note note}
                         codex-remote-origin
                         (assoc :origin-url codex-remote-origin
                                :remote-proxy? true))]
          (rt/register-codex! {:agent-id codex-agent-id
                               :invoke-fn proxy-invoke-fn
                               :metadata metadata})
          (when-not proxy-invoke-fn
            (println "[dev][warn] codex relay has no proxy origin; IRC invokes will fail until laptop WS bridge connects.")
            (println "[dev][warn] set FUTON3C_LAPTOP_URL (or FUTON3C_IRC_SEND_BASE / FUTON3C_CODEX_REMOTE_BASE / FUTON3C_PEERS)."))
          (println (str "[dev] Codex agent registered: " codex-agent-id " (remote peer"
                        (if proxy-invoke-fn
                          (str ", proxy invoke via " codex-remote-origin)
                          ", no local invoke")
                        ")")))))
    (when (and irc-sys relay-claude?)
      (start-dispatch-relay!
       {:relay-bridge (:relay-bridge irc-sys)
        :irc-server (:server irc-sys)
        :agent-id "claude-1"
        :nick "claude"
        :channel relay-channel
        :invoke-timeout-ms relay-invoke-timeout-ms
        :invoke-hard-timeout-ms relay-invoke-hard-timeout-ms}))
    (when (and irc-sys relay-codex?)
      (start-dispatch-relay!
       {:relay-bridge (:relay-bridge irc-sys)
        :irc-server (:server irc-sys)
        :agent-id codex-agent-id
        :nick codex-relay-nick
        :channel relay-channel
        :invoke-timeout-ms relay-invoke-timeout-ms
        :invoke-hard-timeout-ms relay-invoke-hard-timeout-ms}))
    (println "[dev] Agent layer started.")))
