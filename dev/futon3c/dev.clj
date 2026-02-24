(ns futon3c.dev
  "Dev server: boots futon1a (XTDB), futon3c (HTTP+WS), IRC, and Drawbridge.

   Claude and Codex are registered at startup with inline invoke-fns that run
   their CLIs in-JVM. Evidence emission (start, heartbeat, complete) and
   blackboard updates are built into the invoke path. No external bridge
   scripts needed for local agents — WS bridges are for remote scenarios.

   Environment variables:
     FUTON3C_ROLE       — deployment role override (linode|laptop|default);
                          when unset, role is auto-detected from configured URLs
     FUTON1A_PORT       — HTTP port for futon1a (default 7071)
     FUTON1A_DATA_DIR   — XTDB storage directory
     FUTON1A_ALLOWED_PENHOLDERS — comma-separated penholders (default: api,joe)
     FUTON3C_DIRECT_XTDB — enable direct futon3c -> XTDB writes (default false)
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
     CODEX_SANDBOX      — codex sandbox (default: danger-full-access)
     CODEX_APPROVAL_POLICY / CODEX_APPROVAL
                        — codex approval policy (default: never)
     CODEX_INVOKE_TIMEOUT_MS — hard timeout for codex exec (default: 120000)
     CODEX_SESSION_FILE — path to codex session ID file (default: /tmp/futon-codex-session-id)
     FUTON3C_CODEX_WS_BRIDGE — enable codex WS bridge mode (default true on laptop role)
     FUTON3C_CODEX_WS_BASE   — override codex WS bridge base URL
     FUTON3C_CODEX_WS_HTTP_BASE — optional HTTP base for remote ws-bridge agent registration
     FUTON3C_CODEX_WS_PATH   — override codex WS bridge path (default /agency/ws)
     FUTON3C_CODEX_WS_REPLICATE_EVIDENCE — enable WS evidence replication (default true for remote WS target)
     EVIDENCE_REPLICATION_INTERVAL_MS — WS replication poll interval (default 30000)
     FUTON3C_REGISTER_CLAUDE — whether to register claude-1 on this host
     FUTON3C_REGISTER_CODEX  — whether to register codex-1 on this host"
  (:require [futon1a.system :as f1]
            [futon3c.agents.codex-cli :as codex-cli]
            [futon3c.agents.tickle :as tickle]
            [futon3c.agents.tickle-orchestrate :as orch]
            [futon3c.blackboard :as bb]
            [futon3c.evidence.store :as estore]
            [futon3c.evidence.xtdb-backend :as xb]
            [futon3c.mission-control.service :as mcs]
            [futon3c.agency.federation :as federation]
            [futon3c.agency.registry :as reg]
            [futon3c.runtime.agents :as rt]
            [futon3c.transport.http :as http]
            [futon3c.transport.irc :as irc]
            [futon3c.transport.ws.replication :as ws-repl]
            [repl.http :as drawbridge]
            [cheshire.core :as json]
            [clojure.java.shell :as shell]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clojure.java.io :as io]
            [org.httpkit.server :as hk])
  (:import [java.time Instant Duration]
           [java.util UUID]
           [java.net URI InetAddress NetworkInterface]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers WebSocket WebSocket$Listener]
           [java.util.concurrent CompletableFuture]))

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

(defn- first-peer-url
  "Return first configured federation peer URL, if any."
  []
  (first (env-list "FUTON3C_PEERS" [])))

(defn- normalize-ws-base
  "Normalize URL into ws:// or wss:// base URL (no trailing slash)."
  [url]
  (when-let [raw (some-> url str/trim not-empty)]
    (let [lower (str/lower-case raw)]
      (cond
        (str/starts-with? lower "ws://") (str/replace raw #"/$" "")
        (str/starts-with? lower "wss://") (str/replace raw #"/$" "")
        (str/starts-with? lower "http://") (str/replace (str "ws://" (subs raw 7)) #"/$" "")
        (str/starts-with? lower "https://") (str/replace (str "wss://" (subs raw 8)) #"/$" "")
        :else nil))))

(defn- normalize-http-base
  "Normalize URL into http:// or https:// base URL (no trailing slash)."
  [url]
  (when-let [raw (some-> url str/trim not-empty)]
    (let [lower (str/lower-case raw)]
      (cond
        (str/starts-with? lower "http://") (str/replace raw #"/$" "")
        (str/starts-with? lower "https://") (str/replace raw #"/$" "")
        (str/starts-with? lower "ws://") (str/replace (str "http://" (subs raw 5)) #"/$" "")
        (str/starts-with? lower "wss://") (str/replace (str "https://" (subs raw 6)) #"/$" "")
        :else nil))))

(defn- local-ws-target?
  "True when WS-BASE points at localhost on WS-PORT."
  [ws-base ws-port]
  (try
    (let [u (URI/create ws-base)
          host (some-> (.getHost u) str/lower-case)
          port (let [p (.getPort u)]
                 (if (neg? p)
                   (if (= "wss" (some-> (.getScheme u) str/lower-case)) 443 80)
                   p))]
      (and (#{"127.0.0.1" "localhost" "::1"} host)
           (= port (int ws-port))))
    (catch Exception _
      false)))

(defn- parse-url-host
  "Extract host from URL string; nil on parse failure."
  [url]
  (try
    (when-let [raw (some-> url str/trim not-empty)]
      (some-> (URI/create raw) .getHost))
    (catch Exception _
      nil)))

(defn- local-ip-set
  "Best-effort set of local interface IP addresses (IPv4/IPv6 textual forms)."
  []
  (try
    (->> (enumeration-seq (NetworkInterface/getNetworkInterfaces))
         (mapcat #(enumeration-seq (.getInetAddresses ^NetworkInterface %)))
         (map #(.getHostAddress ^InetAddress %))
         (remove str/blank?)
         set)
    (catch Exception _
      #{})))

(defn- resolve-host-ip-set
  "Resolve HOST to a set of IP string forms. Empty on failure."
  [host]
  (try
    (->> (InetAddress/getAllByName host)
         (map #(.getHostAddress ^InetAddress %))
         (remove str/blank?)
         set)
    (catch Exception _
      #{})))

(defn- host-local?
  "True when HOST resolves to this machine."
  [host]
  (let [h (some-> host str/lower-case str/trim)]
    (cond
      (str/blank? h) false
      (#{"localhost" "127.0.0.1" "::1"} h) true
      :else (let [local-ips (local-ip-set)
                  host-ips (resolve-host-ip-set h)]
              (boolean (seq (set/intersection local-ips host-ips)))))))

(defn deployment-role-info
  "Resolve deployment role with explicit override first, then conservative auto-detection.

   Auto rules (only when signals are clear):
   - Remote FUTON3C_LINODE_URL => :laptop
   - Remote FUTON3C_LAPTOP_URL => :linode
   - Remote FUTON3C_PEERS with no FUTON3C_SELF_URL => :laptop
   - Otherwise :default"
  []
  (let [explicit (some-> (env "FUTON3C_ROLE") str/trim str/lower-case)
        linode-host (parse-url-host (env "FUTON3C_LINODE_URL"))
        laptop-host (parse-url-host (env "FUTON3C_LAPTOP_URL"))
        self-host (parse-url-host (env "FUTON3C_SELF_URL"))
        peer-hosts (->> (env-list "FUTON3C_PEERS" [])
                        (keep parse-url-host))
        remote-peer? (some #(not (host-local? %)) peer-hosts)]
    (cond
      (= explicit "linode")
      {:role :linode :source "FUTON3C_ROLE"}

      (= explicit "laptop")
      {:role :laptop :source "FUTON3C_ROLE"}

      (= explicit "default")
      {:role :default :source "FUTON3C_ROLE"}

      (and linode-host (not (host-local? linode-host)))
      {:role :laptop :source "auto:FUTON3C_LINODE_URL"}

      (and laptop-host (not (host-local? laptop-host)))
      {:role :linode :source "auto:FUTON3C_LAPTOP_URL"}

      (and remote-peer? (nil? self-host))
      {:role :laptop :source "auto:FUTON3C_PEERS"}

      :else
      {:role :default :source "auto:default"})))

(defn deployment-role
  "Resolve effective deployment role.
   FUTON3C_ROLE overrides; otherwise falls back to auto-detection."
  []
  (:role (deployment-role-info)))

(defn role-defaults
  "Role-driven defaults. Explicit env vars still override."
  [role]
  (case role
    :linode {:irc-port 6667
             :irc-bind-host "0.0.0.0"
             :register-claude? true
             :register-codex? false
             :direct-xtdb? true}
    :laptop {:irc-port 0
             :irc-bind-host "127.0.0.1"
             :register-claude? false
             :register-codex? true}
    ;; Legacy behavior when role is not set.
    {:irc-port 6667
     :irc-bind-host "0.0.0.0"
     :register-claude? true
     :register-codex? true
     :direct-xtdb? true}))

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

(defn- session-file->file
  "Normalize session-file input into java.io.File when possible."
  [session-file]
  (cond
    (instance? java.io.File session-file) session-file
    (string? session-file) (io/file session-file)
    :else nil))

(defn- preferred-session-id
  "Pick the best session id for invoke continuity.
   When a session file is configured, treat file/incoming state as authoritative
   so clearing continuity on disk actually resets invoke behavior.
   Priority with file: session file -> incoming invoke session.
   Priority without file: incoming invoke session -> sid atom."
  [session-file incoming-session-id session-id-atom]
  (let [file-sid (some-> (session-file->file session-file) read-session-id)
        incoming (some-> incoming-session-id str str/trim not-empty)
        atom-sid (some-> session-id-atom deref str str/trim not-empty)]
    (if (session-file->file session-file)
      (or file-sid incoming)
      (or incoming atom-sid))))

(defn- start-codex-ws-bridge!
  "Start an in-process Codex WS bridge.

   Connects codex-1 to this host's /agency/ws endpoint, handles invoke frames,
   runs codex-cli invoke-fn, replies with invoke_result frames, and (optionally)
   replicates local evidence over the same WS channel.
   Returns {:stop-fn fn}."
  [{:keys [agent-id invoke-fn initial-sid session-file ws-base ws-path register-http-base
           evidence-store evidence-replication? replication-interval-ms]
    :or {agent-id "codex-1"
         ws-path "/agency/ws"}}]
  (let [sid* (atom initial-sid)
        running? (atom true)
        ws* (atom nil)
        client (HttpClient/newHttpClient)
        replication-interval-ms (long (max 1 (or replication-interval-ms 30000)))
        replication-enabled? (and evidence-replication? evidence-store)
        replication (when replication-enabled?
                      (ws-repl/start! {:evidence-store evidence-store
                                       :interval-ms replication-interval-ms}))
        register-http-base (some-> register-http-base str/trim (str/replace #"/$" ""))
        send-json! (fn [^WebSocket ws payload]
                     (.join (.sendText ws (json/generate-string payload) true)))
        request-json! (fn [method url payload]
                        (let [builder (doto (HttpRequest/newBuilder (URI/create url))
                                        (.header "Content-Type" "application/json")
                                        (.timeout (Duration/ofSeconds 10)))
                              req (case method
                                    :post (.build (.POST builder (HttpRequest$BodyPublishers/ofString payload)))
                                    :delete (.build (.DELETE builder))
                                    (.build (.GET builder)))
                              resp (.send client req (HttpResponse$BodyHandlers/ofString))]
                          {:status (.statusCode resp)
                           :body (.body resp)}))
        ensure-registered! (fn []
                             (if-not register-http-base
                               true
                               (try
                                 (let [url (str register-http-base "/api/alpha/agents")
                                       agent-url (str register-http-base "/api/alpha/agents/" agent-id)
                                       payload (json/generate-string {"agent-id" agent-id
                                                                      "type" "codex"
                                                                      "ws-bridge" true})
                                       attempt-register! #(request-json! :post url payload)
                                       {:keys [status body]} (attempt-register!)]
                                   (cond
                                     (= 201 status) true
                                     (= 409 status)
                                     (let [{del-status :status del-body :body} (request-json! :delete agent-url nil)
                                           _ (when-not (contains? #{200 404} del-status)
                                               (println (str "[dev] codex ws bridge remote delete failed: "
                                                             del-status " " del-body))
                                               (flush))
                                           {status2 :status body2 :body} (attempt-register!)]
                                       (if (or (= 201 status2) (= 409 status2))
                                         true
                                         (do
                                           (println (str "[dev] codex ws bridge registration failed after replace: "
                                                         status2 " " body2))
                                           (flush)
                                           false)))
                                     :else
                                     (do
                                       (println (str "[dev] codex ws bridge registration failed: "
                                                     status " " body))
                                       (flush)
                                       false)))
                                 (catch Exception e
                                   (println (str "[dev] codex ws bridge registration exception: "
                                                 (.getMessage e)))
                                   (flush)
                                   false))))
        ws-url (fn []
                 (str (str/replace ws-base #"/$" "")
                      ws-path
                      "?agent-id=" agent-id
                      (when-let [sid (some-> @sid* str not-empty)]
                        (str "&session-id=" sid))))
        handle-invoke! (fn [^WebSocket ws frame]
                         (let [invoke-id (:invoke_id frame)
                               prompt (:prompt frame)
                               incoming-session (:session_id frame)]
                           (when (string? invoke-id)
                             (future
                               (let [result (invoke-fn (str prompt) incoming-session)
                                     sid (:session-id result)]
                                 (when (and (string? sid) (not (str/blank? sid)))
                                   (reset! sid* sid)
                                   (persist-session-id! session-file sid))
                                 (let [payload (cond-> {"type" "invoke_result"
                                                        "invoke_id" invoke-id}
                                                 sid (assoc "session_id" sid)
                                                 (:error result) (assoc "error" (str (:error result)))
                                                 (not (:error result)) (assoc "result" (or (:result result) "")))]
                                   (try
                                     (send-json! ws payload)
                                     (catch Exception e
                                       (println (str "[dev] codex ws bridge send failed: "
                                                     (.getMessage e)))))))))))
        worker (future
                 (while @running?
                   (if-not (ensure-registered!)
                     (Thread/sleep 5000)
                     (let [closed (promise)
                           url (ws-url)]
                       (try
                         (println (str "[dev] codex ws bridge connecting: " url))
                         (flush)
                         (let [listener (reify WebSocket$Listener
                                         (onOpen [_ ws]
                                            (reset! ws* ws)
                                            (send-json! ws {"type" "ready"
                                                            "agent_id" agent-id
                                                            "session_id" (or @sid* (str "sess-" (System/currentTimeMillis)))})
                                            (when replication
                                              (try
                                                ((:reset-connection! replication))
                                                ((:set-send-fn! replication)
                                                 (fn [payload] (send-json! ws payload)))
                                                ((:poll! replication))
                                                (catch Exception e
                                                  (println (str "[dev] codex ws bridge replication init failed: "
                                                                (.getMessage e))))))
                                            (.request ws 1))
                                          (onText [_ ws data _last]
                                            (try
                                              (let [frame (json/parse-string (str data) true)]
                                                (when (= "invoke" (:type frame))
                                                  (handle-invoke! ws frame))
                                                (when replication
                                                  ((:handle-frame! replication) frame)))
                                              (catch Exception e
                                                (println (str "[dev] codex ws bridge parse failed: "
                                                              (.getMessage e)))))
                                            (.request ws 1)
                                            (CompletableFuture/completedFuture nil))
                                          (onClose [_ _ws _code _reason]
                                            (reset! ws* nil)
                                            (when replication
                                              ((:set-send-fn! replication) nil)
                                              ((:reset-connection! replication)))
                                            (deliver closed true)
                                            (CompletableFuture/completedFuture nil))
                                          (onError [_ _ws e]
                                            (reset! ws* nil)
                                            (when replication
                                              ((:set-send-fn! replication) nil)
                                              ((:reset-connection! replication)))
                                            (println (str "[dev] codex ws bridge error: " (.getMessage e)))
                                            (flush)
                                            (deliver closed true)))]
                           (.join (.buildAsync (.newWebSocketBuilder client)
                                               (URI/create url)
                                               listener))
                           (deref closed 600000 nil))
                         (catch Exception e
                           (println (str "[dev] codex ws bridge connect failed: " (.getMessage e)))
                           (flush)))))
                   (when @running?
                     (Thread/sleep 5000))))]
    {:stop-fn (fn []
                (reset! running? false)
                (when-let [ws @ws*]
                  (try
                    (.join (.sendClose ws WebSocket/NORMAL_CLOSURE "shutdown"))
                    (catch Exception _)))
                (when replication
                  ((:stop-fn replication)))
                (future-cancel worker))
     :sid* sid*}))

;; =============================================================================
;; Runtime atoms — populated by -main, accessible from Drawbridge REPL
;; =============================================================================

(defonce !f1-sys (atom nil))
(defonce !evidence-store (atom nil))
(defonce !irc-sys (atom nil))
(defonce !f3c-sys (atom nil))
(defonce !tickle (atom nil))
(defonce !codex-ws-bridge (atom nil))

(defn- direct-xtdb-enabled?
  "Whether futon3c may write evidence directly to XTDB.
   On the Linode role this defaults to true (same JVM as futon1a).
   Explicit env var FUTON3C_DIRECT_XTDB always overrides."
  [role-cfg]
  (env-bool "FUTON3C_DIRECT_XTDB" (:direct-xtdb? role-cfg false)))

(defn- make-evidence-store
  "Build the evidence store based on direct-xtdb? flag.
   When true, uses shared XTDB node (evidence persists to futon1a).
   When false, uses in-memory atom (evidence lost on restart)."
  [f1-sys direct-xtdb?]
  (if direct-xtdb?
    (do
      (println "[dev] direct XTDB path: ENABLED")
      (xb/make-xtdb-backend (:node f1-sys)))
    (do
      (println "[dev] direct XTDB path: disabled (using in-memory evidence store)")
      (atom {:entries {} :order []}))))

;; =============================================================================
;; Invoke evidence emission — writes directly to the evidence store in-JVM
;; =============================================================================

(defn- emit-invoke-evidence!
  "Emit an invoke lifecycle evidence entry. Fire-and-forget.
   Uses the configured in-process evidence store — no HTTP round-trip."
  [agent-id event-type body-map & {:keys [session-id tags]}]
  (when-let [store @!evidence-store]
    (future
      (try
        (estore/append* store
                        {:subject {:ref/type "agent" :ref/id agent-id}
                         :type :coordination
                         :claim-type :step
                         :author agent-id
                         :session-id (or session-id "dev-invoke")
                         :body (assoc body-map
                                      "event" event-type
                                      "agent-id" agent-id
                                      "at" (str (Instant/now)))
                         :tags (into ["invoke" "dev" agent-id]
                                     (or tags []))})
        (catch Exception e
          (println (str "[dev] evidence emit error: " (.getMessage e))))))))

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

;; =============================================================================
;; Tickle orchestration — REPL helpers
;; =============================================================================

(defn fetch-futon4-issues!
  "Fetch open Codex-labeled issues from futon4."
  []
  (orch/fetch-open-issues! "/home/joe/code/futon4" :label "codex"))

(defn fetch-ti-issues!
  "Fetch open tickle-integration issues from futon3c."
  []
  (orch/fetch-open-issues! "/home/joe/code/futon3c" :label "tickle-integration"))

(defn kick-all-ti-issues!
  "Kick all open tickle-integration issues to Codex sequentially.
   Usage: (dev/kick-all-ti-issues!)"
  []
  (let [{:keys [ok issues error]} (fetch-ti-issues!)]
    (if ok
      (do (println "[dev] Found" (count issues) "TI issues:" (mapv :number issues))
          (orch/kick-queue! issues
            {:evidence-store @!evidence-store
             :repo-dir "/home/joe/code/futon3c"
             :send-to-channel! (when-let [s @!irc-sys]
                                 (:send-to-channel! (:server s)))
             :room "#futon"}))
      {:ok false :error error})))

(defn kick-ev-issue!
  "Kick a single futon4 EV issue to Codex. No review, just assign and report.
   Usage: (dev/kick-ev-issue! 4)"
  [issue-number]
  (let [{:keys [ok issue error]} (orch/fetch-issue! "/home/joe/code/futon4" issue-number)]
    (if ok
      (orch/kick! issue
        {:evidence-store @!evidence-store
         :repo-dir "/home/joe/code/futon4"
         :send-to-channel! (when-let [s @!irc-sys]
                             (:send-to-channel! (:server s)))
         :room "#futon"})
      {:ok false :error error})))

(defn kick-all-ev-issues!
  "Kick all open Codex-labeled futon4 issues sequentially. No review.
   Usage: (dev/kick-all-ev-issues!)"
  []
  (let [{:keys [ok issues error]} (fetch-futon4-issues!)]
    (if ok
      (orch/kick-queue! issues
        {:evidence-store @!evidence-store
         :repo-dir "/home/joe/code/futon4"
         :send-to-channel! (when-let [s @!irc-sys]
                             (:send-to-channel! (:server s)))
         :room "#futon"})
      {:ok false :error error})))

(defn run-ev-issue!
  "Run full Tickle orchestration (assign + review) for a single futon4 EV issue.
   Usage: (dev/run-ev-issue! 1)"
  [issue-number]
  (let [{:keys [ok issue error]} (orch/fetch-issue! "/home/joe/code/futon4" issue-number)]
    (if ok
      (orch/run-issue-workflow! issue
        {:evidence-store @!evidence-store
         :repo-dir "/home/joe/code/futon4"
         :send-to-channel! (when-let [s @!irc-sys]
                             (:send-to-channel! (:server s)))
         :room "#futon"})
      {:ok false :error error})))

(defn run-all-ev-issues!
  "Run Tickle orchestration for all open Codex-labeled futon4 issues.
   Usage: (dev/run-all-ev-issues!)"
  []
  (let [{:keys [ok issues error]} (fetch-futon4-issues!)]
    (if ok
      (orch/run-batch! issues
        {:evidence-store @!evidence-store
         :repo-dir "/home/joe/code/futon4"
         :send-to-channel! (when-let [s @!irc-sys]
                             (:send-to-channel! (:server s)))
         :room "#futon"})
      {:ok false :error error})))

;; =============================================================================
;; Tickle smoke tests — preflight + definitive runs
;; =============================================================================

(defn tickle-preflight!
  "Check all prerequisites for a Tickle orchestration run.
   Returns a map of checks — all values should be truthy for a real run."
  []
  (let [agents (reg/registered-agents)
        agent-ids (set (map :id/value agents))]
    {:agents-registered (vec agent-ids)
     :codex-available?  (contains? agent-ids "codex-1")
     :claude-available? (contains? agent-ids "claude-1")
     :evidence-store?   (some? @!evidence-store)
     :irc?              (some? @!irc-sys)}))

(defn tickle-smoke!
  "Run a minimal Tickle smoke test with a synthetic issue.
   Exercises the real invoke path end-to-end: assign → evidence → report.

   Options:
     :agent-id   — agent to invoke (default \"codex-1\")
     :repo-dir   — working directory for the agent (default futon3c)
     :timeout-ms — invoke timeout (default 120000)
     :dry-run?   — if true, just show what would happen without invoking

   Usage:
     (dev/tickle-smoke!)                          ; kick codex-1
     (dev/tickle-smoke! :agent-id \"claude-1\")   ; kick claude-1
     (dev/tickle-smoke! :dry-run? true)           ; preflight only"
  [& {:keys [agent-id repo-dir timeout-ms dry-run?]
      :or {agent-id "codex-1"
           repo-dir "/home/joe/code/futon3c"
           timeout-ms 120000}}]
  (let [preflight (tickle-preflight!)
        smoke-issue {:number 0
                     :title "Tickle smoke test"
                     :body (str "## Smoke Test\n\n"
                                "Create the file `test/futon3c/agents/tickle_smoke_output.txt` "
                                "containing exactly the text `SMOKE OK`.\n\n"
                                "This is an automated test of the Tickle orchestration pipeline. "
                                "Do not modify any other files.\n\n"
                                "## Criteria\n\n"
                                "- [ ] File exists at test/futon3c/agents/tickle_smoke_output.txt\n"
                                "- [ ] File contains exactly `SMOKE OK`\n"
                                "- [ ] No other files modified")
                     :labels ["smoke-test"]}
        config {:evidence-store @!evidence-store
                :repo-dir repo-dir
                :agent-id agent-id
                :timeout-ms timeout-ms
                :send-to-channel! (when-let [s @!irc-sys]
                                    (:send-to-channel! (:server s)))
                :room "#futon"}]
    (if dry-run?
      {:dry-run true
       :preflight preflight
       :issue smoke-issue
       :config (dissoc config :evidence-store :send-to-channel!)}
      (do
        (println "[smoke] Preflight:" preflight)
        (println "[smoke] Kicking" agent-id "with smoke issue...")
        (let [result (orch/kick! smoke-issue config)
              ;; Check for the output file
              output-file (java.io.File. (str repo-dir "/test/futon3c/agents/tickle_smoke_output.txt"))
              file-ok? (and (.exists output-file)
                            (= "SMOKE OK" (clojure.string/trim (slurp output-file))))]
          (println "[smoke] Result:" (:status result)
                   (if file-ok? "— output file verified" "— output file NOT found"))
          (assoc result
                 :smoke-file-ok? file-ok?
                 :preflight preflight))))))

(defn tickle-verify!
  "Query evidence store for recent orchestration evidence.
   Useful after a smoke run to verify evidence was emitted correctly.

   Options:
     :limit — max entries to return (default 20)
     :tag   — filter by tag (default :orchestrate)"
  [& {:keys [limit tag] :or {limit 20 tag :orchestrate}}]
  (let [entries (estore/query* @!evidence-store {})
        orch-entries (->> entries
                         (filter #(some #{tag} (:evidence/tags %)))
                         (sort-by :evidence/at)
                         (take-last limit))]
    (println "[verify]" (count orch-entries) "orchestration evidence entries found")
    (doseq [e orch-entries]
      (println "  " (:evidence/at e)
               (last (:evidence/tags e))
               (select-keys (:evidence/body e) [:agent :ok :status :issue-number])))
    orch-entries))

(defn tickle-status!
  "Pretty-print the current Tickle orchestration status summary."
  []
  (let [status-fn (or (ns-resolve 'futon3c.agents.tickle-orchestrate 'tickle-status)
                      (do
                        (require 'futon3c.agents.tickle-orchestrate :reload)
                        (ns-resolve 'futon3c.agents.tickle-orchestrate 'tickle-status)))]
    (when-not status-fn
      (throw (ex-info "tickle-status helper unavailable in futon3c.agents.tickle-orchestrate"
                      {:ns 'futon3c.agents.tickle-orchestrate
                       :var 'tickle-status})))
    (let [summary (status-fn @!evidence-store)]
      (println "[dev] Tickle orchestration status:")
      (pprint/pprint summary)
      summary)))

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
  [direct-xtdb?]
  (let [port (env-int "FUTON1A_PORT" 7071)
        data-dir (env "FUTON1A_DATA_DIR"
                      (str (System/getProperty "user.home")
                           "/code/storage/futon1a/default"))
        static-dir (env "FUTON1A_STATIC_DIR" nil)
        allowed-penholders (->> (env-list "FUTON1A_ALLOWED_PENHOLDERS" ["api" "joe"])
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

(defn start-futon3c!
  "Start futon3c transport HTTP+WS. Returns system map or nil if disabled.

   opts:
     :xtdb-node        — XTDB node for persistent peripheral config
     :evidence-store   — evidence store/backend for peripheral evidence emission
     :irc-interceptor  — (fn [ch conn parsed]) for IRC relay (optional)
     :irc-send-fn      — (fn [channel from text]) for explicit IRC posts (optional)
     :irc-send-base    — remote Agency base URL hint for IRC send fallback"
  [{:keys [xtdb-node evidence-store irc-interceptor irc-send-fn irc-send-base]}]
  (let [port (env-int "FUTON3C_PORT" 7070)]
    (when (pos? port)
      (let [pattern-ids (if-let [s (env "FUTON3C_PATTERNS")]
                          (mapv keyword (remove empty? (.split s ",")))
                          [])
            opts (cond-> {:patterns {:patterns/ids pattern-ids}
                          :irc-send-fn irc-send-fn
                          :irc-send-base irc-send-base}
                   xtdb-node (assoc :xtdb-node xtdb-node)
                   evidence-store (assoc :evidence-store evidence-store))
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
            ;; Do not clobber dispatch-relay callbacks (e.g. codex/claude mention handlers)
            ;; when a WS connection reconnects.
            ((:join-agent! relay-bridge) agent-id agent-id "#futon" send-fn {:overwrite? false})
            ((:join-virtual-nick! irc-server) "#futon" agent-id)
            (catch Exception e
              (println (str "[dev] IRC auto-join failed for " agent-id ": " (.getMessage e))))))))))

;; =============================================================================
;; Claude invoke-fn — real CLI invocation via `claude -p`
;; =============================================================================

(defn- extract-text-from-assistant-message
  "Extract text content from a stream-json assistant message.
   Shape: {\"type\":\"assistant\", \"message\":{\"content\":[{\"type\":\"text\",\"text\":\"...\"}]}}"
  [parsed]
  (when-let [content (get-in parsed [:message :content])]
    (when (sequential? content)
      (->> content
           (keep (fn [block] (when (= "text" (:type block)) (:text block))))
           (str/join "\n")))))

(defn- drain-stream!
  "Read a stream to completion in background, return the content as a string.
   Prevents process blocking when stdout/stderr buffers fill up."
  [^java.io.InputStream stream]
  (slurp stream))

(defn- format-elapsed
  "Format milliseconds as human-readable elapsed time."
  [ms]
  (let [secs (quot ms 1000)
        mins (quot secs 60)
        s (mod secs 60)]
    (if (pos? mins)
      (str mins "m" s "s")
      (str secs "s"))))

(defn- detect-file-changes
  "Check data/proof-state/*.edn for recent modifications.
   Returns a string describing changes, or nil."
  [start-ms]
  (try
    (let [dir (io/file "data/proof-state")]
      (when (.isDirectory dir)
        (let [changed (->> (.listFiles dir)
                           (filter #(str/ends-with? (.getName %) ".edn"))
                           (filter #(> (.lastModified %) start-ms))
                           (mapv #(.getName %)))]
          (when (seq changed)
            (str/join ", " changed)))))
    (catch Throwable _ nil)))

(defn- start-invoke-ticker!
  "Start a background thread that updates both *agents* and the invoke buffer
   with elapsed time, file change detection, and a progress spinner.
   Also emits evidence heartbeats every 30s for long-running invocations.
   Returns a function that stops the ticker when called."
  [buf-name agent-id prompt-str used-sid interval-ms]
  (let [running (atom true)
        start-ms (System/currentTimeMillis)
        spinner-chars [\| \/ \- \\]
        tick (atom 0)
        heartbeat-interval 30000 ;; evidence heartbeat every 30s
        last-heartbeat-ms (atom start-ms)
        prompt-preview (subs prompt-str 0 (min 200 (count prompt-str)))
        aid-val (str agent-id)
        thread (Thread.
                (fn []
                  (while @running
                    (try
                      (Thread/sleep interval-ms)
                      (let [now-ms (System/currentTimeMillis)
                            elapsed (- now-ms start-ms)
                            elapsed-str (format-elapsed elapsed)
                            spin (nth spinner-chars (mod (swap! tick inc) 4))
                            changed-files (detect-file-changes start-ms)
                            ;; Read current activity from registry
                            activity (some-> (get @reg/!registry aid-val)
                                             :agent/invoke-activity)
                            content (str "Invoke: " agent-id " " spin " " elapsed-str "\n"
                                         "Session: " used-sid "\n"
                                         "Prompt: " (subs prompt-str 0 (min 300 (count prompt-str)))
                                         (when (> (count prompt-str) 300) "...")
                                         "\n\n"
                                         (when activity
                                           (str "Activity: " activity "\n"))
                                         (when changed-files
                                           (str "Files modified: " changed-files "\n"))
                                         (if activity
                                           (str "Working... (" activity ")")
                                           "Waiting for response..."))]
                        ;; Update invoke buffer
                        ;; Keep invoke output separate from *agents* in side-window slot 1.
                        (bb/blackboard! buf-name content {:width 80 :slot 1 :no-display true})
                        ;; Update agents buffer
                        (bb/project-agents! (reg/registry-status))
                        ;; Evidence heartbeat (every 30s, not every tick)
                        (when (>= (- now-ms @last-heartbeat-ms) heartbeat-interval)
                          (reset! last-heartbeat-ms now-ms)
                          (emit-invoke-evidence! agent-id "invoke-heartbeat"
                                                 {"elapsed-seconds" (quot elapsed 1000)
                                                  "prompt-preview" prompt-preview}
                                                 :session-id used-sid
                                                 :tags ["heartbeat"])))
                      (catch InterruptedException _
                        (reset! running false))
                      (catch Throwable _))))
                "invoke-ticker")]
    (.setDaemon thread true)
    (.start thread)
    (fn []
      (reset! running false)
      (.interrupt thread))))

(defn make-claude-invoke-fn
  "Create an invoke-fn that calls `claude -p` for real Claude interaction.

   invoke-fn contract: (fn [prompt session-id] -> {:result str :session-id str})

   Uses --output-format json to capture tool-use responses that would
   otherwise produce empty text output.

   Streams stderr to the *invoke: <agent-id>* Emacs buffer for live visibility.
   Periodically refreshes the *agents* buffer to show elapsed time.
   Emits evidence (start, heartbeat, complete) to the evidence store.

   Serialized via locking — only one `claude -p` process at a time (I-1).
   First call with nil session-id generates a new UUID via --session-id.
   Subsequent calls use --resume.

   opts:
     :claude-bin       — path to claude CLI (default \"claude\")
     :permission-mode  — permission mode (default \"bypassPermissions\")
     :agent-id         — agent identifier (default \"claude\")
     :session-file     — path to session ID file for persistence (optional)
     :session-id-atom  — atom holding current session ID (optional)
     :timeout-ms       — hard process timeout in ms (default 1800000 = 30 min).
                         Set high because Emacs sessions replace the CLI and should
                         not be arbitrarily killed. IRC relay enforces its own
                         shorter timeout (120s) via invoke-timeout-ms."
  [{:keys [claude-bin permission-mode agent-id session-file session-id-atom timeout-ms]
    :or {claude-bin "claude" permission-mode "bypassPermissions" agent-id "claude"
         timeout-ms 1800000}}]
  (let [!lock (Object.)
        buf-name (str "*invoke: " agent-id "*")]
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
                            "--output-format" "stream-json" "--verbose"]
                     session-id (into ["--resume" (str session-id)])
                     new-sid    (into ["--session-id" new-sid]))
              used-sid (or session-id new-sid)
              prompt-preview (subs prompt-str 0 (min 200 (count prompt-str)))
              _ (println (str "[invoke] " agent-id " claude -p "
                              (subs (pr-str prompt-str) 0
                                    (min 80 (count (pr-str prompt-str))))
                              "... (session: " (when used-sid (subs used-sid 0 8)) ")"))
              _ (flush)
              ;; Evidence: invoke started
              _ (emit-invoke-evidence! agent-id "invoke-start"
                                       {"prompt-preview" prompt-preview}
                                       :session-id used-sid
                                       :tags ["invoke-start"])
              ;; Show prompt in the invoke buffer + force display
              _ (try
                  (bb/blackboard! buf-name
                                  (str "Invoke: " agent-id "\n"
                                       "Session: " used-sid "\n"
                                       "Prompt: " (subs prompt-str 0 (min 300 (count prompt-str)))
                                       (when (> (count prompt-str) 300) "...")
                                       "\n\nStarting...")
                                  {:width 80 :slot 1})
                  (catch Throwable _))
              ;; Start ticker: updates invoke buffer + agents buffer every 5s
              ;; Also emits evidence heartbeats every 30s
              stop-ticker! (start-invoke-ticker! buf-name agent-id prompt-str used-sid 5000)
              ;; Launch process with ProcessBuilder
              pb (doto (ProcessBuilder. ^java.util.List (vec args))
                    (.redirectInput (java.lang.ProcessBuilder$Redirect/from (java.io.File. "/dev/null"))))
              proc (.start pb)
              ;; Drain stderr in background (prevents buffer blocking)
              stderr-future (future (drain-stream! (.getErrorStream proc)))
              ;; Parse stream-json stdout line by line
              text-acc (StringBuilder.)
              result-sid (atom nil)
              result-error (atom false)
              aid-val (str agent-id)
              stdout-future (future
                              (with-open [r (java.io.BufferedReader.
                                             (java.io.InputStreamReader.
                                              (.getInputStream proc)))]
                                (loop []
                                  (when-let [line (.readLine r)]
                                    (when-not (str/blank? line)
                                      (try
                                        (let [parsed (json/parse-string line true)]
                                          (case (:type parsed)
                                            "assistant"
                                            (let [content (get-in parsed [:message :content])
                                                  tools (when (sequential? content)
                                                          (->> content
                                                               (filter #(= "tool_use" (:type %)))
                                                               (map :name)
                                                               seq))
                                                  text (extract-text-from-assistant-message parsed)]
                                              ;; Surface tool activity to registry when available.
                                              (when-let [update-activity! (ns-resolve 'futon3c.agency.registry
                                                                                      'update-invoke-activity!)]
                                                (when tools
                                                  (update-activity!
                                                   aid-val
                                                   (str "using " (str/join ", " tools))))
                                                (when (and (not tools) text (not (str/blank? text)))
                                                  (update-activity! aid-val "composing response")))
                                              (when (and text (not (str/blank? text)))
                                                (.append text-acc text)))
                                            "result"
                                            (do (reset! result-sid (:session_id parsed))
                                                (when (:is_error parsed)
                                                  (reset! result-error true)))
                                            nil))
                                        (catch Exception e
                                          (println (str "[invoke] stream parse error: " (.getMessage e)))
                                          (flush))))
                                    (recur)))))]
          (try
            ;; Wait for process with hard timeout
            (let [finished? (.waitFor proc timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)]
              (when-not finished?
                (println (str "[invoke] " agent-id " TIMEOUT after " (format-elapsed timeout-ms) " — killing process"))
                (flush)
                (.destroyForcibly proc)
                (.waitFor proc 5000 java.util.concurrent.TimeUnit/MILLISECONDS)))
            @stdout-future
            (let [exit (.exitValue proc)
                  err @stderr-future
                  text (str text-acc)
                  session-id @result-sid
                  final-sid (or session-id used-sid)
                  ok? (and (zero? exit) (not @result-error))]
              ;; Persist session ID
              (when (and session-file final-sid (not (str/blank? final-sid)))
                (persist-session-id! session-file final-sid))
              (when (and session-id-atom final-sid (not (str/blank? final-sid)))
                (reset! session-id-atom final-sid))
              ;; Evidence: invoke complete
              (emit-invoke-evidence! agent-id "invoke-complete"
                                     {"ok" ok?
                                      "exit-code" exit
                                      "result-preview" (when ok?
                                                         (let [r (str (or text ""))]
                                                           (subs r 0 (min 300 (count r)))))
                                      "error" (when-not ok?
                                                (str "exit-" exit))}
                                     :session-id final-sid
                                     :tags ["invoke-complete"])
              ;; Final blackboard update with result summary
              (try
                (bb/blackboard! buf-name
                                (str "Invoke: " agent-id " — DONE (exit=" exit ")\n"
                                     "Session: " final-sid "\n"
                                     "Output: " (count (or text "")) " chars\n"
                                     (when (not (str/blank? err))
                                       (str "\nStderr: " (subs err 0 (min 200 (count err))) "\n"))
                                     (when text
                                       (str "\n--- response ---\n"
                                            (subs text 0 (min 1000 (count text)))
                                            (when (> (count text) 1000) "\n..."))))
                                {:width 80 :slot 1 :no-display true})
                (catch Throwable _))
              (println (str "[invoke] " agent-id " exit=" exit
                            " text-len=" (count (or text ""))
                            " err-len=" (count (or err ""))))
              (flush)
              (if ok?
                {:result (if (str/blank? text)
                           "[Claude used tools but produced no text response]"
                           text)
                 :session-id final-sid}
                {:result nil :session-id final-sid
                 :error (str "Exit " exit ": " (str/trim (or err "")))}))
            (finally
              (stop-ticker!))))))))

(defn make-codex-invoke-fn
  "Create an invoke-fn that calls `codex exec` for real Codex interaction.

   invoke-fn contract: (fn [prompt session-id] -> {:result str :session-id str})

   Wraps codex-cli/make-invoke-fn with evidence emission and blackboard updates.

   opts:
     :codex-bin          — path to codex CLI (default \"codex\")
     :model              — model name (default \"gpt-5-codex\")
     :sandbox            — sandbox mode (default \"danger-full-access\")
     :approval-policy    — approval policy (default \"never\")
     :timeout-ms         — hard timeout for codex process (default 120000)
     :cwd                — working directory (default user.dir)
     :agent-id           — agent identifier (default \"codex\")
     :session-file       — path to session ID file for persistence (optional)
     :session-id-atom    — atom holding current session ID (optional)"
  [{:keys [codex-bin model sandbox approval-policy timeout-ms cwd agent-id
           session-file session-id-atom]
    :or {codex-bin "codex" model "gpt-5-codex" sandbox "danger-full-access"
         approval-policy "never" timeout-ms 120000 agent-id "codex"}}]
  (let [inner-fn (codex-cli/make-invoke-fn {:codex-bin codex-bin
                                             :model model
                                             :sandbox sandbox
                                             :approval-policy approval-policy
                                             :timeout-ms timeout-ms
                                             :cwd cwd})
        buf-name (str "*invoke: " agent-id "*")]
    (fn [prompt session-id]
      (let [prompt-str (cond
                         (string? prompt) prompt
                         (map? prompt)    (or (:prompt prompt) (:text prompt)
                                              (json/generate-string prompt))
                         :else            (str prompt))
            prompt-preview (subs prompt-str 0 (min 200 (count prompt-str)))
            invoke-sid (preferred-session-id session-file session-id session-id-atom)
            used-sid (or invoke-sid "new")]
        (println (str "[invoke] " agent-id " codex exec "
                      (subs prompt-str 0 (min 80 (count prompt-str)))
                      "... (session: " (when invoke-sid (subs invoke-sid 0 (min 8 (count invoke-sid)))) ")"))
        (flush)
        ;; Evidence: invoke started
        (emit-invoke-evidence! agent-id "invoke-start"
                               {"prompt-preview" prompt-preview}
                               :session-id used-sid
                               :tags ["invoke-start"])
        ;; Show prompt in invoke buffer
        (try
          (bb/blackboard! buf-name
                          (str "Invoke: " agent-id "\n"
                               "Session: " used-sid "\n"
                               "Prompt: " (subs prompt-str 0 (min 300 (count prompt-str)))
                               (when (> (count prompt-str) 300) "...")
                               "\n\nStarting...")
                          {:width 80 :slot 1})
          (catch Throwable _))
        ;; Start ticker with evidence heartbeats
        (let [stop-ticker! (start-invoke-ticker! buf-name agent-id prompt-str used-sid 5000)
              result (try
                       (inner-fn prompt invoke-sid)
                       (finally
                         (stop-ticker!)))
              final-sid (:session-id result)
              ok? (nil? (:error result))]
          ;; Persist session ID
          (when (and session-file final-sid (not (str/blank? final-sid)))
            (persist-session-id! session-file final-sid))
          (when (and session-id-atom final-sid (not (str/blank? final-sid)))
            (reset! session-id-atom final-sid))
          ;; Evidence: invoke complete
          (emit-invoke-evidence! agent-id "invoke-complete"
                                 {"ok" ok?
                                  "result-preview" (when ok?
                                                     (let [r (str (or (:result result) ""))]
                                                       (subs r 0 (min 300 (count r)))))
                                  "error" (when-not ok? (:error result))}
                                 :session-id (or final-sid used-sid)
                                 :tags ["invoke-complete"])
          ;; Final blackboard update
          (try
            (bb/blackboard! buf-name
                            (str "Invoke: " agent-id " — DONE"
                                 (if ok? "" (str " ERROR: " (:error result))) "\n"
                                 "Session: " final-sid "\n"
                                 (when (:result result)
                                   (let [r (str (:result result))]
                                     (str "\n--- response ---\n"
                                          (subs r 0 (min 1000 (count r)))
                                          (when (> (count r) 1000) "\n...")))))
                            {:width 80 :slot 1 :no-display true})
            (catch Throwable _))
          (println (str "[invoke] " agent-id
                        (if ok? " ok" (str " error: " (:error result)))
                        " result-len=" (count (or (:result result) ""))))
          (flush)
          result)))))

;; =============================================================================
;; Dispatch-based IRC relay — routes through invoke-agent! (I-1, I-2 compliant)
;; =============================================================================

(defn- mentioned?
  "Check if text mentions nick via @nick or nick: prefix (case-insensitive)."
  [text nick]
  (let [t (str/lower-case (str/trim (str text)))
        n (str/lower-case (str nick))]
    (or (str/includes? t (str "@" n))
        (str/starts-with? t (str n ":"))
        (str/starts-with? t (str n ",")))))

(defn- strip-mention
  "Remove @nick or nick: prefix from text, return the rest."
  [text nick]
  (let [patterns [(re-pattern (str "(?i)@" (java.util.regex.Pattern/quote nick) "\\b[:;,]?\\s*"))
                  (re-pattern (str "(?i)^" (java.util.regex.Pattern/quote nick) "[:;,]\\s*"))]]
    (reduce (fn [t p] (str/trim (str/replace t p "")))
            text patterns)))

(def ^:private irc-send-directive-re
  #"(?i)^IRC_SEND\s+\S+\s*::\s*(.+)$")

(defn- normalize-irc-result
  "Normalize agent reply text before posting to IRC.

   If the model emits `IRC_SEND #channel :: message`, strip that directive
   and post just `message` so chat reads naturally."
  [text]
  (let [lines (str/split-lines (or text ""))
        directive-msg (some (fn [line]
                              (when-let [[_ msg] (re-matches irc-send-directive-re (str/trim line))]
                                (str/trim msg)))
                            lines)
        stripped (->> lines
                      (remove #(re-matches irc-send-directive-re (str/trim %)))
                      (str/join "\n")
                      str/trim)]
    (cond
      (not (str/blank? stripped)) stripped
      (not (str/blank? directive-msg)) directive-msg
      :else (str/trim (or text "")))))

(defn- irc-invoke-prompt
  "Wrap an IRC user message with explicit surface/delivery semantics."
  [{:keys [nick sender channel user-text]}]
  (str "Runtime surface contract:\n"
       "- Current surface: IRC.\n"
       "- Channel: " channel "\n"
       "- Sender: " sender "\n"
       "- Your returned text will be posted to IRC by the server as <" nick ">.\n"
       "- Return natural chat text only; do not emit directive wrappers.\n"
       "- Do not claim to write relay files (/tmp/futon-irc-*.jsonl) or send network traffic unless this turn actually executed such a tool.\n\n"
       "- Before claiming DNS/network/git connectivity failure, run a command that verifies it and quote the actual output.\n"
       "- Do not recommend exporting `CODEX_SANDBOX`/`CODEX_APPROVAL` on IRC; this runtime already applies project defaults.\n\n"
       "User message:\n"
       user-text))

(defn start-dispatch-relay!
  "Wire IRC messages to agent dispatch via invoke-agent!.

   When an IRC message mentions the agent nick (@claude or claude:),
   invoke the registered agent and send the response back to IRC.
   At-mention gating prevents agents from responding to every message
   and chattering with each other.

   Returns {:agent-id str :nick str} or nil if IRC is not running."
  [{:keys [relay-bridge irc-server agent-id nick invoke-timeout-ms]
    :or {agent-id "claude-1" nick "claude" invoke-timeout-ms 120000}}]
  (when (and relay-bridge irc-server)
    ((:join-agent! relay-bridge) agent-id nick "#futon"
     (fn [data]
       (let [parsed (try (json/parse-string data true) (catch Exception _ nil))]
         (when (and parsed (= "irc_message" (:type parsed)))
           (let [text (str (:text parsed))
                 sender (or (:from parsed) (:nick parsed))
                 channel (or (:channel parsed) "#futon")]
             (println (str "[irc] " channel " <" sender "> " text))
             (flush)
             (if (and (mentioned? text nick)
                      (not= sender nick))
               (let [prompt (strip-mention text nick)]
                 (if (str/blank? prompt)
                   (do (println (str "[irc] " nick ": mention detected but prompt empty, ignoring"))
                       (flush))
                   (do
                     (println (str "[irc] " nick ": dispatching invoke (timeout=" invoke-timeout-ms "ms)"))
                     (flush)
                     (future
                       (try
                         (let [invoke-prompt (irc-invoke-prompt {:nick nick
                                                                 :sender sender
                                                                 :channel channel
                                                                 :user-text prompt})
                               resp (reg/invoke-agent! agent-id invoke-prompt invoke-timeout-ms)]
                           (if (and (:ok resp) (string? (:result resp)))
                             (do
                               (let [reply (normalize-irc-result (:result resp))]
                                 ((:send-to-channel! irc-server) channel nick reply)
                                 (println (str "[irc] " nick " → " channel " ("
                                               (count reply) " chars): "
                                               (subs reply 0 (min 120 (count reply))))))
                               (flush))
                             (do
                               (let [err-msg (if (map? (:error resp))
                                               (or (:error/message (:error resp))
                                                   (pr-str (:error resp)))
                                               (str (:error resp)))]
                                 (println (str "[irc] " nick " invoke FAILED: " err-msg))
                                 ((:send-to-channel! irc-server) channel nick
                                  (str "[invoke failed] " err-msg)))
                               (flush))))
                         (catch Exception e
                           (println (str "[irc] " nick " dispatch ERROR: " (.getMessage e)))
                           (flush)))))))
               (do (println (str "[irc] " nick ": not mentioned, skipping"))
                   (flush))))))))
    ((:join-virtual-nick! irc-server) "#futon" nick)
    (println (str "[dev] Dispatch relay: " nick " → invoke-agent! → #futon (mention-gated)"))
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
  (let [role-info (deployment-role-info)
        role (:role role-info)
        role-cfg (role-defaults role)
        register-claude? (env-bool "FUTON3C_REGISTER_CLAUDE" (:register-claude? role-cfg))
        register-codex? (env-bool "FUTON3C_REGISTER_CODEX" (:register-codex? role-cfg))
        relay-claude? (env-bool "FUTON3C_RELAY_CLAUDE" (or register-claude? (= role :linode)))
        relay-codex? (env-bool "FUTON3C_RELAY_CODEX" (or register-codex? (= role :linode)))
        relay-invoke-timeout-ms (or (env-int "FUTON3C_RELAY_INVOKE_TIMEOUT_MS" 120000) 120000)
        codex-ws-bridge? (env-bool "FUTON3C_CODEX_WS_BRIDGE" (= role :laptop))
        irc-send-base-hint (or (some-> (env "FUTON3C_IRC_SEND_BASE") normalize-http-base)
                               (some-> (env "FUTON3C_LINODE_URL") normalize-http-base)
                               (some-> (first-peer-url) normalize-http-base))
        direct-xtdb? (direct-xtdb-enabled? role-cfg)
        f1-sys (start-futon1a! direct-xtdb?)
        evidence-store (make-evidence-store f1-sys direct-xtdb?)
        _ (reset! !f1-sys f1-sys)
        _ (reset! !evidence-store evidence-store)
        _ (mcs/configure! {:evidence-store evidence-store})
        ;; IRC relay bridge + server (before futon3c so interceptor is ready)
        irc-sys (start-irc! evidence-store role)
        _ (reset! !irc-sys irc-sys)
        ;; futon3c HTTP + WS (with IRC interceptor if IRC is running)
        f3c-sys (start-futon3c!
                 {:xtdb-node (when direct-xtdb? (:node f1-sys))
                  :evidence-store evidence-store
                  :irc-send-base irc-send-base-hint
                  :irc-send-fn (when irc-sys
                                 (:send-to-channel! (:server irc-sys)))
                  :irc-interceptor (when irc-sys
                                     (:irc-interceptor (:relay-bridge irc-sys)))})
        _ (reset! !f3c-sys f3c-sys)
        ;; Auto-join agents to #futon when they complete WS handshake
        _ (when (and irc-sys (:ws-connections f3c-sys))
            (install-irc-auto-join!
             (:ws-connections f3c-sys)
             (:relay-bridge irc-sys)
             (:server irc-sys)))
        ;; Register Claude + Codex with inline invoke-fns.
        ;; CLI invocation runs in-JVM with evidence emission and blackboard updates.
        ;; WS bridge scripts remain available for remote agent scenarios.
        _ (when register-claude?
            (let [sf (io/file (or (env "CLAUDE_SESSION_FILE")
                                  "/tmp/futon-session-id"))
                  initial-sid (read-session-id sf)
                  sid-atom (atom initial-sid)
                  invoke-fn (make-claude-invoke-fn
                             {:claude-bin (env "CLAUDE_BIN" "claude")
                              :permission-mode (env "CLAUDE_PERMISSION" "bypassPermissions")
                              :agent-id "claude-1"
                              :session-file sf
                              :session-id-atom sid-atom})
                  _register (rt/register-claude! {:agent-id "claude-1"
                                                  :invoke-fn invoke-fn})
                  ;; Reconcile stale duplicate entries so invoke-fn is always present.
                  _ (reg/update-agent! "claude-1"
                                       :agent/type :claude
                                       :agent/invoke-fn invoke-fn
                                       :agent/capabilities [:explore :edit :test :coordination/execute])]
              (when initial-sid
                (reg/update-agent! "claude-1" :agent/session-id initial-sid))
              (println (str "[dev] Claude agent registered: claude-1 (inline invoke)"
                            (when initial-sid
                              (str " (session: " (subs initial-sid 0
                                                       (min 8 (count initial-sid))) ")"))))))
        _ (when register-codex?
            (let [sf (io/file (or (env "CODEX_SESSION_FILE")
                                  "/tmp/futon-codex-session-id"))
                  initial-sid (read-session-id sf)
                  sid-atom (atom initial-sid)
                  invoke-fn (make-codex-invoke-fn
                             {:codex-bin (env "CODEX_BIN" "codex")
                              :model (env "CODEX_MODEL" "gpt-5-codex")
                              :sandbox (env "CODEX_SANDBOX" "danger-full-access")
                              :approval-policy (or (env "CODEX_APPROVAL_POLICY")
                                                   (env "CODEX_APPROVAL" "never"))
                              :timeout-ms (or (env-int "CODEX_INVOKE_TIMEOUT_MS" 120000) 120000)
                              :agent-id "codex-1"
                              :session-file sf
                              :session-id-atom sid-atom})
                  ws-port (or (:port f3c-sys) (env-int "FUTON3C_PORT" 7070))
                  peer-base (or (some-> (env "FUTON3C_LINODE_URL") str/trim not-empty)
                                (first-peer-url))
                  peer-ws-base (normalize-ws-base peer-base)
                  explicit-ws-base (some-> (env "FUTON3C_CODEX_WS_BASE") str/trim not-empty)
                  ws-base (or explicit-ws-base
                              (when (= role :laptop) peer-ws-base)
                              (str "ws://127.0.0.1:" ws-port))
                  remote-ws-target? (and (pos? (long ws-port))
                                         (not (local-ws-target? ws-base ws-port)))
                  register-http-base (or (some-> (env "FUTON3C_CODEX_WS_HTTP_BASE") str/trim not-empty)
                                         (when remote-ws-target?
                                           (normalize-http-base (or peer-base ws-base))))
                  evidence-replication? (env-bool "FUTON3C_CODEX_WS_REPLICATE_EVIDENCE"
                                                  remote-ws-target?)
                  replication-interval-ms (or (env-int "EVIDENCE_REPLICATION_INTERVAL_MS" 30000)
                                              30000)
                  ws-bridge-enabled? (and codex-ws-bridge? (pos? (long ws-port)))]
              (when-let [old @!codex-ws-bridge]
                ((:stop-fn old))
                (reset! !codex-ws-bridge nil))
              (if ws-bridge-enabled?
                ;; WS bridge mode: Codex connects via WS (local or remote)
                (let [ws-path (or (some-> (env "FUTON3C_CODEX_WS_PATH") str/trim not-empty)
                                  "/agency/ws")
                      codex-invoke-fn (when remote-ws-target? invoke-fn)
                      codex-metadata (cond-> {:ws-bridge? true}
                                       remote-ws-target? (assoc :skip-federation-proxy? true)
                                       remote-ws-target? (assoc :ws-remote? true))
                      bridge (start-codex-ws-bridge!
                              {:agent-id "codex-1"
                               :invoke-fn invoke-fn
                               :initial-sid initial-sid
                               :session-file sf
                               :ws-base ws-base
                               :ws-path ws-path
                               :register-http-base register-http-base
                               :evidence-store evidence-store
                               :evidence-replication? evidence-replication?
                               :replication-interval-ms replication-interval-ms})]
                  (rt/register-codex! {:agent-id "codex-1"
                                       :invoke-fn codex-invoke-fn
                                       :metadata codex-metadata})
                  ;; Reconcile stale duplicate entries before dispatch relay starts.
                  (reg/update-agent! "codex-1"
                                     :agent/type :codex
                                     :agent/invoke-fn codex-invoke-fn
                                     :agent/capabilities [:edit :test :coordination/execute]
                                     :agent/metadata codex-metadata)
                  (when initial-sid
                    (reg/update-agent! "codex-1" :agent/session-id initial-sid))
                  (reset! !codex-ws-bridge bridge)
                  (println (str "[dev] Codex agent registered: codex-1 (ws-bridge mode"
                                (when remote-ws-target? ", remote")
                                ")"
                                (when initial-sid
                                  (str " (session: " (subs initial-sid 0
                                                           (min 8 (count initial-sid))) ")"))))
                  (println (str "[dev] codex ws bridge evidence replication: "
                                (if evidence-replication?
                                  (str "enabled (interval " replication-interval-ms "ms)")
                                  "disabled"))))
                ;; Inline mode: Codex runs directly in this JVM
                (do
                  (when codex-ws-bridge?
                    (println "[dev] codex ws bridge requested but FUTON3C_PORT is disabled; falling back to inline invoke"))
                  (rt/register-codex! {:agent-id "codex-1"
                                       :invoke-fn invoke-fn})
                  ;; Reconcile stale duplicate entries so invoke-fn is always present.
                  (reg/update-agent! "codex-1"
                                     :agent/type :codex
                                     :agent/invoke-fn invoke-fn
                                     :agent/capabilities [:edit :test :coordination/execute])
                  (when initial-sid
                    (reg/update-agent! "codex-1" :agent/session-id initial-sid))
                  (println (str "[dev] Codex agent registered: codex-1 (inline invoke)"
                                (when initial-sid
                                  (str " (session: " (subs initial-sid 0
                                                           (min 8 (count initial-sid))) ")"))))))))
        ;; If codex wasn't registered locally but is expected as a remote peer,
        ;; register a stub so it appears in the agents list. The laptop's WS
        ;; bridge will update the registration with a real invoke-fn on connect.
        _ (when (and (not register-codex?) relay-codex?)
            (rt/register-codex! {:agent-id "codex-1"
                                 :metadata {:remote? true
                                            :note "Awaiting WS bridge from laptop"}})
            (println "[dev] Codex agent registered: codex-1 (remote peer, no local invoke)"))
        ;; Dispatch relays: route IRC messages through invoke-agent!
        _dispatch-relay-claude (when (and irc-sys relay-claude?)
                                 (start-dispatch-relay!
                                 {:relay-bridge (:relay-bridge irc-sys)
                                  :irc-server (:server irc-sys)
                                  :agent-id "claude-1"
                                  :nick "claude"
                                  :invoke-timeout-ms relay-invoke-timeout-ms}))
        _dispatch-relay-codex (when (and irc-sys relay-codex?)
                                (start-dispatch-relay!
                                 {:relay-bridge (:relay-bridge irc-sys)
                                  :irc-server (:server irc-sys)
                                  :agent-id "codex-1"
                                  :nick "codex"
                                  :invoke-timeout-ms relay-invoke-timeout-ms}))
        bridge-sys (start-drawbridge!)
        ;; Federation: configure peers and install announcement hook
        _ (federation/configure-from-env!)
        _ (federation/install-hook!)
        ;; Announce agents that were registered before hook installation
        ;; (startup registers Codex/Claude earlier in this let).
        _ (doseq [typed-id (reg/registered-agents)]
            (when-let [agent-record (reg/get-agent typed-id)]
              (federation/announce! agent-record)))
        fed-peers (federation/peers)
        fed-self (federation/self-url)]
    (println)
    (println (str "[dev] Role: " (name role)
                  " (" (:source role-info) ")"
                  " | register-claude=" register-claude?
                  " register-codex=" register-codex?
                  " | relay-claude=" relay-claude?
                  " relay-codex=" relay-codex?
                  " | codex-ws-bridge=" codex-ws-bridge?))
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
        (when-let [stop (:stop-fn @!codex-ws-bridge)]
          (stop)
          (reset! !codex-ws-bridge nil))
        (when-let [stop (:stop bridge-sys)] (stop))
        (when-let [stop (:stop-fn (:server irc-sys))] (stop))
        (when-let [stop (:server f3c-sys)] (stop))
        ((:stop! f1-sys))
        (println "[dev] Stopped."))))

    ;; Block forever
    @(promise)))
