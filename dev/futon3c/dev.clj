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
     CODEX_REASONING_EFFORT
                       — codex reasoning effort override (for example: low|medium|high)
     CODEX_INVOKE_TIMEOUT_MS — hard timeout for codex exec (default: 1800000)
     CODEX_SESSION_FILE — path to codex session ID file (default: /tmp/futon-codex-session-id)
     FUTON3C_CODEX_WS_BRIDGE — enable codex WS bridge mode (default true on laptop role)
     FUTON3C_CODEX_WS_BASE   — override codex WS bridge base URL
     FUTON3C_CODEX_WS_HTTP_BASE — optional HTTP base for remote ws-bridge agent registration
     FUTON3C_CODEX_WS_PATH   — override codex WS bridge path (default /agency/ws)
     FUTON3C_CODEX_WS_REPLICATE_EVIDENCE — enable WS evidence replication (default true for remote WS target)
     EVIDENCE_REPLICATION_INTERVAL_MS — WS replication poll interval (default 30000)
     FUTON3C_REGISTER_CLAUDE — whether to register claude-1 on this host
     FUTON3C_REGISTER_CLAUDE2 — whether to register claude-2 (mentor, workspace2)
     FUTON3C_REGISTER_CODEX  — whether to register codex-1 on this host
     FUTON3C_TICKLE_AUTOSTART — auto-start Tickle watchdog on boot (default false)
     MEME_DB_PATH            — path to meme.db (auto-detected from futon3a if absent)"
  (:require [futon1a.system :as f1]
            [futon3c.agents.codex-cli :as codex-cli]
            [futon3c.agents.tickle :as tickle]
            [futon3c.agents.tickle-orchestrate :as orch]
            [futon3c.agents.tickle-work-queue :as ct-queue]
            [futon3c.agents.arse-work-queue :as arse-queue]
            [futon3c.blackboard :as bb]
            [futon3c.evidence.store :as estore]
            [futon3c.evidence.xtdb-backend :as xb]
            [futon3c.mission-control.service :as mcs]
            [futon3c.peripheral.mentor :as mentor]
            [futon3c.peripheral.mission-control-backend :as mcb]
            [futon3c.social.whistles :as whistles]
            [futon3c.agency.federation :as federation]
            [futon3c.agency.registry :as reg]
            [futon3c.runtime.agents :as rt]
            [futon3c.cyder :as cyder]
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
           [java.net Socket URI InetAddress NetworkInterface]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers WebSocket WebSocket$Listener]
           [java.io PrintWriter]
           [java.util.concurrent CompletableFuture]))

(declare make-claude-invoke-fn)
(declare record-invoke-delivery!)

(defonce !agents-blackboard-ticker-stop
  (atom nil))

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

(defn configured-codex-agent-id
  "Current codex agent-id, honoring FUTON3C_CODEX_AGENT_ID when set."
  []
  (or (some-> (env "FUTON3C_CODEX_AGENT_ID") str/trim not-empty)
      "codex-1"))

(defn configured-codex-relay-nick
  "Codex IRC nick, honoring FUTON3C_CODEX_NICK or deriving from agent-id."
  []
  (or (some-> (env "FUTON3C_CODEX_NICK") str/trim not-empty)
      (str/replace (configured-codex-agent-id) #"-\d+$" "")))

(defn- nonstarter-fn
  "Resolve a nonstarter API function by symbol name.
   Returns nil when futon5/nonstarter.api is not available on classpath."
  [fn-sym]
  (try
    (require 'nonstarter.api)
    (some-> (ns-resolve 'nonstarter.api fn-sym) var-get)
    (catch Throwable _
      nil)))

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
             :register-claude2? true
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
    :or {agent-id (configured-codex-agent-id)
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
        mark-local-invoke-state! (fn [{:keys [status session-id prompt-preview]}]
                                   (let [updates (cond-> [:agent/status status
                                                          :agent/invoke-started-at (when (= status :invoking) (Instant/now))
                                                          :agent/invoke-prompt-preview (when (= status :invoking) prompt-preview)]
                                                   (some? session-id) (into [:agent/session-id session-id]))]
                                     (try
                                       (apply reg/update-agent! agent-id updates)
                                       (bb/project-agents! (reg/registry-status))
                                       (catch Throwable _))))
        handle-invoke! (fn [^WebSocket ws frame]
                         (let [invoke-id (:invoke_id frame)
                               prompt (:prompt frame)
                               incoming-session (:session_id frame)]
                           (when (string? invoke-id)
                             (future
                               (let [prompt-str (str prompt)
                                     prompt-preview (subs prompt-str 0 (min 120 (count prompt-str)))
                                     final-sid* (atom nil)]
                                 (mark-local-invoke-state! {:status :invoking
                                                            :prompt-preview prompt-preview})
                                 (try
                                   (let [raw-result (try
                                                      (invoke-fn prompt-str incoming-session)
                                                      (catch Throwable t
                                                        {:error (str "invoke exception: " (.getMessage t))
                                                         :session-id incoming-session}))
                                         result (if (map? raw-result)
                                                  raw-result
                                                  {:result (str raw-result)})
                                         sid (some-> (:session-id result) str str/trim not-empty)]
                                     (reset! final-sid* sid)
                                     (when sid
                                       (reset! sid* sid)
                                       (persist-session-id! session-file sid))
                                     (let [invoke-meta (not-empty (dissoc result :result :session-id :error))
                                           payload (cond-> {"type" "invoke_result"
                                                            "invoke_id" invoke-id}
                                                     sid (assoc "session_id" sid)
                                                     (:error result) (assoc "error" (str (:error result)))
                                                     (not (:error result)) (assoc "result" (or (:result result) ""))
                                                     invoke-meta (assoc "invoke_meta" invoke-meta))]
                                       (try
                                         (send-json! ws payload)
                                         (catch Exception e
                                           (println (str "[dev] codex ws bridge send failed: "
                                                         (.getMessage e)))))))
                                   (finally
                                     (mark-local-invoke-state! {:status :idle
                                                                :session-id @final-sid*}))))))))
        handle-invoke-delivery! (fn [frame]
                                  (let [invoke-trace-id (or (:invoke_trace_id frame)
                                                            (:invoke-trace-id frame)
                                                            (:invokeTraceId frame))
                                        delivered (let [v (:delivered frame)]
                                                    (if (boolean? v) v (not= false v)))]
                                    (when-let [tid (some-> invoke-trace-id str str/trim not-empty)]
                                      (record-invoke-delivery!
                                       agent-id
                                       tid
                                       {:surface (or (:surface frame) "irc")
                                        :destination (or (:destination frame)
                                                         (str "#futon as <" agent-id ">"))
                                        :delivered? delivered
                                        :note (or (:note frame) "ws-relayed-invoke-delivery")}))))
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
                                                (when (= "invoke_delivery" (:type frame))
                                                  (handle-invoke-delivery! frame))
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
(defonce !codex-status (atom {}))

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

(defn- sanitize-file-fragment
  "Normalize text for use in local artifact filenames."
  [s fallback]
  (let [raw (or (some-> s str str/trim not-empty) fallback "unknown")
        cleaned (-> raw
                    (str/replace #"[^A-Za-z0-9._-]" "_")
                    (str/replace #"_+" "_"))]
    (if (str/blank? cleaned) fallback cleaned)))

(defn- invoke-result-kind
  "Classify invoke result for trace metadata."
  [text]
  (let [raw (str (or text ""))
        trimmed (str/trim raw)]
    (cond
      (str/blank? trimmed) :empty
      (re-find #"^[\{\[]" trimmed) :structured
      :else :text)))

(defn- sha256-hex
  "Hex SHA-256 for TEXT."
  [text]
  (let [^java.security.MessageDigest md (java.security.MessageDigest/getInstance "SHA-256")
        bytes (.digest md (.getBytes (str text) "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) bytes))))

(defn- escape-elisp-string
  "Escape a string for embedding in an elisp double-quoted string."
  [s]
  (-> (str (or s ""))
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")
      (str/replace "\n" "\\n")))

(defn- format-delivery-receipt-line
  [invoke-trace-id {:keys [surface destination delivered? note]}]
  (let [status (if (false? delivered?) "failed" "delivered")
        note* (some-> note str str/trim not-empty)
        note* (when note*
                (if (> (count note*) 120)
                  (str (subs note* 0 117) "...")
                  note*))]
    (str "Delivery: " status
         " via " (or (some-> surface str str/trim not-empty) "unknown")
         " -> " (or (some-> destination str str/trim not-empty) "unknown")
         " (trace-id " invoke-trace-id ")"
         (when note* (str " [" note* "]")))))

(defn- invoke-meta-trace-id
  "Extract invoke trace id from invoke-meta maps with keyword or string keys."
  [invoke-meta]
  (some (fn [k]
          (let [v (or (get invoke-meta k)
                      (get invoke-meta (name k)))]
            (when-let [tid (some-> v str str/trim not-empty)]
              tid)))
        [:invoke-trace-id :invoke_trace_id :invokeTraceId]))

(defn- write-invoke-artifact!
  "Persist full invoke output to a local artifact file.
   Returns absolute file path on success, nil on failure or blank text."
  [agent-id session-id text]
  (let [payload (str (or text ""))]
    (when-not (str/blank? (str/trim payload))
      (try
        (let [root (io/file (or (some-> (env "FUTON3C_INVOKE_ARTIFACT_DIR") str/trim not-empty)
                                "/tmp/futon-invoke-artifacts"))
              _ (.mkdirs root)
              agent-frag (sanitize-file-fragment agent-id "agent")
              sid-frag (sanitize-file-fragment (some-> session-id (subs 0 (min 8 (count session-id))))
                                               "nosid")
              file (io/file root (format "%s-%s-%d.txt"
                                         agent-frag
                                         sid-frag
                                         (System/currentTimeMillis)))]
          (spit file payload)
          (.getAbsolutePath file))
        (catch Exception e
          (println (str "[dev] invoke artifact write error: " (.getMessage e)))
          nil)))))

(defn- invoke-trace-response-block
  "Build invoke-trace metadata only (never semantic response text)."
  [agent-id session-id invoke-trace-id result-text]
  (let [payload (str (or result-text ""))
        kind (invoke-result-kind payload)
        chars (count payload)
        digest (when (pos? chars) (sha256-hex payload))
        artifact-path (write-invoke-artifact! agent-id session-id result-text)]
    (str "\n--- response trace (metadata only) ---\n"
         "Result: kind=" (name kind)
         ", chars=" chars
         (when digest (str ", sha256=" digest))
         "\n"
         (if artifact-path
           (str "Artifact: " artifact-path "\n")
           "Artifact: [not written]\n")
         "Delivery: pending (trace-id " invoke-trace-id ")\n"
         "Delivery guarantee: caller must record where reply was sent.\n")))

(defn record-invoke-delivery!
  "Record where an invoke reply was actually delivered.
   Appends/updates a delivery receipt line in *invoke: <agent>* trace buffer."
  [agent-id invoke-trace-id receipt]
  (let [aid (some-> agent-id str str/trim)
        tid (some-> invoke-trace-id str str/trim)]
    (when (and (seq aid) (seq tid))
      (let [agent-record (reg/get-agent aid)
            emacs-socket (some-> agent-record :agent/metadata :emacs-socket str str/trim not-empty)
            bb-opts (cond-> {} emacs-socket (assoc :emacs-socket emacs-socket))
            buf-name (str "*invoke: " aid "*")
            pending-line (str "Delivery: pending (trace-id " tid ")")
            receipt-line (format-delivery-receipt-line tid receipt)
            elisp (str "(let ((buf (get-buffer \"" (escape-elisp-string buf-name) "\")))"
                       "(if (not buf) "
                       "\"missing-buffer\" "
                       "(with-current-buffer buf "
                       "(let ((inhibit-read-only t)) "
                       "(goto-char (point-min)) "
                       "(if (search-forward \"" (escape-elisp-string pending-line) "\" nil t) "
                       "(progn (replace-match \"" (escape-elisp-string receipt-line) "\" t t) \"replaced\") "
                       "(progn "
                       "(goto-char (point-max)) "
                       "(unless (bolp) (insert \"\\n\")) "
                       "(insert \"" (escape-elisp-string receipt-line) "\\n\") "
                       "\"appended\"))))))")]
        (loop [attempt 1]
          (let [{:keys [ok output]} (bb/blackboard-eval! elisp bb-opts)
                out (some-> output str str/trim)
                status (cond
                         (#{"\"replaced\"" "replaced"} out) :replaced
                         (#{"\"appended\"" "appended"} out) :appended
                         (#{"\"missing-buffer\"" "missing-buffer"} out) :missing-buffer
                         :else :unknown)
                success? (and ok (#{:replaced :appended} status))]
            (cond
              success?
              true

              ;; Buffer doesn't exist — expected for WS-bridged agents
              ;; where invoke buffer lives on a different machine.
              (= :missing-buffer status)
              false

              (< attempt 3)
              (do
                (Thread/sleep (* 120 attempt))
                (recur (inc attempt)))

              :else
              (do
                (println (str "[invoke-delivery] failed for " aid
                              " trace-id=" tid
                              " status=" (name status)
                              " ok=" ok
                              " output=" out))
                (flush)
                false))))))))

;; =============================================================================
;; ngircd IRC sender — persistent connection for Tickle paging
;; =============================================================================

(defonce !irc-conn (atom nil))
(defonce !irc-log (atom []))
(def ^:private irc-log-max 200)

(defn- irc-conn-alive?
  "Check if the persistent IRC connection is still open."
  [conn]
  (and conn
       (let [{:keys [^Socket socket]} conn]
         (and socket (not (.isClosed socket))))))

(defn- parse-irc-privmsg
  "Parse a raw IRC PRIVMSG line into {:nick :channel :text :at}."
  [raw-line]
  (when-let [[_ prefix channel trailing] (re-matches #":([^ ]+) PRIVMSG ([^ ]+) :(.*)" raw-line)]
    (let [nick (first (str/split prefix #"!"))]
      {:nick nick
       :channel channel
       :text trailing
       :at (str (Instant/now))})))

(defn- irc-connect!
  "Open a persistent connection to ngircd. JOINs #math and stays connected.
   Background thread handles PINGs and captures PRIVMSG lines to !irc-log."
  [nick]
  (let [nick (str/replace (str nick) #"[^a-zA-Z0-9_-]" "")
        sock (Socket. "127.0.0.1" 6667)
        out (PrintWriter. (.getOutputStream sock) true)
        in (io/reader (.getInputStream sock))]
    (.println out "PASS MonsterMountain")
    (.println out (str "NICK " nick))
    (.println out (str "USER " nick " 0 * :" nick))
    (Thread/sleep 500)
    (.println out "JOIN #math")
    (Thread/sleep 300)
    ;; Background thread: respond to PINGs + capture PRIVMSG to ring buffer
    (let [running (atom true)
          reader-thread (Thread.
                         (fn []
                           (try
                             (while @running
                               (when-let [line (try (.readLine in) (catch Exception _ nil))]
                                 (cond
                                   (str/starts-with? line "PING")
                                   (.println out (str/replace line "PING" "PONG"))

                                   (str/includes? line "PRIVMSG")
                                   (when-let [msg (parse-irc-privmsg line)]
                                     (swap! !irc-log
                                            (fn [log]
                                              (let [log (conj log msg)]
                                                (if (> (count log) irc-log-max)
                                                  (subvec log (- (count log) irc-log-max))
                                                  log))))))))
                             (catch Exception _))))]
      (.setDaemon reader-thread true)
      (.start reader-thread)
      ;; :out is intentionally omitted — this connection is read-only.
      ;; PONG responses use the writer internally via the reader thread.
      ;; All sends must go through the bridge HTTP /say endpoint.
      {:socket sock :nick nick :running running})))

(defn- ensure-irc-conn!
  "Return the persistent IRC connection, reconnecting if needed.
   Nick is always 'listener' — a read-only observer. All sends go through
   the bridge HTTP /say endpoint with correct per-nick routing."
  [nick]
  (let [conn @!irc-conn]
    (if (irc-conn-alive? conn)
      conn
      (let [new-conn (irc-connect! nick)]
        (reset! !irc-conn new-conn)
        (println (str "[irc] Connected to ngircd as " nick))
        new-conn))))

(defn close-irc-conn!
  "Close the persistent IRC connection."
  []
  (when-let [{:keys [^Socket socket ^PrintWriter out running]} @!irc-conn]
    (when running (reset! running false))
    (try (.println out "QUIT :shutting down") (catch Exception _))
    (try (.close socket) (catch Exception _))
    (reset! !irc-conn nil)
    (println "[irc] Disconnected from ngircd")))

(defn irc-recent
  "Return the last N messages from the IRC log ring buffer.
   Each entry is {:nick string :text string :at iso-timestamp}."
  ([] (irc-recent 50))
  ([n] (let [log @!irc-log]
         (subvec log (max 0 (- (count log) n))))))

(defn irc-catchup!
  "Print a formatted summary of recent IRC activity for REPL use.
   Returns the message count."
  ([] (irc-catchup! 50))
  ([n]
   (let [msgs (irc-recent n)]
     (if (empty? msgs)
       (do (println "[irc] No messages captured yet.") 0)
       (do
         (println (str "[irc] Last " (count msgs) " messages:"))
         (doseq [{:keys [nick text at]} msgs]
           (let [time-part (when at (subs at 11 19))]
             (println (str "  " time-part " <" nick "> " text))))
         (count msgs))))))

(declare send-irc!)

(defn make-irc-send-fn
  "Create a send-to-channel! function backed by persistent ngircd connection."
  ([] (make-irc-send-fn "tickle-1"))
  ([default-nick]
   (fn [channel from-nick message]
     (send-irc! channel (or from-nick default-nick) message))))

(defn make-bridge-irc-send-fn
  "Create a send-fn that posts via the ngircd bridge's HTTP /say endpoint.
   This lets agents post as 'claude' or 'codex' without opening separate IRC
   connections (which would conflict with the bridge's nicks)."
  ([] (make-bridge-irc-send-fn 6769))
  ([port]
   (fn [channel from-nick message]
     (let [url (str "http://127.0.0.1:" port "/say")
           payload (json/generate-string (cond-> {"from" (or from-nick "claude")
                                                   "text" (str message)
                                                   "max_lines" 4}
                                           channel (assoc "channel" (str channel))))
           conn (doto (-> (java.net.URI. url) .toURL .openConnection)
                  (.setRequestMethod "POST")
                  (.setRequestProperty "Content-Type" "application/json")
                  (.setDoOutput true)
                  (.setConnectTimeout 3000)
                  (.setReadTimeout 5000))]
       (with-open [os (.getOutputStream conn)]
         (.write os (.getBytes payload "UTF-8")))
       (let [code (.getResponseCode conn)]
         (when (>= code 400)
           (throw (ex-info (str "bridge /say returned " code)
                           {:status code :from from-nick})))
         {:ok true :status code})))))

(def ^:private bridge-send-fn*
  (make-bridge-irc-send-fn))

(defn send-irc!
  "Send a message to IRC via the bridge HTTP /say endpoint."
  [channel from-nick message]
  (try
    (bridge-send-fn* channel from-nick message)
    true
    (catch Exception e
      (println (str "[irc] send-irc! error: " (.getMessage e)))
      false)))

;; =============================================================================
;; Tickle task state machine — full lifecycle tracking
;; =============================================================================

;; Lifecycle: proposal → approved → implementing → pr-open → merged
;; Labels:    ct-proposal → ct-approved → ct-implementing → ct-pr-open → ct-merged
;; Also:      ct-needs-rework (loops back to proposal)
;;
;; Task map: {:id string :gh-issue int :title string :phase keyword
;;            :assignee string :status keyword :last-nudge-at string
;;            :created-at string :labels #{string}}

(defonce !tickle-tasks (atom {}))  ;; {task-id → task-map}
(defonce !tickle-conductor (atom nil))
(defonce !last-batch-assign (atom nil))  ;; Instant of last batch assignment

(def ^:private ct-repo "tothedarktowercame/18_Category_theory_homological_algebra")

(def ^:private nudge-cooldown-ms
  "Minimum time between re-paging the same task (3 minutes)."
  180000)

(defn- gh-issue-labels
  "Extract label name set from a GitHub issue map."
  [issue]
  (into #{} (map :name) (:labels issue)))

(defn- issue-phase
  "Determine lifecycle phase from issue labels."
  [label-set]
  (cond
    (label-set "ct-merged")       :merged
    (label-set "ct-pr-open")      :pr-open
    (label-set "ct-implementing") :implementing
    (label-set "ct-needs-rework") :needs-rework
    (label-set "ct-approved")     :approved
    (label-set "ct-proposal")     :proposal
    :else                         :unknown))

(defn- issue-has-comment-matching?
  "Check if any comment on the issue matches the regex."
  [issue pattern]
  (some (fn [c]
          (when-let [body (:body c)]
            (re-find pattern body)))
        (:comments issue)))

(defn- issue-last-verdict
  "Return the last review verdict (:approve, :request-changes, or nil).
   Scans comments in order; last one matching a verdict wins."
  [issue]
  (let [verdicts (keep (fn [c]
                         (when-let [body (:body c)]
                           (cond
                             (re-find #"(?i)\bAPPROVE\b" body) :approve
                             (re-find #"(?i)\bREQUEST_CHANGES\b" body) :request-changes)))
                       (:comments issue))]
    (last verdicts)))

(defn- gh-relabel!
  "Remove old-label, add new-label on a GitHub issue. Fire-and-forget."
  [issue-number old-label new-label]
  (future
    (try
      (when old-label
        (shell/sh "gh" "issue" "edit" (str issue-number)
                  "--repo" ct-repo
                  "--remove-label" old-label))
      (when new-label
        (shell/sh "gh" "issue" "edit" (str issue-number)
                  "--repo" ct-repo
                  "--add-label" new-label))
      (println (str "[tickle] #" issue-number ": " old-label " → " new-label))
      (catch Exception e
        (println (str "[tickle] relabel error #" issue-number ": " (.getMessage e)))))))

(defn tickle-task-sync!
  "Sync task state from GitHub issues + IRC log.
   Detects lifecycle transitions and auto-promotes labels.
   Returns the updated task map."
  []
  (let [;; 1. Fetch all issues with comments
        gh-issues (try
                    (-> (shell/sh "gh" "issue" "list"
                                  "--repo" ct-repo
                                  "--state" "all"
                                  "--json" "number,title,labels,state,comments,assignees"
                                  "--limit" "30")
                        :out
                        (json/parse-string true))
                    (catch Exception _ []))
        ;; 2. Scan recent IRC for acks
        recent-msgs (irc-recent 50)
        ack-patterns #"(?i)(ack|received|on it|working on|will do|reviewing|filed|posted|updated|opened PR|opened pull)"
        acked-by (into #{}
                       (keep (fn [{:keys [nick text]}]
                               (when (and text (re-find ack-patterns text))
                                 nick)))
                       recent-msgs)
        now (Instant/now)]
    ;; 3. Process each issue
    (doseq [issue gh-issues]
      (let [n (:number issue)
            labels (gh-issue-labels issue)
            phase (issue-phase labels)
            closed? (= "closed" (str/lower-case (or (:state issue) "")))]
        (when (and (not= 1 n) (not= :unknown phase))
          ;; Auto-promote: proposal with APPROVE comment → ct-approved
          (when (and (= :proposal phase)
                     (issue-has-comment-matching? issue #"(?i)\bAPPROVE\b"))
            (if (issue-has-comment-matching? issue #"(?i)\bREQUEST_CHANGES\b")
              ;; Has REQUEST_CHANGES — mark needs-rework
              (gh-relabel! n "ct-proposal" "ct-needs-rework")
              ;; Clean approve
              (gh-relabel! n "ct-proposal" "ct-approved")))
          ;; Auto-promote: needs-rework with latest verdict APPROVE → ct-approved
          (when (and (= :needs-rework phase)
                     (= :approve (issue-last-verdict issue)))
            (gh-relabel! n "ct-needs-rework" "ct-approved")))))
    ;; 4. Re-fetch after auto-promotion (labels may have changed)
    (let [gh-issues-fresh (try
                            (-> (shell/sh "gh" "issue" "list"
                                          "--repo" ct-repo
                                          "--state" "all"
                                          "--json" "number,title,labels,state"
                                          "--limit" "30")
                                :out
                                (json/parse-string true))
                            (catch Exception _ gh-issues))]
      ;; 5. Update task atom
      (swap! !tickle-tasks
             (fn [tasks]
               (reduce
                (fn [ts issue]
                  (let [n (:number issue)
                        tid (str "gh-" n)
                        labels (gh-issue-labels issue)
                        phase (issue-phase labels)
                        closed? (= "closed" (str/lower-case (or (:state issue) "")))
                        existing (get ts tid)]
                    (if (or (= 1 n) (= :unknown phase))
                      ts
                      (let [status (cond
                                     closed?          :merged
                                     (= phase :merged) :merged
                                     (= phase :pr-open) :pr-review
                                     (= phase :implementing) :implementing
                                     (= phase :needs-rework) :needs-rework
                                     (= phase :approved) :ready-to-implement
                                     (= phase :proposal) :proposal-review
                                     :else :unknown)
                            assignee (cond
                                       (#{:proposal-review :pr-review} status) "claude-1"
                                       (#{:ready-to-implement :implementing :needs-rework} status)
                                       (configured-codex-agent-id)
                                       (:assignee existing) (:assignee existing)
                                       :else nil)]
                        (assoc ts tid
                               (merge {:id tid
                                       :gh-issue n
                                       :title (:title issue)
                                       :labels labels
                                       :created-at (or (:created-at existing) (str now))
                                       :last-nudge-at (:last-nudge-at existing)}
                                      {:phase phase
                                       :status status
                                       :assignee assignee}))))))
                {}
                gh-issues-fresh))))))

(defn tickle-tasks-summary
  "Return a concise summary of task states for the LLM prompt."
  []
  (let [tasks (vals @!tickle-tasks)
        by-status (group-by :status tasks)]
    (str/join "\n"
              (for [[status items] (sort-by (comp str key) by-status)]
                (str (name status) " (" (count items) "): "
                     (str/join ", " (map (fn [t] (str "#" (:gh-issue t)
                                                      (when (:assignee t)
                                                        (str " → " (:assignee t)))))
                                        items)))))))

(defn tickle-tasks-needing-nudge
  "Return tasks that need a nudge — actionable tasks past cooldown."
  []
  (let [now (Instant/now)]
    (->> (vals @!tickle-tasks)
         (filter (fn [{:keys [status last-nudge-at]}]
                   (and (#{:ready-to-implement :needs-rework :proposal-review :pr-review} status)
                        (or (nil? last-nudge-at)
                            (> (.toMillis (Duration/between
                                          (Instant/parse last-nudge-at) now))
                               nudge-cooldown-ms))))))))

;; =============================================================================
;; tickle-lite: DONE signal processing — immediate queue advancement
;; =============================================================================

(defonce !done-signals-seen (atom #{}))  ;; set of {:at :nick} already processed

(def ^:private done-signal-re
  "Match: DONE #N :: <artifact-ref>"
  #"(?i)^DONE\s+#?(\d+)\s*::\s*(.+)")

(defn- parse-done-signal
  "Parse a DONE signal from IRC text. Returns {:task-ref N :artifact text} or nil."
  [text]
  (when-let [[_ num artifact] (re-find done-signal-re (str/trim (or text "")))]
    {:task-ref (parse-long num)
     :artifact (str/trim artifact)}))

(defn- nick->agent-id
  "Map IRC nick to agent-id (codex → codex-1, claude → claude-1)."
  [nick]
  (let [nick-str (some-> nick str)
        nick-lower (some-> nick-str str/lower-case)
        codex-nick (configured-codex-relay-nick)
        codex-nick-lower (some-> codex-nick str/lower-case)]
    (cond
      (and codex-nick-lower nick-lower
           (str/starts-with? nick-lower codex-nick-lower))
      (configured-codex-agent-id)
      (and nick-lower (str/starts-with? nick-lower "codex"))
      (configured-codex-agent-id)
      (and nick-lower (str/starts-with? nick-lower "claude")) "claude-1"
      :else nil)))

(defn- next-queued-task
  "Find the next task that's ready for the same worker, by issue number order."
  [worker-id]
  (->> (vals @!tickle-tasks)
       (filter (fn [{:keys [status assignee]}]
                 (and (= assignee worker-id)
                      (#{:ready-to-implement :proposal-review :pr-review} status))))
       (sort-by :gh-issue)
       first))

(defn process-done-signals!
  "Scan recent IRC for DONE signals, validate, transition tasks, assign next.
   Returns seq of processed signals (may be empty)."
  []
  (let [msgs (irc-recent 30)
        now (str (Instant/now))
        processed (atom [])]
    (doseq [{:keys [nick text at] :as msg} msgs]
      (when-let [{:keys [task-ref artifact]} (parse-done-signal text)]
        (let [sig-key {:at at :nick nick}]
          (when-not (contains? @!done-signals-seen sig-key)
            (swap! !done-signals-seen conj sig-key)
            (let [tid (str "gh-" task-ref)
                  task (get @!tickle-tasks tid)
                  agent-id (nick->agent-id nick)]
              (cond
                ;; No such task
                (nil? task)
                (do (println (str "[tickle-lite] Ignoring DONE #" task-ref " — unknown task"))
                    (send-irc! "#futon" "tickle-1"
                               (str "@" nick " unknown task #" task-ref ", ignoring.")))

                ;; Not from assigned worker
                (not= agent-id (:assignee task))
                (do (println (str "[tickle-lite] Ignoring DONE #" task-ref " from " nick
                                  " — assigned to " (:assignee task)))
                    (send-irc! "#futon" "tickle-1"
                               (str "@" nick " #" task-ref " is assigned to "
                                    (:assignee task) ", not you. Ignoring.")))

                ;; Valid done signal
                :else
                (do
                  (println (str "[tickle-lite] DONE #" task-ref " from " nick
                                " artifact: " artifact))
                  ;; Transition to done-pending-verify
                  (swap! !tickle-tasks assoc-in [tid :status] :done-pending-verify)
                  (swap! !tickle-tasks assoc-in [tid :done-artifact] artifact)
                  (swap! !tickle-tasks assoc-in [tid :done-at] now)
                  (swap! processed conj {:task-ref task-ref :agent-id agent-id
                                         :artifact artifact})
                  ;; Assign next queued task to same worker
                  (if-let [next-task (next-queued-task agent-id)]
                    (let [msg (str "@" agent-id " #" task-ref " done. Next: #"
                                   (:gh-issue next-task) " " (:title next-task)
                                   ". Repo at /home/joe/code/18_Category_theory_homological_algebra")]
                      (send-irc! "#futon" "tickle-1" msg)
                      (swap! !tickle-tasks assoc-in
                             [(str "gh-" (:gh-issue next-task)) :last-nudge-at] now))
                    ;; Queue empty — check if all tasks merged → immediate next batch
                    (let [all-merged? (and (seq @!tickle-tasks)
                                          (every? #(#{:merged :done-pending-verify} (:status %))
                                                  (vals @!tickle-tasks)))]
                      (if all-merged?
                        (do (println "[tickle-lite] All tasks done, assigning next batch immediately")
                            (reset! !last-batch-assign (Instant/now))
                            (send-irc! "#futon" "tickle-1"
                                       (str "@" agent-id " #" task-ref " done. Next batch: pick 5 new arXiv math.CT"
                                            " entries, write .tex files, open ONE PR."
                                            " Repo: /home/joe/code/18_Category_theory_homological_algebra"
                                            " Signal DONE #N :: <pr-url> when ready.")))
                        (send-irc! "#futon" "tickle-1"
                                   (str "@" agent-id " #" task-ref
                                        " done, nice work. Queue empty — stand by."))))))))))))
    @processed))

;; =============================================================================
;; LLM-backed Tickle conductor — evidence-aware, stateful orchestration
;; =============================================================================

(def ^:private tickle-system-prompt
  "You are Tickle, the mechanical conductor for a multi-agent fulab system.

AGENTS:
  claude-1 (Lab Manager): infra, wiring, coordination design
  claude-2 (Mentor): epistemic oversight, proof ledger (per-mission)
  codex-1 (Worker): scoped task execution via GitHub issues
  corpus-1 (Corpus): data/search agent (ws-only, no invoke)

CURRENT MISSION: FM-001 (FrontierMath)
  The FM conductor handles proof obligation assignment separately.
  You handle general coordination: stall detection, agent availability, escalation.

SURFACE: IRC #futon. Your text output will be posted as <tickle>.

RULES:
1. Only nudge agents that appear stalled (no recent activity).
2. Keep messages short: 1-2 lines, <400 chars. Always @mention target.
3. Do NOT assign math tasks — the FM conductor handles proof obligations.
4. Do NOT reference Category Theory, PlanetMath, or arXiv — those are stale context.
5. If all agents are active, respond PASS.
6. If an agent is stalled, ask them to check #futon or report status.

RESPOND WITH ONLY:
- A short IRC message (start with @agent-id), OR
- PASS")

(declare tickle-build-context)
(declare !fm-conductor)

(defn backpack-add!
  "Add a pattern to an agent's backpack (stored in registry metadata).
   Pattern is {:pattern \"f0/p2\" :sigil \"才\" :query \"...\" :at \"...\"}."
  [agent-id pattern-entry]
  (swap! reg/!registry
         update-in [agent-id :agent/metadata :backpack]
         (fn [bp] (vec (conj (or bp []) pattern-entry)))))

(defn backpack-clear!
  "Clear an agent's pattern backpack."
  [agent-id]
  (swap! reg/!registry
         assoc-in [agent-id :agent/metadata :backpack] []))

(defn backpack
  "Read an agent's current pattern backpack."
  [agent-id]
  (get-in @reg/!registry [agent-id :agent/metadata :backpack]))

(defn make-tickle-invoke-fn
  "Create an invoke-fn for tickle-1 that wraps each prompt with the tickle
   system prompt and live context (task state, IRC log, GitHub issues).
   This satisfies I-1: the agent always knows who it is and what it can see.

   Delegates to a claude invoke-fn for actual LLM execution."
  [claude-invoke-fn]
  (fn [prompt session-id]
    (let [context (try (tickle-build-context)
                       (catch Throwable e
                         (str "(context unavailable: " (.getMessage e) ")")))
          wrapped (str tickle-system-prompt
                       "\n\n---\n\n"
                       context
                       "\n\n---\n\n"
                       "SURFACE: IRC #futon. Your text output will be posted as <tickle>.\n"
                       "CONSTRAINT: Do NOT use tools, create files, run commands, or take actions.\n"
                       "You are a read-only observer. Respond with ONLY a short text message.\n\n"
                       "User message from IRC:\n"
                       (cond
                         (string? prompt) prompt
                         (map? prompt)    (or (:prompt prompt) (:text prompt) (str prompt))
                         :else            (str prompt)))]
      (claude-invoke-fn wrapped session-id))))

(defonce !tickle-llm-invoke (atom nil))

(defn start-tickle-llm!
  "Register the tickle-llm agent — a dedicated Claude instance for Tickle decisions.
   Same pattern as claude-1 but with its own session."
  []
  (let [sf (io/file "/tmp/futon-tickle-session-id")
        initial-sid (read-session-id sf)
        sid-atom (atom initial-sid)
        invoke-fn (make-claude-invoke-fn
                   {:claude-bin (env "CLAUDE_BIN" "claude")
                    :permission-mode "default"
                    :agent-id "tickle-llm"
                    :session-file sf
                    :session-id-atom sid-atom})]
    (reset! !tickle-llm-invoke invoke-fn)
    (rt/register-claude! {:agent-id "tickle-llm"
                          :invoke-fn invoke-fn})
    (reg/update-agent! "tickle-llm"
                       :agent/type :claude
                       :agent/invoke-fn invoke-fn
                       :agent/capabilities [:coordination/execute])
    (println (str "[dev] Tickle LLM agent registered: tickle-llm"
                  (when initial-sid
                    (str " (session: " (subs initial-sid 0 (min 8 (count initial-sid))) ")"))))
    invoke-fn))

(defn- tickle-llm-call
  "Invoke tickle-llm for a one-shot decision.
   Returns the response string, or nil on failure."
  [prompt]
  (when-let [invoke-fn @!tickle-llm-invoke]
    (try
      (let [result (invoke-fn prompt nil)]
        (when-let [text (:result result)]
          (str/trim text)))
      (catch Exception e
        (println (str "[tickle-llm] invoke error: " (.getMessage e)))
        nil))))

(defn- tickle-build-context
  "Assemble the context snapshot for the LLM."
  []
  (let [msgs (irc-recent 30)
        irc-text (if (empty? msgs)
                   "(no recent IRC messages)"
                   (str/join "\n"
                             (map (fn [{:keys [nick text at]}]
                                    (str (when at (subs at 11 19)) " <" nick "> " text))
                                  msgs)))
        agents (reg/registered-agents)
        agent-summary (str/join "\n"
                        (map (fn [[id a]]
                               (str "  " id " (" (name (or (:agent/type a) :unknown)) ")"
                                    (when-let [ws (:agent/ws-connected? a)] " [ws]")))
                             agents))]
    (str "Current time: " (Instant/now) "\n\n"
         "## Registered agents\n"
         agent-summary "\n\n"
         "## IRC log (last " (count msgs) " messages)\n"
         irc-text "\n\n"
         "What should Tickle do next?")))

(defn tickle-think!
  "Tickle reads state + IRC + GitHub, decides whether to intervene.
   Syncs task state first, then consults LLM.
   Returns {:action :pass|:message, :text string-or-nil}."
  []
  (tickle-task-sync!)
  (let [context (tickle-build-context)]
    (println "[tickle-llm] Thinking...")
    (if-let [response (tickle-llm-call (str tickle-system-prompt "\n\n---\n\n" context))]
      (let [trimmed (str/trim response)
            first-line (first (str/split-lines trimmed))]
        (if (re-find #"(?i)^PASS\b" (str/trim (or first-line "")))
          (do (println "[tickle-llm] PASS") {:action :pass})
          (do (println (str "[tickle-llm] → " first-line))
              {:action :message :text first-line})))
      (do (println "[tickle-llm] LLM call failed")
          {:action :pass}))))

(defn tickle-conduct!
  "Run one Tickle conductor cycle: process done signals, sync state, think, act.
   Done signals are processed first for immediate queue advancement.
   Updates last-nudge-at on tasks that get paged.
   Returns the action taken."
  []
  (let [done (process-done-signals!)
        ;; Deterministic fast path: all tasks merged, nothing queued → assign new batch
        all-merged? (and (seq @!tickle-tasks)
                         (every? #(= :merged (:status %)) (vals @!tickle-tasks))
                         ;; 10-min cooldown on batch assignments
                         (or (nil? @!last-batch-assign)
                             (> (.toMillis (Duration/between @!last-batch-assign (Instant/now)))
                                600000)))
        {:keys [action text]}
        (cond
          ;; Done signals processed — already acted
          (seq done)
          (do (println (str "[tickle-lite] Processed " (count done)
                            " done signal(s), skipping LLM"))
              {:action :done-signal})
          ;; All tasks merged — deterministic new-batch assignment
          all-merged?
          (do (println "[tickle-lite] All tasks merged, assigning new batch")
              (reset! !last-batch-assign (Instant/now))
              {:action :message
               :text (str "@codex-1 All previous entries merged. Next batch: pick 5 new arXiv math.CT"
                          " entries, write .tex files, open ONE PR. Repo: /home/joe/code/18_Category_theory_homological_algebra"
                          " Signal DONE #N :: <pr-url> when ready.")})
          ;; Otherwise consult LLM
          :else
          (tickle-think!))]
    (when (and (= action :message) text)
      (send-irc! "#futon" "tickle-1" text)
      ;; Mark nudged tasks
      (let [now (str (Instant/now))]
        (doseq [[tid task] @!tickle-tasks]
          (when (and (:gh-issue task)
                     (str/includes? (str text) (str "#" (:gh-issue task))))
            (swap! !tickle-tasks assoc-in [tid :last-nudge-at] now)
            (when (= :queued (:status task))
              (swap! !tickle-tasks assoc-in [tid :status] :assigned))))))
    action))

(defn start-tickle-conductor!
  "Start the LLM-backed Tickle conductor loop.
   Runs every interval-ms (default 120000 = 2 min).
   Returns {:stop-fn (fn []) :started-at Instant}."
  ([] (start-tickle-conductor! {}))
  ([{:keys [interval-ms] :or {interval-ms 120000}}]
   (when-let [old @!tickle-conductor]
     ((:stop-fn old))
     (println "[tickle-llm] Stopped previous conductor."))
   (let [running (atom true)
         thread (Thread.
                 (fn []
                   (while @running
                     (try
                       (tickle-conduct!)
                       (catch Exception e
                         (println (str "[tickle-llm] Error: " (.getMessage e)))))
                     (Thread/sleep interval-ms))))]
     (.setDaemon thread true)
     (.start thread)
     (let [handle {:stop-fn #(do (reset! running false) (println "[tickle-llm] Conductor stopped."))
                   :started-at (Instant/now)}]
       (reset! !tickle-conductor handle)
       (println (str "[tickle-llm] Conductor started (interval=" interval-ms "ms)"))
       handle))))

(defn stop-tickle-conductor!
  "Stop the LLM-backed Tickle conductor."
  []
  (when-let [h @!tickle-conductor]
    ((:stop-fn h))
    (reset! !tickle-conductor nil)))

(defn tickle-dashboard
  "Print Tickle's current state for REPL inspection."
  []
  (tickle-task-sync!)
  (let [tasks (sort-by :gh-issue (vals @!tickle-tasks))]
    (println "Tickle task lifecycle:")
    (println "───────────────────────────────────────────────────────")
    (println "  #     Phase              Assignee   Title")
    (println "───────────────────────────────────────────────────────")
    (doseq [{:keys [gh-issue status phase assignee title]} tasks]
      (println (format "  #%-3d  %-18s %-10s %s"
                       (or gh-issue 0)
                       (name (or status :unknown))
                       (or assignee "—")
                       (subs (or title "") 0 (min 45 (count (or title "")))))))
    (println "───────────────────────────────────────────────────────")
    (let [by-status (group-by :status tasks)]
      (println (str "  " (count tasks) " tasks"
                    (when-let [n (seq (get by-status :ready-to-implement))]
                      (str " | " (count n) " ready to implement"))
                    (when-let [n (seq (get by-status :implementing))]
                      (str " | " (count n) " implementing"))
                    (when-let [n (seq (get by-status :merged))]
                      (str " | " (count n) " merged"))
                    " | " (count (tickle-tasks-needing-nudge)) " need nudge")))))

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
     :auto-restart?     — restart stalled agents on escalation (default true)

   Returns the watchdog handle, or nil if evidence store is not available."
  ([] (start-tickle! {}))
  ([opts]
   (when-let [evidence-store @!evidence-store]
     (when-let [old @!tickle]
       (println "[dev] Stopping previous tickle watchdog...")
       ((:stop-fn old)))
     (let [irc-sys @!irc-sys
           send-fn (or (:send-to-channel! (:server irc-sys))
                       (:send-fn opts)
                       ;; Fallback: use bridge HTTP /say (correct per-nick routing)
                       (make-bridge-irc-send-fn))
             auto-restart? (if (contains? opts :auto-restart?)
                             (:auto-restart? opts)
                             true)
             interval-ms (or (:interval-ms opts) 60000)
             threshold-seconds (or (:threshold-seconds opts) 300)
             ;; Scan history for CYDER state-fn and blackboard
             !scan-history (atom {:cycles-completed 0
                                  :last-cycle nil
                                  :recent-history []
                                  :interval-ms interval-ms
                                  :threshold-seconds threshold-seconds})
             config {:evidence-store evidence-store
                     :interval-ms interval-ms
                     :threshold-seconds threshold-seconds
                     ;; Scan all agents except those that are truly unreachable
                     ;; (no invoke-fn, no ws, no IRC relay). Agents on IRC are
                     ;; reachable via @mention dispatch relay even without invoke-fn.
                     :registry-snapshot-fn
                     (fn []
                       (let [skip #{"corpus-1"}] ;; ws-only, no relay
                         (->> (keys @reg/!registry)
                              (remove skip)
                              vec)))
                     :page-config {;; Skip bell (always returns "paged" without actually
                                   ;; reaching the agent). Go straight to IRC, where the
                                   ;; dispatch relay invokes the agent for real.
                                   :ring-test-bell! (constantly {:ok false :error :skip-to-irc})
                                   ;; Wrap send-fn to fix nick: page-agent! passes "tickle-1"
                                   ;; but the bridge knows "tickle". Force the correct nick.
                                   :send-to-channel! (fn [channel _from-nick msg]
                                                       (send-fn channel "tickle" msg))
                                   :room (or (:room opts) "#futon")
                                   :make-page-message
                                   (fn [agent-id]
                                     (str "@" agent-id " you appear stalled — check #futon and #math for current tasks"))}
                     :escalate-config {:notify-fn
                                       (fn [agent-id reason]
                                         ;; 1. Blackboard notification
                                         (bb/blackboard!
                                          "*Tickle*"
                                          (str "ESCALATION\n"
                                               "Agent: " agent-id "\n"
                                               "Reason: " reason "\n"
                                               "Time: " (Instant/now)
                                               (when auto-restart?
                                                 "\nAction: restarting agent layer")))
                                         ;; 2. Emit escalation evidence
                                         (estore/append* evidence-store
                                                         {:subject {:ref/type :agent
                                                                    :ref/id agent-id}
                                                          :type :coordination
                                                          :claim-type :observation
                                                          :author "tickle-1"
                                                          :tags [:tickle :escalation]
                                                          :session-id "tickle-watchdog"
                                                          :body {:event "escalation"
                                                                 :agent-id agent-id
                                                                 :reason (str reason)
                                                                 :auto-restart? auto-restart?
                                                                 :at (str (Instant/now))}})
                                         ;; 3. IRC notification
                                         (when send-fn
                                           (send-fn (or (:room opts) "#futon")
                                                    "tickle-1"
                                                    (str "ESCALATION: " agent-id
                                                         " unresponsive (reason: " reason ")"
                                                         (when auto-restart?
                                                           " — restarting agent layer"))))
                                         ;; 4. Restart agents if enabled
                                         (when auto-restart?
                                           (println (str "[tickle] Restarting agent layer after "
                                                         agent-id " escalation..."))
                                           ;; Use resolve to avoid forward-reference compile error
                                           ;; (start-tickle! is defined before restart-agents!)
                                           (future
                                             (when-let [restart-fn (resolve 'futon3c.dev/restart-agents!)]
                                               (restart-fn)))))}
                     :on-cycle (fn [{:keys [scanned stalled paged escalated] :as cycle-result}]
                                 (let [entry (assoc cycle-result :at (str (Instant/now)))]
                                   ;; Track history (keep last 20 cycles)
                                   (swap! !scan-history
                                          (fn [h]
                                            (-> h
                                                (update :cycles-completed inc)
                                                (assoc :last-cycle entry)
                                                (update :recent-history
                                                        #(vec (take-last 20 (conj % entry)))))))
                                   ;; Project to blackboard
                                   (bb/project! :tickle @!scan-history)
                                   ;; Touch CYDER process
                                   (cyder/touch! "tickle-watchdog")
                                   ;; Console log on stalls
                                   (when (seq stalled)
                                     (println (str "[tickle] stalled: " stalled
                                                   " paged: " paged)))))}
             handle (tickle/start-watchdog! config)]
         (reset! !tickle handle)
         ;; Ensure tickle-1's invoke path matches the current watchdog config.
         (let [tickle-invoke-fn (fn [prompt session-id]
                                  (tickle/invoke! config prompt session-id))]
           (if-not (reg/agent-registered? "tickle-1")
             (rt/register-tickle!
              {:agent-id "tickle-1"
               :invoke-fn tickle-invoke-fn})
             (reg/update-agent! "tickle-1"
                                :agent/type :tickle
                                :agent/invoke-fn tickle-invoke-fn
                                :agent/capabilities [:mission-control :discipline :coordination/execute])))
         ;; Register watchdog with CYDER for inspection
         (cyder/deregister! "tickle-watchdog")
         (cyder/register!
          {:id "tickle-watchdog"
           :type :daemon
           :layer :repl
           :stop-fn (fn []
                      ((:stop-fn handle))
                      (reset! !tickle nil))
           :state-fn (fn [] @!scan-history)
           :step-fn (fn []
                      (let [cycle-result (tickle/run-scan-cycle! config)
                            entry (assoc cycle-result :at (str (Instant/now)))]
                        (swap! !scan-history
                               (fn [h]
                                 (-> h
                                     (update :cycles-completed inc)
                                     (assoc :last-cycle entry)
                                     (update :recent-history
                                             #(vec (take-last 20 (conj % entry)))))))
                        (bb/project! :tickle @!scan-history)
                        cycle-result))
           :metadata {:interval-ms interval-ms
                      :threshold-seconds threshold-seconds
                      :auto-restart? auto-restart?}})
         (println (str "[dev] Tickle started: interval="
                       interval-ms "ms"
                       " threshold=" threshold-seconds "s"
                       " auto-restart=" auto-restart?
                       (when-not send-fn " (no IRC send-fn)")))
         handle))))

(defn stop-tickle!
  "Stop the Tickle watchdog."
  []
  (when-let [handle @!tickle]
    ((:stop-fn handle))
    (reset! !tickle nil)
    (cyder/deregister! "tickle-watchdog")
    (println "[dev] Tickle stopped.")))

;; =============================================================================
;; FM-001 task dispatch — Tickle assigns proof obligations on #math
;; =============================================================================

;; --- FM Conductor thin wrappers (delegates to tickle_orchestrate.clj) ---
;; See src/futon3c/agents/tickle_orchestrate.clj for the real implementation.
;; These wrappers wire dev-specific helpers (IRC read, bridge send) into the
;; orchestrator's config map.

(defn fm-assignable-obligations
  "Find FM-001 ledger obligations that are assignable (open, all deps met).
   Delegates to tickle_orchestrate."
  [problem-id]
  (orch/fm-assignable-obligations problem-id))

(defn fm-dispatch!
  "Have Tickle assign the top FM-001 obligation on #math."
  ([] (fm-dispatch! "FM-001"))
  ([problem-id]
   (let [tasks (fm-assignable-obligations problem-id)]
     (if (empty? tasks)
       (do (println "[tickle-fm] No assignable obligations for " problem-id)
           nil)
       (let [{:item/keys [id label]} (first tasks)
             msg (str "TASK ASSIGNMENT [" problem-id " / " id "]: " label
                      ". Current mode: FALSIFY. "
                      "Who wants to take this? Claim with @tickle I'll take " id)]
         ((make-bridge-irc-send-fn) "#math" "tickle" msg)
         (println (str "[tickle-fm] Assigned " id " on #math"))
         {:assigned id :label label})))))

(defn fm-status!
  "Print FM-001 ledger status for Tickle's view."
  ([] (fm-status! "FM-001"))
  ([problem-id]
   (let [tasks (fm-assignable-obligations problem-id)]
     (println (str "FM-001 assignable obligations (" (count tasks) "):"))
     (doseq [{:item/keys [id label]} tasks]
       (println (str "  " id ": " label))))))

(defn irc-recent-channel
  "Return last N messages from a specific channel."
  [channel n]
  (->> (irc-recent (* n 3))  ;; over-fetch since we're filtering
       (filter #(= channel (:channel %)))
       (take-last n)
       vec))

(defn- refresh-processes-buffer!
  "Re-project the *processes* buffer to all connected Emacs instances."
  []
  (try
    (let [entries (vals @cyder/!processes)]
      (bb/project-processes! entries))
    (catch Exception _ nil)))

(defn project-tickle-state!
  "Project combined tickle state (conductor + watchdog) to the *tickle* blackboard."
  []
  (try
    (let [conductor-handle @!fm-conductor
          conductor-state (when conductor-handle
                            @(:conductor-state conductor-handle))
          watchdog-state (when @!tickle
                            (when-let [p (get @cyder/!processes "tickle-watchdog")]
                              (when-let [sf (:process/state-fn p)]
                                (try (sf) (catch Exception _ nil)))))
          now-ms (System/currentTimeMillis)
          state (cond-> {}
                  conductor-state
                  (assoc :conductor
                         (let [s conductor-state
                               rotation (or (:rotation s) ["claude-1" "codex-1" "claude-2"])
                               idx (or (:idx s) 0)
                               next-agent (nth rotation (mod idx (count rotation)))
                               cooldowns (:last-paged s)
                               cooldown-ms 900000
                               fmt-cd (fn [a]
                                        (let [marker (if (= a next-agent) "▶ " "  ")]
                                          (if-let [ts (get cooldowns a)]
                                            (let [ago-s (quot (- now-ms ts) 1000)
                                                  remaining (- (quot cooldown-ms 1000) ago-s)]
                                              (if (pos? remaining)
                                                (str marker a " → cooldown " remaining "s")
                                                (str marker a " → ready (paged " ago-s "s ago)")))
                                            (str marker a " → ready"))))]
                           (let [base-ms (or (:last-cycle-ms s) (:started-at-ms s))
                                 step-ms-raw (or (some-> (get @cyder/!processes "fm-conductor")
                                                         :process/metadata :step-ms)
                                                 300000)
                                 next-at (when base-ms
                                           (str (.truncatedTo
                                                  (Instant/ofEpochMilli (+ base-ms step-ms-raw))
                                                  java.time.temporal.ChronoUnit/SECONDS)))]
                             (cond-> {:problem-id (or (:problem-id s) "FM-001")
                                      :cycles (:cycles-completed s 0)
                                      :step-ms (str (quot step-ms-raw 1000) "s")
                                      :rotation rotation
                                      :agents (mapv fmt-cd rotation)
                                      :idx idx
                                      :last-cycle (:last-cycle s)}
                               next-at (assoc :next-at next-at)))))
                  watchdog-state (assoc :watchdog watchdog-state))]
      (bb/project! :tickle state))
    (catch Exception _ nil)))

(defn- make-fm-conductor-config
  "Build the config map for tickle_orchestrate FM conductor functions,
   wiring in dev-specific IRC helpers and evidence store."
  [overrides]
  (merge {:problem-id "FM-001"
          :cooldown-ms (* 3 60 1000) ;; 3 min (default was 15 min)
          :irc-read-fn #(irc-recent-channel "#math" 20)
          :bridge-send-fn (make-bridge-irc-send-fn)
          :evidence-store @!evidence-store
          ;; No session-file or session-id-atom — each cycle is a fresh Haiku
          ;; call. build-fm-context provides full context every time, so session
          ;; continuity would only accumulate stale obligation state.
          :invoke-fn (make-claude-invoke-fn
                       {:claude-bin (env "CLAUDE_BIN" "claude")
                        :permission-mode "default"
                        :agent-id "fm-conductor"
                        :model "claude-haiku-4-5-20251001"
                        :timeout-ms 120000})
          :whistle-fn (fn [{:keys [to] :as msg}]
                        (println (str "[fm-conductor] whistle → " to ": " (:reason msg)))
                        (try
                          (let [result (whistles/whistle!
                                         {:agent-id to
                                          :prompt (str "[whistle from tickle-1] " (:reason msg)
                                                       "\nPlease review the proof ledger for " (:problem-id msg)
                                                       " and either unblock existing obligations or create new ones.")
                                          :author "tickle-1"
                                          :timeout-ms 120000
                                          :evidence-store @!evidence-store})]
                            (println (str "[fm-conductor] whistle response: "
                                          (if (:whistle/ok result) "ok" (:whistle/error result))))
                            result)
                          (catch Exception e
                            (println (str "[fm-conductor] whistle delivery failed: " (.getMessage e))))))
          :on-cycle-fn (fn [_result]
                         (cyder/touch! "fm-conductor")
                         (refresh-processes-buffer!)
                         (project-tickle-state!))}
         overrides))

(defonce !fm-conductor (atom nil))
(defonce !post-invoke-hook (atom nil))

(defn fm-conduct-targeted!
  "Run one FM-001 mechanical dispatch targeting a specific agent."
  ([target-agent] (fm-conduct-targeted! "FM-001" target-agent))
  ([problem-id target-agent]
   (fm-dispatch-mechanical! target-agent (make-fm-conductor-config {:problem-id problem-id}))))

(defn fm-conduct!
  "Run one FM-001 conductor cycle — dispatches to all idle workers."
  ([] (fm-conduct! "FM-001"))
  ([problem-id]
   (fm-dispatch-idle-agents! (make-fm-conductor-config {:problem-id problem-id}))))

;; =============================================================================
;; Mechanical conductor — state-driven, no LLM needed for dispatch decisions
;; =============================================================================

(def ^:private fm-agent-nicks
  {"claude-1" "claude" "claude-2" "claude-2" "codex-1" "codex"
   "claude-3" "claude-3" "codex-2" "codex-2" "codex-3" "codex-3"})

(defn- agent-idle?
  "Check if an agent is idle (not currently invoking).
   Returns false for agents not in the registry."
  [agent-id]
  (let [a (get @reg/!registry agent-id)]
    (and (some? a)
         (not= :invoking (:agent/status a)))))

(defn- idle-agents
  "Return agent IDs from the rotation that are currently idle."
  [rotation]
  (filterv agent-idle? rotation))

(defn- fm-dispatch-mechanical!
  "Mechanical FM conductor dispatch. No LLM — just state + obligations.
   If agent is idle and obligations exist, page with a templated message.
   Tracks paged obligations to avoid re-paging the same work."
  [agent-id {:keys [problem-id bridge-send-fn conductor-state cooldown-ms]
             :or {problem-id "FM-001" cooldown-ms (* 3 60 1000)}}]
  (let [conductor-state (or conductor-state (atom {}))
        nick (get fm-agent-nicks agent-id agent-id)]
    (cond
      (not (agent-idle? agent-id))
      {:action :skip :target agent-id}

      (let [last-paged (get-in @conductor-state [:last-paged agent-id])]
        (and last-paged (<= (- (System/currentTimeMillis) last-paged) cooldown-ms)))
      {:action :cooldown :target agent-id}

      :else
      (let [assignable (orch/fm-assignable-obligations problem-id)
            already-paged (get-in @conductor-state [:paged-obligations agent-id] #{})
            fresh (remove #(contains? already-paged (:item/id %)) assignable)]
        (if (empty? fresh)
          {:action :pass :target agent-id}
          (let [ob (first fresh)
                ob-id (:item/id ob)
                ob-label (:item/label ob)
                msg (str "@" nick " " ob-id ": " ob-label
                         ". Push results to git when done.")]
            (println (str "[conductor] " agent-id " → PAGE " ob-id))
            (swap! conductor-state
                   (fn [s]
                     (-> s
                         (assoc-in [:last-paged agent-id] (System/currentTimeMillis))
                         (update-in [:paged-obligations agent-id] (fnil conj #{}) ob-id))))
            (when (fn? bridge-send-fn)
              (bridge-send-fn "#math" "tickle" msg))
            {:action :page :target agent-id :text msg :obligation ob-id}))))))

(defn- fm-dispatch-idle-agents!
  "Scan all agents in rotation, dispatch work to any that are idle."
  [config]
  (let [rotation (or (:rotation config) ["codex-1" "claude-3" "codex-2" "codex-3"])
        idle (idle-agents rotation)]
    (when (seq idle)
      (mapv #(fm-dispatch-mechanical! % config) idle))))

(defn start-fm-conductor!
  "Start the mechanical FM conductor loop.
   Scans all agents each tick — idle + obligations = PAGE.
   No LLM needed for dispatch. Workers only (no Mentor).
   Registers with CYDER (I-6..I-10) for inspectability."
  ([] (start-fm-conductor! {}))
  ([{:keys [step-ms] :or {step-ms 60000}}]
   (when-let [old @!fm-conductor]
     ((:stop-fn old))
     (cyder/deregister! "fm-conductor")
     (println "[fm-conductor] Stopped previous conductor."))
   (let [config (make-fm-conductor-config {:step-ms step-ms})
         conductor-state (atom {:cycles-completed 0
                                :last-cycle nil
                                :started-at-ms (System/currentTimeMillis)})
         config (assoc config :conductor-state conductor-state)
         running (atom true)
         rotation (or (:rotation config) ["codex-1" "claude-3" "codex-2" "codex-3"])
         cooldown-ms (or (:cooldown-ms config) (* 3 60 1000))
         handle {:stop-fn #(reset! running false)
                 :conductor-state conductor-state
                 :running running}]
     (future
       (while @running
         (try
           (Thread/sleep (long step-ms))
           (when @running
             (let [results (fm-dispatch-idle-agents! config)]
               (swap! conductor-state assoc
                      :cycles-completed (inc (or (:cycles-completed @conductor-state) 0))
                      :last-cycle (when (seq results) (last results))
                      :last-cycle-ms (System/currentTimeMillis))
               (cyder/touch! "fm-conductor")
               (refresh-processes-buffer!)
               (project-tickle-state!)))
           (catch Exception e
             (println (str "[fm-conductor] Error: " (.getMessage e)))))))
     (reset! !fm-conductor handle)
     (reset! !post-invoke-hook
       (fn [agent-id]
         (when @running
           (println (str "[post-invoke] " agent-id " idle → mechanical dispatch"))
           (let [result (fm-dispatch-mechanical! agent-id config)]
             (when result
               (swap! conductor-state assoc :last-cycle result :last-cycle-ms (System/currentTimeMillis))
               (cyder/touch! "fm-conductor")
               (refresh-processes-buffer!))))))
     (cyder/deregister! "fm-conductor")
     (cyder/register!
      {:id "fm-conductor"
       :type :daemon
       :layer :repl
       :stop-fn (fn []
                  (reset! running false)
                  (reset! !post-invoke-hook nil)
                  (reset! !fm-conductor nil))
       :state-fn (fn []
                   (let [state @conductor-state
                         now-ms (System/currentTimeMillis)
                         cooldowns (:last-paged state)
                         cycles (or (:cycles-completed state) 0)
                         last-cycle (:last-cycle state)
                         fmt-agent (fn [agent-id]
                                     (let [a (get @reg/!registry agent-id)
                                           status (or (:agent/status a) :unknown)
                                           idle? (not= :invoking status)
                                           cd-ts (get cooldowns agent-id)
                                           cd-remaining (when cd-ts
                                                          (let [r (- cooldown-ms (- now-ms cd-ts))]
                                                            (when (pos? r) r)))]
                                       (str "  " agent-id
                                            " [" (name status) "]"
                                            (if idle?
                                              (if cd-remaining
                                                (str " cooldown " (quot cd-remaining 1000) "s")
                                                " ready")
                                              (when-let [t (:agent/invoke-started-at a)]
                                                (str " for " (quot (- now-ms (.toEpochMilli t)) 1000) "s"))))))]
                     (cond-> {:problem-id (or (:problem-id config) "FM-001")
                              :step-ms (str (quot step-ms 1000) "s")
                              :cooldown (str (quot cooldown-ms 1000) "s")
                              :cycles cycles
                              :agents (mapv fmt-agent rotation)}
                       last-cycle (assoc :last (str (:target last-cycle) " "
                                                    (name (:action last-cycle))
                                                    (when (:text last-cycle)
                                                      (str ": " (subs (:text last-cycle)
                                                                       0 (min 50 (count (:text last-cycle)))))))))))
       :step-fn (fn []
                  (fm-dispatch-idle-agents! (assoc config :cooldown-ms 0)))
       :metadata {:step-ms step-ms
                  :problem-id (or (:problem-id config) "FM-001")}})
     handle)))

(defn stop-fm-conductor!
  "Stop the FM-001 conductor loop."
  []
  (when-let [h @!fm-conductor]
    ((:stop-fn h))
    (reset! !post-invoke-hook nil)
    (reset! !fm-conductor nil)
    (cyder/deregister! "fm-conductor")))

;; =============================================================================
;; Tickle orchestration — REPL helpers (CT work)
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
     :codex-available?  (contains? agent-ids (configured-codex-agent-id))
     :claude-available? (contains? agent-ids "claude-1")
     :evidence-store?   (some? @!evidence-store)
     :irc?              (some? @!irc-sys)}))

(defn tickle-smoke!
  "Run a minimal Tickle smoke test with a synthetic issue.
   Exercises the real invoke path end-to-end: assign → evidence → report.

   Options:
     :agent-id   — agent to invoke (default \"codex-1\")
     :repo-dir   — working directory for the agent (default futon3c)
     :timeout-ms — invoke timeout (default 600000)
     :dry-run?   — if true, just show what would happen without invoking

   Usage:
     (dev/tickle-smoke!)                          ; kick codex-1
     (dev/tickle-smoke! :agent-id \"claude-1\")   ; kick claude-1
     (dev/tickle-smoke! :dry-run? true)           ; preflight only"
  [& {:keys [agent-id repo-dir timeout-ms dry-run?]
      :or {agent-id (configured-codex-agent-id)
           repo-dir "/home/joe/code/futon3c"
           timeout-ms 600000}}]
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

(defn tickle-report!
  "Emit a one-line Tickle status report (IRC + evidence)."
  [& {:keys [room repo-dir now]
      :or {room "#futon"
           repo-dir "/home/joe/code/futon3c"}}]
  (let [store @!evidence-store
        send-fn (or (some-> @!irc-sys :server :send-to-channel!)
                    (make-irc-send-fn "tickle-1"))]
    (if-not store
      (do
        (println "[dev] No evidence store available; cannot emit Tickle report.")
        {:ok false :error :missing-evidence-store})
      (if-let [report-status! (or (ns-resolve 'futon3c.agents.tickle-orchestrate 'report-status!)
                                  (do
                                    (require 'futon3c.agents.tickle-orchestrate :reload)
                                    (ns-resolve 'futon3c.agents.tickle-orchestrate 'report-status!)))]
        (let [result (report-status!
                      {:evidence-store store
                       :send-to-channel! send-fn
                       :room room
                       :repo-dir repo-dir
                       :now now})]
          (println "[dev] Tickle report:" (:message result))
          result)
        (do
          (println "[dev] Tickle report unavailable: futon3c.agents.tickle-orchestrate/report-status! missing")
          {:ok false :error :missing-report-status})))))

;; =============================================================================
;; CT work queue — PlanetMath wiring diagram extraction
;; =============================================================================

(defn ct-progress!
  "Show CT work queue progress: how many of 313 entries have been processed."
  []
  (let [status (ct-queue/queue-status @!evidence-store)]
    (println (str "[ct] Progress: " (:completed status) "/" (:total status)
                  " completed, " (:remaining status) " remaining"))
    status))

(defn run-ct-entry!
  "Process a single CT entity through the extract→review pipeline.
   Uses Codex for extraction, Claude for review.

   Options:
     :entity-id — specific entity to process (default: next unprocessed)
     :agent-id  — extraction agent (default \"codex-1\")
     :timeout-ms — extraction timeout (default 300000 = 5 min)
     :review?   — run Claude review after extraction (default true)
     :review-timeout-ms — review timeout (default 300000 = 5 min)

   Usage:
     (dev/run-ct-entry!)                                    ; next unprocessed
     (dev/run-ct-entry! :entity-id \"pm-ct-FunctorCategory\") ; specific entry"
  [& {:keys [entity-id agent-id timeout-ms review? review-timeout-ms]
      :or {agent-id (configured-codex-agent-id)
           timeout-ms 300000
           review? true
           review-timeout-ms 300000}}]
  (let [evidence-store @!evidence-store
        send-fn (or (some-> @!irc-sys :server :send-to-channel!)
                    (make-irc-send-fn "tickle-1"))
        ;; Find the issue to process
        issue (if entity-id
                ;; Find specific entity
                (let [entities (ct-queue/load-ct-entities)
                      idx (.indexOf (mapv :entity-id entities) entity-id)]
                  (when (>= idx 0)
                    (ct-queue/entity->issue (nth entities idx) idx)))
                ;; Next unprocessed
                (first (ct-queue/next-unprocessed evidence-store 1)))]
    (if-not issue
      (do (println "[ct] No entries to process"
                   (if entity-id (str "(entity " entity-id " not found)") "(queue complete)"))
          {:ok false :error :no-entries})
      (let [session-id (str "ct-" (UUID/randomUUID))
            eid (:entity-id issue)]
        (println (str "[ct] Processing: " eid " — " (:title issue)))
        ;; Emit start evidence
        (ct-queue/emit-ct-evidence! evidence-store
                                    {:entity-id eid
                                     :entity-type (:entity-type issue)
                                     :session-id session-id
                                     :event-tag :workflow-start
                                     :ground-truth (:ground-truth issue)})
        ;; Assign to extraction agent
        (println (str "[ct] Assigning to " agent-id "..."))
        (let [extract-result (orch/assign-issue! issue
                                                  {:evidence-store evidence-store
                                                   :repo-dir "/home/joe/code/futon6"
                                                   :agent-id agent-id
                                                   :timeout-ms timeout-ms
                                                   :session-id session-id})]
          (if-not (:ok extract-result)
            (do
              (println (str "[ct] Extraction failed: " (:error extract-result)))
              (ct-queue/emit-ct-evidence! evidence-store
                                          {:entity-id eid
                                           :entity-type (:entity-type issue)
                                           :session-id session-id
                                           :event-tag :workflow-complete
                                           :ground-truth (:ground-truth issue)})
              {:ok false :entity-id eid :error (:error extract-result)})
            (let [extraction (:result extract-result)]
              (println (str "[ct] Extraction complete (" (:elapsed-ms extract-result) "ms)"))
              (ct-queue/emit-ct-evidence! evidence-store
                                          {:entity-id eid
                                           :entity-type (:entity-type issue)
                                           :session-id session-id
                                           :event-tag :extraction-complete
                                           :extraction-result extraction
                                           :ground-truth (:ground-truth issue)})
              (if-not review?
                ;; No review — done
                (do
                  (ct-queue/emit-ct-evidence! evidence-store
                                              {:entity-id eid
                                               :entity-type (:entity-type issue)
                                               :session-id session-id
                                               :event-tag :workflow-complete
                                               :extraction-result extraction
                                               :ground-truth (:ground-truth issue)})
                  (when send-fn
                    (send-fn "#futon" "tickle-1"
                             (str "CT extracted: " eid " (" (:elapsed-ms extract-result) "ms)")))
                  {:ok true :entity-id eid :status :extracted
                   :elapsed-ms (:elapsed-ms extract-result)})
                ;; Review with Claude
                (let [entities (ct-queue/load-ct-entities)
                      entity (first (filter #(= eid (:entity-id %)) entities))
                      review-prompt (ct-queue/make-review-prompt entity extraction)
                      review-issue {:number (:number issue)
                                    :title (str "CT-review: " (:title issue))
                                    :body review-prompt}]
                  (println "[ct] Requesting Claude review...")
                  (let [review-result (orch/request-review! review-issue extract-result
                                                             {:evidence-store evidence-store
                                                              :repo-dir "/home/joe/code/futon6"
                                                              :timeout-ms review-timeout-ms
                                                              :session-id session-id})
                        verdict (or (:verdict review-result) :unclear)]
                    (println (str "[ct] Review: " (name verdict)
                                  " (" (:elapsed-ms review-result) "ms)"))
                    (ct-queue/emit-ct-evidence! evidence-store
                                                {:entity-id eid
                                                 :entity-type (:entity-type issue)
                                                 :session-id session-id
                                                 :event-tag :workflow-complete
                                                 :extraction-result extraction
                                                 :verdict (name verdict)
                                                 :ground-truth (:ground-truth issue)})
                    (when send-fn
                      (send-fn "#futon" "tickle-1"
                               (str "CT " eid ": " (name verdict)
                                    " (extract " (:elapsed-ms extract-result) "ms"
                                    ", review " (:elapsed-ms review-result) "ms)")))
                    {:ok true :entity-id eid :status :reviewed
                     :verdict verdict
                     :extract-elapsed-ms (:elapsed-ms extract-result)
                     :review-elapsed-ms (:elapsed-ms review-result)}))))))))))

(defn run-ct-batch!
  "Process N CT entries overnight. Resumable — skips already-processed entries.

   Options:
     :n           — max entries to process (default 10)
     :cooldown-ms — pause between entries (default 5000 = 5s)
     :agent-id    — extraction agent (default \"codex-1\")
     :timeout-ms  — per-entry extraction timeout (default 300000 = 5 min)
     :review?     — run Claude review (default true)
     :order       — :asc (quickest first) or :desc (longest first) (default :asc)

   Usage:
     (dev/run-ct-batch!)                          ; 10 entries, quickest first
     (dev/run-ct-batch! :n 50 :order :desc)       ; 50 entries, longest first
     (dev/run-ct-batch! :n 313)                   ; full corpus overnight"
  [& {:keys [n cooldown-ms agent-id timeout-ms review? order]
      :or {n 10 cooldown-ms 5000 agent-id (configured-codex-agent-id)
           timeout-ms 300000 review? true order :asc}}]
  (let [evidence-store @!evidence-store
        issues (ct-queue/next-unprocessed evidence-store n)
        total (count issues)
        start (System/currentTimeMillis)]
    (println (str "[ct-batch] Starting: " total " entries"
                  " (agent=" agent-id
                  " review=" review?
                  " cooldown=" cooldown-ms "ms)"))
    (when (some-> @!irc-sys :server :send-to-channel!)
      (let [send-fn (:send-to-channel! (:server @!irc-sys))]
        (send-fn "#futon" "tickle-1"
                 (str "CT batch starting: " total " entries"))))
    (let [results
          (reduce
           (fn [acc [idx issue]]
             (println (str "\n[ct-batch] " (inc idx) "/" total
                           " — " (:entity-id issue)))
             (let [result (run-ct-entry!
                           :entity-id (:entity-id issue)
                           :agent-id agent-id
                           :timeout-ms timeout-ms
                           :review? review?)]
               (when (and (< (inc idx) total) (pos? cooldown-ms))
                 (Thread/sleep cooldown-ms))
               (conj acc result)))
           []
           (map-indexed vector issues))
          elapsed (- (System/currentTimeMillis) start)
          ok-count (count (filter :ok results))
          fail-count (- total ok-count)]
      (println (str "\n[ct-batch] Complete: " ok-count "/" total " succeeded"
                    " (" fail-count " failed)"
                    " in " (long (/ elapsed 1000)) "s"))
      (when (some-> @!irc-sys :server :send-to-channel!)
        (let [send-fn (:send-to-channel! (:server @!irc-sys))]
          (send-fn "#futon" "tickle-1"
                   (str "CT batch complete: " ok-count "/" total
                        " in " (long (/ elapsed 60000)) "min"))))
      {:total total
       :ok ok-count
       :failed fail-count
       :elapsed-ms elapsed
       :results results})))

;; =============================================================================
;; Mentor peripheral — claude-2 on #math
;; =============================================================================

(defonce !mentor (atom {}))

(defn make-math-irc-read-fn
  "Create an irc-read-fn that pulls #math messages from the IRC log ring buffer.
   Ensures the IRC connection is alive (auto-reconnects if needed).
   Returns a fn that returns all #math messages as [{:nick :text :at}]."
  []
  (fn []
    ;; Ensure we have an IRC connection reading messages
    ;; Nick is read-only listener; all sends go through bridge /say
    (ensure-irc-conn! "listener")
    (->> @!irc-log
         (filter #(= "#math" (:channel %)))
         (mapv #(select-keys % [:nick :text :at])))))

(defn make-math-irc-send-fn
  "Create an irc-send-fn that posts to #math via the bridge.
   Always uses the bridge so the correct nick (tickle, claude-2, etc.) is used."
  []
  (let [bridge-send (make-bridge-irc-send-fn)]
    (fn [channel from-nick message]
      (bridge-send channel from-nick message))))

(defn start-mentor!
  "Start a mentor peripheral with a handle.
   The handle (e.g. 'mentor:FM-001') owns the map — any agent can inhabit it.
   State persists to futon1a; restarts restore the latest checkpoint.

   Options:
     :handle     — mentor handle (default \"mentor:FM-001\")
     :problem-id — FM problem to track (default \"FM-001\")
     :channel    — IRC channel to observe (default \"#math\")
     :agent-id   — agent inhabiting this mentor (default \"claude-2\")"
  [& {:keys [handle problem-id channel agent-id]
      :or {handle "mentor:FM-001"
           problem-id "FM-001"
           channel "#math"
           agent-id "claude-2"}}]
  (let [backend (futon3c.peripheral.tools/make-mock-backend)
        irc-read-fn (make-math-irc-read-fn)
        irc-send-fn (make-math-irc-send-fn)
        peripheral (mentor/make-mentor backend
                     {:irc-read-fn irc-read-fn :irc-send-fn irc-send-fn}
                     handle)
        context {:session-id (str "mentor-" (UUID/randomUUID))
                 :agent-id agent-id
                 :problem-id problem-id
                 :channel channel
                 :evidence-store @!evidence-store}
        start-result (futon3c.peripheral.runner/start peripheral context)]
    (if (:ok start-result)
      (do
        (swap! !mentor assoc handle
               {:peripheral peripheral
                :state (atom (:state start-result))
                :handle handle})
        ;; Register agent with mentor capabilities
        (reg/update-agent! agent-id
                           :agent/type :claude
                           :agent/capabilities [:mentor/observe :mentor/intervene])
        (println (str "[dev] Mentor started: " agent-id " as " handle
                      " on " channel " tracking " problem-id))
        :ok)
      (do
        (println (str "[dev] Mentor start failed: " start-result))
        start-result))))

(defn- resolve-mentor
  "Look up a mentor by handle. Defaults to 'mentor:FM-001'."
  ([] (resolve-mentor "mentor:FM-001"))
  ([handle] (get @!mentor handle)))

(defn mentor-observe!
  "Run one observation cycle: pull new messages, enrich the map.
   Returns the observation result."
  ([] (mentor-observe! "mentor:FM-001"))
  ([handle]
   (when-let [{:keys [peripheral state]} (resolve-mentor handle)]
     (let [result (futon3c.peripheral.runner/step
                    peripheral @state {:tool :mentor-observe :args []})]
       (when (:ok result)
         (reset! state (:state result)))
       (:result result)))))

(defn mentor-evaluate!
  "Evaluate triggers against the conversation map.
   Returns gap analysis and trigger state."
  ([] (mentor-evaluate! "mentor:FM-001"))
  ([handle]
   (when-let [{:keys [peripheral state]} (resolve-mentor handle)]
     (let [result (futon3c.peripheral.runner/step
                    peripheral @state {:tool :mentor-evaluate :args []})]
       (when (:ok result)
         (reset! state (:state result)))
       (:result result)))))

(defn mentor-intervene!
  "Post an intervention when a trigger fires.
   trigger-id: keyword e.g. :QP-1
   message: string to post"
  ([trigger-id message] (mentor-intervene! "mentor:FM-001" trigger-id message))
  ([handle trigger-id message]
   (when-let [{:keys [peripheral state]} (resolve-mentor handle)]
     (let [result (futon3c.peripheral.runner/step
                    peripheral @state {:tool :mentor-intervene :args [trigger-id message]})]
       (when (:ok result)
         (reset! state (:state result)))
       (:result result)))))

(defn mentor-status
  "Get current mentor map summary."
  ([] (mentor-status "mentor:FM-001"))
  ([handle]
   (when-let [{:keys [peripheral state]} (resolve-mentor handle)]
     (let [result (futon3c.peripheral.runner/step
                    peripheral @state {:tool :mentor-status :args []})]
       (when (:ok result)
         (reset! state (:state result)))
       (:result result)))))

(defn mentor-map
  "Get full conversation map for a mentor handle."
  ([] (mentor-map "mentor:FM-001"))
  ([handle]
   (when-let [{:keys [peripheral state]} (resolve-mentor handle)]
     (let [result (futon3c.peripheral.runner/step
                    peripheral @state {:tool :mentor-map :args []})]
       (when (:ok result)
         (reset! state (:state result)))
       (:result result)))))

(defn mentor-handles
  "List all active mentor handles."
  []
  (vec (keys @!mentor)))

(defn stop-mentor!
  "Stop a mentor peripheral by handle. Stops all if no handle given."
  ([] (doseq [h (mentor-handles)] (stop-mentor! h)))
  ([handle]
   (when-let [{:keys [peripheral state]} (resolve-mentor handle)]
     (futon3c.peripheral.runner/stop peripheral @state "session ended")
     (swap! !mentor dissoc handle)
     (println (str "[dev] Mentor stopped: " handle)))))

(defn status
  "Quick runtime status for the REPL."
  []
  {:agents (reg/registered-agents)
   :tickle (when @!tickle {:running true :started-at (:started-at @!tickle)})
   :mentor (when (seq @!mentor)
             {:running true :handles (mentor-handles)})
   :irc (when @!irc-sys {:port (:port @!irc-sys)})
   :evidence-count (when @!evidence-store
                     (count (futon3c.evidence.store/query* @!evidence-store {})))
   :ct-queue (when @!evidence-store
               (let [s (ct-queue/queue-status @!evidence-store)]
                 {:completed (:completed s) :remaining (:remaining s)}))
   :arse-queue (when (and @!evidence-store
                         (.exists (io/file "/home/joe/code/futon6/data/arse-queue/entities.json")))
                (let [s (arse-queue/queue-status @!evidence-store)]
                  {:completed (:completed s) :remaining (:remaining s)
                   :by-problem (:by-problem s)}))})

;; =============================================================================
;; ArSE work queue — Artificial Stack Exchange synthetic QA generation
;; =============================================================================

(defn arse-progress!
  "Show ArSE work queue progress: how many work items have been processed."
  []
  (let [status (arse-queue/queue-status @!evidence-store)]
    (println (str "[arse] Progress: " (:completed status) "/" (:total status)
                  " completed, " (:remaining status) " remaining"))
    (doseq [[p stats] (:by-problem status)]
      (println (str "  P" p ": " (:done stats) "/" (:total stats)
                    " (" (:remaining stats) " remaining)")))
    status))

(defn run-arse-entry!
  "Process a single ArSE work item through the generate→review pipeline.
   Uses Codex for generation, Claude for review.

   Options:
     :entity-id — specific entity to process (default: next unprocessed, highest severity)
     :agent-id  — generation agent (default \"codex-1\")
     :timeout-ms — generation timeout (default 300000 = 5 min)
     :review?   — run Claude review after generation (default true)
     :review-timeout-ms — review timeout (default 300000 = 5 min)

   Usage:
     (dev/run-arse-entry!)                                         ; next unprocessed
     (dev/run-arse-entry! :entity-id \"p7-problem-qa-001\")         ; specific entry"
  [& {:keys [entity-id agent-id timeout-ms review? review-timeout-ms]
      :or {agent-id (configured-codex-agent-id)
           timeout-ms 300000
           review? true
           review-timeout-ms 300000}}]
  (let [evidence-store @!evidence-store
        send-fn (or (some-> @!irc-sys :server :send-to-channel!)
                    (make-irc-send-fn "tickle-1"))
        ;; Find the issue to process
        issue (if entity-id
                ;; Find specific entity (entity-id may be string or keyword)
                (let [entities (arse-queue/load-arse-entities)
                      eid-str (if (keyword? entity-id) (name entity-id) (str entity-id))
                      idx (.indexOf (mapv :entity-id-str entities) eid-str)]
                  (when (>= idx 0)
                    (arse-queue/entity->issue (nth entities idx) idx)))
                ;; Next unprocessed (highest severity)
                (first (arse-queue/next-unprocessed evidence-store 1)))]
    (if-not issue
      (do (println "[arse] No entries to process"
                   (if entity-id (str "(entity " entity-id " not found)") "(queue complete)"))
          {:ok false :error :no-entries})
      (let [session-id (str "arse-" (UUID/randomUUID))
            eid (:entity-id issue)]
        (println (str "[arse] Processing: " eid " — " (:title issue)))
        ;; Emit start evidence
        (arse-queue/emit-arse-evidence! evidence-store
                                      {:entity-id eid
                                       :node-id (:node-id issue)
                                       :problem (:problem issue)
                                       :instance (:instance issue)
                                       :session-id session-id
                                       :event-tag :workflow-start})
        ;; Assign to generation agent
        (println (str "[arse] Assigning to " agent-id "..."))
        (let [gen-result (orch/assign-issue! issue
                                              {:evidence-store evidence-store
                                               :repo-dir "/home/joe/code/futon6"
                                               :agent-id agent-id
                                               :timeout-ms timeout-ms
                                               :session-id session-id})]
          (if-not (:ok gen-result)
            (do
              (println (str "[arse] Generation failed: " (:error gen-result)))
              (arse-queue/emit-arse-evidence! evidence-store
                                            {:entity-id eid
                                             :node-id (:node-id issue)
                                             :problem (:problem issue)
                                             :instance (:instance issue)
                                             :session-id session-id
                                             :event-tag :workflow-complete})
              {:ok false :entity-id eid :error (:error gen-result)})
            (let [generation (:result gen-result)]
              (println (str "[arse] Generation complete (" (:elapsed-ms gen-result) "ms)"))
              (arse-queue/emit-arse-evidence! evidence-store
                                            {:entity-id eid
                                             :node-id (:node-id issue)
                                             :problem (:problem issue)
                                             :instance (:instance issue)
                                             :session-id session-id
                                             :event-tag :generation-complete
                                             :generation-result generation})
              (if-not review?
                ;; No review — done
                (do
                  (arse-queue/emit-arse-evidence! evidence-store
                                                {:entity-id eid
                                                 :node-id (:node-id issue)
                                                 :problem (:problem issue)
                                                 :instance (:instance issue)
                                                 :session-id session-id
                                                 :event-tag :workflow-complete
                                                 :generation-result generation})
                  (when send-fn
                    (send-fn "#futon" "tickle-1"
                             (str "ArSEgenerated: " eid " (" (:elapsed-ms gen-result) "ms)")))
                  {:ok true :entity-id eid :status :generated
                   :elapsed-ms (:elapsed-ms gen-result)})
                ;; Review with Claude
                (let [entities (arse-queue/load-arse-entities)
                      entity (first (filter #(= eid (:entity-id %)) entities))
                      review-prompt (arse-queue/make-review-prompt entity generation)
                      review-issue {:number (:number issue)
                                    :title (str "ArSE-review: " (:title issue))
                                    :body review-prompt}]
                  (println "[arse] Requesting Claude review...")
                  (let [review-result (orch/request-review! review-issue gen-result
                                                             {:evidence-store evidence-store
                                                              :repo-dir "/home/joe/code/futon6"
                                                              :timeout-ms review-timeout-ms
                                                              :session-id session-id})
                        verdict (or (:verdict review-result) :unclear)]
                    (println (str "[arse] Review: " (name verdict)
                                  " (" (:elapsed-ms review-result) "ms)"))
                    (arse-queue/emit-arse-evidence! evidence-store
                                                  {:entity-id eid
                                                   :node-id (:node-id issue)
                                                   :problem (:problem issue)
                                                   :instance (:instance issue)
                                                   :session-id session-id
                                                   :event-tag :workflow-complete
                                                   :generation-result generation
                                                   :verdict (name verdict)})
                    (when send-fn
                      (send-fn "#futon" "tickle-1"
                               (str "ArSE" eid ": " (name verdict)
                                    " (gen " (:elapsed-ms gen-result) "ms"
                                    ", review " (:elapsed-ms review-result) "ms)")))
                    {:ok true :entity-id eid :status :reviewed
                     :verdict verdict
                     :gen-elapsed-ms (:elapsed-ms gen-result)
                     :review-elapsed-ms (:elapsed-ms review-result)}))))))))))

(defn run-arse-batch!
  "Process N ArSE work items overnight. Resumable — skips already-processed entries.
   Work items are processed in gap-severity order (most severe first).

   Options:
     :n           — max items to process (default 10)
     :cooldown-ms — pause between items (default 5000 = 5s)
     :agent-id    — generation agent (default \"codex-1\")
     :timeout-ms  — per-item generation timeout (default 300000 = 5 min)
     :review?     — run Claude review (default true)
     :problem     — filter to specific problem number (default: all)

   Usage:
     (dev/run-arse-batch!)                          ; 10 items, highest severity
     (dev/run-arse-batch! :n 50)                    ; 50 items
     (dev/run-arse-batch! :problem 7 :n 16)         ; all P7 items"
  [& {:keys [n cooldown-ms agent-id timeout-ms review? problem]
      :or {n 10 cooldown-ms 5000 agent-id (configured-codex-agent-id)
           timeout-ms 300000 review? true}}]
  (let [evidence-store @!evidence-store
        all-issues (arse-queue/next-unprocessed evidence-store 1000)
        issues (cond->> all-issues
                 problem (filter #(= problem (:problem %)))
                 true (take n))
        total (count issues)
        start (System/currentTimeMillis)]
    (println (str "[arse-batch] Starting: " total " items"
                  (when problem (str " (P" problem ")"))
                  " (agent=" agent-id
                  " review=" review?
                  " cooldown=" cooldown-ms "ms)"))
    (when (some-> @!irc-sys :server :send-to-channel!)
      (let [send-fn (:send-to-channel! (:server @!irc-sys))]
        (send-fn "#futon" "tickle-1"
                 (str "ArSEbatch starting: " total " items"
                      (when problem (str " (P" problem ")"))))))
    (let [results
          (reduce
           (fn [acc [idx issue]]
             (println (str "\n[arse-batch] " (inc idx) "/" total
                           " — " (:entity-id issue)))
             (let [result (run-arse-entry!
                           :entity-id (:entity-id issue)
                           :agent-id agent-id
                           :timeout-ms timeout-ms
                           :review? review?)]
               (when (and (< (inc idx) total) (pos? cooldown-ms))
                 (Thread/sleep cooldown-ms))
               (conj acc result)))
           []
           (map-indexed vector issues))
          elapsed (- (System/currentTimeMillis) start)
          ok-count (count (filter :ok results))
          fail-count (- total ok-count)]
      (println (str "\n[arse-batch] Complete: " ok-count "/" total " succeeded"
                    " (" fail-count " failed)"
                    " in " (long (/ elapsed 1000)) "s"))
      (when (some-> @!irc-sys :server :send-to-channel!)
        (let [send-fn (:send-to-channel! (:server @!irc-sys))]
          (send-fn "#futon" "tickle-1"
                   (str "ArSEbatch complete: " ok-count "/" total
                        " in " (long (/ elapsed 60000)) "min"))))
      {:total total
       :ok ok-count
       :failed fail-count
       :elapsed-ms elapsed
       :results results})))

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

(defn start-futon5!
  "Start futon5 nonstarter heartbeat API. Returns system map or nil if disabled.
   Port defaults to 7072 (7071 is used by futon1a)."
  []
  (let [port (env-int "FUTON5_PORT" 7072)
        db (env "FUTON5_DB"
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
              ;; Do not clobber dispatch-relay callbacks (e.g. codex/claude mention handlers)
              ;; when a WS connection reconnects.
              ((:join-agent! relay-bridge) agent-id nick "#futon" send-fn {:overwrite? false})
              ;; Remove raw agent-id virtual nick (e.g. codex-1) if it was added earlier.
              (when-let [part-virtual-nick! (:part-virtual-nick! irc-server)]
                (part-virtual-nick! "#futon" (str agent-id)))
              ((:join-virtual-nick! irc-server) "#futon" nick)
              (catch Exception e
                (println (str "[dev] IRC auto-join failed for " agent-id ": " (.getMessage e)))))))))))

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

(defn- codex-fallback-event-summary
  "Best-effort human summary when codex-cli/event->activity is nil."
  [evt]
  (let [evt-type (:type evt)]
    (cond
      (= "thread.started" evt-type)
      (str "thread.started session="
           (or (:thread_id evt) (:session_id evt) "?"))

      (= "turn.failed" evt-type)
      (str "turn.failed "
           (or (get-in evt [:error :message]) "unknown error"))

      (= "error" evt-type)
      (str "error " (or (:message evt) "unknown error"))

      (string? evt-type)
      (str "event " evt-type)

      :else nil)))

(defn- append-trace-entry!
  "Append one trace line while keeping only the most recent max-entries."
  [!trace entry max-entries]
  (swap! !trace
         (fn [entries]
           (let [next (conj entries entry)
                 n (count next)]
             (if (> n max-entries)
               (subvec next (- n max-entries))
               next)))))

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

(defn- codex-agent?
  [agent-id]
  (str/starts-with? (str agent-id) "codex"))

(defn- compact-single-line
  [text max-len]
  (let [s (-> (str (or text ""))
              (str/replace #"\s+" " ")
              str/trim)]
    (if (<= (count s) max-len)
      s
      (str (subs s 0 (max 0 (- max-len 3))) "..."))))

(defn- codex-board-status
  [{:keys [lifecycle-status]}]
  (case lifecycle-status
    :invoking "invoking"
    :resting "resting"
    :failed "resting"
    :done "resting"
    :idle "resting"
    (name (or lifecycle-status :resting))))

(defn- codex-board-phase
  [{:keys [phase lifecycle-status]}]
  (or (some-> phase name)
      (case lifecycle-status
        :invoking "starting"
        :done "completed"
        :failed "failed"
        :idle "resting"
        "resting")))

(defn- codex-terminal-snapshot
  [{:keys [last-terminal last-terminal-status finished-at result-preview error
           invoke-trace-id changed-files execution]}]
  (or last-terminal
      (when (or last-terminal-status
                finished-at
                result-preview
                error
                invoke-trace-id
                changed-files
                execution)
        {:status (or last-terminal-status
                     (when (or finished-at result-preview error invoke-trace-id execution)
                       (if error :failed :done)))
         :finished-at finished-at
         :result-preview result-preview
         :error error
         :invoke-trace-id invoke-trace-id
         :changed-files changed-files
         :execution execution})))

(defn- format-codex-status-board
  [status-map]
  (let [entries (->> status-map
                     (filter (fn [[aid _]] (codex-agent? aid)))
                     (sort-by key))]
    (str "Codex Code\n"
         "==========\n\n"
         (if (seq entries)
           (str/join
            "\n\n"
            (map (fn [[aid {:keys [lifecycle-status phase updated-at started-at session-id
                                   prompt-preview activity trace] :as state}]]
                   (let [status-label (codex-board-status {:lifecycle-status lifecycle-status})
                         phase-label (codex-board-phase {:phase phase
                                                         :lifecycle-status lifecycle-status})
                         terminal (codex-terminal-snapshot state)
                         terminal-status (:status terminal)
                         terminal-execution (:execution terminal)
                         executed? (boolean (or (:executed? terminal-execution)
                                                (:executed terminal-execution)))
                         tool-events (long (or (:tool-events terminal-execution) 0))
                         command-events (long (or (:command-events terminal-execution) 0))
                         enforced-retry? (boolean (:enforced-retry? terminal-execution))
                         trace-lines (seq (take-last 4 (or trace [])))]
                     (str aid "\n"
                          "  Status: " status-label "\n"
                          "  Phase: " phase-label "\n"
                          (when updated-at
                            (str "  Last updated: " updated-at "\n"))
                          (when started-at
                            (str "  Started: " started-at "\n"))
                          (when session-id
                            (str "  Session: " session-id "\n"))
                          (when prompt-preview
                            (str "  Prompt: " (compact-single-line prompt-preview 180) "\n"))
                          (when activity
                            (str "  Detail: " activity "\n"))
                          (when trace-lines
                            (str "  Recent transitions:\n"
                                 (str/join "\n" (map #(str "    " %) trace-lines))
                                 "\n"))
                          (when terminal
                            (str "  Last terminal: "
                                 (or (some-> terminal-status name) "unknown")
                                 (when-let [finished-at (:finished-at terminal)]
                                   (str " at " finished-at))
                                 "\n"
                                 (when-let [invoke-trace-id (:invoke-trace-id terminal)]
                                   (str "  Last trace: " invoke-trace-id "\n"))
                                 (when-let [changed-files (:changed-files terminal)]
                                   (str "  Last files modified: " changed-files "\n"))
                                 "  Last evidence: executed=" executed?
                                 ", tool-events=" tool-events
                                 ", command-events=" command-events
                                 (when enforced-retry? ", enforced-retry=true")
                                 "\n"
                                 (when-let [result-preview (:result-preview terminal)]
                                   (str "  Last outcome: " (compact-single-line result-preview 180) "\n"))
                                 (when-let [error (:error terminal)]
                                   (str "  Last error: " (compact-single-line error 180) "\n")))))))
                 entries))
           "No Codex invokes recorded yet.\n"))))

(defn- project-codex-status!
  []
  (try
    (bb/blackboard! "*Codex Code*"
                    (format-codex-status-board @!codex-status)
                    {:width 72 :slot 2 :no-display true})
    (catch Throwable _ nil)))

(defn- update-codex-status!
  [agent-id updates]
  (when (codex-agent? agent-id)
    (let [aid (str agent-id)]
      (swap! !codex-status
             (fn [m]
               (update m aid
                       (fn [prev]
                         (merge {:agent-id aid}
                                prev
                                updates
                                {:updated-at (str (Instant/now))})))))
      (project-codex-status!))))

(defn- bell-tickle-available!
  "Notify mechanical conductor that an agent is now available.
   Uses the post-invoke hook (if set) instead of invoking tickle-1's Haiku.
   The hook does mechanical dispatch: idle + obligations = PAGE."
  [agent-id {:keys [ok? session-id invoke-trace-id]}]
  ;; The post-invoke hook is set by start-fm-conductor! and does the right thing.
  ;; We do NOT invoke tickle-1's Haiku here — that caused feedback loops where
  ;; codex output @tickle → tickle invoke → more IRC noise.
  (when-let [hook @!post-invoke-hook]
    (future
      (try
        (hook (str agent-id))
        (catch Throwable t
          (println (str "[bell] post-invoke hook error for " agent-id ": "
                        (.getMessage t)))
          (flush))))))

(defn- start-invoke-ticker!
  "Start a background thread that updates both *agents* and the invoke buffer
   with elapsed time, file change detection, and a progress spinner.
   Also emits evidence heartbeats every 30s for long-running invocations.
   Returns a function that stops the ticker when called."
  [buf-name agent-id prompt-str used-sid interval-ms & {:keys [bb-opts event-trace]}]
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
                            trace-entries (when event-trace
                                            (let [entries @event-trace]
                                              (when (seq entries)
                                                (take-last 20 entries))))
                            trace-lines (when (seq trace-entries)
                                          (str/join "\n" trace-entries))
                            status-line (cond
                                          activity
                                          (str "\nWorking... (" activity ")")

                                          (seq trace-entries)
                                          "\nReceiving stream events..."

                                          :else
                                          "\nWaiting for response...")
                            backpack (some-> (get @reg/!registry aid-val)
                                              :agent/metadata :backpack seq)
                            content (str "Invoke: " agent-id " " spin " " elapsed-str "\n"
                                         "Session: " used-sid "\n"
                                         (when (seq backpack)
                                           (str "Patterns: "
                                                (str/join " " (map (fn [{:keys [sigil pattern]}]
                                                                     (str "[" sigil "] " pattern))
                                                                   backpack))
                                                "\n"))
                                         "Prompt: " (subs prompt-str 0 (min 300 (count prompt-str)))
                                         (when (> (count prompt-str) 300) "...")
                                         "\n\n"
                                         (when activity
                                           (str "Activity: " activity "\n"))
                                         (when changed-files
                                           (str "Files modified: " changed-files "\n"))
                                         (when trace-lines
                                           (str "\n--- trace ---\n" trace-lines "\n"))
                                         status-line)]
                        ;; Update invoke buffer
                        ;; Keep invoke output separate from *agents* in side-window slot 1.
                        (bb/blackboard! buf-name content (merge {:width 80 :slot 1 :no-display true} bb-opts))
                        (update-codex-status!
                         agent-id
                         {:lifecycle-status :invoking
                          :phase (cond
                                   activity :executing
                                   (seq trace-entries) :streaming
                                   :else :awaiting-response)
                          :session-id used-sid
                          :prompt-preview prompt-preview
                          :started-at (str (Instant/ofEpochMilli start-ms))
                          :elapsed-ms elapsed
                          :activity activity
                          :changed-files changed-files
                          :trace (vec (or trace-entries []))})
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

(defn- start-agents-blackboard-ticker!
  "Keep *agents* aligned with the current registry view, including polled
   external state such as ProcessHandle-based Codex detection and expiring
   external invoke heartbeats."
  ([] (start-agents-blackboard-ticker! 5000))
  ([interval-ms]
   (when-let [stop-fn @!agents-blackboard-ticker-stop]
     (try
       (stop-fn)
       (catch Throwable _)))
   (let [running (atom true)
         thread (Thread.
                 (fn []
                   (while @running
                     (try
                       (Thread/sleep interval-ms)
                       (bb/project-agents! (reg/registry-status))
                       (catch InterruptedException _
                         (reset! running false))
                       (catch Throwable _))))
                 "agents-blackboard-ticker")
         stop-fn (fn []
                   (reset! running false)
                   (.interrupt thread))]
     (.setDaemon thread true)
     (.start thread)
     (reset! !agents-blackboard-ticker-stop stop-fn)
     stop-fn)))

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
                         shorter timeout (120s) via invoke-timeout-ms.
     :model            — Claude model override (e.g. \"claude-haiku-4-5-20251001\").
                         When nil, uses the CLI default."
  [{:keys [claude-bin permission-mode agent-id session-file session-id-atom timeout-ms emacs-socket model]
    :or {claude-bin "claude" permission-mode "bypassPermissions" agent-id "claude"
         timeout-ms 1800000}}]
  (let [!lock (Object.)
        buf-name (str "*invoke: " agent-id "*")
        bb-opts (cond-> {} emacs-socket (assoc :emacs-socket emacs-socket))]
    (fn [prompt session-id]
      (locking !lock
        (let [prompt-str (cond
                           (string? prompt) prompt
                           (map? prompt)    (or (:prompt prompt) (:text prompt)
                                                (json/generate-string prompt))
                           :else            (str prompt))
              new-sid (when-not session-id (str (UUID/randomUUID)))
              args (cond-> [claude-bin "-p"
                            "--permission-mode" permission-mode
                            "--output-format" "stream-json" "--verbose"]
                     model      (into ["--model" model])
                     session-id (into ["--resume" (str session-id)])
                     new-sid    (into ["--session-id" new-sid])
                     :always    (into ["--" prompt-str]))
              used-sid (or session-id new-sid)
              invoke-trace-id (str "invoke-" (UUID/randomUUID))
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
                                  (merge {:width 80 :slot 1} bb-opts))
                  (catch Throwable _))
              ;; Start ticker: updates invoke buffer + agents buffer every 5s
              ;; Also emits evidence heartbeats every 30s
              stop-ticker! (start-invoke-ticker! buf-name agent-id prompt-str used-sid 5000 :bb-opts bb-opts)
              ;; Launch process with ProcessBuilder
              pb (doto (ProcessBuilder. ^java.util.List (vec args))
                    (.redirectInput (java.lang.ProcessBuilder$Redirect/from (java.io.File. "/dev/null"))))
              proc (.start pb)
              ;; Drain stderr in background (prevents buffer blocking)
              stderr-future (future (drain-stream! (.getErrorStream proc)))
              ;; Parse stream-json stdout line by line
              ;; Only keep text from the LAST assistant turn (not intermediate
              ;; process notes between tool uses). Reset on each new text-only
              ;; assistant message so the final result is the actual answer.
              text-acc (StringBuilder.)
              last-had-tools? (atom false)
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
                                              ;; Only keep text from the last response turn.
                                              ;; When a text-only message arrives after a tool-use
                                              ;; turn, clear the accumulator — that's a new response.
                                              (when (and text (not (str/blank? text)))
                                                (when (and (not tools) @last-had-tools?)
                                                  (.setLength text-acc 0))
                                                (.append text-acc text))
                                              (reset! last-had-tools? (boolean tools))
                                              ;; Emit to streaming event sink (if any)
                                              (when-let [get-sink (ns-resolve 'futon3c.agency.registry
                                                                              'get-invoke-event-sink)]
                                                (when-let [sink (get-sink aid-val)]
                                                  (try
                                                    (when tools
                                                      (sink {:type "tool_use" :tools (vec tools)}))
                                                    (when (and text (not (str/blank? text)))
                                                      (sink {:type "text" :text text}))
                                                    (catch Throwable _)))))
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
                                     (invoke-trace-response-block agent-id final-sid invoke-trace-id text))
                                (merge {:width 80 :slot 1 :no-display true} bb-opts))
                (catch Throwable _))
              (println (str "[invoke] " agent-id " exit=" exit
                            " text-len=" (count (or text ""))
                            " err-len=" (count (or err ""))))
              (flush)
              (if ok?
                {:result (if (str/blank? text)
                           "[Claude used tools but produced no text response]"
                           text)
                 :session-id final-sid
                 :invoke-trace-id invoke-trace-id}
                {:result nil :session-id final-sid
                 :error (str "Exit " exit ": " (str/trim (or err "")))
                 :invoke-trace-id invoke-trace-id}))
            (finally
              (stop-ticker!))))))))

(def ^:private codex-work-claim-re
  #"(?i)\b(i['’]?ll|i will|we['’]?ll|we will|claiming|i claim|taking|i(?:'m| am) taking|proceeding|starting|kicking off|working on|i(?:'m| am) on it)\b")

(def ^:private codex-planning-only-re
  #"(?i)\b(planning-only|not started|need clarification|need more context|cannot execute yet|blocked)\b")

(def ^:private codex-separate-message-request-re
  #"(?i)\b(separate\s+irc\s+messages?|one\s+per\s+message|each\s+of\s+them\s+in\s+a\s+separate\s+irc\s+message|post\s+each\s+.*\s+separate\s+message)\b")

(def ^:private codex-format-excuse-re
  #"(?i)\b(surface|interface|mode|channel|irc)\b.{0,60}\b(cap|caps|limit|limited|can't|cannot|unable)\b|\b\d+\s+lines?\s+per\s+turn\b")

(defn- codex-task-mode-prompt?
  "True when prompt came from IRC bridge task-mode envelope."
  [prompt]
  (boolean (re-find #"(?i)\bmode:\s*task\b" (str (or prompt "")))))

(defn- codex-mission-work-prompt?
  "True when prompt is a mission/work request that should require execution evidence.
   Keeps this enforcement in the invoke engine, independent of bridge classification."
  [prompt]
  (boolean
   (re-find #"(?i)\b(task assignment|fm-\d{3}|falsify|prove|counterexample|state of play)\b"
            (str (or prompt "")))))

(defn- codex-no-execution-evidence?
  "True when invoke result reports no executed/tool/command evidence."
  [result]
  (let [execution (:execution result)
        executed? (boolean (:executed? execution))
        tool-events (long (or (:tool-events execution) 0))
        command-events (long (or (:command-events execution) 0))]
    (and (nil? (:error result))
         (not executed?)
         (zero? tool-events)
         (zero? command-events))))

(defn- codex-planning-only-text?
  "True when response explicitly declares planning-only / blocked state."
  [text]
  (let [t (str/trim (or text ""))]
    (and (not (str/blank? t))
         (boolean (re-find codex-planning-only-re t)))))

(defn- codex-work-claim-without-execution?
  "True when Codex returned work/progress claim text with no execution evidence."
  [result]
  (let [text (str/trim (or (:result result) ""))]
    (and (codex-no-execution-evidence? result)
         (not (str/blank? text))
         (boolean (re-find codex-work-claim-re text)))))

(defn- codex-task-reply-without-execution?
  "True when execution-required reply has no execution evidence and is not planning-only.
   This prevents non-evidenced 'done' text from being emitted for work turns."
  [prompt result]
  (let [text (str/trim (or (:result result) ""))]
    (and (or (codex-task-mode-prompt? prompt)
             (codex-mission-work-prompt? prompt))
         (codex-no-execution-evidence? result)
         (not (str/blank? text))
         (not (codex-planning-only-text? text)))))

(defn- codex-separate-message-request?
  "True when caller asked for separate multi-message IRC output."
  [prompt]
  (boolean (re-find codex-separate-message-request-re (str (or prompt "")))))

(defn- codex-format-excuse-text?
  "True when reply refuses feasible formatting due invented surface limits."
  [text]
  (let [t (str/trim (or text ""))]
    (and (not (str/blank? t))
         (boolean (re-find codex-format-excuse-re t)))))

(defn- codex-format-refusal?
  "True when reply refuses separate-message request using a transport/format excuse."
  [prompt result]
  (let [text (str/trim (or (:result result) ""))]
    (and (codex-separate-message-request? prompt)
         (not (str/blank? text))
         (codex-format-excuse-text? text))))

(defn- codex-execution-followup-prompt
  "Prompt used when Codex claimed work without execution evidence.
   Keeps the invariant at the invoke engine (not transport bridge)."
  [original-prompt prior-reply]
  (letfn [(clip [s max-len]
            (let [txt (str (or s ""))]
              (if (<= (count txt) max-len)
                txt
                (str (subs txt 0 (max 0 (- max-len 3))) "..."))))]
    (str "Your previous reply made a work/progress claim without execution evidence.\n"
         "Execute one concrete first step now (tool/command activity is required).\n"
         "Then reply in one short line with actual status and artifact refs.\n\n"
         "Original request:\n"
         (clip original-prompt 900)
         "\n\nPrevious reply:\n"
         (clip prior-reply 600))))

(defn- codex-format-followup-prompt
  "Prompt used when Codex refuses feasible output formatting with invented limits."
  [original-prompt prior-reply]
  (letfn [(clip [s max-len]
            (let [txt (str (or s ""))]
              (if (<= (count txt) max-len)
                txt
                (str (subs txt 0 (max 0 (- max-len 3))) "..."))))]
    (str "Your previous reply refused a feasible output format due invented surface limits.\n"
         "Do not claim per-turn line caps or transport limits unless an actual tool call failed.\n"
         "Complete the user request directly.\n"
         "If they asked for separate IRC messages, output newline-separated one-line items,\n"
         "one intended post per line.\n\n"
         "Original request:\n"
         (clip original-prompt 900)
         "\n\nPrevious reply:\n"
         (clip prior-reply 600))))

(defn make-codex-invoke-fn
  "Create an invoke-fn that calls `codex exec` for real Codex interaction.

   invoke-fn contract: (fn [prompt session-id] -> {:result str :session-id str})

   Wraps codex-cli/make-invoke-fn with evidence emission and blackboard updates.

   opts:
     :codex-bin          — path to codex CLI (default \"codex\")
     :model              — model name (default \"gpt-5-codex\")
     :sandbox            — sandbox mode (default \"danger-full-access\")
     :approval-policy    — approval policy (default \"never\")
     :reasoning-effort   — override reasoning effort (optional)
     :timeout-ms         — hard timeout for codex process (default 1800000)
     :cwd                — working directory (default user.dir)
     :agent-id           — agent identifier (default \"codex\")
     :session-file       — path to session ID file for persistence (optional)
     :session-id-atom    — atom holding current session ID (optional)"
  [{:keys [codex-bin model sandbox approval-policy reasoning-effort timeout-ms cwd agent-id
           session-file session-id-atom]
    :or {codex-bin "codex" model "gpt-5-codex" sandbox "danger-full-access"
        approval-policy "never" timeout-ms 1800000 agent-id "codex"}}]
  (let [aid-val (str agent-id)
        update-activity! (ns-resolve 'futon3c.agency.registry 'update-invoke-activity!)
        get-event-sink (ns-resolve 'futon3c.agency.registry 'get-invoke-event-sink)
        !event-trace (atom [])
        !invoke-start-ms (atom (System/currentTimeMillis))
        on-event (fn [evt]
                   (let [activity (codex-cli/event->activity evt)
                         summary (or activity (codex-fallback-event-summary evt))]
                     (when summary
                       (try
                         (let [ts (format-elapsed (- (System/currentTimeMillis) @!invoke-start-ms))]
                           (append-trace-entry! !event-trace (str ts " " summary) 200))
                         (catch Throwable _)))
                     (when (and update-activity! activity)
                       (try
                         (update-activity! aid-val activity)
                         (catch Throwable _)))
                     ;; Mirror Codex stream events to any active HTTP stream sink.
                     (when get-event-sink
                       (when-let [sink (get-event-sink aid-val)]
                         (try
                           (sink evt)
                           (catch Throwable _))))))
        inner-fn (codex-cli/make-invoke-fn {:codex-bin codex-bin
                                             :model model
                                             :sandbox sandbox
                                             :approval-policy approval-policy
                                             :reasoning-effort reasoning-effort
                                             :timeout-ms timeout-ms
                                             :cwd cwd
                                             :on-event on-event})
        buf-name (str "*invoke: " agent-id "*")]
    (fn [prompt session-id]
      (let [prompt-str (cond
                         (string? prompt) prompt
                         (map? prompt)    (or (:prompt prompt) (:text prompt)
                                              (json/generate-string prompt))
                         :else            (str prompt))
            prompt-preview (subs prompt-str 0 (min 200 (count prompt-str)))
            invoke-trace-id (str "invoke-" (UUID/randomUUID))
            invoke-sid (preferred-session-id session-file session-id session-id-atom)
            used-sid (or invoke-sid "new")]
        ;; Reset trace for this invocation
        (reset! !event-trace [])
        (reset! !invoke-start-ms (System/currentTimeMillis))
        (println (str "[invoke] " agent-id " codex exec "
                      (subs prompt-str 0 (min 80 (count prompt-str)))
                      "... (session: " (when invoke-sid (subs invoke-sid 0 (min 8 (count invoke-sid)))) ")"))
        (flush)
        (update-codex-status!
         agent-id
         {:lifecycle-status :invoking
          :phase :starting
          :session-id used-sid
          :prompt-preview prompt-preview
          :started-at (str (Instant/now))
          :finished-at nil
          :result-preview nil
          :error nil
          :invoke-trace-id nil
          :execution nil
          :changed-files nil
          :activity "starting"
          :trace []})
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
        ;; Start ticker with evidence heartbeats + event trace
        (let [stop-ticker! (start-invoke-ticker! buf-name agent-id prompt-str used-sid 5000
                                                 :event-trace !event-trace)
              result (try
                       (let [initial (inner-fn prompt invoke-sid)]
                         (if (or (codex-work-claim-without-execution? initial)
                                 (codex-task-reply-without-execution? prompt-str initial)
                                 (codex-format-refusal? prompt-str initial))
                           (let [exec-enforce? (or (codex-work-claim-without-execution? initial)
                                                   (codex-task-reply-without-execution? prompt-str initial))
                                 format-enforce? (codex-format-refusal? prompt-str initial)
                                 retry-prompt (if exec-enforce?
                                                (codex-execution-followup-prompt prompt-str (:result initial))
                                                (codex-format-followup-prompt prompt-str (:result initial)))
                                 retry-sid (or (:session-id initial) invoke-sid)]
                             (println (str "[invoke] " agent-id
                                           (if exec-enforce?
                                             " claimed work without execution evidence; retrying with enforcement prompt"
                                             " refused feasible output format; retrying with enforcement prompt")))
                             (flush)
                             (update-codex-status!
                              agent-id
                              {:lifecycle-status :invoking
                               :phase (if exec-enforce? :enforcement-retry :format-retry)
                               :activity (if exec-enforce?
                                           "retrying after no execution evidence"
                                           "retrying after format refusal")
                               :trace (vec @!event-trace)})
                             (let [retry (inner-fn retry-prompt retry-sid)]
                                (if (or (codex-work-claim-without-execution? retry)
                                        (codex-task-reply-without-execution? prompt-str retry)
                                        (codex-format-refusal? prompt-str retry))
                                 {:result nil
                                  :session-id (or (:session-id retry) retry-sid used-sid)
                                  :execution (assoc (or (:execution retry) {})
                                                    :enforced-retry? true
                                                    :executed? false
                                                    :tool-events (long (or (:tool-events (:execution retry)) 0))
                                                    :command-events (long (or (:command-events (:execution retry)) 0)))
                                  :error (if exec-enforce?
                                           "work-claim without execution evidence after enforcement retry"
                                           "format-refusal after enforcement retry")}
                                 (update retry :execution #(assoc (or % {}) :enforced-retry? true)))))
                           initial))
                       (finally
                         (stop-ticker!)))
              final-sid (:session-id result)
              execution (:execution result)
              tool-events (long (or (:tool-events execution) 0))
              command-events (long (or (:command-events execution) 0))
              execution-evidence? (boolean (:executed? execution))
              ok? (nil? (:error result))
              finished-at (str (Instant/now))]
          ;; Persist session ID
          (when (and session-file final-sid (not (str/blank? final-sid)))
            (persist-session-id! session-file final-sid))
          (when (and session-id-atom final-sid (not (str/blank? final-sid)))
            (reset! session-id-atom final-sid))
          ;; Evidence: invoke complete
          (emit-invoke-evidence! agent-id "invoke-complete"
                                 {"ok" ok?
                                  "execution-evidence" execution-evidence?
                                  "tool-events" tool-events
                                  "command-events" command-events
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
                                 "Runtime evidence: executed=" execution-evidence?
                                 ", tool-events=" tool-events
                                 ", command-events=" command-events "\n"
                                 (invoke-trace-response-block agent-id final-sid invoke-trace-id (:result result)))
                            {:width 80 :slot 1 :no-display true})
            (catch Throwable _))
          (update-codex-status!
           agent-id
           {:lifecycle-status :resting
            :phase (if ok? :completed :failed)
            :last-terminal-status (if ok? :done :failed)
            :last-terminal {:status (if ok? :done :failed)
                            :finished-at finished-at
                            :result-preview (when ok? (:result result))
                            :error (:error result)
                            :execution execution
                            :invoke-trace-id invoke-trace-id
                            :changed-files (detect-file-changes @!invoke-start-ms)}
            :session-id final-sid
            :finished-at finished-at
            :activity nil
            :result-preview (when ok? (:result result))
            :error (:error result)
            :execution execution
            :invoke-trace-id invoke-trace-id
            :changed-files (detect-file-changes @!invoke-start-ms)
            :trace (vec @!event-trace)})
          (bell-tickle-available! agent-id {:ok? ok?
                                            :session-id final-sid
                                            :invoke-trace-id invoke-trace-id})
          (println (str "[invoke] " agent-id
                        (if ok? " ok" (str " error: " (:error result)))
                        " result-len=" (count (or (:result result) ""))
                        " execution-evidence=" execution-evidence?
                        " tool-events=" tool-events
                        " command-events=" command-events))
          (flush)
          (assoc result :invoke-trace-id invoke-trace-id))))))

;; =============================================================================
;; IRC-based Codex invoke — send @codex on IRC, poll for [done] response
;; =============================================================================

(defn make-irc-codex-invoke-fn
  "Create an invoke-fn that sends prompts to Codex via IRC @codex mention.

   Codex's multipass peripheral posts:
     [accepted codex-TIMESTAMP-N] queued ...
     [done codex-TIMESTAMP-N] <response> (session XXXXXX)

   This fn sends the prompt, then polls !irc-log for the [done] message.

   opts:
     :channel     — IRC channel (default \"#futon\")
     :from-nick   — nick to send as (default \"tickle-1\")
     :poll-ms     — poll interval in ms (default 3000)
     :timeout-ms  — max wait for response (default 1800000 = 30 min)"
  [{:keys [channel from-nick poll-ms timeout-ms]
    :or {channel "#futon" from-nick "tickle-1" poll-ms 3000 timeout-ms 1800000}}]
  (fn [prompt _session-id]
    (let [prompt-str (cond
                       (string? prompt) prompt
                       (map? prompt) (or (:prompt prompt) (:text prompt) (str prompt))
                       :else (str prompt))
          ;; Snapshot log position before sending
          log-pos (count @!irc-log)
          ;; Send @codex prompt via IRC
          prompt-lines (str/split-lines prompt-str)
          first-line (str "@codex " (first prompt-lines))
          _ (send-irc! channel from-nick first-line)
          ;; Send remaining lines (if multi-line prompt)
          _ (doseq [line (rest prompt-lines)]
              (send-irc! channel from-nick line))
          start-ms (System/currentTimeMillis)
          deadline-ms (+ start-ms timeout-ms)]
      (println (str "[irc-invoke] Sent @codex prompt (" (count prompt-str) " chars) to " channel))
      (flush)
      ;; Poll !irc-log for [done ...] response from codex
      (loop []
        (let [now-ms (System/currentTimeMillis)]
          (if (> now-ms deadline-ms)
            {:error (str "IRC invoke timeout after " timeout-ms "ms")
             :exit-code -1
             :timeout-ms timeout-ms}
            (let [log @!irc-log
                  ;; Look at messages after our send
                  new-msgs (subvec log (min log-pos (count log)))
                  codex-msgs (filter #(= "codex" (:nick %)) new-msgs)
                  ;; Find [done ...] message
                  done-msg (first (filter #(re-find #"^\[done " (:text %)) codex-msgs))]
              (if done-msg
                ;; Parse response: [done codex-ID] <response> (session XXXX)
                (let [text (:text done-msg)
                      ;; Strip [done codex-ID] prefix
                      response (str/replace text #"^\[done [^\]]+\]\s*" "")
                      ;; Extract session ID if present
                      session-match (re-find #"\(session ([0-9a-f]+)\)\s*$" response)
                      session-id (second session-match)
                      ;; Strip (session ...) suffix and artifact refs noise
                      clean-response (-> response
                                         (str/replace #"\s*\(no artifact refs\)\s*$" "")
                                         (str/replace #"\s*\(session [0-9a-f]+\)\s*$" "")
                                         str/trim)
                      ;; Also collect any continuation lines between accepted and done
                      accepted-idx (some (fn [[i m]]
                                           (when (and (= "codex" (:nick m))
                                                      (re-find #"^\[accepted " (:text m)))
                                             i))
                                         (map-indexed vector new-msgs))
                      done-idx (some (fn [[i m]]
                                       (when (and (= "codex" (:nick m))
                                                  (re-find #"^\[done " (:text m)))
                                         i))
                                     (map-indexed vector new-msgs))
                      ;; Gather intermediate codex lines (between accepted and done)
                      intermediate (when (and accepted-idx done-idx (< accepted-idx done-idx))
                                     (->> (subvec (vec new-msgs) (inc accepted-idx) done-idx)
                                          (filter #(= "codex" (:nick %)))
                                          (mapv :text)))
                      full-response (if (seq intermediate)
                                      (str (str/join "\n" intermediate) "\n" clean-response)
                                      clean-response)
                      elapsed (- now-ms start-ms)]
                  (println (str "[irc-invoke] Got [done] after " elapsed "ms"
                                " (" (count full-response) " chars)"))
                  (flush)
                  {:result full-response
                   :session-id session-id})
                ;; Not done yet — check for [accepted] to confirm receipt
                (do
                  (Thread/sleep poll-ms)
                  (recur))))))))))

;; =============================================================================
;; Dispatch-based IRC relay — routes through invoke-agent! (I-1, I-2 compliant)
;; =============================================================================

;; Set of IRC nicks that receive all messages without @mention gating.
;; Toggle with !ungate <nick> and !gate <nick> in IRC.
(defonce ^:private !ungated-nicks (atom #{}))

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

(def ^:private irc-progress-promise-re
  #"(?i)\b(i['’]?ll|i will|we['’]?ll|we will|kicking off|starting (?:now|right away|immediately)|about to|will push|will open|will send|in the next few|soon)\b")

(def ^:private irc-artifact-ref-re
  #"(?ix)
    (https?://github\.com/\S+/(?:pull|issues)/\d+)
    |
    (\bPR\s*\#\d+\b)
    |
    (\b[0-9a-f]{7,40}\b)
    |
    ((?:/|\.{1,2}/|~?/)[^\s]+?\.(?:clj|cljs|cljc|el|md|txt|sh|py|js|ts|tsx|java|go|rs|tex|json|edn)\b)")

(def ^:private irc-summary-max-chars 220)
(def ^:private irc-summary-hard-limit 320)
(def ^:private irc-summary-max-refs 3)

(def ^:private irc-ref-github-re
  #"(?i)https?://github\.com/\S+/(?:pull|issues)/\d+")

(def ^:private irc-ref-pr-re
  #"(?i)\bPR\s*\#\d+\b")

(def ^:private irc-ref-commit-re
  #"(?i)\b(?:commit|sha)\s*[:#]?\s*([0-9a-f]{7,40})\b")

(def ^:private irc-ref-path-re
  #"(?i)(?:/|\.{1,2}/|~?/)[^\s]+?\.(?:clj|cljs|cljc|el|md|txt|sh|py|js|ts|tsx|java|go|rs|tex|json|edn)\b")

(defn- surface-safe-local-paths
  "Normalize local filesystem prefixes before projecting text to IRC."
  [text]
  (let [raw (str (or text ""))
        home (some-> (System/getProperty "user.home") str str/trim not-empty)
        replacements (cond-> []
                       home (conj [(str home "/code/") "~/code/"])
                       home (conj [(str home "/") "~/"]))]
    (reduce (fn [s [prefix replacement]]
              (str/replace s (re-pattern (java.util.regex.Pattern/quote prefix)) replacement))
            raw
            replacements)))

(defn- truncate-with-ellipsis
  [s max-len]
  (let [txt (str (or s ""))]
    (if (<= (count txt) max-len)
      txt
      (str (subs txt 0 (max 0 (- max-len 3))) "..."))))

(defn- extract-artifact-refs
  [text]
  (let [raw (surface-safe-local-paths text)
        github-refs (re-seq irc-ref-github-re raw)
        pr-refs (re-seq irc-ref-pr-re raw)
        commit-refs (map second (re-seq irc-ref-commit-re raw))
        path-refs (re-seq irc-ref-path-re raw)]
    (->> (concat github-refs pr-refs commit-refs path-refs)
         (map str/trim)
         (remove str/blank?)
         distinct
         (take irc-summary-max-refs)
         vec)))

(defn- summarize-irc-result
  "Render agent text as a short IRC-friendly line with artifact refs."
  [text]
  (let [raw (surface-safe-local-paths text)
        normalized (-> raw
                       (str/replace #"\s+" " ")
                       str/trim)
        refs (extract-artifact-refs raw)
        base (cond
               (str/blank? normalized)
               "[no textual response]"

               (re-find #"^\s*[\{\[]" raw)
               "Structured output generated."

               :else
               (truncate-with-ellipsis normalized irc-summary-max-chars))
        suffix (cond
                 (seq refs)
                 (str " refs: " (str/join ", " refs))

                 (re-find #"(?i)\bplanning-only\b" base)
                 ""

                 :else
                 " (no artifact reference yet)")
        msg (str base suffix)]
    (truncate-with-ellipsis msg irc-summary-hard-limit)))

(defn- irc-progress-promise-without-evidence?
  "True when TEXT makes an execution/progress promise but cites no artifact."
  [text]
  (let [t (str/trim (or text ""))]
    (and (not (str/blank? t))
         (boolean (re-find irc-progress-promise-re t))
         (not (boolean (re-find irc-artifact-ref-re t))))))

(defn- enforce-irc-planning-guard
  "Prefix TEXT with a planning-only disclaimer when unsupported progress claims appear."
  [text]
  (let [t (str/trim (or text ""))]
    (if (irc-progress-promise-without-evidence? t)
      (str "Planning-only: no execution evidence cited yet (no command output/artifact reference).\n"
           "No work has started in this reply.\n\n"
           t)
      t)))

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
    (-> (cond
          (not (str/blank? stripped)) stripped
          (not (str/blank? directive-msg)) directive-msg
          :else (str/trim (or text "")))
        surface-safe-local-paths
        enforce-irc-planning-guard)))

(defn- invoke-error-text
  "Render a stable human-readable invoke error string."
  [resp]
  (let [err (when (map? resp) (:error resp))]
    (cond
      (map? err) (or (:error/message err) (pr-str err))
      (some? err) (str err)
      :else "unknown invoke error")))

(defn- invoke-response->irc-reply
  "Convert invoke-agent! response map into a single IRC reply line.
   Always returns a non-blank string."
  [resp]
  (let [[raw summarize?] (cond
                           (and (map? resp) (:ok resp) (string? (:result resp)))
                           [(:result resp) true]

                           (and (map? resp) (:ok resp) (some? (:result resp)))
                           [(pr-str (:result resp)) true]

                           (and (map? resp) (:ok resp))
                           ["[invoke completed with empty response]" false]

                           :else
                           [(str "[invoke failed] " (invoke-error-text resp)) false])
        reply (if summarize?
                (-> raw normalize-irc-result summarize-irc-result)
                (truncate-with-ellipsis (str/trim (or raw "")) irc-summary-hard-limit))
        trimmed (str/trim (or reply ""))]
    (if (str/blank? trimmed)
      "[invoke completed with empty response]"
      trimmed)))

(defn- irc-invoke-prompt
  "Wrap an IRC user message with explicit surface/delivery semantics."
  [{:keys [nick sender channel user-text]}]
  (str "Runtime surface contract:\n"
       "- Current surface: IRC.\n"
       "- Channel: " channel "\n"
       "- Sender: " sender "\n"
       "- Your returned text will be posted to IRC by the server as <" nick ">.\n"
       "- Return natural chat text only; do not emit directive wrappers.\n"
       "- Keep replies short: one line, <= 220 chars before refs.\n"
       "- If user asks for separate IRC messages, return newline-separated one-line items (one intended post per line).\n"
       "- If work happened, include refs to concrete artifacts (commit, PR/issue URL, or changed file path).\n"
       "- Do not claim to write relay files (/tmp/futon-irc-*.jsonl) or send network traffic unless this turn actually executed such a tool.\n\n"
       "- Do not invent per-turn line caps or transport limits.\n"
       "- Do not claim to be actively starting/running work unless this turn executed tools/commands.\n"
       "- If no execution happened in this turn, explicitly say it is planning-only and not started yet.\n"
       "- Any progress claim must include an artifact reference (commit SHA, PR URL, issue comment URL, or changed file path).\n\n"
       "- If you cannot cite an artifact, do not use future-commitment phrasing like \"I'll start now\".\n\n"
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
  [{:keys [relay-bridge irc-server agent-id nick invoke-timeout-ms invoke-hard-timeout-ms]
    :or {agent-id "claude-1" nick "claude"
         invoke-timeout-ms 600000
         invoke-hard-timeout-ms 1800000}}]
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
             ;; Handle !ungate / !gate commands from any user
             (when-let [[_ cmd target] (re-matches #"(?i)^!(un)?gate\s+(\S+)\s*$" text)]
               (let [target-nick (str/lower-case target)]
                 (if cmd
                   (do
                     (swap! !ungated-nicks conj target-nick)
                     (println (str "[irc] UNGATED: " target-nick " — receiving all messages"))
                     ((:send-to-channel! irc-server) channel "system"
                      (str target-nick " is now ungated — listening to all messages")))
                   (do
                     (swap! !ungated-nicks disj target-nick)
                     (println (str "[irc] GATED: " target-nick " — mention-only"))
                     ((:send-to-channel! irc-server) channel "system"
                      (str target-nick " is now gated — mention-only mode"))))
                 (flush)))
             (let [ungated? (contains? @!ungated-nicks (str/lower-case nick))
                   addressed? (or ungated? (mentioned? text nick))]
               (if (and addressed?
                        (not= sender nick)
                        ;; Don't dispatch !gate/!ungate commands as prompts
                        (not (re-matches #"(?i)^!(un)?gate\s+.*" text)))
                 (let [prompt (if ungated? text (strip-mention text nick))]
                   (if (str/blank? prompt)
                     (do
                       (println (str "[irc] " nick ": mention detected but prompt empty, ignoring"))
                       (flush))
                     (do
                       (println (str "[irc] " nick ": dispatching invoke (soft-timeout="
                                     invoke-timeout-ms "ms, hard-timeout="
                                     invoke-hard-timeout-ms "ms)"))
                       (flush)
                       (future
                         (let [!invoke-trace-id (atom nil)]
                           (try
                             (let [invoke-prompt (irc-invoke-prompt {:nick nick
                                                                     :sender sender
                                                                     :channel channel
                                                                     :user-text prompt})
                                   started-ms (System/currentTimeMillis)
                                   soft-timeout-ms (when (and invoke-timeout-ms (pos? (long invoke-timeout-ms)))
                                                     (long invoke-timeout-ms))
                                   hard-timeout-ms (cond
                                                     (and invoke-hard-timeout-ms (pos? (long invoke-hard-timeout-ms)))
                                                     (long invoke-hard-timeout-ms)
                                                     soft-timeout-ms soft-timeout-ms
                                                     :else 1800000)
                                   invoke-fut (future (reg/invoke-agent! agent-id invoke-prompt hard-timeout-ms))
                                   resp (loop [soft-notified? false]
                                          (if (realized? invoke-fut)
                                            @invoke-fut
                                            (let [elapsed (- (System/currentTimeMillis) started-ms)]
                                              (when (and soft-timeout-ms
                                                         (not soft-notified?)
                                                         (>= elapsed soft-timeout-ms))
                                                (let [msg (str "[invoke still running after "
                                                               soft-timeout-ms
                                                               "ms] waiting for completion...")]
                                                  (println (str "[irc] " nick " invoke SOFT TIMEOUT: " msg))
                                                  ((:send-to-channel! irc-server) channel nick msg)
                                                  (flush)))
                                              (Thread/sleep 1000)
                                              (recur (or soft-notified?
                                                         (and soft-timeout-ms
                                                              (>= elapsed soft-timeout-ms)))))))
                                   reply* (invoke-response->irc-reply resp)
                                   invoke-trace-id (invoke-meta-trace-id (:invoke-meta resp))]
                               (reset! !invoke-trace-id invoke-trace-id)
                               ((:send-to-channel! irc-server) channel nick reply*)
                               (when (and (string? invoke-trace-id) (not (str/blank? invoke-trace-id)))
                                 (record-invoke-delivery!
                                  agent-id invoke-trace-id
                                  {:surface "irc"
                                   :destination (str channel " as <" nick ">")
                                   :delivered? true
                                   :note "dispatch-relay"}))
                               (println (str "[irc] " nick " → " channel " ("
                                             (count reply*) " chars): "
                                             (subs reply* 0 (min 120 (count reply*)))))
                               (flush)
                               ;; Post-invoke hook: agent just went idle.
                               ;; Calls the dynamic hook fn if set.
                               (when-let [hook @!post-invoke-hook]
                                 (future
                                   (try (hook agent-id)
                                        (catch Exception e
                                          (println (str "[post-invoke] hook error: " (.getMessage e))))))))
                             (catch Exception e
                               (println (str "[irc] " nick " dispatch ERROR: " (.getMessage e)))
                               (let [fallback-delivered?
                                     (try
                                       ((:send-to-channel! irc-server) channel nick
                                        (str "[invoke dispatch error] " (.getMessage e)))
                                       true
                                       (catch Exception send-e
                                         (println (str "[irc] " nick " dispatch ERROR while sending fallback: "
                                                       (.getMessage send-e)))
                                         false))]
                                 (when-let [invoke-trace-id @!invoke-trace-id]
                                   (record-invoke-delivery!
                                    agent-id invoke-trace-id
                                    {:surface "irc"
                                     :destination (str channel " as <" nick ">")
                                     :delivered? fallback-delivered?
                                     :note (if fallback-delivered?
                                             "dispatch-relay-error-fallback"
                                             (str "dispatch-relay-error: " (.getMessage e)))})))
                               (flush)))))))
                  (do
                    (println (str "[irc] " nick ": not mentioned, skipping"))
                    (flush)))))))))
    )
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

;; =============================================================================
;; Agent layer lifecycle — independently restartable from IRC
;; =============================================================================

(defn stop-agents!
  "Stop the WS transport + agent registrations. IRC stays up.
   Use restart-agents! to bring them back."
  []
  ;; Remove IRC auto-join watcher (if ws-connections atom exists)
  (when-let [f3c @!f3c-sys]
    (when-let [ws-conns (:ws-connections f3c)]
      (remove-watch ws-conns :irc-auto-join)))
  ;; Stop codex WS bridge
  (when-let [bridge @!codex-ws-bridge]
    (try ((:stop-fn bridge)) (catch Exception _))
    (reset! !codex-ws-bridge nil))
  ;; Stop tickle (depends on agents)
  (stop-tickle!)
  ;; Stop http-kit WS server
  (when-let [f3c @!f3c-sys]
    (when-let [stop-fn (:server f3c)]
      (try (stop-fn) (catch Exception _)))
    (reset! !f3c-sys nil))
  (println "[dev] Agent layer stopped. IRC still running."))

(defn start-agents!
  "Start (or restart) the WS transport + agent registrations.
   Uses the existing IRC server and evidence store from atoms.
   Safe to call after stop-agents!."
  []
  (let [irc-sys @!irc-sys
        evidence-store @!evidence-store
        f1-sys @!f1-sys
        role-info (deployment-role-info)
        role (:role role-info)
        role-cfg (role-defaults role)
        register-claude? (env-bool "FUTON3C_REGISTER_CLAUDE" (:register-claude? role-cfg))
        register-codex? (env-bool "FUTON3C_REGISTER_CODEX" (:register-codex? role-cfg))
        relay-claude? (env-bool "FUTON3C_RELAY_CLAUDE" (or register-claude? (= role :linode)))
        relay-codex? (env-bool "FUTON3C_RELAY_CODEX" (or register-codex? (= role :linode)))
        codex-agent-id (configured-codex-agent-id)
        codex-relay-nick (configured-codex-relay-nick)
        relay-invoke-timeout-ms (or (env-int "FUTON3C_RELAY_INVOKE_TIMEOUT_MS" 600000) 600000)
        relay-invoke-hard-timeout-ms (or (env-int "FUTON3C_RELAY_INVOKE_HARD_TIMEOUT_MS" 1800000) 1800000)
        codex-ws-bridge? (env-bool "FUTON3C_CODEX_WS_BRIDGE" (= role :laptop))
        codex-remote-origin (or (some-> (env "FUTON3C_CODEX_REMOTE_BASE") normalize-http-base)
                                (some-> (env "FUTON3C_LAPTOP_URL") normalize-http-base)
                                (some-> (env "FUTON3C_IRC_SEND_BASE") normalize-http-base)
                                (some-> (first-peer-url) normalize-http-base))
        irc-send-base-hint (or (some-> (env "FUTON3C_IRC_SEND_BASE") normalize-http-base)
                               (some-> (env "FUTON3C_LINODE_URL") normalize-http-base)
                               (some-> (first-peer-url) normalize-http-base))
        direct-xtdb? (direct-xtdb-enabled? role-cfg)
        ;; Start WS transport (with IRC interceptor if IRC is running)
        f3c-sys (start-futon3c!
                 {:xtdb-node (when direct-xtdb? (:node f1-sys))
                  :evidence-store evidence-store
                  :irc-send-base irc-send-base-hint
                  :irc-send-fn (or (when irc-sys
                                     (:send-to-channel! (:server irc-sys)))
                                   ;; No built-in IRC — route through ngircd bridge HTTP
                                   (make-bridge-irc-send-fn))
                  :irc-interceptor (when irc-sys
                                     (:irc-interceptor (:relay-bridge irc-sys)))})
        _ (reset! !f3c-sys f3c-sys)
        ;; CYDER: register futon3c HTTP+WS server
        _ (when f3c-sys
            (cyder/register!
             {:id "futon3c-http"
              :type :server
              :stop-fn (or (:server f3c-sys) (fn []))
              :state-fn #(let [s @!f3c-sys]
                           {:port (:port s)
                            :ws-connections (count (some-> s :ws-connections deref))})}))
        ;; Auto-join agents to #futon when they complete WS handshake
        _ (when (and irc-sys (:ws-connections f3c-sys))
            (install-irc-auto-join!
             (:ws-connections f3c-sys)
             (:relay-bridge irc-sys)
             (:server irc-sys)))
        ;; Register Claude
        _ (when register-claude?
            (let [sf (io/file (or (env "CLAUDE_SESSION_FILE")
                                  "/tmp/futon-session-id"))
                  initial-sid (read-session-id sf)
                  sid-atom (atom initial-sid)
                  claude-socket (or (env "CLAUDE_EMACS_SOCKET")
                                    (env "FUTON3C_EMACS_SOCKET"))
                  invoke-fn (make-claude-invoke-fn
                             {:claude-bin (env "CLAUDE_BIN" "claude")
                              :permission-mode (env "CLAUDE_PERMISSION" "bypassPermissions")
                              :agent-id "claude-1"
                              :session-file sf
                              :session-id-atom sid-atom
                              :emacs-socket claude-socket})]
              (rt/register-claude! {:agent-id "claude-1"
                                    :invoke-fn invoke-fn})
              (let [claude-metadata (cond-> {}
                                      claude-socket (assoc :emacs-socket claude-socket))]
              (reg/update-agent! "claude-1"
                                 :agent/type :claude
                                 :agent/invoke-fn invoke-fn
                                 :agent/metadata claude-metadata
                                 :agent/capabilities [:explore :edit :test :coordination/execute])
              (when initial-sid
                (reg/update-agent! "claude-1" :agent/session-id initial-sid))
              (println (str "[dev] Claude agent registered: claude-1 (inline invoke)"
                            (when claude-socket
                              (str " (emacs: " claude-socket ")"))
                            (when initial-sid
                              (str " (session: " (subs initial-sid 0
                                                       (min 8 (count initial-sid))) ")")))))))
        ;; Register Claude-2 (Mentor role, workspace2)
        _ (when (env-bool "FUTON3C_REGISTER_CLAUDE2" (:register-claude2? role-cfg))
            (let [sf2 (io/file (or (env "CLAUDE2_SESSION_FILE")
                                    "/tmp/futon-session-id-claude-2"))
                  initial-sid2 (read-session-id sf2)
                  sid-atom2 (atom initial-sid2)
                  claude2-socket (or (env "CLAUDE2_EMACS_SOCKET") "workspace2")
                  invoke-fn2 (make-claude-invoke-fn
                               {:claude-bin (env "CLAUDE_BIN" "claude")
                                :permission-mode (env "CLAUDE_PERMISSION" "bypassPermissions")
                                :agent-id "claude-2"
                                :session-file sf2
                                :session-id-atom sid-atom2
                                :emacs-socket claude2-socket})]
              (rt/register-claude! {:agent-id "claude-2"
                                    :invoke-fn invoke-fn2})
              (reg/update-agent! "claude-2"
                                 :agent/type :claude
                                 :agent/invoke-fn invoke-fn2
                                 :agent/metadata {:role "mentor" :emacs-socket claude2-socket}
                                 :agent/capabilities [:explore :edit :test :coordination/execute])
              (when initial-sid2
                (reg/update-agent! "claude-2" :agent/session-id initial-sid2))
              (println (str "[dev] Claude agent registered: claude-2 (mentor, inline invoke)"
                            (str " (emacs: " claude2-socket ")")
                            (when initial-sid2
                              (str " (session: " (subs initial-sid2 0
                                                       (min 8 (count initial-sid2))) ")"))))))
        ;; Register corpus-1 (WS-only, connects from laptop)
        _ (do (reg/register-agent! {:agent-id "corpus-1"
                                     :type :corpus
                                     :invoke-fn nil
                                     :capabilities []
                                     :metadata {:role "corpus-bot"}})
              (println "[dev] Corpus agent registered: corpus-1 (ws-only)"))
        ;; Register tickle-1: haiku + tickle identity wrapper (I-1 compliant)
        _ (let [sf-t (io/file "/tmp/futon-tickle-1-session-id")
                sid-t (read-session-id sf-t)
                sid-atom-t (atom sid-t)
                raw-fn (make-claude-invoke-fn
                         {:claude-bin (env "CLAUDE_BIN" "claude")
                          :permission-mode "default"
                          :agent-id "tickle-1"
                          :session-file sf-t
                          :session-id-atom sid-atom-t
                          :model "claude-haiku-4-5-20251001"})
                invoke-fn-t (make-tickle-invoke-fn raw-fn)]
            (reg/register-agent! {:agent-id "tickle-1"
                                   :type :tickle
                                   :invoke-fn invoke-fn-t
                                   :capabilities [:coordination/orchestrate]
                                   :metadata {:role "watchdog"}})
            (println (str "[dev] Tickle agent registered: tickle-1 (claude invoke)"
                          (when sid-t
                            (str " (session: " (subs sid-t 0 (min 8 (count sid-t))) ")")))))
        ;; Register Codex — guard against missing binary
        codex-bin-name (env "CODEX_BIN" "codex")
        codex-bin-exists? (try
                            (let [which (.start (ProcessBuilder.
                                                 ^java.util.List ["which" codex-bin-name]))]
                              (.waitFor which 5000 java.util.concurrent.TimeUnit/MILLISECONDS)
                              (zero? (.exitValue which)))
                            (catch Exception _ false))
        register-codex? (if (and register-codex? (not codex-bin-exists?))
                          (do (println (str "[dev][WARN] FUTON3C_REGISTER_CODEX=true but `"
                                            codex-bin-name "` not found on PATH."))
                              (println "[dev][WARN] Falling back to relay mode. Remove FUTON3C_REGISTER_CODEX override.")
                              false)
                          register-codex?)
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
                               :reasoning-effort (env "CODEX_REASONING_EFFORT")
                               :timeout-ms (or (env-int "CODEX_INVOKE_TIMEOUT_MS" 1800000) 1800000)
                               :agent-id codex-agent-id
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
                (let [ws-path (or (some-> (env "FUTON3C_CODEX_WS_PATH") str/trim not-empty)
                                  "/agency/ws")
                      codex-invoke-fn (when remote-ws-target? invoke-fn)
                       codex-metadata (cond-> {:ws-bridge? true}
                                        remote-ws-target? (assoc :skip-federation-proxy? true)
                                        remote-ws-target? (assoc :ws-remote? true))
                       bridge (start-codex-ws-bridge!
                              {:agent-id codex-agent-id
                               :invoke-fn invoke-fn
                               :initial-sid initial-sid
                               :session-file sf
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
                                  (str " (session: " (subs initial-sid 0
                                                            (min 8 (count initial-sid))) ")")))))
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
                                  (str " (session: " (subs initial-sid 0
                                                            (min 8 (count initial-sid))) ")"))))))))
        ;; Remote codex stub
        _ (when (and (not register-codex?) relay-codex?)
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
                            ")"))))
        ;; Dispatch relays
        _ (when (and irc-sys relay-claude?)
            (start-dispatch-relay!
             {:relay-bridge (:relay-bridge irc-sys)
              :irc-server (:server irc-sys)
              :agent-id "claude-1"
              :nick "claude"
              :invoke-timeout-ms relay-invoke-timeout-ms
              :invoke-hard-timeout-ms relay-invoke-hard-timeout-ms}))
        _ (when (and irc-sys relay-codex?)
            (start-dispatch-relay!
             {:relay-bridge (:relay-bridge irc-sys)
              :irc-server (:server irc-sys)
              :agent-id codex-agent-id
              :nick codex-relay-nick
              :invoke-timeout-ms relay-invoke-timeout-ms
              :invoke-hard-timeout-ms relay-invoke-hard-timeout-ms}))]
    (println "[dev] Agent layer started.")))

(defn restart-agents!
  "Restart WS transport + agents. IRC stays up."
  []
  (stop-agents!)
  (start-agents!))

(defn -main [& _args]
  (let [role-info (deployment-role-info)
        role (:role role-info)
        role-cfg (role-defaults role)
        ;; Wire meme.db path for concepts API (futon3a on classpath)
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
        ;; CYDER: register futon1a
        _ (cyder/register!
           {:id "futon1a"
            :type :server
            :stop-fn (:stop! f1-sys)
            :state-fn #(let [s @!f1-sys]
                         {:port (:http/port s)
                          :direct-xtdb? direct-xtdb?})})
        ;; futon5 nonstarter heartbeat API (portfolio bid/clear persistence)
        f5-sys (start-futon5!)
        ;; CYDER: register futon5 (if started)
        _ (when f5-sys
            (cyder/register!
             {:id "futon5"
              :type :server
              :stop-fn (or (nonstarter-fn 'stop!) (fn []))
              :state-fn #(do {:port (env-int "FUTON5_PORT" 7072)})}))
        ;; IRC relay bridge + server (independent of agent layer)
        irc-sys (start-irc! evidence-store role)
        _ (reset! !irc-sys irc-sys)
        ;; CYDER: register IRC server (if started)
        _ (when irc-sys
            (cyder/register!
             {:id "irc-server"
              :type :server
              :stop-fn (or (get-in irc-sys [:server :stop-fn])
                           (fn []))
              :state-fn #(let [s @!irc-sys]
                           {:port (:port s)
                            :relay-bridge? (boolean (:relay-bridge s))})}))
        ;; Agent layer: WS transport + Claude/Codex + dispatch relays
        ;; Uses start-agents! so it can be restarted independently via REPL
        _ (start-agents!)
        ;; Tickle watchdog — auto-start unless FUTON3C_TICKLE_AUTOSTART=false
        _ (when (env-bool "FUTON3C_TICKLE_AUTOSTART" true)
            (start-tickle! {:auto-restart? true}))
        ;; FM conductor — auto-start unless FUTON3C_FM_CONDUCTOR_AUTOSTART=false
        _ (when (env-bool "FUTON3C_FM_CONDUCTOR_AUTOSTART" true)
            (start-fm-conductor!))
        bridge-sys (start-drawbridge!)
        ;; CYDER: register drawbridge (if started)
        _ (when bridge-sys
            (cyder/register!
             {:id "drawbridge"
              :type :server
              :stop-fn (or (:stop bridge-sys) (fn []))
              :state-fn #(do {:port (:port bridge-sys)
                              :bind (:bind bridge-sys)})}))
        ;; Federation: configure peers and install announcement hook
        _ (federation/configure-from-env!)
        _ (federation/install-hook!)
        ;; Announce agents that were registered before hook installation
        ;; (startup registers Codex/Claude earlier in this let).
        _ (doseq [typed-id (reg/registered-agents)]
            (when-let [agent-record (reg/get-agent typed-id)]
              (federation/announce! agent-record)))
        ;; Import already-running peer agents so the local registry and *agents*
        ;; reflect the full federated surface, not only post-start announcements.
        fed-sync-results (federation/sync-peers!)
        fed-peers (federation/peers)
        fed-self (federation/self-url)
        ;; CYDER: register active missions from holes/missions/
        mission-count (cyder/register-missions!)
        ;; CYDER: project *processes* buffer on every registry change
        _ (add-watch cyder/!processes :blackboard
            (fn [_ _ _ new-val]
              (bb/project-processes!
                (sort-by :process/id (vals new-val)))))
        _ (start-agents-blackboard-ticker! 5000)
        _ (bb/project-agents! (reg/registry-status))
        ;; Initial projection so the buffer appears on startup
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
        ;; CYDER: remove watch and deregister all (don't call stop-fns — shutdown does that below)
        (remove-watch cyder/!processes :blackboard)
        (reset! cyder/!processes {})
        ;; Agent layer (WS + codex bridge + tickle)
        (stop-agents!)
        ;; Drawbridge
        (when-let [stop (:stop bridge-sys)] (stop))
        ;; IRC server
        (when-let [irc @!irc-sys]
          (when-let [stop (:stop-fn (:server irc))] (stop)))
        ;; futon5
        (when f5-sys
          (when-let [stop! (nonstarter-fn 'stop!)]
            (stop! f5-sys)))
        ;; futon1a
        (when-let [f1 @!f1-sys]
          ((:stop! f1)))
        (println "[dev] Stopped."))))

    ;; Block forever
    @(promise)))
