(ns futon3c.dev.config
  "Environment parsing, deployment role detection, and session ID management.

   Extracted from futon3c.dev (Phase 1 of TN-dev-clj-decomposition).
   All functions are pure or read-only (env vars, files). No runtime atoms."
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str])
  (:import [java.net URI InetAddress NetworkInterface]))

;; ---------------------------------------------------------------------------
;; Environment helpers
;; ---------------------------------------------------------------------------

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

(defn direct-invoke-timeout-seconds
  "Configured curl/client-side timeout for synchronous /api/alpha/invoke examples.
   Default remains 10 seconds so upstream futon behavior stays unchanged."
  []
  (or (env-int "FUTON3C_DIRECT_INVOKE_TIMEOUT_SECONDS" 10) 10))

(defn frontiermath-room
  "Configured FrontierMath IRC room, defaulting to #math."
  []
  (or (some-> (env "FUTON3C_FRONTIERMATH_ROOM") str/trim not-empty)
      "#math"))

;; ---------------------------------------------------------------------------
;; Codex identity
;; ---------------------------------------------------------------------------

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

(defn parse-nick-agent-pair
  "Parse a NICK_AGENT_MAP entry into [nick agent-id], lower-casing the nick."
  [entry]
  (when-let [[_ nick agent-id]
             (and entry
                  (re-matches #"(?i)\s*([^:,\s]+)\s*:\s*([^:,\s]+)\s*"
                              (str entry)))]
    [(str/lower-case nick) agent-id]))

(defn configured-nick-agent-map
  "Nick -> agent-id mapping for IRC-visible agents.

   FUTON3C/NICK_AGENT_MAP entries override the built-in compatibility defaults."
  []
  (let [defaults {(str/lower-case (configured-codex-relay-nick))
                  (configured-codex-agent-id)
                  "codex-vscode" "codex-vscode"
                  "codex-2" "codex-2"
                  "codex-3" "codex-3"
                  "claude" "claude-1"
                  "claude-2" "claude-2"
                  "claude-3" "claude-3"
                  "tickle" "tickle-1"}
        configured (into {}
                         (keep parse-nick-agent-pair)
                         (env-list "NICK_AGENT_MAP" []))]
    (merge defaults configured)))

(defn configured-agent-nick-map
  "Agent-id -> nick mapping for IRC-visible agents.

   FUTON3C/NICK_AGENT_MAP entries override the built-in compatibility defaults."
  []
  (let [defaults {(configured-codex-agent-id) (configured-codex-relay-nick)
                  "codex-vscode" "codex-vscode"
                  "codex-2" "codex-2"
                  "codex-3" "codex-3"
                  "claude-1" "claude"
                  "claude-2" "claude-2"
                  "claude-3" "claude-3"
                  "tickle-1" "tickle"}
        configured (into {}
                         (keep (fn [entry]
                                 (when-let [[nick agent-id]
                                            (parse-nick-agent-pair entry)]
                                   [agent-id nick])))
                         (env-list "NICK_AGENT_MAP" []))]
    (merge defaults configured)))

(defn agent-id-for-irc-nick
  "Resolve an IRC nick or nick-like probe sender to an agent-id."
  [nick]
  (let [nick-lower (some-> nick str str/trim str/lower-case)
        configured-map (configured-nick-agent-map)
        codex-nick-lower (some-> (configured-codex-relay-nick)
                                 str/lower-case)]
    (cond
      (str/blank? nick-lower) nil
      (contains? configured-map nick-lower) (get configured-map nick-lower)
      (str/starts-with? nick-lower "claude-3")
      (get configured-map "claude-3" "claude-3")
      (str/starts-with? nick-lower "claude-2")
      (get configured-map "claude-2" "claude-2")
      (str/starts-with? nick-lower "claude")
      (get configured-map "claude" "claude-1")
      (str/starts-with? nick-lower "codex-vscode")
      (get configured-map "codex-vscode" "codex-vscode")
      (str/starts-with? nick-lower "codex-3")
      (get configured-map "codex-3" "codex-3")
      (str/starts-with? nick-lower "codex-2")
      (get configured-map "codex-2" "codex-2")
      (and codex-nick-lower (str/starts-with? nick-lower codex-nick-lower))
      (configured-codex-agent-id)
      (str/starts-with? nick-lower "codex")
      (configured-codex-agent-id)
      (str/starts-with? nick-lower "tickle")
      (get configured-map "tickle" "tickle-1")
      :else nil)))

(defn irc-nick-for-agent-id
  "Resolve an agent-id to the room-visible IRC nick used for delivery."
  [agent-id]
  (let [aid (some-> agent-id str str/trim)
        aid-lower (some-> aid str/lower-case)
        configured-map (configured-agent-nick-map)]
    (cond
      (str/blank? aid) nil
      (contains? configured-map aid) (get configured-map aid)
      (str/starts-with? aid-lower "claude-3")
      (get configured-map "claude-3" "claude-3")
      (str/starts-with? aid-lower "claude-2")
      (get configured-map "claude-2" "claude-2")
      (str/starts-with? aid-lower "claude")
      (get configured-map "claude-1" "claude")
      (str/starts-with? aid-lower "codex-vscode")
      (get configured-map "codex-vscode" "codex-vscode")
      (str/starts-with? aid-lower "codex-3")
      (get configured-map "codex-3" "codex-3")
      (str/starts-with? aid-lower "codex-2")
      (get configured-map "codex-2" "codex-2")
      (str/starts-with? aid-lower "codex")
      (configured-codex-relay-nick)
      (str/starts-with? aid-lower "tickle")
      (get configured-map "tickle-1" "tickle")
      :else aid)))

(defn workspace-root-dir
  "Return the nearest ancestor of START-DIR that contains AGENTS.md, or nil."
  [start-dir]
  (when (and (string? start-dir) (not (str/blank? start-dir)))
    (loop [cur (.getAbsoluteFile (io/file start-dir))]
      (when cur
        (if (.exists (io/file cur "AGENTS.md"))
          (.getAbsolutePath cur)
          (recur (.getParentFile cur)))))))

(defn configured-codex-cwd
  "Resolve the default working directory for Codex execution.

   Priority:
   1. CODEX_CWD env override
   2. nearest ancestor containing AGENTS.md
   3. current JVM user.dir"
  []
  (or (some-> (env "CODEX_CWD") str/trim not-empty)
      (workspace-root-dir (System/getProperty "user.dir"))
      (System/getProperty "user.dir")))

;; ---------------------------------------------------------------------------
;; Nonstarter (futon5) integration
;; ---------------------------------------------------------------------------

(defn nonstarter-fn
  "Resolve a nonstarter API function by symbol name.
   Returns nil when futon5/nonstarter.api is not available on classpath."
  [fn-sym]
  (try
    (require 'nonstarter.api)
    (some-> (ns-resolve 'nonstarter.api fn-sym) var-get)
    (catch Throwable _
      nil)))

;; ---------------------------------------------------------------------------
;; URL / network helpers
;; ---------------------------------------------------------------------------

(defn first-peer-url
  "Return first configured federation peer URL, if any."
  []
  (first (env-list "FUTON3C_PEERS" [])))

(defn normalize-ws-base
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

(defn normalize-http-base
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

(defn local-ws-target?
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

(defn parse-url-host
  "Extract host from URL string; nil on parse failure."
  [url]
  (try
    (when-let [raw (some-> url str/trim not-empty)]
      (some-> (URI/create raw) .getHost))
    (catch Exception _
      nil)))

(defn local-ip-set
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

(defn resolve-host-ip-set
  "Resolve HOST to a set of IP string forms. Empty on failure."
  [host]
  (try
    (->> (InetAddress/getAllByName host)
         (map #(.getHostAddress ^InetAddress %))
         (remove str/blank?)
         set)
    (catch Exception _
      #{})))

(defn host-local?
  "True when HOST resolves to this machine."
  [host]
  (let [h (some-> host str/lower-case str/trim)]
    (cond
      (str/blank? h) false
      (#{"localhost" "127.0.0.1" "::1"} h) true
      :else (let [local-ips (local-ip-set)
                  host-ips (resolve-host-ip-set h)]
              (boolean (seq (set/intersection local-ips host-ips)))))))

;; ---------------------------------------------------------------------------
;; Deployment role
;; ---------------------------------------------------------------------------

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

;; ---------------------------------------------------------------------------
;; Session ID management
;; ---------------------------------------------------------------------------

(defn read-session-id [f]
  (when (.exists f)
    (let [s (str/trim (slurp f))]
      (when-not (str/blank? s) s))))

(defn persist-session-id!
  [f sid]
  (when (and sid (not (str/blank? sid)))
    (try (spit f sid)
         (catch Exception e
           (println (str "[dev] session-id persist warning: " (.getMessage e)))))))

(defn session-file->file
  "Normalize session-file input into java.io.File when possible."
  [session-file]
  (cond
    (instance? java.io.File session-file) session-file
    (string? session-file) (io/file session-file)
    :else nil))

(defn preferred-session-id
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
