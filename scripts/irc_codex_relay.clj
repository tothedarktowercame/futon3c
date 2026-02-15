(ns scripts.irc-codex-relay
  "IRC client <-> Codex relay with shared session continuity.

   Connects to a remote IRC server as `codex`, listens in `#futon`, relays
   messages to `codex exec --json`, and posts responses back to IRC.

   Session continuity is shared with `emacs/codex-repl.el` via:
   - /tmp/futon-codex-session-id (default)
   - CODEX_SESSION_ID (optional startup override)

   Usage:
     IRC_HOST=172.236.28.208 clojure -M scripts/irc_codex_relay.clj"
  (:require [cheshire.core :as json]
            [clojure.java.shell :as sh]
            [clojure.string :as str])
  (:import [java.io BufferedReader File InputStreamReader OutputStreamWriter]
           [java.net Socket]
           [java.time Instant]
           [java.util.regex Pattern]))

;; =============================================================================
;; Config
;; =============================================================================

(defn- env [k default] (or (System/getenv k) default))
(defn- parse-int [s default]
  (try (Integer/parseInt (str s)) (catch Exception _ default)))
(defn- parse-bool [s default]
  (if (nil? s)
    default
    (let [v (str/lower-case (str/trim s))]
      (not (contains? #{"0" "false" "no" "off"} v)))))
(defn- now-str [] (str (Instant/now)))

(def irc-host (env "IRC_HOST" "127.0.0.1"))
(def irc-port (parse-int (env "IRC_PORT" "6667") 6667))
(def irc-nick (env "IRC_NICK" "codex"))
(def irc-channel (env "IRC_CHANNEL" "#futon"))

(def codex-bin (env "CODEX_BIN" "codex"))
(def codex-cwd (env "CODEX_CWD" (System/getProperty "user.dir")))
(def codex-model (env "CODEX_MODEL" "gpt-5-codex"))
(def codex-sandbox (env "CODEX_SANDBOX" "workspace-write"))
(def codex-approval (env "CODEX_APPROVAL" "never"))
(def session-file (env "CODEX_SESSION_FILE" "/tmp/futon-codex-session-id"))
(def startup-session-id (System/getenv "CODEX_SESSION_ID"))
(def require-mention? (parse-bool (System/getenv "IRC_REQUIRE_MENTION") true))

;; =============================================================================
;; Session helpers
;; =============================================================================

(defn- load-session-id [path]
  (let [f (File. path)]
    (when (.exists f)
      (let [sid (str/trim (slurp f))]
        (when-not (str/blank? sid)
          sid)))))

(defn- persist-session-id! [path sid]
  (when-not (str/blank? sid)
    (spit path sid)))

;; =============================================================================
;; Codex invocation
;; =============================================================================

(defn- extract-agent-text [item]
  (let [content (or (:text item) (:content item))]
    (cond
      (string? content) content
      (sequential? content)
      (->> content
           (keep (fn [part]
                   (when (and (map? part) (= "text" (:type part)))
                     (:text part))))
           (remove str/blank?)
           (str/join ""))
      :else nil)))

(defn- parse-codex-output [raw-output prior-session-id]
  (let [events (keep (fn [line]
                       (try (json/parse-string line true)
                            (catch Exception _ nil)))
                     (str/split-lines (or raw-output "")))
        session-id (or (some (fn [evt]
                               (when (= "thread.started" (:type evt))
                                 (or (:thread_id evt) (:session_id evt))))
                             events)
                       prior-session-id)
        text (or (some->> events
                          (filter #(= "item.completed" (:type %)))
                          (map :item)
                          (filter #(= "agent_message" (:type %)))
                          (map extract-agent-text)
                          (remove str/blank?)
                          last)
                 (some->> events
                          (filter #(= "error" (:type %)))
                          (map :message)
                          (remove str/blank?)
                          last)
                 (some-> raw-output str/trim not-empty)
                 "[No assistant message returned]")]
    {:session-id session-id
     :text text}))

(defn- build-codex-cmd [sid]
  (let [exec-opts ["--json"
                   "--skip-git-repo-check"
                   "--sandbox" codex-sandbox
                   "-c" (format "approval_policy=\"%s\"" codex-approval)]
        exec-opts (if (str/blank? codex-model)
                    exec-opts
                    (concat exec-opts ["--model" codex-model]))]
    (if (str/blank? sid)
      (into [codex-bin "exec"] (concat exec-opts ["-"]))
      (into [codex-bin "exec"] (concat exec-opts ["resume" sid "-"])))))

(defn- call-codex!
  [text from sid*]
  (let [sid @sid*
        cmd (build-codex-cmd sid)
        prompt (format (str "Transport: irc. Channel: %s. Sender: %s. "
                            "Keep replies concise for IRC. "
                            "This session is shared with emacs/codex-repl.\n\n"
                            "%s: %s")
                       irc-channel from from text)
        result (apply sh/sh (concat cmd [:in (str prompt "\n") :dir codex-cwd]))
        parsed (parse-codex-output (str (:out result) (:err result)) @sid*)]
    (when-let [new-sid (:session-id parsed)]
      (when-not (str/blank? new-sid)
        (reset! sid* new-sid)
        (persist-session-id! session-file new-sid)))
    (if (zero? (:exit result))
      (some-> (:text parsed) str/trim not-empty)
      (do
        (println (format "  [codex error, exit %d] %s"
                         (:exit result)
                         (str/trim (:text parsed))))
        (flush)
        nil))))

;; =============================================================================
;; IRC client
;; =============================================================================

(defn- irc-send! [^OutputStreamWriter writer line]
  (.write writer (str line "\r\n"))
  (.flush writer))

(defn- irc-privmsg! [writer channel text]
  (doseq [line (str/split-lines text)
          :when (not (str/blank? line))]
    (let [truncated (if (> (count line) 450)
                      (str (subs line 0 447) "...")
                      line)]
      (irc-send! writer (format "PRIVMSG %s :%s" channel truncated)))))

(defn- parse-privmsg [line]
  (when-let [m (re-matches #"^:([^!\s:]+)(?:!\S+)? PRIVMSG (\S+) :(.+)$" line)]
    [(nth m 1) (nth m 2) (nth m 3)]))

(defn- mention-pattern [nick]
  (re-pattern (str "(?i)@" (Pattern/quote nick) "\\b")))

(defn- addressed-to-agent? [text nick]
  (boolean (and (string? text)
                (re-find (mention-pattern nick) text))))

(defn- strip-agent-mention [text nick]
  (let [p (re-pattern (str "(?i)@" (Pattern/quote nick) "\\b[:;,]?\\s*"))
        stripped (str/trim (str/replace text p ""))]
    (if (str/blank? stripped) text stripped)))

(defn- connect-and-relay! [sid*]
  (let [socket (Socket. ^String irc-host ^int irc-port)
        reader (BufferedReader. (InputStreamReader. (.getInputStream socket)))
        writer (OutputStreamWriter. (.getOutputStream socket))
        !processor (agent nil :error-handler
                     (fn [_ag ex]
                       (println "Processor error:" (.getMessage ex))
                       (flush)))]
    (println (format "Connected to %s:%d at %s" irc-host irc-port (now-str)))
    (flush)
    (irc-send! writer (format "NICK %s" irc-nick))
    (irc-send! writer (format "USER %s 0 * :Codex Agent" irc-nick))
    (loop []
      (when-let [line (.readLine reader)]
        (cond
          (str/starts-with? line "PING")
          (irc-send! writer (str/replace line "PING" "PONG"))

          (re-find #"^\S+ 001 " line)
          (do
            (println (format "Registered as %s. Joining %s..." irc-nick irc-channel))
            (flush)
            (irc-send! writer (format "JOIN %s" irc-channel)))

          :else
          (when-let [[from channel text] (parse-privmsg line)]
            (let [targeted? (or (not require-mention?)
                                (addressed-to-agent? text irc-nick))
                  text* (if require-mention?
                          (strip-agent-mention text irc-nick)
                          text)]
              (when (and (not= from irc-nick) (= channel irc-channel) targeted?)
                (println (format "[%s -> %s] %s" from channel text))
                (flush)
                (send-off !processor
                  (fn [_]
                    (let [response (call-codex! text* from sid*)]
                      (when response
                        (irc-privmsg! writer irc-channel response)
                        (println (format "[%s -> %s] %s" irc-nick irc-channel response))
                        (flush)))))))))
        (recur)))))

;; =============================================================================
;; Main
;; =============================================================================

(let [initial-sid (or startup-session-id (load-session-id session-file))
      sid* (atom initial-sid)]
  (when-not (str/blank? startup-session-id)
    (persist-session-id! session-file startup-session-id))
  (println "=== futon3c IRC <-> Codex Relay (shared session) ===")
  (println "  irc:" (str irc-host ":" irc-port))
  (println "  nick:" irc-nick)
  (println "  channel:" irc-channel)
  (println "  codex:" codex-bin)
  (println "  cwd:" codex-cwd)
  (println "  model:" codex-model)
  (println "  sandbox:" codex-sandbox)
  (println "  approval:" codex-approval)
  (println "  require-mention:" require-mention?)
  (println "  session-file:" session-file)
  (println "  session:" (or @sid* "(new session on first turn)"))
  (println)
  (flush)
  (loop []
    (try
      (connect-and-relay! sid*)
      (catch Exception e
        (println (format "Connection error: %s. Reconnecting in 5s..."
                         (.getMessage e)))
        (flush)
        (Thread/sleep 5000)))
    (recur)))
