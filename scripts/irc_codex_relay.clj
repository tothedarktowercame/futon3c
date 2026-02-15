(ns scripts.irc-codex-relay
  "IRC client <-> Codex relay.

   Connects to a remote futon3c IRC server as nick 'codex', listens for
   messages in #futon, relays them to `codex exec`, and sends responses back.

   Runs on the machine where the `codex` CLI is installed (e.g. Joe's laptop).
   The Claude relay runs server-side; this relay runs client-side. Together
   they give three-way chat: Joe + Claude + Codex on IRC.

   Usage: clojure -M scripts/irc_codex_relay.clj
          IRC_HOST=172.236.28.208 clojure -M scripts/irc_codex_relay.clj"
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str])
  (:import [java.io BufferedReader InputStreamReader OutputStreamWriter]
           [java.net Socket]))

;; =============================================================================
;; Config
;; =============================================================================

(defn- env [k default] (or (System/getenv k) default))
(defn- parse-int [s default]
  (try (Integer/parseInt (str s)) (catch Exception _ default)))

(def irc-host (env "IRC_HOST" "127.0.0.1"))
(def irc-port (parse-int (env "IRC_PORT" "6667") 6667))
(def irc-nick (env "IRC_NICK" "codex"))
(def irc-channel (env "IRC_CHANNEL" "#futon"))
(def codex-bin (env "CODEX_BIN" "codex"))
(def codex-cwd (env "CODEX_CWD" (System/getProperty "user.dir")))

;; =============================================================================
;; Codex invocation
;; =============================================================================

(defn- call-codex!
  "Call `codex exec` with TEXT. Returns response string or nil on error."
  [text from]
  (let [system-prompt (format (str "Transport: irc. Channel: %s. "
                                   "The speaker is %s, chatting via IRC. "
                                   "Keep responses concise — IRC lines are short. "
                                   "You are Codex, participating in a shared IRC "
                                   "channel with Joe and Claude. Be direct and useful.")
                              irc-channel from)
        prompt (format "[System: %s]\n\n%s: %s" system-prompt from text)
        result (sh/sh codex-bin "exec"
                       "--skip-git-repo-check"
                       "-c" "approval_policy=\"never\""
                       prompt
                       :dir codex-cwd)]
    (if (= 0 (:exit result))
      (let [out (str/trim (:out result))]
        (when-not (str/blank? out) out))
      (do
        (println (format "  [codex error, exit %d] %s"
                         (:exit result)
                         (str/trim (str (:out result) (:err result)))))
        (flush)
        nil))))

;; =============================================================================
;; IRC client
;; =============================================================================

(defn- irc-send! [^OutputStreamWriter writer line]
  (.write writer (str line "\r\n"))
  (.flush writer))

(defn- irc-privmsg! [writer channel text]
  ;; Split multi-line responses into individual PRIVMSGs
  (doseq [line (str/split-lines text)
          :when (not (str/blank? line))]
    ;; IRC line length limit ~512 bytes; truncate if needed
    (let [truncated (if (> (count line) 450)
                      (str (subs line 0 447) "...")
                      line)]
      (irc-send! writer (format "PRIVMSG %s :%s" channel truncated)))))

(defn- parse-privmsg
  "Parse :nick!user@host PRIVMSG #channel :text → [nick channel text] or nil."
  [line]
  (when-let [m (re-matches #"^:([^!]+)!\S+ PRIVMSG (\S+) :(.+)$" line)]
    [(nth m 1) (nth m 2) (nth m 3)]))

(defn- connect-and-relay! []
  (let [socket (Socket. ^String irc-host ^int irc-port)
        reader (BufferedReader. (InputStreamReader. (.getInputStream socket)))
        writer (OutputStreamWriter. (.getOutputStream socket))
        ;; Serial processing via Clojure agent
        !processor (agent nil :error-handler
                     (fn [_ag ex]
                       (println "Processor error:" (.getMessage ex))
                       (flush)))]

    (println (format "Connected to %s:%d" irc-host irc-port))
    (flush)

    ;; Register
    (irc-send! writer (format "NICK %s" irc-nick))
    (irc-send! writer (format "USER %s 0 * :Codex Agent" irc-nick))

    ;; Read loop
    (loop []
      (when-let [line (.readLine reader)]
        (cond
          ;; PING/PONG keepalive
          (str/starts-with? line "PING")
          (irc-send! writer (str/replace line "PING" "PONG"))

          ;; RPL_WELCOME (001) → join channel
          (re-find #"^\S+ 001 " line)
          (do
            (println (format "Registered as %s. Joining %s..." irc-nick irc-channel))
            (flush)
            (irc-send! writer (format "JOIN %s" irc-channel)))

          ;; PRIVMSG in our channel
          :else
          (when-let [[from channel text] (parse-privmsg line)]
            (when (and (not= from irc-nick) (= channel irc-channel))
              (println (format "[%s -> %s] %s" from channel text))
              (flush)
              ;; Enqueue for serial processing
              (send-off !processor
                (fn [_]
                  (let [response (call-codex! text from)]
                    (when response
                      (irc-privmsg! writer irc-channel response)
                      (println (format "[%s -> %s] %s" irc-nick irc-channel response))
                      (flush))))))))
        (recur)))))

;; =============================================================================
;; Main
;; =============================================================================

(println "=== futon3c IRC <-> Codex Relay ===")
(println "  irc:" (str irc-host ":" irc-port))
(println "  nick:" irc-nick)
(println "  channel:" irc-channel)
(println "  codex:" codex-bin)
(println "  cwd:" codex-cwd)
(println)
(flush)

(loop []
  (try
    (connect-and-relay!)
    (catch Exception e
      (println (format "Connection error: %s. Reconnecting in 5s..."
                       (.getMessage e)))
      (flush)
      (Thread/sleep 5000)))
  (recur))
