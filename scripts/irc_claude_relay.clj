(ns scripts.irc-claude-relay
  "IRC <-> Claude relay using real `claude -p` invocations.

   No mocks, no WS scaffolding. IRC messages go directly to Claude via CLI,
   and Claude's responses come back to IRC. Uses shared session from
   /tmp/futon-session-id for conversation continuity across transports
   (Emacs chat, IRC, CLI).

   The drawbridge pattern: one Claude identity, multiple transports.

   Usage: clojure -M scripts/irc_claude_relay.clj"
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str]
            [futon3c.transport.irc :as irc])
  (:import [java.io File]
           [java.util UUID]
           [java.util.regex Pattern]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- env [k default] (or (System/getenv k) default))
(defn- parse-int [s default]
  (try (Integer/parseInt (str s)) (catch Exception _ default)))
(defn- parse-bool [s default]
  (if (nil? s)
    default
    (let [v (str/lower-case (str/trim s))]
      (not (contains? #{"0" "false" "no" "off"} v)))))

;; =============================================================================
;; Session ID (shared with Emacs chat)
;; =============================================================================

(def session-file "/tmp/futon-session-id")
(def claude-nick (env "FUTON3C_IRC_NICK" "claude"))
(def require-mention? (parse-bool (System/getenv "FUTON3C_IRC_REQUIRE_MENTION") true))

(defn- mention-pattern [nick]
  (re-pattern (str "(?i)@" (Pattern/quote nick) "\\b")))

(defn- addressed-to-agent? [text nick]
  (boolean (and (string? text)
                (re-find (mention-pattern nick) text))))

(defn- strip-agent-mention [text nick]
  (let [p (re-pattern (str "(?i)@" (Pattern/quote nick) "\\b[:;,]?\\s*"))
        stripped (str/trim (str/replace text p ""))]
    (if (str/blank? stripped) text stripped)))

(defn- ensure-session-id! []
  (let [f (File. session-file)]
    (if (.exists f)
      (let [id (str/trim (slurp f))]
        (if (str/blank? id)
          (let [uuid (str (UUID/randomUUID))]
            (spit f uuid)
            uuid)
          id))
      (let [uuid (str (UUID/randomUUID))]
        (spit f uuid)
        uuid))))

;; =============================================================================
;; Claude CLI invocation
;; =============================================================================

(defn- find-claude-cmd []
  (let [result (try
                 (let [p (.start (ProcessBuilder. ["which" "claude"]))]
                   (str/trim (slurp (.getInputStream p))))
                 (catch Exception _ nil))]
    (if (and result (not (str/blank? result)))
      result
      (let [fallback (str (System/getenv "HOME") "/.local/bin/claude")]
        (if (.exists (File. fallback))
          fallback
          (throw (ex-info "Cannot find claude CLI" {})))))))

(defn- call-claude!
  "Call `claude -p` with TEXT. Returns response string or nil on error.
   Uses --continue to maintain shared conversation across transports."
  [text claude-cmd channel from]
  (let [system-prompt (format (str "Transport: irc. Channel: %s. "
                                   "The speaker is %s, chatting via IRC client. "
                                   "Keep responses concise — IRC lines are short. "
                                   "This is a shared session with Emacs chat. "
                                   "If you've been talking to Joe in Emacs, "
                                   "that context carries over here.")
                              channel from)
        ;; Build clean environment without CLAUDECODE nesting vars
        clean-env (into {}
                    (remove (fn [[k _]]
                              (or (= k "CLAUDECODE")
                                  (= k "CLAUDE_CODE_ENTRYPOINT"))))
                    (System/getenv))
        session-id (str/trim (slurp session-file))
        result (sh/sh claude-cmd "-p" text
                       "--resume" session-id
                       "--permission-mode" "bypassPermissions"
                       "--append-system-prompt" system-prompt
                       :env clean-env)]
    (if (= 0 (:exit result))
      (str/trim (:out result))
      (do
        (println (format "  [claude error, exit %d] %s"
                         (:exit result)
                         (str/trim (str (:out result) (:err result)))))
        (flush)
        nil))))

;; =============================================================================
;; Main
;; =============================================================================

(let [irc-port (parse-int (env "FUTON3C_IRC_PORT" "6667") 6667)
      bind-host (env "FUTON3C_BIND_HOST" "0.0.0.0")
      session-id (ensure-session-id!)
      claude-cmd (find-claude-cmd)
      evidence-store (atom {:entries {} :order []})
      !send-to-channel (atom nil)
      ;; Clojure agent for serial processing (one claude call at a time)
      !processor (agent nil :error-handler
                   (fn [_ag ex]
                     (println "Processor error:" (.getMessage ex))
                     (flush)))]

  (println "=== futon3c IRC <-> Claude Relay ===")
  (println "  irc-port:" irc-port)
  (println "  nick:" claude-nick)
  (println "  require-mention:" require-mention?)
  (println "  session:" session-id)
  (println "  claude:" claude-cmd)
  (println)

  (let [relay-fn
        (fn [channel from text]
          (let [targeted? (or (not require-mention?)
                              (addressed-to-agent? text claude-nick))
                text* (if require-mention?
                        (strip-agent-mention text claude-nick)
                        text)]
            ;; Enqueue for serial processing via Clojure agent
            (when targeted?
              (send-off !processor
                (fn [_]
                  (println (format "[%s -> %s] %s" from channel text))
                  (flush)
                  (let [response (call-claude! text* claude-cmd channel from)]
                    (when (and response @!send-to-channel)
                      ;; Split multi-line responses into individual PRIVMSGs
                      (doseq [line (str/split-lines response)
                              :when (not (str/blank? line))]
                        (@!send-to-channel channel claude-nick line))
                      (println (format "[%s -> %s] %s" claude-nick channel response))
                      (flush))))))))

        irc-server (irc/start-irc-server!
                    {:port irc-port
                     :bind-host bind-host
                     :relay-fn relay-fn
                     :evidence-store evidence-store})]

    ;; Wire up the send function (breaks the circular dependency)
    (reset! !send-to-channel (:send-to-channel! irc-server))

    ;; Add Claude as a virtual nick in #futon so IRC clients see it in NAMES
    ((:join-virtual-nick! irc-server) "#futon" claude-nick)

    (println "IRC server started on port" irc-port)
    (println (format "Claude appears as '%s' in #futon" claude-nick))
    (println)
    (println "READY — Connect via IRC client and start chatting.")
    (println "Messages in #futon are relayed to Claude via `claude -p --continue`.")
    (println "Shared session means Emacs chat context carries over.")
    (println)
    (flush)

    ;; Keep alive until interrupted
    (try
      (.addShutdownHook (Runtime/getRuntime)
        (Thread. (fn []
                   (println "\nShutting down...")
                   ((:stop-fn irc-server))
                   (println "Done."))))
      (while true
        (Thread/sleep 10000))
      (catch InterruptedException _
        ((:stop-fn irc-server))
        (println "Shutdown complete.")))))
