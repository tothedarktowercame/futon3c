(ns futon3c.dev.irc
  "Persistent ngircd IRC connection and bridge send functions.

   Extracted from futon3c.dev (Phase 1 of TN-dev-clj-decomposition).
   Manages a read-only IRC listener and routes all sends through
   the bridge HTTP /say endpoint."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io PrintWriter]
           [java.net Socket]
           [java.time Instant]))

;; ---------------------------------------------------------------------------
;; State — callers may supply their own atoms or use the defaults
;; ---------------------------------------------------------------------------

(defonce !irc-conn (atom nil))
(defonce !irc-log (atom []))
(def ^:private irc-log-max 200)

;; ---------------------------------------------------------------------------
;; Connection management
;; ---------------------------------------------------------------------------

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
      {:socket sock :nick nick :running running})))

(defn ensure-irc-conn!
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

;; ---------------------------------------------------------------------------
;; Log access
;; ---------------------------------------------------------------------------

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

;; ---------------------------------------------------------------------------
;; Bridge send (HTTP /say endpoint)
;; ---------------------------------------------------------------------------

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
  "Singleton bridge send fn. All IRC sends route through the bridge HTTP /say
   endpoint, which handles per-nick routing. The raw !irc-conn is read-only."
  (make-bridge-irc-send-fn))

(defn send-irc!
  "Send a message to IRC via the bridge HTTP /say endpoint.
   Signature matches send-to-channel!: (send-irc! channel from-nick message).
   The from-nick determines which IRC nick posts the message.
   Returns true on success, false on error."
  [channel from-nick message]
  (try
    (bridge-send-fn* channel from-nick message)
    true
    (catch Exception e
      (println (str "[irc] send-irc! error: " (.getMessage e)))
      false)))
