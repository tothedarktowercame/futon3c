(ns futon3c.transport.irc
  "IRC transport adapter — minimal RFC 1459 server wired to the social pipeline.

   Replaces futon3's musn_irc_bridge.clj (666 lines, 14+ fix commits) with a
   single-layer adapter: IRC protocol ↔ futon3c pipeline directly. No MUSN
   backend, no polling, no separate state store.

   Architecture:
   - TCP server on port 6667 (configurable)
   - Per-client state: nick, user, registered?, channels, writer
   - Per-room state: channel → #{nicks}
   - Relay bridge: IRC PRIVMSG → WS frame to agents, agent response → IRC PRIVMSG
   - Evidence emission per message for transcript persistence

   Commands: NICK, USER, JOIN, PART, PRIVMSG, PING, PONG, QUIT, WHO, MODE

   Pattern references:
   - realtime/connection-state-machine (L2): client registration lifecycle
   - realtime/structured-events-only (R9): evidence entries are typed maps"
  (:require [futon3c.evidence.store :as estore]
            [futon3c.transport.protocol :as proto]
            [clojure.string :as str])
  (:import [java.io BufferedReader BufferedWriter InputStreamReader OutputStreamWriter]
           [java.net ServerSocket Socket]
           [java.time Instant]
           [java.util UUID]))

;; =============================================================================
;; Internal helpers
;; =============================================================================

(defn- now-str [] (str (Instant/now)))

(def ^:private server-name "futon3c")

;; =============================================================================
;; IRC line parsing
;; =============================================================================

(defn parse-irc-line
  "Parse a raw IRC line into {:command :params :trailing}.
   RFC 1459: [:prefix] COMMAND params... [:trailing]"
  [line]
  (when (and (string? line) (not (str/blank? line)))
    (let [line (str/trim line)
          ;; Strip optional prefix (lines starting with :)
          [line prefix] (if (str/starts-with? line ":")
                          (let [sp (str/index-of line " ")]
                            (if sp
                              [(subs line (inc sp)) (subs line 1 sp)]
                              [line nil]))
                          [line nil])
          ;; Split trailing (after " :")
          [main trailing] (if-let [idx (str/index-of line " :")]
                            [(subs line 0 idx) (subs line (+ 2 idx))]
                            [line nil])
          parts (str/split (str/trim main) #"\s+")
          command (str/upper-case (first parts))
          params (vec (rest parts))]
      (cond-> {:command command :params params}
        prefix   (assoc :prefix prefix)
        trailing (assoc :trailing trailing)))))

;; =============================================================================
;; IRC line sending
;; =============================================================================

(defn- send-line!
  "Send a raw IRC line to a client. Thread-safe via writer lock."
  [writer line]
  (locking writer
    (.write writer (str line "\r\n"))
    (.flush writer)))

(defn- send-numeric!
  "Send an IRC numeric reply via a send function (fn [line])."
  [send nick numeric text]
  (send (str ":" server-name " " numeric " " (or nick "*") " " text)))

(defn- send-welcome!
  "Send RFC 1459 welcome numerics (001-004) after registration."
  [send nick]
  (send-numeric! send nick "001" (str ":Welcome to " server-name " " nick))
  (send-numeric! send nick "002" (str ":Your host is " server-name))
  (send-numeric! send nick "003" (str ":This server was created " (now-str)))
  (send-numeric! send nick "004" (str server-name " futon3c-1.0 o o")))

;; =============================================================================
;; IRC callbacks — testable without real TCP
;;
;; Same pattern as ws/make-ws-callbacks: mock send-fn for testing.
;; =============================================================================

(defn make-irc-callbacks
  "Create IRC lifecycle callbacks, testable without real TCP.

   config:
     :send-fn       — (fn [client-id line]) for sending IRC lines
     :close-fn      — (fn [client-id]) for closing client connection
     :relay-fn      — (fn [channel from text]) relay PRIVMSG to WS-connected agents
     :evidence-store — atom for evidence persistence (optional)

   Returns:
     {:on-connect    (fn [client-id])
      :on-line       (fn [client-id line])
      :on-disconnect (fn [client-id])
      :clients       atom  — {client-id → {:nick :user :registered? :channels}}
      :rooms         atom  — {channel → #{nicks}}}"
  [config]
  (let [!clients (atom {})
        !rooms (atom {})
        send-fn (or (:send-fn config) (fn [_ _]))
        close-fn (or (:close-fn config) (fn [_]))
        relay-fn (or (:relay-fn config) (fn [_ _ _]))
        evidence-store (:evidence-store config)

        emit-evidence!
        (fn [channel from text]
          (when evidence-store
            (estore/append* evidence-store
                            {:evidence/id (str "e-" (UUID/randomUUID))
                             :evidence/subject {:ref/type :thread :ref/id (str "irc/" channel)}
                             :evidence/type :forum-post
                             :evidence/claim-type :observation
                             :evidence/author from
                             :evidence/at (now-str)
                             :evidence/body {:channel channel
                                             :text text
                                             :from from
                                             :transport :irc}
                             :evidence/tags [:irc :chat :transport/irc (keyword "channel" channel)]
                             :evidence/session-id (str "irc-sess-" channel)})))

        broadcast!
        (fn [channel from text & {:keys [exclude]}]
          (let [nicks (get @!rooms channel #{})
                clients @!clients]
            (doseq [[cid client] clients
                     :when (and (contains? nicks (:nick client))
                                (not= cid exclude))]
              (send-fn cid (str ":" from " PRIVMSG " channel " :" text)))))

        names-reply!
        (fn [client-id nick channel]
          (let [nicks (get @!rooms channel #{})]
            (send-numeric! (partial send-fn client-id) nick "353"
                           (str "= " channel " :" (str/join " " nicks)))
            (send-numeric! (partial send-fn client-id) nick "366"
                           (str channel " :End of /NAMES list"))))]

    {:clients !clients
     :rooms !rooms

     :on-connect
     (fn [client-id]
       (swap! !clients assoc client-id
              {:nick nil :user nil :registered? false :channels #{}}))

     :on-line
     (fn [client-id line]
       (let [parsed (parse-irc-line line)]
         (when parsed
           (let [client (get @!clients client-id)
                 nick (:nick client)]
             (case (:command parsed)

               "NICK"
               (let [new-nick (first (:params parsed))]
                 (when new-nick
                   (swap! !clients update client-id assoc :nick new-nick)
                   ;; Check if registration complete (have both NICK and USER)
                   (let [updated (get @!clients client-id)]
                     (when (and (not (:registered? updated))
                                (:nick updated) (:user updated))
                       (swap! !clients update client-id assoc :registered? true)
                       (send-welcome! (partial send-fn client-id) new-nick)))))

               "USER"
               (let [user-str (first (:params parsed))]
                 (when user-str
                   (swap! !clients update client-id assoc :user user-str)
                   ;; Check if registration complete
                   (let [updated (get @!clients client-id)]
                     (when (and (not (:registered? updated))
                                (:nick updated) (:user updated))
                       (swap! !clients update client-id assoc :registered? true)
                       (send-welcome! (partial send-fn client-id) (:nick updated))))))

               "JOIN"
               (let [channel (first (:params parsed))]
                 (when (and nick channel (str/starts-with? channel "#"))
                   (swap! !rooms update channel (fnil conj #{}) nick)
                   (swap! !clients update client-id update :channels conj channel)
                   ;; Send JOIN confirmation to all in channel
                   (broadcast! channel nick (str "JOIN " channel))
                   (send-fn client-id (str ":" nick " JOIN " channel))
                   (names-reply! client-id nick channel)))

               "PART"
               (let [channel (first (:params parsed))]
                 (when (and nick channel)
                   (swap! !rooms update channel disj nick)
                   (swap! !clients update client-id update :channels disj channel)
                   (send-fn client-id (str ":" nick " PART " channel))
                   (broadcast! channel nick (str "PART " channel))))

               "PRIVMSG"
               (let [target (first (:params parsed))
                     text (:trailing parsed)]
                 (when (and nick target text)
                   (if (str/starts-with? target "#")
                     ;; Channel message
                     (do
                       (emit-evidence! target nick text)
                       (broadcast! target nick text :exclude client-id)
                       (relay-fn target nick text))
                     ;; Private message (relay as well)
                     (relay-fn target nick text))))

               "PING"
               (let [token (or (:trailing parsed) (first (:params parsed)) server-name)]
                 (send-fn client-id (str ":" server-name " PONG " server-name " :" token)))

               "PONG"
               nil ;; Ignore

               "QUIT"
               (do
                 ;; Remove from all rooms
                 (doseq [channel (:channels client)]
                   (swap! !rooms update channel disj nick)
                   (broadcast! channel nick (str "QUIT :" (or (:trailing parsed) "leaving"))))
                 (swap! !clients dissoc client-id)
                 (close-fn client-id))

               "WHO"
               (let [channel (first (:params parsed))
                     nicks (get @!rooms channel #{})]
                 (doseq [n nicks]
                   (send-numeric! (partial send-fn client-id) nick "352"
                                  (str channel " " n " " server-name " " server-name " " n " H :0 " n)))
                 (send-numeric! (partial send-fn client-id) nick "315"
                                (str channel " :End of /WHO list")))

               "MODE"
               ;; Minimal MODE support — just acknowledge
               (let [target (first (:params parsed))]
                 (when (and target (str/starts-with? target "#"))
                   (send-numeric! (partial send-fn client-id) nick "324"
                                  (str target " +"))))

               ;; Unknown command — ignore silently
               nil)))))

     :on-disconnect
     (fn [client-id]
       (let [client (get @!clients client-id)
             nick (:nick client)]
         (when nick
           (doseq [channel (:channels client)]
             (swap! !rooms update channel disj nick)
             (broadcast! channel nick (str "QUIT :connection lost"))))
         (swap! !clients dissoc client-id)))}))

;; =============================================================================
;; Agent relay bridge
;; =============================================================================

(defn make-relay-bridge
  "Create a relay bridge between IRC and WS-connected agents.

   The bridge:
   - Tracks which agents are in which IRC channels
   - Relays IRC PRIVMSG to agents via their WS send-fn
   - Provides an IRC interceptor for WS (handles irc_response frames)
   - Emits evidence for agent responses

   config:
     :evidence-store — atom (optional)

   Returns:
     {:relay-fn       (fn [channel from text]) — call from IRC on PRIVMSG
      :irc-interceptor (fn [ch conn parsed]) — pass to WS config
      :join-agent!    (fn [agent-id nick channel]) — register agent in IRC channel
      :part-agent!    (fn [agent-id]) — remove agent from all channels
      :agents         atom}"
  [config]
  (let [!agents (atom {})
        ;; {agent-id → {:nick str :channels #{str} :ws-send-fn fn}}
        evidence-store (:evidence-store config)
        !irc-send-fn (atom nil)

        emit-evidence!
        (fn [channel from text]
          (when evidence-store
            (estore/append* evidence-store
                            {:evidence/id (str "e-" (UUID/randomUUID))
                             :evidence/subject {:ref/type :thread :ref/id (str "irc/" channel)}
                             :evidence/type :forum-post
                             :evidence/claim-type :observation
                             :evidence/author from
                             :evidence/at (now-str)
                             :evidence/body {:channel channel
                                             :text text
                                             :from from
                                             :transport :ws-relay}
                             :evidence/tags [:irc :chat :transport/ws-relay (keyword "channel" channel)]
                             :evidence/session-id (str "irc-sess-" channel)})))]

    {:agents !agents

     :set-irc-send-fn!
     (fn [f]
       (reset! !irc-send-fn f))

     :join-agent!
     (fn [agent-id nick channel ws-send-fn]
       (swap! !agents assoc agent-id
              {:nick nick
               :channels #{channel}
               :ws-send-fn ws-send-fn}))

     :part-agent!
     (fn [agent-id]
       (swap! !agents dissoc agent-id))

     :relay-fn
     (fn [channel from text]
       ;; Relay IRC message to all agents in the channel
       (doseq [[_agent-id {:keys [channels ws-send-fn]}] @!agents
               :when (contains? channels channel)]
         (ws-send-fn (proto/render-irc-message channel from text))))

     :irc-interceptor
     (fn [_ch conn parsed]
       ;; Handle irc_response frames from agents
       (let [agent-id (:agent-id conn)
             channel (:irc/channel parsed)
             text (:irc/text parsed)
             agent (get @!agents agent-id)]
         (when (and agent channel text)
           (let [nick (:nick agent)]
             ;; Send to IRC channel via the IRC server
             (when-let [irc-send @!irc-send-fn]
               (irc-send channel nick text))
             ;; Emit evidence for agent response
             (emit-evidence! channel nick text)))
         true))}))

;; =============================================================================
;; TCP server
;; =============================================================================

(defn start-irc-server!
  "Start an IRC server on the given port.

   config:
     :port           — TCP port (default 6667)
     :bind-host      — bind address (default \"0.0.0.0\")
     :relay-fn       — (fn [channel from text]) for relaying to agents
     :evidence-store — atom for evidence persistence (optional)

   Returns:
     {:server ServerSocket
      :stop-fn (fn [])
      :clients atom
      :rooms atom
      :port int
      :send-to-channel! (fn [channel from-nick text])}"
  [config]
  (let [port (or (:port config) 6667)
        bind-host (or (:bind-host config) "0.0.0.0")
        !running (atom true)
        !writers (atom {}) ;; {client-id → BufferedWriter}
        send-fn (fn [client-id line]
                  (when-let [writer (get @!writers client-id)]
                    (try
                      (send-line! writer line)
                      (catch Exception _
                        ;; Client disconnected — will be cleaned up
                        nil))))
        close-fn (fn [client-id]
                   (when-let [writer (get @!writers client-id)]
                     (try (.close writer) (catch Exception _))
                     (swap! !writers dissoc client-id)))
        {:keys [on-connect on-line on-disconnect clients rooms]}
        (make-irc-callbacks {:send-fn send-fn
                             :close-fn close-fn
                             :relay-fn (or (:relay-fn config) (fn [_ _ _]))
                             :evidence-store (:evidence-store config)})
        server-socket (ServerSocket. port 50
                                     (java.net.InetAddress/getByName bind-host))

        send-to-channel!
        (fn [channel from-nick text]
          ;; Send PRIVMSG to a channel from an agent nick (used by relay bridge)
          (let [nicks (get @rooms channel #{})
                all-clients @clients]
            (doseq [[cid client] all-clients
                     :when (contains? nicks (:nick client))]
              (send-fn cid (str ":" from-nick " PRIVMSG " channel " :" text)))))

        accept-loop
        (future
          (try
            (while @!running
              (let [^Socket socket (try
                                     (.accept server-socket)
                                     (catch Exception _
                                       nil))]
                (when socket
                  (let [client-id (str "irc-" (UUID/randomUUID))
                        reader (BufferedReader.
                                (InputStreamReader. (.getInputStream socket) "UTF-8"))
                        writer (BufferedWriter.
                                (OutputStreamWriter. (.getOutputStream socket) "UTF-8"))]
                    (swap! !writers assoc client-id writer)
                    (on-connect client-id)
                    ;; Spawn reader thread per client
                    (future
                      (try
                        (loop []
                          (when-let [line (.readLine reader)]
                            (on-line client-id line)
                            (recur)))
                        (catch Exception _))
                      (on-disconnect client-id)
                      (close-fn client-id)
                      (try (.close socket) (catch Exception _)))))))
            (catch Exception _)))]

    {:server server-socket
     :clients clients
     :rooms rooms
     :port port
     :send-to-channel! send-to-channel!
     :join-virtual-nick! (fn [channel nick]
                           ;; Add an agent nick to a room without a TCP connection.
                           ;; Makes the nick visible in NAMES/WHO replies.
                           (swap! rooms update channel (fnil conj #{}) nick))
     :part-virtual-nick! (fn [channel nick]
                           (swap! rooms update channel disj nick))
     :stop-fn (fn []
                (reset! !running false)
                (try (.close server-socket) (catch Exception _))
                (doseq [[cid _] @!writers]
                  (close-fn cid))
                (future-cancel accept-loop))}))
