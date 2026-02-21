(ns futon3c.transport.irc
  "IRC transport adapter — minimal RFC 1459 server wired to the social pipeline.

   Replaces futon3's musn_irc_bridge.clj (666 lines, 14+ fix commits) with a
   single-layer adapter: IRC protocol ↔ futon3c pipeline directly. No MUSN
   backend, no polling, no separate state store.

   Architecture:
   - TCP server on port 6667 (configurable)
   - Per-client state: nick, user, registered?, channels, writer, liveness
   - Per-room state: channel → #{nicks}
   - Relay bridge: IRC PRIVMSG → WS frame to agents, agent response → IRC PRIVMSG
   - Evidence emission per message for transcript persistence
   - Server-initiated PING/PONG keepalive (M-IRC-stability F1)
   - Socket read timeout (F2), error logging (F3)
   - Nick reclaim on reconnect (F4), relay timeout (F5)
   - Reader thread tracking and coordinated shutdown (F6)

   Commands: NICK, USER, JOIN, PART, PRIVMSG, PING, PONG, QUIT, WHO, MODE

   Pattern references:
   - realtime/liveness-heartbeats (F1): server-initiated probes
   - realtime/connection-state-machine (L2): client registration lifecycle
   - realtime/loop-failure-signals (F3): errors surface, not swallowed
   - realtime/structured-events-only (R9): evidence entries are typed maps"
  (:require [futon3c.evidence.store :as estore]
            [futon3c.transport.protocol :as proto]
            [clojure.string :as str])
  (:import [java.io BufferedReader BufferedWriter InputStreamReader OutputStreamWriter]
           [java.net ServerSocket Socket SocketTimeoutException]
           [java.time Instant]
           [java.util UUID]))

;; =============================================================================
;; Internal helpers
;; =============================================================================

(defn- now-str [] (str (Instant/now)))

(defn- now-ms [] (System/currentTimeMillis))

(def ^:private server-name "futon3c")

(defn- emit-error-evidence!
  "Emit a tension evidence entry for an IRC transport error."
  [evidence-store client-id context error-msg]
  (when evidence-store
    (try
      (estore/append* evidence-store
                      {:evidence/id (str "e-" (UUID/randomUUID))
                       :evidence/subject {:ref/type :component :ref/id "irc-server"}
                       :evidence/type :coordination
                       :evidence/claim-type :tension
                       :evidence/author "irc-server"
                       :evidence/at (now-str)
                       :evidence/body {:error error-msg
                                       :client-id (str client-id)
                                       :context context}
                       :evidence/tags [:irc :error :transport/irc]})
      (catch Exception _ nil))))

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
     :ping-interval-ms — server PING interval (default 30000)
     :ping-timeout-ms  — reap after no PONG for this long (default 90000)

   Returns:
     {:on-connect    (fn [client-id])
      :on-line       (fn [client-id line])
      :on-disconnect (fn [client-id])
      :reap-dead!    (fn []) — check and reap dead clients
      :clients       atom  — {client-id → {:nick :user :registered? :channels
                                           :last-activity-at :ping-pending?}}
      :rooms         atom  — {channel → #{nicks}}}"
  [config]
  (let [!clients (atom {})
        !rooms (atom {})
        send-fn (or (:send-fn config) (fn [_ _]))
        close-fn (or (:close-fn config) (fn [_]))
        relay-fn (or (:relay-fn config) (fn [_ _ _]))
        evidence-store (:evidence-store config)
        ping-timeout-ms (or (:ping-timeout-ms config) 90000)

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

        touch!
        (fn [client-id]
          (swap! !clients update client-id assoc :last-activity-at (now-ms)))

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
                           (str channel " :End of /NAMES list"))))

        ;; F4: Check if a nick is held by a ghost (dead connection)
        nick-ghost?
        (fn [nick]
          (let [clients @!clients]
            (some (fn [[cid client]]
                    (when (= nick (:nick client))
                      (let [last-act (:last-activity-at client 0)
                            elapsed (- (now-ms) last-act)]
                        (when (or (:ping-pending? client)
                                  (> elapsed ping-timeout-ms))
                          cid))))
                  clients)))

        ;; F4: Kill a ghost connection
        kill-ghost!
        (fn [ghost-cid]
          (let [client (get @!clients ghost-cid)
                nick (:nick client)]
            (when nick
              (doseq [channel (:channels client)]
                (swap! !rooms update channel disj nick)
                (broadcast! channel nick (str "QUIT :ghost killed (nick reclaim)"))))
            (swap! !clients dissoc ghost-cid)
            (close-fn ghost-cid)))]

    {:clients !clients
     :rooms !rooms

     :on-connect
     (fn [client-id]
       (swap! !clients assoc client-id
              {:nick nil :user nil :registered? false :channels #{}
               :last-activity-at (now-ms) :ping-pending? false}))

     :on-line
     (fn [client-id line]
       ;; Any received line proves liveness (F1)
       (touch! client-id)
       (let [parsed (parse-irc-line line)]
         (when parsed
           (let [client (get @!clients client-id)
                 nick (:nick client)]
             (case (:command parsed)

               "NICK"
               (let [new-nick (first (:params parsed))]
                 (when new-nick
                   ;; F4: Check for ghost nick and reclaim
                   (let [existing-cid (some (fn [[cid c]]
                                              (when (and (= new-nick (:nick c))
                                                         (not= cid client-id))
                                                cid))
                                            @!clients)]
                     (if existing-cid
                       ;; Nick in use — ghost or live?
                       (if (nick-ghost? new-nick)
                         (do (kill-ghost! existing-cid)
                             (swap! !clients update client-id assoc :nick new-nick))
                         ;; Not a ghost — ERR_NICKNAMEINUSE (433)
                         (send-numeric! (partial send-fn client-id)
                                        (or nick "*") "433"
                                        (str new-nick " :Nickname is already in use")))
                       ;; Nick is free
                       (swap! !clients update client-id assoc :nick new-nick)))
                   ;; Check if registration complete (have both NICK and USER)
                   (let [updated (get @!clients client-id)]
                     (when (and (not (:registered? updated))
                                (:nick updated) (:user updated))
                       (swap! !clients update client-id assoc :registered? true)
                       (send-welcome! (partial send-fn client-id) (:nick updated))))))

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
               ;; Clear ping-pending flag (F1)
               (swap! !clients update client-id assoc :ping-pending? false)

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
         (swap! !clients dissoc client-id)))

     ;; F1: Reap dead clients — called by keepalive loop
     :reap-dead!
     (fn []
       (let [now (now-ms)
             clients @!clients
             dead (for [[cid client] clients
                        :let [elapsed (- now (:last-activity-at client 0))]
                        :when (> elapsed ping-timeout-ms)]
                    cid)]
         (doseq [cid dead]
           (let [client (get @!clients cid)
                 nick (:nick client)]
             (when nick
               (emit-error-evidence! evidence-store cid "keepalive-reap"
                                     (str "Reaped dead connection for nick " nick
                                          " (no activity for " ping-timeout-ms "ms)"))
               (doseq [channel (:channels client)]
                 (swap! !rooms update channel disj nick)
                 (broadcast! channel nick (str "QUIT :ping timeout"))))
             (swap! !clients dissoc cid)
             (close-fn cid)))
         (count dead)))

     ;; F1: Send PINGs to clients that haven't sent anything recently
     :ping-idle!
     (fn [ping-interval-ms]
       (let [now (now-ms)
             clients @!clients]
         (doseq [[cid client] clients
                 :let [elapsed (- now (:last-activity-at client 0))]
                 :when (and (> elapsed ping-interval-ms)
                            (not (:ping-pending? client)))]
           (swap! !clients update cid assoc :ping-pending? true)
           (send-fn cid (str "PING :" server-name)))))}))

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
   - F5: Per-agent relay timeout (5s default)

   config:
     :evidence-store — atom (optional)
     :relay-timeout-ms — per-agent send timeout (default 5000)

   Returns:
     {:relay-fn       (fn [channel from text]) — call from IRC on PRIVMSG
      :irc-interceptor (fn [ch conn parsed]) — pass to WS config
      :join-agent!    (fn [agent-id nick channel ws-send-fn & [opts]]) — register/update agent
      :part-agent!    (fn [agent-id]) — remove agent from all channels
      :agents         atom}"
  [config]
  (let [!agents (atom {})
        ;; {agent-id → {:nick str :channels #{str} :ws-send-fn fn}}
        evidence-store (:evidence-store config)
        relay-timeout-ms (or (:relay-timeout-ms config) 5000)
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
     (fn join-agent!
       ([agent-id nick channel ws-send-fn]
        (join-agent! agent-id nick channel ws-send-fn {}))
       ([agent-id nick channel ws-send-fn {:keys [overwrite?]
                                           :or {overwrite? true}}]
        (swap! !agents
               (fn [m]
                 (let [existing (get m agent-id)
                       preserve? (and existing (not overwrite?))
                       nick* (if preserve? (:nick existing) nick)
                       ws-send-fn* (if preserve? (:ws-send-fn existing) ws-send-fn)
                       channels* (conj (or (:channels existing) #{}) channel)]
                   (assoc m agent-id
                          {:nick nick*
                           :channels channels*
                           :ws-send-fn ws-send-fn*}))))))

     :part-agent!
     (fn [agent-id]
       (swap! !agents dissoc agent-id))

     :relay-fn
     (fn [channel from text]
       ;; F5: Relay IRC message to all agents in the channel with timeout
       (doseq [[agent-id {:keys [channels ws-send-fn]}] @!agents
               :when (contains? channels channel)]
         (let [f (future (ws-send-fn (proto/render-irc-message channel from text)))]
           (try
             (deref f relay-timeout-ms ::timeout)
             (when (= ::timeout (deref f 0 ::timeout))
               (future-cancel f)
               (emit-error-evidence! evidence-store agent-id "relay-timeout"
                                     (str "Relay timeout for agent " agent-id
                                          " in " channel " after " relay-timeout-ms "ms")))
             (catch Exception e
               (emit-error-evidence! evidence-store agent-id "relay-error"
                                     (str "Relay error for agent " agent-id
                                          ": " (.getMessage e))))))))

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
     :port              — TCP port (default 6667)
     :bind-host         — bind address (default \"0.0.0.0\")
     :relay-fn          — (fn [channel from text]) for relaying to agents
     :evidence-store    — atom for evidence persistence (optional)
     :ping-interval-ms  — server PING interval (default 30000)
     :ping-timeout-ms   — reap after no activity for this long (default 90000)
     :socket-timeout-ms — SO_TIMEOUT for client sockets (default 120000)

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
        ping-interval-ms (or (:ping-interval-ms config) 30000)
        ping-timeout-ms (or (:ping-timeout-ms config) 90000)
        socket-timeout-ms (or (:socket-timeout-ms config) 120000)
        evidence-store (:evidence-store config)
        !running (atom true)
        !writers (atom {}) ;; {client-id → BufferedWriter}
        !reader-futures (atom {}) ;; F6: {client-id → future}
        send-fn (fn [client-id line]
                  (when-let [writer (get @!writers client-id)]
                    (try
                      (send-line! writer line)
                      (catch Exception e
                        ;; F3: Log send errors
                        (emit-error-evidence! evidence-store client-id "send-error"
                                              (.getMessage e))
                        nil))))
        close-fn (fn [client-id]
                   (when-let [writer (get @!writers client-id)]
                     (try (.close writer) (catch Exception _))
                     (swap! !writers dissoc client-id)))
        {:keys [on-connect on-line on-disconnect reap-dead! ping-idle! clients rooms]}
        (make-irc-callbacks {:send-fn send-fn
                             :close-fn close-fn
                             :relay-fn (or (:relay-fn config) (fn [_ _ _]))
                             :evidence-store evidence-store
                             :ping-interval-ms ping-interval-ms
                             :ping-timeout-ms ping-timeout-ms})
        server-socket (ServerSocket. port 50
                                     (java.net.InetAddress/getByName bind-host))

        send-to-channel!
        (fn [channel from-nick text]
          ;; Send PRIVMSG to a channel from an agent nick (used by relay bridge)
          ;; Also relay to other agents so they can see each other's messages
          (let [nicks (get @rooms channel #{})
                all-clients @clients]
            (doseq [[cid client] all-clients
                     :when (contains? nicks (:nick client))]
              (send-fn cid (str ":" from-nick " PRIVMSG " channel " :" text))))
          (emit-evidence! channel from-nick text)
          (relay-fn channel from-nick text))

        ;; F1: Keepalive loop — single future for all clients
        keepalive-loop
        (future
          (try
            (while @!running
              (Thread/sleep (min ping-interval-ms 5000))
              (when @!running
                (reap-dead!)
                (ping-idle! ping-interval-ms)))
            (catch InterruptedException _)
            (catch Exception e
              (emit-error-evidence! evidence-store nil "keepalive-error"
                                    (.getMessage e)))))

        accept-loop
        (future
          (try
            (while @!running
              (let [^Socket socket (try
                                     (.accept server-socket)
                                     (catch Exception _
                                       nil))]
                (when socket
                  ;; F2: Set socket read timeout
                  (.setSoTimeout socket (int socket-timeout-ms))
                  (let [client-id (str "irc-" (UUID/randomUUID))
                        reader (BufferedReader.
                                (InputStreamReader. (.getInputStream socket) "UTF-8"))
                        writer (BufferedWriter.
                                (OutputStreamWriter. (.getOutputStream socket) "UTF-8"))]
                    (swap! !writers assoc client-id writer)
                    (on-connect client-id)
                    ;; F6: Track reader future
                    (let [reader-future
                          (future
                            (try
                              (loop []
                                (when-let [line (.readLine reader)]
                                  (on-line client-id line)
                                  (recur)))
                              (catch SocketTimeoutException _
                                ;; F2: Timeout — keepalive will handle reaping
                                nil)
                              (catch Exception e
                                ;; F3: Log reader errors
                                (emit-error-evidence! evidence-store client-id "reader-error"
                                                      (.getMessage e))))
                            (on-disconnect client-id)
                            (close-fn client-id)
                            (swap! !reader-futures dissoc client-id)
                            (try (.close socket) (catch Exception _)))]
                      (swap! !reader-futures assoc client-id reader-future))))))
            (catch Exception e
              (emit-error-evidence! evidence-store nil "accept-error"
                                    (.getMessage e)))))]

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
                ;; F6: Cancel all reader futures
                (doseq [[_ f] @!reader-futures]
                  (future-cancel f))
                ;; Stop keepalive
                (future-cancel keepalive-loop)
                ;; Close all writers
                (doseq [[cid _] @!writers]
                  (close-fn cid))
                (future-cancel accept-loop))}))
