(ns scripts.irc-chat-relay
  "Long-running IRC chat relay for Claude to participate in IRC conversations.

   Starts Agency (WS + HTTP) and IRC server, connects Claude via WS,
   then relays messages via files:
   - Incoming IRC messages → /tmp/futon-irc-inbox.jsonl (append)
   - Outgoing responses ← /tmp/futon-irc-outbox.jsonl (poll + consume)

   Claude Code reads the inbox and writes to the outbox to participate.

   Usage: clojure -M scripts/irc_chat_relay.clj"
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [futon3c.agency.registry :as reg]
            [futon3c.evidence.store :as estore]
            [futon3c.peripheral.registry :as preg]
            [futon3c.peripheral.tools :as tools]
            [futon3c.runtime.agents :as runtime]
            [futon3c.social.persist :as persist]
            [futon3c.transport.irc :as irc]
            [futon3c.transport.ws :as ws]
            [org.httpkit.server :as hk])
  (:import [java.io BufferedWriter File FileWriter]
           [java.net URI]
           [java.net.http HttpClient WebSocket WebSocket$Listener]
           [java.nio.file Files Paths StandardOpenOption]
           [java.time Instant]
           [java.util.concurrent CompletableFuture]))

(defn- now-str [] (str (Instant/now)))

(defn- env [k default]
  (or (System/getenv k) default))

(defn- parse-int [s default]
  (try (Integer/parseInt (str s)) (catch Exception _ default)))

;; =============================================================================
;; File I/O
;; =============================================================================

(def inbox-path "/tmp/futon-irc-inbox.jsonl")
(def outbox-path "/tmp/futon-irc-outbox.jsonl")

(defn- append-inbox! [data]
  (let [line (str (json/generate-string data) "\n")]
    (locking inbox-path
      (spit inbox-path line :append true))))

(defn- read-and-clear-outbox! []
  (locking outbox-path
    (let [f (File. outbox-path)]
      (when (.exists f)
        (let [content (slurp f)]
          (spit f "")
          (when-not (str/blank? content)
            (->> (str/split-lines content)
                 (remove str/blank?)
                 (mapv #(try (json/parse-string % true)
                             (catch Exception _ nil)))
                 (remove nil?))))))))

;; =============================================================================
;; WS client (same pattern as live gate)
;; =============================================================================

(defn- mk-listener [frames errors opened-p closed-p]
  (reify WebSocket$Listener
    (onOpen [_ web-socket]
      (deliver opened-p web-socket)
      (.request web-socket 1))
    (onText [_ web-socket data _last]
      (swap! frames conj (str data))
      (.request web-socket 1)
      (CompletableFuture/completedFuture nil))
    (onClose [_ _web-socket _status-code _reason]
      (deliver closed-p true)
      (CompletableFuture/completedFuture nil))
    (onError [_ _web-socket error]
      (swap! errors conj (str error)))))

(defn- start-ws-client! [client url]
  (let [frames (atom [])
        errors (atom [])
        opened-p (promise)
        closed-p (promise)
        listener (mk-listener frames errors opened-p closed-p)
        ws-client (.join (.buildAsync (.newWebSocketBuilder client)
                                      (URI/create url)
                                      listener))]
    {:ws ws-client :frames frames :errors errors
     :opened opened-p :closed closed-p :url url}))

(defn- send-json! [^WebSocket web-socket payload]
  (.join (.sendText web-socket (json/generate-string payload) true)))

(defn- find-frame [frames pred]
  (some (fn [f] (when (pred f) f))
        (map #(json/parse-string % true) @frames)))

(defn- wait-for-frame [frames pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (if-let [frame (find-frame frames pred)]
        frame
        (if (> (System/currentTimeMillis) deadline)
          nil
          (do (Thread/sleep 50) (recur)))))))

;; =============================================================================
;; Main
;; =============================================================================

(let [ws-port (parse-int (env "FUTON3C_PORT" "7070") 7070)
      irc-port (parse-int (env "FUTON3C_IRC_PORT" "6667") 6667)
      bind-host (env "FUTON3C_BIND_HOST" "0.0.0.0")
      session-id (env "FUTON3C_SESSION_ID" "sess-irc-chat")
      claude-id "claude-1"
      evidence-store (atom {:entries {} :order []})
      !running (atom true)]

  ;; Clear files
  (spit inbox-path "")
  (spit outbox-path "")

  (println "=== futon3c IRC Chat Relay ===")
  (println "  ws-port:" ws-port)
  (println "  irc-port:" irc-port)
  (println "  bind-host:" bind-host)
  (println "  inbox:" inbox-path)
  (println "  outbox:" outbox-path)
  (println)

  (reg/reset-registry!)
  (persist/reset-sessions!)

  (runtime/register-claude!
    {:agent-id claude-id
     :invoke-fn (fn [prompt prior-session-id]
                  {:result (str "claude-ok:" prompt)
                   :session-id prior-session-id
                   :exit-code 0})})

  (let [periph-config {:backend (tools/make-mock-backend)
                       :peripherals (preg/load-peripherals)}
        relay-bridge (irc/make-relay-bridge {:evidence-store evidence-store})
        opts {:patterns {:patterns/ids [:alleycat/irc-chat]}
              :peripheral-config periph-config
              :send-fn hk/send!
              :close-fn hk/close
              :irc-interceptor (:irc-interceptor relay-bridge)}
        http-handler (runtime/make-http-handler opts)
        {:keys [handler connections]} (runtime/make-ws-handler opts)
        app (fn [req]
              (cond
                (and (= :get (:request-method req))
                     (= "/agency/connected" (:uri req)))
                {:status 200
                 :headers {"Content-Type" "application/json"}
                 :body (json/generate-string
                        {"connected_agents" (ws/connected-agents connections)
                         "count" (count (ws/connected-agents connections))})}
                (= "/agency/ws" (:uri req))
                (handler req)
                :else
                (http-handler req)))
        ws-stop (hk/run-server app {:ip bind-host :port ws-port})
        _ (Thread/sleep 200)

        irc-server (irc/start-irc-server!
                    {:port irc-port
                     :bind-host bind-host
                     :relay-fn (:relay-fn relay-bridge)
                     :evidence-store evidence-store})
        _ ((:set-irc-send-fn! relay-bridge) (:send-to-channel! irc-server))

        http-client (HttpClient/newHttpClient)]

    (try
      (println "Agency started on port" ws-port)
      (println "IRC server started on port" irc-port)
      (println)

      ;; Connect Claude via WS
      (println "Connecting Claude via WS...")
      (let [claude-url (str "ws://127.0.0.1:" ws-port "/agency/ws?agent-id=" claude-id "&session-id=" session-id)
            claude-ws (start-ws-client! http-client claude-url)]
        (when-not (deref (:opened claude-ws) 5000 nil)
          (throw (ex-info "Claude WS open timeout" {:errors @(:errors claude-ws)})))
        (send-json! (:ws claude-ws) {"type" "ready" "agent_id" claude-id "session_id" session-id})
        (when-not (wait-for-frame (:frames claude-ws) #(= "ready_ack" (:type %)) 5000)
          (throw (ex-info "Claude missing ready_ack" {:frames @(:frames claude-ws)})))
        (println "Claude connected and handshake complete.")

        ;; Register Claude in the relay bridge via server-side channel
        (Thread/sleep 200)
        (let [claude-ch (some (fn [[ch conn]]
                                (when (and (= claude-id (:agent-id conn))
                                           (:connected? conn))
                                  ch))
                              @connections)]
          (when-not claude-ch
            (throw (ex-info "Could not find Claude's http-kit channel" {})))
          ((:join-agent! relay-bridge) claude-id "claude" "#futon"
           (fn [data] (hk/send! claude-ch data)))
          ;; Add Claude's nick to IRC server room state so it appears in NAMES/WHO
          ((:join-virtual-nick! irc-server) "#futon" "claude"))

        (println)
        (println "READY — Waiting for IRC connections on port" irc-port)
        (println "Messages from IRC will appear in" inbox-path)
        (println "Write responses to" outbox-path)
        (println "Format: {\"channel\":\"#futon\",\"text\":\"Your message here\"}")
        (println)
        (flush)

        ;; Track which frames we've already processed
        (let [!last-frame-idx (atom 0)]

          ;; Message relay loop
          (while @!running
            (try
              ;; Check for new incoming IRC messages (via WS frames)
              (let [frames @(:frames claude-ws)
                    new-start @!last-frame-idx
                    new-frames (subvec (vec frames) (min new-start (count frames)))]
                (doseq [raw-frame new-frames]
                  (let [frame (try (json/parse-string raw-frame true) (catch Exception _ nil))]
                    (when (and frame (= "irc_message" (:type frame)))
                      (let [msg {:timestamp (now-str)
                                 :channel (:channel frame)
                                 :from (:from frame)
                                 :text (:text frame)}]
                        (append-inbox! msg)
                        (println (str "[" (:from frame) " → " (:channel frame) "] " (:text frame)))
                        (flush)))))
                (reset! !last-frame-idx (count frames)))

              ;; Check outbox for responses to send
              (doseq [resp (read-and-clear-outbox!)]
                (when (and (:channel resp) (:text resp))
                  (send-json! (:ws claude-ws)
                              {"type" "irc_response"
                               "channel" (:channel resp)
                               "text" (:text resp)})
                  (println (str "[claude → " (:channel resp) "] " (:text resp)))
                  (flush)))

              (Thread/sleep 250)
              (catch Exception e
                (println "Relay error:" (.getMessage e))
                (flush))))))

      (finally
        (reset! !running false)
        ((:stop-fn irc-server))
        (ws-stop)
        (println "Shutdown complete.")))))
