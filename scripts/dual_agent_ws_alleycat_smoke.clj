(ns scripts.dual-agent-ws-alleycat-smoke
  "Alleycat-style dual-agent WS readiness smoke.

   Validates practical concurrent runtime behavior:
   1) Codex and Claude connect concurrently on /agency/ws
   2) both complete ready handshake
   3) Codex action routes to :edit
   4) Claude action routes to :explore
   5) coordination message routes to direct invoke
   6) connected roster reflects both connect/disconnect transitions"
  (:require [cheshire.core :as json]
            [futon3c.agency.registry :as reg]
            [futon3c.peripheral.registry :as preg]
            [futon3c.peripheral.tools :as tools]
            [futon3c.runtime.agents :as runtime]
            [futon3c.social.persist :as persist]
            [futon3c.transport.http :as http]
            [futon3c.transport.ws :as ws])
  (:import [java.net ServerSocket URI]
           [java.net.http HttpClient WebSocket WebSocket$Listener]
           [java.util.concurrent CompletableFuture]))

(defn- free-port []
  (with-open [ss (ServerSocket. 0)]
    (.getLocalPort ss)))

(defn- ensure!
  [pred message data]
  (when-not pred
    (throw (ex-info message data))))

(defn- wait-until
  [pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (cond
        (pred) true
        (> (System/currentTimeMillis) deadline) false
        :else (do (Thread/sleep 25) (recur))))))

(defn- find-frame
  [frames pred]
  (some (fn [frame]
          (when (pred frame) frame))
        (map #(json/parse-string % true) @frames)))

(defn- wait-for-frame
  [frames pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (if-let [frame (find-frame frames pred)]
        frame
        (if (> (System/currentTimeMillis) deadline)
          nil
          (do (Thread/sleep 25) (recur)))))))

(defn- connected-agents
  [connected-url]
  (let [body (json/parse-string (slurp connected-url) true)]
    {:agents (set (get body :connected_agents []))
     :count (get body :count 0)
     :raw body}))

(defn- mk-listener
  [frames errors opened-p closed-p]
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

(defn- send-json!
  [^WebSocket web-socket payload]
  (.join (.sendText web-socket (json/generate-string payload) true)))

(defn- run-smoke!
  []
  (let [port (free-port)
        host "127.0.0.1"
        codex-id "codex-1"
        claude-id "claude-1"
        session-id "sess-alleycat-dual-smoke"
        codex-url (str "ws://" host ":" port "/agency/ws?agent-id=" codex-id "&session-id=" session-id)
        claude-url (str "ws://" host ":" port "/agency/ws?agent-id=" claude-id "&session-id=" session-id)
        connected-url (str "http://" host ":" port "/agency/connected")]
    (reg/reset-registry!)
    (persist/reset-sessions!)

    (runtime/register-codex!
      {:agent-id codex-id
       :invoke-fn (fn [prompt prior-session-id]
                    {:result (str "codex-ok:" prompt)
                     :session-id prior-session-id
                     :exit-code 0})})

    (runtime/register-claude!
      {:agent-id claude-id
       :invoke-fn (fn [prompt prior-session-id]
                    {:result (str "claude-ok:" prompt)
                     :session-id prior-session-id
                     :exit-code 0})})

    (let [opts {:patterns {:patterns/ids [:alleycat/ws-smoke :alleycat/dual-agent]}
                :peripheral-config {:backend (tools/make-mock-backend)
                                    :peripherals (preg/load-peripherals)}}
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
          server (http/start-server! app port)
          client (HttpClient/newHttpClient)
          codex-frames (atom [])
          codex-errors (atom [])
          codex-opened (promise)
          codex-closed (promise)
          claude-frames (atom [])
          claude-errors (atom [])
          claude-opened (promise)
          claude-closed (promise)]
      (try
        (let [codex-ws (.join (.buildAsync (.newWebSocketBuilder client)
                                           (URI/create codex-url)
                                           (mk-listener codex-frames codex-errors codex-opened codex-closed)))
              claude-ws (.join (.buildAsync (.newWebSocketBuilder client)
                                            (URI/create claude-url)
                                            (mk-listener claude-frames claude-errors claude-opened claude-closed)))]

          (ensure! (some? (deref codex-opened 2000 nil))
                   "Codex WS open timeout"
                   {:url codex-url :errors @codex-errors})
          (ensure! (some? (deref claude-opened 2000 nil))
                   "Claude WS open timeout"
                   {:url claude-url :errors @claude-errors})

          (send-json! codex-ws {"type" "ready" "agent_id" codex-id "session_id" session-id})
          (send-json! claude-ws {"type" "ready" "agent_id" claude-id "session_id" session-id})

          (let [codex-ack (wait-for-frame codex-frames #(= "ready_ack" (:type %)) 3000)
                claude-ack (wait-for-frame claude-frames #(= "ready_ack" (:type %)) 3000)]
            (ensure! (some? codex-ack)
                     "Missing codex ready_ack"
                     {:frames @codex-frames :errors @codex-errors})
            (ensure! (some? claude-ack)
                     "Missing claude ready_ack"
                     {:frames @claude-frames :errors @claude-errors}))

          (let [{:keys [agents count raw]} (connected-agents connected-url)]
            (println "Connected after ready:" raw)
            (ensure! (= #{codex-id claude-id} agents)
                     "Connected roster mismatch after ready"
                     {:agents agents :raw raw})
            (ensure! (= 2 count)
                     "Connected count mismatch after ready"
                     {:count count :raw raw}))

          (send-json! codex-ws {"type" "message"
                                "msg_id" "alleycat-codex-edit-1"
                                "payload" "fix failing integration test"
                                "to" codex-id})

          (let [codex-edit-receipt
                (wait-for-frame
                 codex-frames
                 #(and (= "receipt" (:type %))
                       (= "alleycat-codex-edit-1" (:msg_id %)))
                 3000)]
            (println "Codex action receipt:" codex-edit-receipt)
            (ensure! (some? codex-edit-receipt)
                     "Missing codex action receipt"
                     {:frames @codex-frames :errors @codex-errors})
            (ensure! (= "peripheral/run-chain" (:route codex-edit-receipt))
                     "Codex action route mismatch"
                     {:receipt codex-edit-receipt})
            (ensure! (= "edit" (:peripheral_id codex-edit-receipt))
                     "Codex default peripheral mismatch"
                     {:receipt codex-edit-receipt}))

          (send-json! claude-ws {"type" "message"
                                 "msg_id" "alleycat-claude-explore-1"
                                 "payload" "investigate flaky test cluster"
                                 "to" claude-id})

          (let [claude-action-receipt
                (wait-for-frame
                 claude-frames
                 #(and (= "receipt" (:type %))
                       (= "alleycat-claude-explore-1" (:msg_id %)))
                 3000)]
            (println "Claude action receipt:" claude-action-receipt)
            (ensure! (some? claude-action-receipt)
                     "Missing claude action receipt"
                     {:frames @claude-frames :errors @claude-errors})
            (ensure! (= "peripheral/run-chain" (:route claude-action-receipt))
                     "Claude action route mismatch"
                     {:receipt claude-action-receipt})
            (ensure! (= "explore" (:peripheral_id claude-action-receipt))
                     "Claude default peripheral mismatch"
                     {:receipt claude-action-receipt}))

          (send-json! codex-ws {"type" "message"
                                "msg_id" "alleycat-codex-coordination-1"
                                "payload" {"type" "standup" "note" "handoff to claude"}
                                "to" claude-id})

          (let [coord-receipt
                (wait-for-frame
                 codex-frames
                 #(and (= "receipt" (:type %))
                       (= "alleycat-codex-coordination-1" (:msg_id %)))
                 3000)]
            (println "Coordination receipt:" coord-receipt)
            (ensure! (some? coord-receipt)
                     "Missing coordination receipt"
                     {:frames @codex-frames :errors @codex-errors})
            (ensure! (= "registry/invoke" (:route coord-receipt))
                     "Coordination route mismatch"
                     {:receipt coord-receipt}))

          (.join (.sendClose codex-ws WebSocket/NORMAL_CLOSURE "codex done"))
          (ensure! (some? (deref codex-closed 2000 nil))
                   "Codex WS close timeout"
                   {:errors @codex-errors})

          (ensure! (wait-until #(= #{claude-id} (:agents (connected-agents connected-url))) 2000)
                   "Connected roster did not drop codex after close"
                   {:connected (connected-agents connected-url)})
          (println "Connected after codex close:" (:raw (connected-agents connected-url)))

          (.join (.sendClose claude-ws WebSocket/NORMAL_CLOSURE "claude done"))
          (ensure! (some? (deref claude-closed 2000 nil))
                   "Claude WS close timeout"
                   {:errors @claude-errors})

          (ensure! (wait-until #(empty? (:agents (connected-agents connected-url))) 2000)
                   "Connected roster not empty after both closes"
                   {:connected (connected-agents connected-url)})
          (println "Connected after both close:" (:raw (connected-agents connected-url)))
          (println "PASS: Dual-agent WS runtime validated (codex+claude peripherals + coordination)."))
        (finally
          ((:server server)))))))

(run-smoke!)
