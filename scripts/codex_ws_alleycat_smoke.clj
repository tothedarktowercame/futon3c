(ns scripts.codex-ws-alleycat-smoke
  "Alleycat-style Codex WS readiness smoke.

   Validates the practical runtime path:
   1) WS connect at /agency/ws
   2) ready handshake
   3) action frame dispatch
   4) receipt route is peripheral/run-chain with peripheral_id=edit
   5) connected roster reflects connect/disconnect transitions"
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

(defn- wait-until
  [pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (cond
        (pred) true
        (> (System/currentTimeMillis) deadline) false
        :else (do (Thread/sleep 25) (recur))))))

(defn- run-smoke!
  []
  (let [port (free-port)
        host "127.0.0.1"
        codex-id "codex-1"
        session-id "sess-alleycat-codex-smoke"
        ws-url (str "ws://" host ":" port "/agency/ws?agent-id=" codex-id "&session-id=" session-id)
        connected-url (str "http://" host ":" port "/agency/connected")]
    (reg/reset-registry!)
    (persist/reset-sessions!)

    (runtime/register-codex!
      {:agent-id codex-id
       :invoke-fn (fn [_prompt prior-session-id]
                    {:result "ok" :session-id prior-session-id :exit-code 0})})

    (let [opts {:patterns {:patterns/ids [:alleycat/ws-smoke]}
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
          frames (atom [])
          errors (atom [])
          open-p (promise)
          close-p (promise)]
      (try
        (let [listener (reify WebSocket$Listener
                         (onOpen [_ web-socket]
                           (deliver open-p web-socket)
                           (.request web-socket 1))
                         (onText [_ web-socket data _last]
                           (swap! frames conj (str data))
                           (.request web-socket 1)
                           (CompletableFuture/completedFuture nil))
                         (onClose [_ _web-socket _status-code _reason]
                           (deliver close-p true)
                           (CompletableFuture/completedFuture nil))
                         (onError [_ _web-socket error]
                           (swap! errors conj (str error))))
              web-socket (.join (.buildAsync (.newWebSocketBuilder client)
                                             (URI/create ws-url)
                                             listener))]

          (when-not (deref open-p 2000 nil)
            (throw (ex-info "WS open timeout" {})))

          (.join (.sendText web-socket
                            (json/generate-string {"type" "ready"
                                                   "agent_id" codex-id
                                                   "session_id" session-id})
                            true))

          (.join (.sendText web-socket
                            (json/generate-string {"type" "message"
                                                   "msg_id" "alleycat-codex-1"
                                                   "payload" "fix failing integration test"
                                                   "to" codex-id})
                            true))

          (when-not (wait-until #(>= (count @frames) 2) 3000)
            (throw (ex-info "Did not receive expected WS frames"
                            {:frames @frames :errors @errors})))

          (let [[ack-frame receipt-frame] (map #(json/parse-string % true) @frames)
                connected-before (json/parse-string (slurp connected-url) true)]
            (println "WS URL:" ws-url)
            (println "Ack frame:" ack-frame)
            (println "Receipt frame:" receipt-frame)
            (println "Connected before close:" connected-before)

            (when-not (= "ready_ack" (:type ack-frame))
              (throw (ex-info "Expected ready_ack" {:ack ack-frame})))
            (when-not (= "receipt" (:type receipt-frame))
              (throw (ex-info "Expected receipt frame" {:receipt receipt-frame})))
            (when-not (= "peripheral/run-chain" (:route receipt-frame))
              (throw (ex-info "Expected peripheral route" {:receipt receipt-frame})))
            (when-not (= "edit" (:peripheral_id receipt-frame))
              (throw (ex-info "Expected codex route to :edit" {:receipt receipt-frame})))
            (when-not (= [codex-id] (get connected-before :connected_agents))
              (throw (ex-info "Expected codex in connected list"
                              {:connected connected-before}))))

          (.join (.sendClose web-socket WebSocket/NORMAL_CLOSURE "done"))
          (deref close-p 2000 nil)
          (Thread/sleep 100)

          (let [connected-after (json/parse-string (slurp connected-url) true)]
            (println "Connected after close:" connected-after)
            (when-not (= [] (get connected-after :connected_agents))
              (throw (ex-info "Expected connected list to be empty after close"
                              {:connected connected-after}))))

          (println "PASS: Codex WS ready/action flow routed through edit peripheral."))
        (finally
          ((:server server)))))))

(run-smoke!)
