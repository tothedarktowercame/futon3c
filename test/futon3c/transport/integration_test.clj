(ns futon3c.transport.integration-test
  "M-transport-adapters Part IV: integration tests.

   End-to-end scenarios combining HTTP and WebSocket adapters with the full
   social pipeline (presence → mode → dispatch), peripheral dispatch, evidence
   threading, and session persistence.

   HTTP tests exercise the Ring handler directly. WS tests use mock
   send-fn/close-fn callbacks (same pattern as ws_test.clj). The 'integration'
   aspect is that both adapters route through the real pipeline, including
   peripheral dispatch with mock backends."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [cheshire.core :as json]
            [futon3c.transport.http :as http]
            [futon3c.transport.ws :as ws]
            [futon3c.transport.protocol :as proto]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.social.persist :as persist]
            [futon3c.agency.registry :as reg]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(use-fixtures
  :each
  (fn [f]
    (reg/reset-registry!)
    (persist/reset-sessions!)
    (f)))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- register-mock-agent!
  "Register a mock agent in the live registry."
  [agent-id-str type]
  (reg/register-agent!
   {:agent-id {:id/value agent-id-str :id/type :continuity}
    :type type
    :invoke-fn (fn [_prompt _session-id] {:result "ok" :session-id nil :exit-code 0})
    :capabilities [:explore :edit :test]}))

(defn- make-shared-config
  "Create a pipeline config usable by both HTTP and WS adapters.
   Includes peripheral-config for action-mode dispatch."
  ([] (make-shared-config {}))
  ([overrides]
   (let [registry (fix/mock-registry
                   {:peripheral-config (fix/make-peripheral-config)})]
     (merge {:registry registry
             :patterns (fix/mock-patterns)}
            overrides))))

(defn- make-http-handler
  "Create HTTP handler with shared config."
  ([] (make-http-handler (make-shared-config)))
  ([config] (http/make-handler config)))

(defn- make-test-ws
  "Create WS callbacks with mock send/close and shared config."
  ([] (make-test-ws (make-shared-config)))
  ([config]
   (let [sent (atom [])
         closed (atom [])
         ws-config (assoc config
                          :send-fn (fn [ch data] (swap! sent conj {:ch ch :data data}))
                          :close-fn (fn [ch] (swap! closed conj ch)))
         callbacks (ws/make-ws-callbacks ws-config)]
     (assoc callbacks :sent sent :closed closed))))

(defn- post [handler uri body-str]
  (handler {:request-method :post :uri uri :body body-str}))

(defn- get-req [handler uri]
  (handler {:request-method :get :uri uri}))

(defn- parse-body [response]
  (json/parse-string (:body response) true))

(defn- last-sent [sent-atom]
  (when-let [entry (last @sent-atom)]
    (json/parse-string (:data entry) true)))

(defn- wait-until
  [pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (cond
        (pred) true
        (> (System/currentTimeMillis) deadline) false
        :else (do (Thread/sleep 10) (recur))))))

(defn- ready-frame [agent-id & {:keys [session-id]}]
  (json/generate-string
   (cond-> {"type" "ready" "agent_id" agent-id}
     session-id (assoc "session_id" session-id))))

(defn- message-frame [msg-id payload to]
  (json/generate-string
   {"type" "message" "msg_id" msg-id "payload" payload "to" to}))

;; =============================================================================
;; 1. HTTP dispatch round-trip
;; =============================================================================

(deftest http-dispatch-round-trip
  (testing "POST /dispatch → pipeline → JSON receipt (coordination message)"
    (register-mock-agent! "claude-1" :claude)
    (let [handler (make-http-handler)
          body (json/generate-string {"msg_id" "int-msg-1"
                                      "payload" {"type" "standup"}
                                      "from" "claude-1"
                                      "to" "claude-1"})
          response (post handler "/dispatch" body)
          receipt (parse-body response)]
      (is (= 200 (:status response)))
      (is (= "application/json" (get-in response [:headers "Content-Type"])))
      (is (= "int-msg-1" (:msg_id receipt)))
      (is (true? (:delivered receipt)))
      (is (= "registry/invoke" (:route receipt))
          "coordination message → direct invoke route"))))

;; =============================================================================
;; 2. HTTP action message → peripheral
;; =============================================================================

(deftest http-action-message-to-peripheral
  (testing "POST /dispatch with action payload → peripheral dispatch → receipt with session-id and fruit"
    (register-mock-agent! "claude-1" :claude)
    (let [handler (make-http-handler)
          body (json/generate-string {"msg_id" "int-msg-2"
                                      "payload" "implement feature X"
                                      "from" "claude-1"
                                      "to" "claude-1"})
          response (post handler "/dispatch" body)
          receipt (parse-body response)]
      (is (= 200 (:status response)))
      (is (= "int-msg-2" (:msg_id receipt)))
      (is (true? (:delivered receipt)))
      (is (= "peripheral/run-chain" (:route receipt))
          "action message → peripheral route")
      (is (string? (:session_id receipt))
          "receipt includes session-id from peripheral dispatch")
      (is (= "explore" (:peripheral_id receipt))
          "claude agent → :explore peripheral"))))

;; =============================================================================
;; 3. WS connection lifecycle: open → ready → message → receipt → close
;; =============================================================================

(deftest ws-connection-lifecycle
  (testing "full WS lifecycle: open → ready handshake → message → receipt → close"
    (register-mock-agent! "claude-1" :claude)
    (let [{:keys [on-open on-receive on-close connections sent closed]} (make-test-ws)
          ch :int-ch-1
          request {:request-method :get :uri "/ws" :request-uri "/ws"}]
      ;; Open
      (on-open ch request)
      (is (some? (get @connections ch)) "connection registered after open")
      (is (false? (:connected? (get @connections ch))) "starts in :connecting state")

      ;; Ready handshake
      (on-receive ch (ready-frame "claude-1"))
      (is (true? (:connected? (get @connections ch))) "connected after handshake")
      (let [ack (last-sent sent)]
        (is (= "ready_ack" (:type ack))))

      ;; Send coordination message
      (reset! sent [])
      (on-receive ch (message-frame "int-msg-3" {"type" "standup"} "claude-1"))
      (let [receipt (last-sent sent)]
        (is (= "receipt" (:type receipt)))
        (is (= "int-msg-3" (:msg_id receipt)))
        (is (true? (:delivered receipt))))

      ;; Close
      (on-close ch :normal)
      (is (nil? (get @connections ch)) "connection removed after close"))))

;; =============================================================================
;; 4. WS coordination message
;; =============================================================================

(deftest ws-coordination-message
  (testing "WS ready → coordination payload → direct-invoke receipt (no peripheral)"
    (register-mock-agent! "claude-1" :claude)
    (let [{:keys [on-open on-receive sent]} (make-test-ws)
          ch :int-ch-2
          request {:request-method :get :uri "/ws" :request-uri "/ws"}]
      (on-open ch request)
      (on-receive ch (ready-frame "claude-1"))
      (reset! sent [])

      (on-receive ch (message-frame "int-msg-4" {"type" "standup"} "claude-1"))
      (let [receipt (last-sent sent)]
        (is (= "receipt" (:type receipt)))
        (is (true? (:delivered receipt)))
        (is (= "registry/invoke" (:route receipt))
            "coordination → direct invoke")
        (is (nil? (:session_id receipt))
            "no session-id for coordination message")
        (is (nil? (:peripheral_id receipt))
            "no peripheral-id for coordination message")))))

(deftest http-dispatch-to-ws-invoke-agent
  (testing "HTTP coordination dispatch can invoke a WS-only agent via invoke/invoke_result"
    (register-mock-agent! "claude-1" :claude)
    ;; Target agent has no local invoke-fn; registry should fall back to WS bridge.
    (reg/register-agent!
     {:agent-id {:id/value "codex-ws-1" :id/type :continuity}
      :type :codex
      :invoke-fn nil
      :capabilities [:edit]})
    (let [handler (make-http-handler)
          {:keys [on-open on-receive sent]} (make-test-ws)
          ch :int-ch-ws-invoke-1
          request {:request-method :get :uri "/ws" :request-uri "/ws"}]
      (on-open ch request)
      (on-receive ch (ready-frame "codex-ws-1"))
      (reset! sent [])

      (let [http-f (future
                     (post handler
                           "/dispatch"
                           (json/generate-string {"msg_id" "int-msg-ws-invoke-1"
                                                  "payload" {"type" "standup"}
                                                  "from" "claude-1"
                                                  "to" "codex-ws-1"})))]
        (is (wait-until #(some? (last-sent sent)) 2000)
            "expected server to send invoke frame to WS agent")
        (let [invoke-frame (last-sent sent)
              invoke-id (:invoke_id invoke-frame)]
          (is (= "invoke" (:type invoke-frame)))
          (is (string? invoke-id))
          (on-receive ch (json/generate-string {"type" "invoke_result"
                                                "invoke_id" invoke-id
                                                "result" "ws-ack"
                                                "session_id" "sess-ws-invoke-1"})))

        (let [response (deref http-f 3000 :timeout)]
          (is (not= :timeout response) "HTTP dispatch should complete after invoke_result")
          (is (= 200 (:status response)))
          (let [receipt (parse-body response)]
            (is (= "int-msg-ws-invoke-1" (:msg_id receipt)))
            (is (true? (:delivered receipt)))
            (is (= "registry/invoke" (:route receipt))))
          (is (= "sess-ws-invoke-1"
                 (:agent/session-id
                  (reg/get-agent {:id/value "codex-ws-1" :id/type :continuity})))))))))

;; =============================================================================
;; 5. WS action message → peripheral
;; =============================================================================

(deftest ws-action-message-to-peripheral
  (testing "WS ready → action payload → peripheral dispatch → receipt with session-id"
    (register-mock-agent! "claude-1" :claude)
    (let [{:keys [on-open on-receive sent]} (make-test-ws)
          ch :int-ch-3
          request {:request-method :get :uri "/ws" :request-uri "/ws"}]
      (on-open ch request)
      (on-receive ch (ready-frame "claude-1"))
      (reset! sent [])

      ;; String payload → classified as :action by S-mode
      (on-receive ch (message-frame "int-msg-5" "implement feature Y" "claude-1"))
      (let [receipt (last-sent sent)]
        (is (= "receipt" (:type receipt)))
        (is (true? (:delivered receipt)))
        (is (= "peripheral/run-chain" (:route receipt))
            "action message → peripheral route")
        (is (string? (:session_id receipt))
            "receipt includes session-id from peripheral dispatch")
        (is (= "explore" (:peripheral_id receipt))
            "claude agent → :explore peripheral")))))

;; =============================================================================
;; 5b. HTTP codex action message → :edit peripheral
;; =============================================================================

(deftest http-codex-action-message-to-edit-peripheral
  (testing "POST /dispatch codex action payload → :edit peripheral route"
    (register-mock-agent! "codex-1" :codex)
    (let [handler (make-http-handler)
          body (json/generate-string {"msg_id" "int-msg-codex-http-1"
                                      "payload" "fix failing test"
                                      "from" "codex-1"
                                      "to" "codex-1"})
          response (post handler "/dispatch" body)
          receipt (parse-body response)]
      (is (= 200 (:status response)))
      (is (true? (:delivered receipt)))
      (is (= "peripheral/run-chain" (:route receipt)))
      (is (= "edit" (:peripheral_id receipt))
          "codex action should route to :edit by default"))))

;; =============================================================================
;; 5c. WS codex action message → :edit peripheral
;; =============================================================================

(deftest ws-codex-action-message-to-edit-peripheral
  (testing "WS ready codex → action payload → :edit peripheral receipt"
    (register-mock-agent! "codex-1" :codex)
    (let [{:keys [on-open on-receive sent]} (make-test-ws)
          ch :int-ch-codex-1
          request {:request-method :get :uri "/ws" :request-uri "/ws"}]
      (on-open ch request)
      (on-receive ch (ready-frame "codex-1"))
      (reset! sent [])
      (on-receive ch (message-frame "int-msg-codex-ws-1" "fix auth bug" "codex-1"))
      (let [receipt (last-sent sent)]
        (is (= "receipt" (:type receipt)))
        (is (true? (:delivered receipt)))
        (is (= "peripheral/run-chain" (:route receipt)))
        (is (= "edit" (:peripheral_id receipt))
            "codex action should route to :edit by default")))))

;; =============================================================================
;; 6. WS handshake failure
;; =============================================================================

(deftest ws-handshake-failure
  (testing "WS open → ready with unknown agent → error + close"
    (let [{:keys [on-open on-receive sent closed]} (make-test-ws)
          ch :int-ch-4
          request {:request-method :get :uri "/ws" :request-uri "/ws"}]
      ;; No agent registered — "ghost-agent" is unknown
      (on-open ch request)
      (on-receive ch (ready-frame "ghost-agent"))
      (let [frame (last-sent sent)]
        (is (= "error" (:type frame)))
        (is (string? (:code frame))))
      (is (contains? (set @closed) ch)
          "connection closed after failed handshake"))))

;; =============================================================================
;; 7. WS message before ready
;; =============================================================================

(deftest ws-message-before-ready
  (testing "WS open → message without handshake → error frame"
    (let [{:keys [on-open on-receive sent]} (make-test-ws)
          ch :int-ch-5
          request {:request-method :get :uri "/ws" :request-uri "/ws"}]
      (on-open ch request)
      ;; Send message WITHOUT ready handshake
      (on-receive ch (message-frame "int-msg-6" "hello" "claude-1"))
      (let [frame (last-sent sent)]
        (is (= "error" (:type frame)))
        (is (= "not-ready" (:code frame))
            "R7: must complete readiness handshake first")))))

;; =============================================================================
;; 8. HTTP + WS coexistence
;; =============================================================================

(deftest http-and-ws-coexistence
  (testing "HTTP and WS adapters share the same pipeline config and both work"
    (register-mock-agent! "claude-1" :claude)
    (let [config (make-shared-config)
          handler (make-http-handler config)
          {:keys [on-open on-receive sent]} (make-test-ws config)
          ch :int-ch-6
          request {:request-method :get :uri "/ws" :request-uri "/ws"}]
      ;; HTTP dispatch — coordination message
      (let [response (post handler "/dispatch"
                           (json/generate-string {"msg_id" "int-co-1"
                                                  "payload" {"type" "standup"}
                                                  "from" "claude-1"
                                                  "to" "claude-1"}))
            receipt (parse-body response)]
        (is (= 200 (:status response)))
        (is (true? (:delivered receipt))))

      ;; WS dispatch — action message
      (on-open ch request)
      (on-receive ch (ready-frame "claude-1"))
      (reset! sent [])
      (on-receive ch (message-frame "int-ws-1" "implement feature Z" "claude-1"))
      (let [receipt (last-sent sent)]
        (is (= "receipt" (:type receipt)))
        (is (true? (:delivered receipt)))
        (is (string? (:session_id receipt))
            "WS action message produces peripheral session"))

      ;; HTTP health check still works
      (let [health (get-req handler "/health")]
        (is (= 200 (:status health)))
        (let [parsed (parse-body health)]
          (is (= "ok" (:status parsed)))
          (is (= 3 (:agents parsed))
              "registry has 3 agents (from mock-registry)"))))))

;; =============================================================================
;; 9. Error propagation via WS
;; =============================================================================

(deftest ws-error-propagation
  (testing "peripheral failure → WS error frame with correct code"
    (register-mock-agent! "claude-1" :claude)
    (with-redefs [futon3c.peripheral.registry/run-chain
                  (fn [_ _ _] (throw (ex-info "peripheral crashed" {:reason :test})))]
      (let [{:keys [on-open on-receive sent]} (make-test-ws)
            ch :int-ch-7
            request {:request-method :get :uri "/ws" :request-uri "/ws"}]
        (on-open ch request)
        (on-receive ch (ready-frame "claude-1"))
        (reset! sent [])

        ;; Action message triggers peripheral dispatch, which will fail
        (on-receive ch (message-frame "int-msg-err" "trigger failure" "claude-1"))
        (let [frame (last-sent sent)]
          (is (= "error" (:type frame))
              "peripheral failure surfaces as error frame")
          (is (= "peripheral-failed" (:code frame))
              "error code propagated correctly through dispatch → WS"))))))

;; =============================================================================
;; 10. Graceful shutdown with WS connected
;; =============================================================================

(deftest graceful-shutdown-with-ws-connected
  (testing "stop server while WS connected → connections closed cleanly"
    (register-mock-agent! "claude-1" :claude)
    (let [free-port (with-open [ss (java.net.ServerSocket. 0)]
                      (.getLocalPort ss))
          config (make-shared-config)
          ;; Start real HTTP server
          handler (make-http-handler config)
          server-info (http/start-server! handler free-port)]
      (try
        (is (= free-port (:port server-info)))
        ;; Verify server is running
        (let [sock (java.net.Socket.)]
          (try
            (.connect sock (java.net.InetSocketAddress. "localhost" (int free-port)) 1000)
            (is true "Server is reachable")
            (finally (.close sock))))

        ;; Simulate WS connection via callbacks (separate from server —
        ;; the integration point is that both use the same config)
        (let [{:keys [on-open on-receive on-close connections]} (make-test-ws config)
              ch :int-ch-shutdown
              request {:request-method :get :uri "/ws" :request-uri "/ws"}]
          (on-open ch request)
          (on-receive ch (ready-frame "claude-1"))
          (is (= ["claude-1"] (ws/connected-agents connections)))

          ;; Shutdown server
          ((:server server-info))
          (Thread/sleep 200)

          ;; Clean up WS connection (in production, http-kit would fire on-close)
          (on-close ch :server-close)
          (is (empty? (ws/connected-agents connections))
              "WS connections cleaned up after close"))

        ;; Port should no longer be listening
        (let [reachable? (try
                           (with-open [sock (java.net.Socket.)]
                             (.connect sock (java.net.InetSocketAddress. "localhost" (int free-port)) 500)
                             true)
                           (catch Exception _ false))]
          (is (not reachable?) "Port closed after shutdown"))
        (finally
          ;; Ensure cleanup even on test failure
          (try ((:server server-info)) (catch Exception _)))))))
