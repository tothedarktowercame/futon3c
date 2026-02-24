(ns futon3c.transport.ws-test
  "Tests for WebSocket adapter (Part III).

   Tests use make-ws-callbacks directly with mock send-fn/close-fn,
   avoiding the need for actual http-kit connections. Mock channels are
   plain keywords used as map keys in the connection registry."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [cheshire.core :as json]
            [futon3c.transport.ws :as ws]
            [futon3c.transport.protocol :as proto]
            [futon3c.transport.ws.invoke :as ws-invoke]
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
;; Test helpers
;; =============================================================================

(defn- register-mock-agent!
  "Register a mock agent in the live registry."
  [agent-id-str type]
  (reg/register-agent!
   {:agent-id {:id/value agent-id-str :id/type :continuity}
    :type type
    :invoke-fn (fn [_prompt _session-id] {:result "ok" :session-id nil})
    :capabilities [:explore :edit]}))

(defn- make-test-ws
  "Create WS callbacks with mock send/close for testing.
   Returns callbacks + :sent atom + :closed atom for assertions."
  [& {:keys [registry patterns on-connect on-disconnect]
      :or {registry (fix/mock-registry)
           patterns (fix/mock-patterns)}}]
  (let [sent (atom [])
        closed (atom [])
        config {:registry registry
                :patterns patterns
                :send-fn (fn [ch data] (swap! sent conj {:ch ch :data data}))
                :close-fn (fn [ch] (swap! closed conj ch))
                :on-connect on-connect
                :on-disconnect on-disconnect}
        callbacks (ws/make-ws-callbacks config)]
    (assoc callbacks :sent sent :closed closed)))

(defn- mock-request
  "Create a mock Ring request for WS upgrade."
  [& {:keys [query-string request-uri]
      :or {request-uri "/ws"}}]
  (cond-> {:request-method :get
           :uri "/ws"
           :request-uri request-uri}
    query-string (assoc :query-string query-string)))

(defn- last-sent
  "Get the last sent frame as a parsed map."
  [sent-atom]
  (when-let [entry (last @sent-atom)]
    (json/parse-string (:data entry) true)))

(defn- ready-frame [agent-id & {:keys [session-id]}]
  (json/generate-string
   (cond-> {"type" "ready" "agent_id" agent-id}
     session-id (assoc "session_id" session-id))))

(defn- message-frame [msg-id payload to]
  (json/generate-string
   {"type" "message" "msg_id" msg-id "payload" payload "to" to}))

;; =============================================================================
;; on-open tests
;; =============================================================================

(deftest ws-open-creates-connecting-state
  (testing "on-open creates connection in :connecting state (L2: not yet :connected)"
    (let [{:keys [on-open connections]} (make-test-ws)
          ch :test-ch-1
          request (mock-request)]
      (on-open ch request)
      (let [conn (get @connections ch)]
        (is (some? conn))
        (is (false? (:connected? conn)))
        (is (= ch (:channel conn)))
        (is (= :websocket (:transport conn)) "connection records transport type")
        (is (string? (:opened-at conn)))))))

(deftest ws-open-extracts-params-from-request-uri
  (testing "on-open extracts agent-id from request-uri (L3: handles missing :query-string)"
    (let [{:keys [on-open connections]} (make-test-ws)
          ch :test-ch-2
          request (mock-request :request-uri "/ws?agent_id=claude-1&session_id=sess-42")]
      (on-open ch request)
      (let [conn (get @connections ch)]
        (is (= "claude-1" (:agent-id conn)))
        (is (= "sess-42" (:session-id conn)))))))

(deftest ws-open-extracts-params-from-query-string
  (testing "on-open prefers :query-string over :request-uri (L3: tier 1)"
    (let [{:keys [on-open connections]} (make-test-ws)
          ch :test-ch-3
          request (mock-request :query-string "agent_id=from-qs"
                                :request-uri "/ws?agent_id=from-uri")]
      (on-open ch request)
      (is (= "from-qs" (:agent-id (get @connections ch)))))))

;; =============================================================================
;; Readiness handshake tests (R7)
;; =============================================================================

(deftest ws-ready-handshake-valid-agent
  (testing "readiness handshake with known agent → ready_ack sent, :connected true"
    (let [{:keys [on-open on-receive connections sent]} (make-test-ws)
          ch :test-ch-4
          request (mock-request)]
      (on-open ch request)
      (on-receive ch (ready-frame "claude-1" :session-id "sess-1"))
      ;; Check connection state
      (let [conn (get @connections ch)]
        (is (true? (:connected? conn)))
        (is (= "claude-1" (:agent-id conn)))
        (is (= "sess-1" (:session-id conn))))
      ;; Check sent frame
      (let [frame (last-sent sent)]
        (is (= "ready_ack" (:type frame)))))))

(deftest ws-ready-handshake-unknown-agent
  (testing "readiness handshake with unknown agent → error sent + connection closed"
    (let [{:keys [on-open on-receive connections sent closed]} (make-test-ws)
          ch :test-ch-5
          request (mock-request)]
      (on-open ch request)
      (on-receive ch (ready-frame "ghost-agent"))
      ;; Connection should still be :connecting (not promoted)
      ;; Actually it might be removed by close, but let's check sent/closed
      (let [frame (last-sent sent)]
        (is (= "error" (:type frame))))
      (is (contains? (set @closed) ch)
          "Connection should be closed after failed handshake"))))

(deftest ws-ready-handshake-generates-session-id
  (testing "ready frame without session_id → server generates one"
    (let [{:keys [on-open on-receive connections]} (make-test-ws)
          ch :test-ch-6
          request (mock-request)]
      (on-open ch request)
      (on-receive ch (ready-frame "claude-1"))
      (let [conn (get @connections ch)]
        (is (string? (:session-id conn)))
        (is (clojure.string/starts-with? (:session-id conn) "sess-"))))))

;; =============================================================================
;; Message dispatch tests
;; =============================================================================

(deftest ws-message-after-handshake-dispatched
  (testing "message after successful handshake → dispatched, receipt sent"
    (register-mock-agent! "claude-1" :claude)
    (let [{:keys [on-open on-receive sent]} (make-test-ws)
          ch :test-ch-7
          request (mock-request)]
      (on-open ch request)
      ;; Complete handshake
      (on-receive ch (ready-frame "claude-1"))
      (reset! sent [])
      ;; Send message
      (on-receive ch (message-frame "msg-1" {"type" "standup"} "claude-1"))
      (let [frame (last-sent sent)]
        (is (= "receipt" (:type frame)))
        (is (= "msg-1" (:msg_id frame)))
        (is (true? (:delivered frame)))))))

(deftest ws-message-before-handshake-rejected
  (testing "message before readiness handshake → error frame (R7)"
    (let [{:keys [on-open on-receive sent]} (make-test-ws)
          ch :test-ch-8
          request (mock-request)]
      (on-open ch request)
      ;; Send message WITHOUT completing handshake
      (on-receive ch (message-frame "msg-2" "hello" "claude-1"))
      (let [frame (last-sent sent)]
        (is (= "error" (:type frame)))
        (is (= "not-ready" (:code frame)))))))

(deftest ws-message-uses-authenticated-agent-id
  (testing "message :from is overridden with handshake agent-id (prevents impersonation)"
    (register-mock-agent! "claude-1" :claude)
    (let [{:keys [on-open on-receive sent]} (make-test-ws)
          ch :test-ch-9
          request (mock-request)]
      (on-open ch request)
      (on-receive ch (ready-frame "claude-1"))
      (reset! sent [])
      ;; Send message claiming to be from a different agent
      (on-receive ch (json/generate-string
                      {"type" "message"
                       "msg_id" "msg-3"
                       "payload" {"type" "standup"}
                       "from" {"value" "evil-agent" "type" "continuity"}
                       "to" "claude-1"}))
      ;; Should still dispatch successfully (from is overridden to claude-1)
      (let [frame (last-sent sent)]
        (is (= "receipt" (:type frame)))))))

(deftest ws-invoke-result-resolves-pending-call
  (testing "invoke_result frame resolves pending WS invoke and emits no error"
    (let [resolved (atom nil)
          {:keys [on-open on-receive sent]} (make-test-ws)
          ch :test-ch-invoke-ok
          request (mock-request)]
      (with-redefs [ws-invoke/resolve! (fn [agent-id invoke-id result]
                                         (reset! resolved [agent-id invoke-id result])
                                         true)]
        (on-open ch request)
        (on-receive ch (ready-frame "claude-1"))
        (reset! sent [])
        (on-receive ch (json/generate-string {"type" "invoke_result"
                                              "invoke_id" "inv-1"
                                              "result" "ok"
                                              "session_id" "sess-x"}))
        (is (= ["claude-1" "inv-1" {:result "ok"
                                    :session-id "sess-x"
                                    :error nil}]
               @resolved))
        (is (empty? @sent))))))

(deftest ws-invoke-result-unknown-invoke-returns-error
  (testing "invoke_result for unknown invoke emits unknown-invoke error"
    (let [{:keys [on-open on-receive sent]} (make-test-ws)
          ch :test-ch-invoke-miss
          request (mock-request)]
      (with-redefs [ws-invoke/resolve! (fn [_ _ _] false)]
        (on-open ch request)
        (on-receive ch (ready-frame "claude-1"))
        (reset! sent [])
        (on-receive ch (json/generate-string {"type" "invoke_result"
                                              "invoke_id" "inv-missing"
                                              "result" "ok"}))
        (let [frame (last-sent sent)]
          (is (= "error" (:type frame)))
          (is (= "unknown-invoke" (:code frame))))))))

;; =============================================================================
;; on-close tests (L2: connection-state-machine)
;; =============================================================================

(deftest ws-close-after-handshake-triggers-disconnect-hook
  (testing "close after successful handshake → on-disconnect hook called (L2)"
    (let [disconnected (atom [])
          {:keys [on-open on-receive on-close]} (make-test-ws
                                                  :on-disconnect #(swap! disconnected conj %))
          ch :test-ch-10
          request (mock-request)]
      (on-open ch request)
      (on-receive ch (ready-frame "claude-1"))
      (on-close ch :normal)
      (is (= ["claude-1"] @disconnected)))))

(deftest ws-close-before-handshake-no-cleanup
  (testing "close before handshake → no disconnect hook (L2: spurious close)"
    (let [disconnected (atom [])
          {:keys [on-open on-close connections]} (make-test-ws
                                                   :on-disconnect #(swap! disconnected conj %))
          ch :test-ch-11
          request (mock-request)]
      (on-open ch request)
      ;; Close without completing handshake
      (on-close ch :client-close)
      (is (empty? @disconnected)
          "Disconnect hook should NOT fire for non-connected close (L2)")
      (is (nil? (get @connections ch))
          "Connection should be removed from registry regardless"))))

;; =============================================================================
;; Connection tracking
;; =============================================================================

(deftest ws-connected-agents-tracking
  (testing "connected-agents returns only handshake-completed agent-ids"
    (let [{:keys [on-open on-receive connections]} (make-test-ws)
          request (mock-request)]
      ;; Open two connections, only handshake one
      (on-open :ch-a request)
      (on-open :ch-b request)
      (on-receive :ch-a (ready-frame "claude-1"))
      ;; :ch-b stays in :connecting state
      (let [agents (ws/connected-agents connections)]
        (is (= ["claude-1"] agents))))))

(deftest ws-connection-removed-after-close
  (testing "closed connection no longer appears in connected-agents"
    (let [{:keys [on-open on-receive on-close connections]} (make-test-ws)]
      (on-open :ch-x (mock-request))
      (on-receive :ch-x (ready-frame "claude-1"))
      (is (= ["claude-1"] (ws/connected-agents connections)))
      (on-close :ch-x :normal)
      (is (empty? (ws/connected-agents connections))))))

;; =============================================================================
;; State storage (L4: no metadata on channels)
;; =============================================================================

(deftest ws-state-stored-in-atom-not-metadata
  (testing "connection state is in the connections atom, not on channel metadata (L4)"
    (let [{:keys [on-open connections]} (make-test-ws)
          ch :test-ch-l4
          request (mock-request :request-uri "/ws?agent_id=claude-1")]
      (on-open ch request)
      ;; State is in the connections atom keyed by channel
      (let [conn (get @connections ch)]
        (is (map? conn) "State should be a plain map")
        (is (contains? conn :channel))
        (is (contains? conn :agent-id))
        (is (contains? conn :connected?))
        (is (contains? conn :opened-at))))))

;; =============================================================================
;; Invalid frame handling (R9)
;; =============================================================================

(deftest ws-invalid-json-returns-error-frame
  (testing "invalid JSON → error frame sent"
    (let [{:keys [on-open on-receive sent]} (make-test-ws)
          ch :test-ch-json
          request (mock-request)]
      (on-open ch request)
      (on-receive ch "{not valid json")
      (let [frame (last-sent sent)]
        (is (= "error" (:type frame)))))))

(deftest ws-unknown-frame-type-returns-error
  (testing "frame with unknown type → error frame sent"
    (let [{:keys [on-open on-receive sent]} (make-test-ws)
          ch :test-ch-unknown
          request (mock-request)]
      (on-open ch request)
      (on-receive ch (json/generate-string {"type" "ping" "data" "hello"}))
      (let [frame (last-sent sent)]
        (is (= "error" (:type frame)))))))
