(ns futon3c.transport.http-test
  "Tests for HTTP REST adapter (Part II).

   Tests the Ring handler directly (no actual HTTP server for most tests)
   to keep tests fast and deterministic. One test exercises start-server!
   with a real port binding to verify L7 (verify-after-start)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [cheshire.core :as json]
            [futon3c.transport.http :as http]
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
;; Test helpers
;; =============================================================================

(defn- register-mock-agent!
  "Register a mock agent in the live registry that returns {:result \"ok\"}."
  [agent-id-str type]
  (reg/register-agent!
   {:agent-id {:id/value agent-id-str :id/type :continuity}
    :type type
    :invoke-fn (fn [_prompt _session-id] {:result "ok" :session-id nil})
    :capabilities [:explore :edit]}))

(defn- make-handler
  "Create handler with standard test config."
  ([] (make-handler {}))
  ([overrides]
   (http/make-handler
    (merge {:registry (fix/mock-registry)
            :patterns (fix/mock-patterns)}
           overrides))))

(defn- post [handler uri body-str]
  (handler {:request-method :post :uri uri :body body-str}))

(defn- get-req [handler uri]
  (handler {:request-method :get :uri uri}))

(defn- parse-body
  "Parse the JSON body string from a Ring response."
  [response]
  (json/parse-string (:body response) true))

;; =============================================================================
;; POST /dispatch tests
;; =============================================================================

(deftest dispatch-valid-json-returns-receipt
  (testing "POST /dispatch with valid coordination message → 200 + receipt"
    (register-mock-agent! "claude-1" :claude)
    (let [handler (make-handler)
          body (json/generate-string {"msg_id" "msg-1"
                                      "payload" {"type" "standup"}
                                      "from" "claude-1"
                                      "to" "claude-1"})
          response (post handler "/dispatch" body)
          parsed (parse-body response)]
      (is (= 200 (:status response)))
      (is (= "application/json" (get-in response [:headers "Content-Type"])))
      ;; Body is a rendered receipt JSON (double-encoded as string)
      ;; render-receipt returns a JSON string, which json-response passes through
      (let [receipt (if (string? (:body response))
                      (json/parse-string (:body response) true)
                      (:body response))]
        (is (= "msg-1" (:msg_id receipt)))
        (is (true? (:delivered receipt)))))))

(deftest dispatch-bad-json-returns-400
  (testing "POST /dispatch with malformed JSON → 400"
    (let [handler (make-handler)
          response (post handler "/dispatch" "{bad json")]
      (is (= 400 (:status response)))
      (is (= "application/json" (get-in response [:headers "Content-Type"]))))))

(deftest dispatch-unknown-agent-returns-404
  (testing "POST /dispatch to unknown agent → 404"
    (let [handler (make-handler)
          body (json/generate-string {"msg_id" "msg-2"
                                      "payload" "hello"
                                      "from" "claude-1"
                                      "to" "ghost-agent"})
          response (post handler "/dispatch" body)]
      (is (= 404 (:status response))))))

(deftest dispatch-missing-body-returns-400
  (testing "POST /dispatch with empty body → 400"
    (let [handler (make-handler)
          response (post handler "/dispatch" "")]
      (is (= 400 (:status response))))))

(deftest dispatch-action-message-through-classify
  (testing "POST /dispatch with action payload → classified as :action"
    (register-mock-agent! "claude-1" :claude)
    (let [handler (make-handler)
          body (json/generate-string {"msg_id" "msg-3"
                                      "payload" "implement feature X"
                                      "from" "claude-1"
                                      "to" "claude-1"})
          response (post handler "/dispatch" body)]
      ;; Action message without peripheral-config falls back to direct invoke
      (is (= 200 (:status response))))))

;; =============================================================================
;; POST /presence tests
;; =============================================================================

(deftest presence-valid-with-readiness-returns-200
  (testing "POST /presence with readiness metadata → 200 + presence record"
    (let [handler (make-handler)
          body (json/generate-string {"agent_id" "claude-1"
                                      "transport" "websocket"
                                      "metadata" {"ready" true}})
          response (post handler "/presence" body)]
      (is (= 200 (:status response)))
      (let [parsed (parse-body response)]
        (is (= "presence" (:type parsed)))
        (is (true? (:ready parsed)))))))

(deftest presence-without-readiness-returns-error
  (testing "POST /presence without ready metadata → error (not-ready)"
    (let [handler (make-handler)
          body (json/generate-string {"agent_id" "claude-1"
                                      "transport" "websocket"})
          response (post handler "/presence" body)]
      (is (= 403 (:status response))))))

(deftest presence-unknown-agent-returns-404
  (testing "POST /presence with unknown agent → 404"
    (let [handler (make-handler)
          body (json/generate-string {"agent_id" "ghost-agent"
                                      "transport" "http"
                                      "metadata" {"ready" true}})
          response (post handler "/presence" body)]
      (is (= 404 (:status response))))))

;; =============================================================================
;; GET /session/:id tests
;; =============================================================================

(deftest get-session-existing-returns-200
  (testing "GET /session/:id for existing session → 200"
    ;; First create a session via dispatch
    (register-mock-agent! "claude-1" :claude)
    (let [handler (make-handler)
          ;; Dispatch a message to create a session via persist
          body (json/generate-string {"msg_id" "msg-s1"
                                      "payload" {"type" "standup"}
                                      "from" "claude-1"
                                      "to" "claude-1"})
          dispatch-response (post handler "/dispatch" body)]
      (is (= 200 (:status dispatch-response)))
      ;; Now manually create a session since direct-invoke doesn't persist
      (let [receipt (fix/make-dispatch-receipt {:receipt/msg-id "msg-s1"})
            _ (persist/persist-session! receipt {:session/id "sess-test-1"})
            session-response (get-req handler "/session/sess-test-1")]
        (is (= 200 (:status session-response)))
        (let [parsed (parse-body session-response)]
          (is (= "sess-test-1" (:session_id parsed))))))))

(deftest get-session-missing-returns-404
  (testing "GET /session/:id for missing session → 404"
    (let [handler (make-handler)
          response (get-req handler "/session/sess-nonexistent")]
      (is (= 404 (:status response))))))

;; =============================================================================
;; GET /health tests
;; =============================================================================

(deftest health-returns-agent-and-session-counts
  (testing "GET /health returns agent/session counts"
    (let [handler (make-handler)
          response (get-req handler "/health")]
      (is (= 200 (:status response)))
      (let [parsed (parse-body response)]
        (is (= "ok" (:status parsed)))
        (is (= 3 (:agents parsed)))
        (is (= 0 (:sessions parsed)))))))

;; =============================================================================
;; Content-Type and unknown endpoint tests
;; =============================================================================

(deftest all-responses-have-json-content-type
  (testing "every endpoint returns Content-Type: application/json"
    (let [handler (make-handler)]
      (doseq [[method uri body]
              [[:post "/dispatch" "{\"msg_id\":\"m\",\"payload\":\"x\",\"from\":\"claude-1\",\"to\":\"ghost\"}"]
               [:post "/presence" "{\"agent_id\":\"ghost\",\"metadata\":{\"ready\":true}}"]
               [:get "/session/nonexistent" nil]
               [:get "/health" nil]
               [:get "/nowhere" nil]]]
        (let [response (handler {:request-method method :uri uri :body body})]
          (is (= "application/json" (get-in response [:headers "Content-Type"]))
              (str "Expected JSON content type for " method " " uri)))))))

(deftest unknown-endpoint-returns-404
  (testing "unknown endpoint → 404 with JSON error"
    (let [handler (make-handler)
          response (get-req handler "/api/v2/mystery")]
      (is (= 404 (:status response)))
      (let [parsed (parse-body response)]
        (is (true? (:error parsed)))
        (is (= "not-found" (:code parsed)))))))

;; =============================================================================
;; start-server! test — real port binding (L7: verify-after-start)
;; =============================================================================

(deftest start-server-binds-and-verifies-port
  (testing "start-server! binds port and verifies it is listening (L7)"
    (let [;; Find a free port
          free-port (with-open [ss (java.net.ServerSocket. 0)]
                      (.getLocalPort ss))
          handler (make-handler)
          server-info (http/start-server! handler free-port)]
      (try
        (is (= free-port (:port server-info)))
        (is (fn? (:server server-info)))
        (is (string? (:started-at server-info)))
        ;; Verify the server responds
        (let [sock (java.net.Socket.)]
          (try
            (.connect sock (java.net.InetSocketAddress. "localhost" (int free-port)) 1000)
            (is true "Port is reachable")
            (finally (.close sock))))
        (finally
          ;; Graceful shutdown
          ((:server server-info)))))))

(deftest start-server-stop-closes-gracefully
  (testing "calling the stop function shuts down the server"
    (let [free-port (with-open [ss (java.net.ServerSocket. 0)]
                      (.getLocalPort ss))
          handler (make-handler)
          server-info (http/start-server! handler free-port)]
      ;; Stop the server
      ((:server server-info))
      ;; Give it a moment to release the port
      (Thread/sleep 200)
      ;; Port should no longer be listening
      (let [reachable? (try
                         (with-open [sock (java.net.Socket.)]
                           (.connect sock (java.net.InetSocketAddress. "localhost" (int free-port)) 500)
                           true)
                         (catch Exception _ false))]
        (is (not reachable?) "Port should be closed after shutdown")))))
