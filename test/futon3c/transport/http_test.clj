(ns futon3c.transport.http-test
  "Tests for HTTP REST adapter (Part II).

   Tests the Ring handler directly (no actual HTTP server for most tests)
   to keep tests fast and deterministic. One test exercises start-server!
   with a real port binding to verify L7 (verify-after-start)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [cheshire.core :as json]
            [futon3c.transport.http :as http]
            [futon3c.transport.encyclopedia :as enc]
            [futon3c.evidence.store :as estore]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.social.persist :as persist]
            [futon3c.agency.registry :as reg]
            [clojure.java.io :as io]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(use-fixtures
  :each
  (fn [f]
    (reg/reset-registry!)
    (persist/reset-sessions!)
    (estore/reset-store!)
    (enc/clear-cache!)
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

(defn- delete-req [handler uri]
  (handler {:request-method :delete :uri uri}))

(defn- get-req-with-query [handler uri query-string]
  (handler {:request-method :get
            :uri uri
            :query-string query-string}))

(defn- parse-body
  "Parse the JSON body string from a Ring response."
  [response]
  (json/parse-string (:body response) true))

(defn- with-temp-dir
  [f]
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory "futon3c-http-test-"
                                                               (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try
      (f dir)
      (finally
        (doseq [file (reverse (file-seq dir))]
          (.delete ^java.io.File file))))))

(defn- write-corpus!
  [dir corpus-name entries]
  (spit (io/file dir (str corpus-name ".edn")) (pr-str entries)))

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
          response (post handler "/dispatch" body)]
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
;; GET /api/alpha/agents/:id tests
;; =============================================================================

(deftest agent-get-returns-registered-agent
  (testing "GET /api/alpha/agents/:id returns agent details"
    (let [handler (make-handler)
          register-body (json/generate-string {"agent-id" "test-agent-1"
                                               "type" "codex"})
          register-response (post handler "/api/alpha/agents" register-body)
          response (get-req handler "/api/alpha/agents/test-agent-1")
          parsed (parse-body response)]
      (is (= 201 (:status register-response)))
      (is (= 200 (:status response)))
      (is (true? (:ok parsed)))
      (is (= "test-agent-1" (:agent-id parsed)))
      (is (= "test-agent-1" (get-in parsed [:agent :id :id/value])))
      (is (= "codex" (get-in parsed [:agent :type]))))))

(deftest agent-get-returns-404-for-unknown
  (testing "GET /api/alpha/agents/:id returns 404 for unknown agent"
    (let [handler (make-handler)
          response (get-req handler "/api/alpha/agents/nonexistent")
          parsed (parse-body response)]
      (is (= 404 (:status response)))
      (is (false? (:ok parsed)))
      (is (= "Agent not found: nonexistent" (:error parsed))))))

(deftest delete-agent-deregisters
  (testing "DELETE /api/alpha/agents/:id deregisters and subsequent GET returns 404"
    (let [handler (make-handler)
          register-body (json/generate-string {"agent-id" "delete-agent-1"
                                               "type" "codex"})
          register-response (post handler "/api/alpha/agents" register-body)
          delete-response (delete-req handler "/api/alpha/agents/delete-agent-1")
          delete-parsed (parse-body delete-response)
          get-response (get-req handler "/api/alpha/agents/delete-agent-1")
          get-parsed (parse-body get-response)]
      (is (= 201 (:status register-response)))
      (is (= 200 (:status delete-response)))
      (is (true? (:ok delete-parsed)))
      (is (= "delete-agent-1" (:agent-id delete-parsed)))
      (is (true? (:deregistered delete-parsed)))
      (is (= 404 (:status get-response)))
      (is (false? (:ok get-parsed)))
      (is (= "Agent not found: delete-agent-1" (:error get-parsed))))))

(deftest delete-unknown-agent-returns-404
  (testing "DELETE /api/alpha/agents/nonexistent returns 404"
    (let [handler (make-handler)
          response (delete-req handler "/api/alpha/agents/nonexistent")
          parsed (parse-body response)]
      (is (= 404 (:status response)))
      (is (false? (:ok parsed)))
      (is (= "Agent not found: nonexistent" (:error parsed))))))

(deftest agent-register-ws-bridge-opt-in
  (testing "POST /api/alpha/agents with ws-bridge=true registers without local invoke-fn"
    (let [handler (make-handler)
          register-body (json/generate-string {"agent-id" "bridge-agent-1"
                                               "type" "codex"
                                               "ws-bridge" true})
          register-response (post handler "/api/alpha/agents" register-body)
          register-parsed (parse-body register-response)
          invoke-body (json/generate-string {"agent-id" "bridge-agent-1"
                                             "prompt" "hello"})
          invoke-response (post handler "/api/alpha/invoke" invoke-body)
          invoke-parsed (parse-body invoke-response)
          live (reg/get-agent {:id/value "bridge-agent-1" :id/type :continuity})]
      (is (= 201 (:status register-response)))
      (is (true? (:ok register-parsed)))
      (is (true? (:ws-bridge register-parsed)))
      (is (nil? (:agent/invoke-fn live)) "ws-bridge registration should use WS invoke fallback")
      (is (= 502 (:status invoke-response)))
      (is (false? (:ok invoke-parsed)))
      (is (= "invoke-error" (:error invoke-parsed))))))

;; =============================================================================
;; POST /api/alpha/invoke tests
;; =============================================================================

(deftest invoke-registered-codex-agent
  (testing "POST /api/alpha/invoke invokes codex agent via registry"
    (register-mock-agent! "codex-1" :codex)
    (let [handler (make-handler)
          body (json/generate-string {"agent-id" "codex-1"
                                      "prompt" "hello"})
          response (post handler "/api/alpha/invoke" body)
          parsed (parse-body response)]
      (is (= 200 (:status response)))
      (is (true? (:ok parsed)))
      (is (= "ok" (:result parsed))))))

(deftest invoke-missing-agent-returns-404
  (testing "POST /api/alpha/invoke returns 404 for unknown agent"
    (let [handler (make-handler)
          body (json/generate-string {"agent-id" "ghost-agent"
                                      "prompt" "hello"})
          response (post handler "/api/alpha/invoke" body)
          parsed (parse-body response)]
      (is (= 404 (:status response)))
      (is (false? (:ok parsed)))
      (is (= "agent-not-found" (:error parsed))))))

;; =============================================================================
;; POST /api/alpha/irc/send tests
;; =============================================================================

(deftest irc-send-forwards-to-configured-relay
  (testing "POST /api/alpha/irc/send forwards message to configured send fn"
    (let [calls (atom [])
          handler (make-handler {:irc-send-fn (fn [channel from text]
                                                (swap! calls conj [channel from text]))})
          body (json/generate-string {"channel" "#futon"
                                      "from" "codex"
                                      "text" "@claude ping"})
          response (post handler "/api/alpha/irc/send" body)
          parsed (parse-body response)]
      (is (= 200 (:status response)))
      (is (true? (:ok parsed)))
      (is (= [["#futon" "codex" "@claude ping"]] @calls)))))

(deftest irc-send-without-relay-returns-503
  (testing "POST /api/alpha/irc/send returns 503 when relay is unavailable"
    (let [handler (make-handler)
          body (json/generate-string {"channel" "#futon"
                                      "text" "hello"})
          response (post handler "/api/alpha/irc/send" body)
          parsed (parse-body response)]
      (is (= 503 (:status response)))
      (is (false? (:ok parsed)))
      (is (= "irc-unavailable" (:err parsed))))))

(deftest irc-send-validates-required-fields
  (testing "POST /api/alpha/irc/send validates channel and text"
    (let [handler (make-handler {:irc-send-fn (fn [_ _ _] :ok)})
          body (json/generate-string {"channel" "#futon"})
          response (post handler "/api/alpha/irc/send" body)
          parsed (parse-body response)]
      (is (= 400 (:status response)))
      (is (false? (:ok parsed)))
      (is (= "missing-text" (:err parsed))))))

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

(deftest health-includes-agent-summary
  (testing "GET /health includes per-agent summary for live registered agents"
    (register-mock-agent! "codex-1" :codex)
    (let [handler (make-handler)
          response (get-req handler "/health")]
      (is (= 200 (:status response)))
      (let [parsed (parse-body response)
            agent (get-in parsed [:agent-summary :codex-1])]
        (is (= "ok" (:status parsed)))
        (is (= 3 (:agents parsed)))
        (is (= 0 (:sessions parsed)))
        (is (= "codex" (:type agent)))
        (is (string? (:last-active agent)))
        (is (instance? java.time.Instant (java.time.Instant/parse (:last-active agent))))
        (is (= ["explore" "edit"] (:capabilities agent)))))))

(deftest health-includes-evidence-count
  (testing "GET /health includes evidence count from store"
    (let [_ (estore/append! {:subject {:ref/type :session :ref/id "sess-health-1"}
                             :type :coordination
                             :claim-type :step
                             :author "codex"
                             :session-id "sess-health-1"
                             :body {:msg "first"}
                             :tags [:health]})
          _ (estore/append! {:subject {:ref/type :session :ref/id "sess-health-2"}
                             :type :reflection
                             :claim-type :conclusion
                             :author "claude"
                             :session-id "sess-health-2"
                             :body {:msg "second"}
                             :tags [:health]})
          expected-count (count (estore/query {}))
          handler (make-handler)
          response (get-req handler "/health")]
      (is (= 200 (:status response)))
      (let [parsed (parse-body response)]
        (is (= "ok" (:status parsed)))
        (is (= 3 (:agents parsed)))
        (is (= 0 (:sessions parsed)))
        (is (integer? (:evidence parsed)))
        (is (= expected-count (:evidence parsed)))))))

(deftest health-includes-uptime
  (testing "GET /health includes started-at and non-decreasing uptime seconds"
    (let [handler (make-handler)
          response-1 (get-req handler "/health")
          parsed-1 (parse-body response-1)
          uptime-1 (:uptime-seconds parsed-1)
          started-at (:started-at parsed-1)]
      (Thread/sleep 1200)
      (let [response-2 (get-req handler "/health")
            parsed-2 (parse-body response-2)
            uptime-2 (:uptime-seconds parsed-2)]
        (is (= 200 (:status response-1)))
        (is (= 200 (:status response-2)))
        (is (= "ok" (:status parsed-1)))
        (is (= 3 (:agents parsed-1)))
        (is (= 0 (:sessions parsed-1)))
        (is (string? started-at))
        (is (instance? java.time.Instant (java.time.Instant/parse started-at)))
        (is (integer? uptime-1))
        (is (integer? uptime-2))
        (is (<= 0 uptime-1))
        (is (<= uptime-1 uptime-2))))))

;; =============================================================================
;; GET/POST /api/alpha/evidence tests
;; =============================================================================

(deftest evidence-query-returns-entries
  (testing "GET /api/alpha/evidence returns newest-first entries and supports filters"
    (let [handler (make-handler)
          _ (estore/append! {:subject {:ref/type :session :ref/id "sess-1"}
                             :type :coordination
                             :claim-type :step
                             :author "claude"
                             :session-id "sess-1"
                             :body {:k "older"}
                             :tags [:project/x]})
          _ (Thread/sleep 5)
          _ (estore/append! {:subject {:ref/type :session :ref/id "sess-2"}
                             :type :reflection
                             :claim-type :conclusion
                             :author "codex"
                             :session-id "sess-2"
                             :body {:k "newer"}
                             :tags [:project/y]})
          response (get-req-with-query handler "/api/alpha/evidence" "author=codex")
          parsed (parse-body response)
          entries (:entries parsed)]
      (is (= 200 (:status response)))
      (is (true? (:ok parsed)))
      (is (= 1 (:count parsed)))
      (is (= 1 (count entries)))
      (is (= "codex" (:evidence/author (first entries))))
      (is (= "reflection" (:evidence/type (first entries)))))))

(deftest evidence-get-by-id-and-chain
  (testing "GET /api/alpha/evidence/:id and /chain return expected payload"
    (let [root (-> (estore/append! {:subject {:ref/type :session :ref/id "sess-chain"}
                                    :type :coordination
                                    :claim-type :goal
                                    :author "claude"
                                    :session-id "sess-chain"
                                    :body {:msg "root"}
                                    :tags [:chain]})
                   :entry)
          child (-> (estore/append! {:subject {:ref/type :session :ref/id "sess-chain"}
                                     :type :coordination
                                     :claim-type :step
                                     :author "claude"
                                     :session-id "sess-chain"
                                     :in-reply-to (:evidence/id root)
                                     :body {:msg "child"}
                                     :tags [:chain]})
                    :entry)
          handler (make-handler)
          get-response (get-req handler (str "/api/alpha/evidence/" (:evidence/id child)))
          get-parsed (parse-body get-response)
          chain-response (get-req handler (str "/api/alpha/evidence/" (:evidence/id child) "/chain"))
          chain-parsed (parse-body chain-response)]
      (is (= 200 (:status get-response)))
      (is (true? (:ok get-parsed)))
      (is (= (:evidence/id child) (get-in get-parsed [:entry :evidence/id])))
      (is (= 200 (:status chain-response)))
      (is (true? (:ok chain-parsed)))
      (is (= 2 (count (:chain chain-parsed))))
      (is (= (:evidence/id root) (get-in chain-parsed [:chain 0 :evidence/id])))
      (is (= (:evidence/id child) (get-in chain-parsed [:chain 1 :evidence/id]))))))

(deftest evidence-create-route-accepts-json
  (testing "POST /api/alpha/evidence appends an entry and returns 201"
    (let [handler (make-handler)
          body (json/generate-string
                {"subject" {"ref/type" "session" "ref/id" "sess-post"}
                 "type" "coordination"
                 "claim-type" "step"
                 "author" "codex"
                 "session-id" "sess-post"
                 "body" {"msg" "hello"}
                 "tags" ["api" "write"]})
          response (post handler "/api/alpha/evidence" body)
          parsed (parse-body response)
          entry-id (:evidence/id parsed)]
      (is (= 201 (:status response)))
      (is (true? (:ok parsed)))
      (is (string? entry-id))
      (is (some? (estore/get-entry entry-id))))))

(deftest evidence-count-returns-total
  (testing "GET /api/alpha/evidence/count returns total evidence count"
    (let [_ (estore/append! {:subject {:ref/type :session :ref/id "sess-count-1"}
                             :type :coordination
                             :claim-type :step
                             :author "codex"
                             :session-id "sess-count-1"
                             :body {:msg "one"}
                             :tags [:alpha]})
          _ (estore/append! {:subject {:ref/type :session :ref/id "sess-count-2"}
                             :type :coordination
                             :claim-type :step
                             :author "claude"
                             :session-id "sess-count-2"
                             :body {:msg "two"}
                             :tags [:beta]})
          _ (estore/append! {:subject {:ref/type :session :ref/id "sess-count-3"}
                             :type :reflection
                             :claim-type :conclusion
                             :author "tickle"
                             :session-id "sess-count-3"
                             :body {:msg "three"}
                             :tags [:gamma]})
          handler (make-handler)
          response (get-req handler "/api/alpha/evidence/count")
          parsed (parse-body response)
          session-response (get-req-with-query handler "/api/alpha/evidence/count" "session-id=sess-count-2")
          session-parsed (parse-body session-response)]
      (is (= 200 (:status response)))
      (is (true? (:ok parsed)))
      (is (= 3 (:count parsed)))
      (is (= 200 (:status session-response)))
      (is (true? (:ok session-parsed)))
      (is (= 1 (:count session-parsed))))))

(deftest evidence-count-filters-by-tag
  (testing "GET /api/alpha/evidence/count?tag=foo counts only matching tags"
    (let [_ (estore/append! {:subject {:ref/type :session :ref/id "sess-tag-1"}
                             :type :coordination
                             :claim-type :step
                             :author "codex"
                             :session-id "sess-tag-1"
                             :body {:msg "a"}
                             :tags [:foo]})
          _ (estore/append! {:subject {:ref/type :session :ref/id "sess-tag-2"}
                             :type :reflection
                             :claim-type :conclusion
                             :author "claude"
                             :session-id "sess-tag-2"
                             :body {:msg "b"}
                             :tags [:bar]})
          _ (estore/append! {:subject {:ref/type :session :ref/id "sess-tag-3"}
                             :type :coordination
                             :claim-type :step
                             :author "codex"
                             :session-id "sess-tag-3"
                             :body {:msg "c"}
                             :tags [:foo :bar]})
          handler (make-handler)
          response (get-req-with-query handler "/api/alpha/evidence/count" "tag=foo")
          parsed (parse-body response)]
      (is (= 200 (:status response)))
      (is (true? (:ok parsed)))
      (is (= 2 (:count parsed))))))

(deftest evidence-count-filters-by-author
  (testing "GET /api/alpha/evidence/count?author=alice counts only matching author"
    (let [_ (estore/append! {:subject {:ref/type :session :ref/id "sess-author-1"}
                             :type :coordination
                             :claim-type :step
                             :author "alice"
                             :session-id "sess-author-1"
                             :body {:msg "a"}
                             :tags [:author]})
          _ (estore/append! {:subject {:ref/type :session :ref/id "sess-author-2"}
                             :type :coordination
                             :claim-type :step
                             :author "bob"
                             :session-id "sess-author-2"
                             :body {:msg "b"}
                             :tags [:author]})
          _ (estore/append! {:subject {:ref/type :session :ref/id "sess-author-3"}
                             :type :reflection
                             :claim-type :conclusion
                             :author "alice"
                             :session-id "sess-author-3"
                             :body {:msg "c"}
                             :tags [:author]})
          handler (make-handler)
          response (get-req-with-query handler "/api/alpha/evidence/count" "author=alice")
          parsed (parse-body response)]
      (is (= 200 (:status response)))
      (is (true? (:ok parsed)))
      (is (= 2 (:count parsed))))))

;; =============================================================================
;; Encyclopedia route tests
;; =============================================================================

(deftest encyclopedia-corpuses-lists-local-corpora
  (testing "GET /fulab/encyclopedia/corpuses lists corpus ids + counts"
    (with-temp-dir
      (fn [dir]
        (write-corpus! dir "11a"
                       [{:entry/id "e1" :entry/title "Entry One"}
                        {:entry/id "e2" :entry/title "Entry Two"}])
        (write-corpus! dir "14x"
                       [{:entry/id "e3" :entry/title "Entry Three"}])
        (spit (io/file dir "README.txt") "ignore me")
        (let [handler (make-handler {:encyclopedia {:corpus-root (.getAbsolutePath dir)}})
              response (get-req handler "/fulab/encyclopedia/corpuses")
              parsed (parse-body response)
              by-id (->> (:corpuses parsed)
                         (map (juxt :corpus/id identity))
                         (into {}))]
          (is (= 200 (:status response)))
          (is (true? (:ok parsed)))
          (is (= 2 (count (:corpuses parsed))))
          (is (= 2 (:corpus/count (get by-id "11a"))))
          (is (= 1 (:corpus/count (get by-id "14x"))))
          (is (= "planetmath" (:corpus/source (get by-id "11a")))))))))

(deftest encyclopedia-entries-supports-pagination
  (testing "GET /fulab/encyclopedia/:corpus/entries returns paginated summaries"
    (with-temp-dir
      (fn [dir]
        (write-corpus! dir "11a"
                       [{:entry/id "e1" :entry/title "Entry One" :entry/type :def
                         :entry/msc-codes ["11A"] :entry/related ["e2"]
                         :entry/body "full-text-1"}
                        {:entry/id "e2" :entry/title "Entry Two" :entry/type :theorem
                         :entry/msc-codes ["11B"] :entry/related ["e1"]
                         :entry/body "full-text-2"}])
        (let [handler (make-handler {:encyclopedia {:corpus-root (.getAbsolutePath dir)}})
              response (get-req-with-query handler
                                           "/fulab/encyclopedia/11a/entries"
                                           "limit=1&offset=1")
              parsed (parse-body response)
              first-entry (first (:entries parsed))]
          (is (= 200 (:status response)))
          (is (true? (:ok parsed)))
          (is (= "11a" (:corpus parsed)))
          (is (= 2 (:total parsed)))
          (is (= 1 (:limit parsed)))
          (is (= 1 (:offset parsed)))
          (is (= 1 (count (:entries parsed))))
          (is (= "e2" (:entry/id first-entry)))
          (is (nil? (:entry/body first-entry))))))))

(deftest encyclopedia-entries-missing-corpus-returns-404
  (testing "GET /fulab/encyclopedia/:corpus/entries missing corpus -> 404"
    (with-temp-dir
      (fn [dir]
        (let [handler (make-handler {:encyclopedia {:corpus-root (.getAbsolutePath dir)}})
              response (get-req handler "/fulab/encyclopedia/missing/entries")
              parsed (parse-body response)]
          (is (= 404 (:status response)))
          (is (false? (:ok parsed)))
          (is (= "corpus-not-found" (:err parsed))))))))

(deftest encyclopedia-entry-route-handles-url-encoded-id
  (testing "GET /fulab/encyclopedia/:corpus/entry/:id decodes %2F in entry id"
    (with-temp-dir
      (fn [dir]
        (write-corpus! dir "11a"
                       [{:entry/id "id/with/slash"
                         :entry/title "Slash ID"
                         :entry/body "full text"}])
        (let [handler (make-handler {:encyclopedia {:corpus-root (.getAbsolutePath dir)}})
              response (get-req handler "/fulab/encyclopedia/11a/entry/id%2Fwith%2Fslash")
              parsed (parse-body response)]
          (is (= 200 (:status response)))
          (is (true? (:ok parsed)))
          (is (= "id/with/slash" (get-in parsed [:entry :entry/id])))
          (is (= "full text" (get-in parsed [:entry :entry/body]))))))))

(deftest encyclopedia-entry-missing-returns-404
  (testing "GET /fulab/encyclopedia/:corpus/entry/:id missing entry -> 404"
    (with-temp-dir
      (fn [dir]
        (write-corpus! dir "11a"
                       [{:entry/id "e1" :entry/title "Entry One"}])
        (let [handler (make-handler {:encyclopedia {:corpus-root (.getAbsolutePath dir)}})
              response (get-req handler "/fulab/encyclopedia/11a/entry/nope")
              parsed (parse-body response)]
          (is (= 404 (:status response)))
          (is (false? (:ok parsed)))
          (is (= "entry-not-found" (:err parsed))))))))

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

(deftest start-server-stop-is-idempotent
  (testing "calling stop multiple times is safe and non-throwing"
    (let [free-port (with-open [ss (java.net.ServerSocket. 0)]
                      (.getLocalPort ss))
          handler (make-handler)
          server-info (http/start-server! handler free-port)
          stop! (:server server-info)]
      (is (fn? stop!))
      (is (nil? (stop!)))
      (is (nil? (stop!)))
      (Thread/sleep 200)
      (let [reachable? (try
                         (with-open [sock (java.net.Socket.)]
                           (.connect sock (java.net.InetSocketAddress. "localhost" (int free-port)) 500)
                           true)
                         (catch Exception _ false))]
        (is (not reachable?) "Port should remain closed after repeated stop calls")))))
