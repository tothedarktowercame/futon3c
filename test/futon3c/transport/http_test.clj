(ns futon3c.transport.http-test
  "Tests for HTTP REST adapter (Part II).

   Tests the Ring handler directly (no actual HTTP server for most tests)
   to keep tests fast and deterministic. Invoke-path tests exercise a real
   local HTTP server because /api/alpha/invoke uses async channel semantics."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [cheshire.core :as json]
            [futon3c.mfuton-mode :as mfuton-mode]
            [futon3c.transport.http :as http]
            [futon3c.transport.peripheral-events]
            [futon3c.transport.ws.invoke :as ws-invoke]
            [futon3c.agency.parked-on :as parked-on]
            [futon3c.portfolio.core :as portfolio]
            [futon3c.portfolio.perceive :as perceive]
            [futon3c.peripheral.mission-control-backend :as mcb]
            [futon3c.transport.encyclopedia :as enc]
            [futon3c.evidence.store :as estore]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.social.persist :as persist]
            [futon3c.agency.registry :as reg]
            [futon3c.agency.clock-lineage :as clock-lineage]
            [futon3c.agency.clock-store :as clock-store]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(use-fixtures
  :each
  (fn [f]
    (reg/reset-registry!)
    (clock-store/reset-store!)
    (persist/reset-sessions!)
    (estore/reset-store!)
    (reset! portfolio/!state {:mu perceive/default-mu
                              :prec perceive/default-precision
                              :pending nil
                              :recent []
                              :step-count 0})
    (enc/clear-cache!)
    (http/reset-invoke-jobs!)
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

(deftest parked-resume-buffer-surface-inbox-is-authoritative-ws-is-poke
  (testing "the inbox push always happens; an accepted WS frame is only a poke"
    (let [sent (atom [])
          pushed (atom [])]
      (with-redefs [ws-invoke/send-frame! (fn [agent frame]
                                            (swap! sent conj [agent frame])
                                            true)
                    http/parked-ready-push! (fn [& args]
                                               (swap! pushed conj args))]
        (is (= "park-ready-inbox+poke:pk-1"
               ((var-get #'http/parked-resume!)
                {:id "pk-1" :agent "claude-1" :session "sid"
                 :surface "emacs-repl" :payload "wake" :arrived {}
                 :awaiting #{} :mode :background})))
        (is (= 1 (count @sent)))
        (is (nil? (:prompt (second (first @sent))))
            "the poke frame carries no authoritative payload")
        (is (= [["claude-1" "sid" "pk-1"
                 "wake\n\n--- resumed: parked dependencies complete (0) ---\n"
                 :background]]
               @pushed)))))
  (testing "missing targeted WS sender still enqueues exactly one inbox item"
    (let [sent (atom [])
          pushed (atom [])]
      (with-redefs [ws-invoke/send-frame! (fn [agent frame]
                                            (swap! sent conj [agent frame])
                                            false)
                    http/parked-ready-push! (fn [& args]
                                               (swap! pushed conj args))]
        (is (= "park-ready-inbox:pk-2"
               ((var-get #'http/parked-resume!)
                {:id "pk-2" :agent "claude-1" :session "sid"
                 :surface "emacs-repl" :payload "wake" :arrived {}
                 :awaiting #{} :mode :background})))
        (is (= 1 (count @sent)))
        (is (= [["claude-1" "sid" "pk-2"
                 "wake\n\n--- resumed: parked dependencies complete (0) ---\n"
                 :background]]
               @pushed))))))

(deftest parked-ready-withholds-while-agent-busy
  (testing "server-side busy gate withholds ready items without leasing"
    (let [popped? (atom false)
          response (with-redefs [http/parked-on-enabled? (constantly true)
                                 http/agent-in-flight-turn? (constantly true)
                                 http/parked-ready-pop! (fn [& _]
                                                          (reset! popped? true)
                                                          {:park-id "bad"})]
                     ((var-get #'http/handle-parked-ready)
                      {:query-string "agent=claude-1&session=sid"} nil))
          body (json/parse-string (:body response) true)]
      (is (= 200 (:status response)))
      (is (= [] (:ready body)))
      (is (true? (:withheld body)))
      (is (false? @popped?)))))

(deftest parked-background-record-does-not-defer-within-turn-finalization
  (testing "background parks are excluded from /parked more-pending"
    (let [response (with-redefs [http/parked-on-enabled? (constantly true)
                                 parked-on/snapshot (fn []
                                                      {:records
                                                       {"bg" {:id "bg"
                                                              :agent "claude-1"
                                                              :session "sid"
                                                              :awaiting #{"job-1"}
                                                              :deadline-ms 123
                                                              :mode :background
                                                              :released? false}}})
                                 parked-on/ready-inbox-pending? (constantly false)]
                     ((var-get #'http/handle-parked)
                      {:query-string "agent=claude-1&session=sid"} nil))
          body (json/parse-string (:body response) true)]
      (is (= 200 (:status response)))
      (is (= [] (:parked body)))
      (is (false? (:more-pending body))))))

(deftest parked-all-mode-shows-background-without-deferring-finalization
  (testing "mode=all is an operator view, not a change to more-pending semantics"
    (let [response (with-redefs [http/parked-on-enabled? (constantly true)
                                 parked-on/snapshot (fn []
                                                      {:records
                                                       {"bg" {:id "bg"
                                                              :agent "claude-1"
                                                              :session "sid"
                                                              :awaiting #{"job-1"}
                                                              :deadline-ms 123
                                                              :mode :background
                                                              :released? false}}})
                                 parked-on/ready-inbox-pending? (constantly false)]
                     ((var-get #'http/handle-parked)
                      {:query-string "agent=claude-1&session=sid&mode=all"} nil))
          body (json/parse-string (:body response) true)]
      (is (= [{:id "bg"
               :awaiting ["job-1"]
               :deadline-ms 123
               :mode "background"}]
             (:parked body)))
      (is (false? (:more-pending body))))))

(deftest invoke-job-public-view-exposes-auto-bellback-decision
  (let [decision {:suppressed? true
                  :reason :parked-on
                  :park-id "park-1"}
        view ((var-get #'http/invoke-job-public-view)
              {:job-id "job-1" :state "done" :auto-bellback decision
               :result "private complete response"
               :result-text "private bounded response"})]
    (is (= decision (:auto-bellback view)))
    (is (not (contains? view :result-text)))
    (is (not (contains? view :result)))))

(defn- parse-body
  "Parse the JSON body string from a Ring response."
  [response]
  (json/parse-string (:body response) true))

(deftest missions-can-skip-ancillary-turn-count-scan
  (testing "inventory-only callers do not query live coordination evidence"
    (with-redefs [mcb/build-inventory (fn [] [{:mission/id "M-test"}])
                  mcb/mission-turn-count-telemetry
                  (fn [& _] (throw (ex-info "must not run" {})))]
      (let [response (get-req-with-query (make-handler)
                                         "/api/alpha/missions"
                                         "include-turn-counts=false")
            body (parse-body response)]
        (is (= 200 (:status response)))
        (is (= [{:mission/id "M-test"}] (:missions body)))
        (is (false? (:turn-counts-included? body)))
        (is (nil? (:turn-counts body)))))))

(defn- with-system-properties
  [settings f]
  (let [ks (keys settings)
        old-values (into {}
                         (map (fn [k] [k (System/getProperty k)]))
                         ks)]
    (try
      (doseq [[k v] settings]
        (if (some? v)
          (System/setProperty k v)
          (System/clearProperty k)))
      (f)
      (finally
        (doseq [[k v] old-values]
          (if (some? v)
            (System/setProperty k v)
            (System/clearProperty k)))))))

(defn- with-live-server
  "Run test body against a real local HTTP server (required for async channel paths)."
  [handler f]
  (let [free-port (with-open [ss (java.net.ServerSocket. 0)]
                    (.getLocalPort ss))
        server-info (http/start-server! handler free-port)
        base-url (str "http://localhost:" free-port)]
    (try
      (f base-url)
      (finally
        ((:server server-info))))))

(defn- http-post-json
  "POST JSON to BASE-URL + URI and return a Ring-like response map."
  [base-url uri body-str]
  (let [client (java.net.http.HttpClient/newHttpClient)
        req (-> (java.net.http.HttpRequest/newBuilder
                  (java.net.URI/create (str base-url uri)))
                (.header "Content-Type" "application/json")
                (.POST (java.net.http.HttpRequest$BodyPublishers/ofString body-str))
                (.build))
        resp (.send client req (java.net.http.HttpResponse$BodyHandlers/ofString))]
    {:status (.statusCode resp)
     :headers {"Content-Type" (some-> (.headers resp)
                                      (.firstValue "content-type")
                                      (.orElse nil))}
     :body (.body resp)}))

(defn- wait-for-job-state
  "Poll /api/alpha/invoke/jobs/:id until state is no longer queued/running or timeout."
  [handler job-id timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (let [resp (get-req handler (str "/api/alpha/invoke/jobs/" job-id))
            parsed (parse-body resp)
            state (get-in parsed [:job :state])]
        (if (or (not (#{"queued" "running"} state))
                (>= (System/currentTimeMillis) deadline))
          {:response resp :parsed parsed}
          (do
            (Thread/sleep 20)
            (recur)))))))

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
;; Mission sync endpoint
;; =============================================================================

(deftest mc-sync-mission-pushes-versioned-snapshot
  (testing "POST /api/alpha/mc/sync-mission parses and stores a mission snapshot"
    (with-temp-dir
      (fn [dir]
        (let [missions-dir (io/file dir "holes" "missions")
              path (io/file missions-dir "M-sample.md")
              _ (.mkdirs missions-dir)
              _ (spit path (str "# Mission: Sample\n\n"
                                "**Date:** 2026-04-29\n"
                                "**Status:** IDENTIFY\n"))
              handler (make-handler {:evidence-store estore/!store})
              body (json/generate-string {"path" (.getAbsolutePath path)
                                          "repo" "futonz"})
              response (post handler "/api/alpha/mc/sync-mission" body)
              parsed (parse-body response)]
          (is (= 200 (:status response)))
          (is (true? (:ok parsed)))
          (is (= "sample" (get-in parsed [:mission :mission/id])))
          (is (= "futonz" (get-in parsed [:mission :mission/repo])))
          (is (string? (:evidence/id parsed)))
          (is (true? (:created parsed))))))))

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
          invoke-body (json/generate-string {"agent-id" "bridge-agent-1"
                                             "prompt" "hello"})]
      (with-live-server
        handler
        (fn [base-url]
          (let [register-response (http-post-json base-url "/api/alpha/agents" register-body)
                register-parsed (parse-body register-response)
                invoke-response (http-post-json base-url "/api/alpha/invoke" invoke-body)
                invoke-parsed (parse-body invoke-response)
                live (reg/get-agent {:id/value "bridge-agent-1" :id/type :continuity})]
            (is (= 201 (:status register-response)))
            (is (true? (:ok register-parsed)))
            (is (true? (:ws-bridge register-parsed)))
            (is (nil? (:agent/invoke-fn live)) "ws-bridge registration should use WS invoke fallback")
            (is (= 502 (:status invoke-response)))
            (is (false? (:ok invoke-parsed)))
            (is (= "invoke-error" (:error invoke-parsed)))))))))

(deftest agent-auto-register-seeds-session-id
  (testing "POST /api/alpha/agents/auto seeds session continuity at registration time"
    (let [handler (make-handler)
          session-file (io/file "/tmp/futon-session-id-claude-1")
          backup (when (.exists session-file) (slurp session-file))
          sid "sess-auto-http-register"
          body (json/generate-string {"type" "claude"
                                      "session-id" sid})]
      (try
        (when (.exists session-file)
          (.delete session-file))
        (let [response (post handler "/api/alpha/agents/auto" body)
              parsed (parse-body response)
              agent (reg/get-agent "claude-1")]
          (is (= 201 (:status response)))
          (is (true? (:ok parsed)))
          (is (= "claude-1" (:agent-id parsed)))
          (is (= sid (:session-id parsed)))
          (is (= sid (:agent/session-id agent)))
          (is (= sid (some-> session-file slurp str/trim))))
        (finally
          (if (some? backup)
            (spit session-file backup)
            (when (.exists session-file)
              (.delete session-file))))))))

(deftest agent-auto-register-fresh-ignores-stale-file
  (testing "POST /api/alpha/agents/auto with no session-id deletes stale session file (I-1)"
    ;; Regression: a stale /tmp/futon-session-id-<aid> from a prior
    ;; incarnation of this agent-id must not be silently adopted as the
    ;; new agent's session. Otherwise two agents can share one session
    ;; (observed: claude-3 and claude-6 both resuming 14459c97...).
    (let [handler (make-handler)
          session-file (io/file "/tmp/futon-session-id-claude-1")
          backup (when (.exists session-file) (slurp session-file))
          stale-sid "stale-from-prior-incarnation"
          body (json/generate-string {"type" "claude"})]
      (try
        (spit session-file stale-sid)
        (let [response (post handler "/api/alpha/agents/auto" body)
              parsed (parse-body response)
              agent (reg/get-agent "claude-1")]
          (is (= 201 (:status response)))
          (is (true? (:ok parsed)))
          (is (= "claude-1" (:agent-id parsed)))
          (is (nil? (:session-id parsed))
              "fresh lane must not echo back a session-id")
          (is (nil? (:agent/session-id agent))
              "fresh lane must not inherit the stale file's session-id")
          (is (not (.exists session-file))
              "stale session file must be deleted on fresh registration"))
        (finally
          (if (some? backup)
            (spit session-file backup)
            (when (.exists session-file)
              (.delete session-file))))))))

(deftest codex-auto-register-seeds-session-id
  (testing "POST /api/alpha/agents/auto creates a fresh codex lane with its own session file"
    (let [handler (make-handler)
          session-file (io/file "/tmp/futon-codex-session-id-codex-1")
          backup (when (.exists session-file) (slurp session-file))
          sid "sess-auto-codex-register"
          cwd "/home/joe/code"
          body (json/generate-string {"type" "codex"
                                      "session-id" sid
                                      "cwd" cwd})]
      (try
        (when (.exists session-file)
          (.delete session-file))
        (let [response (post handler "/api/alpha/agents/auto" body)
              parsed (parse-body response)
              agent (reg/get-agent "codex-1")]
          (is (= 201 (:status response)))
          (is (true? (:ok parsed)))
          (is (= "codex-1" (:agent-id parsed)))
          (is (= sid (:session-id parsed)))
          (is (= (.getPath session-file) (:session-file parsed)))
          (is (= cwd (:cwd parsed)))
          (is (= sid (:agent/session-id agent)))
          (is (= true (get-in agent [:agent/metadata :require-execution?])))
          (is (= sid (some-> session-file slurp str/trim)))
          (is (fn? (:agent/invoke-fn agent))))
        (finally
          (if (some? backup)
            (spit session-file backup)
            (when (.exists session-file)
              (.delete session-file))))))))

(deftest codex-auto-register-reclaims-unreachable-remote-placeholder
  (testing "POST /api/alpha/agents/auto reuses stale remote codex-1 instead of allocating codex-2"
    (let [handler (make-handler)
          session-file (io/file "/tmp/futon-codex-session-id-codex-1")
          backup (when (.exists session-file) (slurp session-file))
          sid "sess-auto-codex-reclaim"
          cwd "/home/joe/code"
          body (json/generate-string {"type" "codex"
                                      "session-id" sid
                                      "cwd" cwd})]
      (try
        (when (.exists session-file)
          (.delete session-file))
        (reg/register-agent!
         {:agent-id {:id/value "codex-1" :id/type :continuity}
          :type :codex
          :invoke-fn nil
          :capabilities [:edit :test :coordination/execute]
          :metadata {:remote? true
                     :note "unreachable remote placeholder"}})
        (let [response (post handler "/api/alpha/agents/auto" body)
              parsed (parse-body response)
              agent (reg/get-agent "codex-1")]
          (is (= 201 (:status response)))
          (is (true? (:ok parsed)))
          (is (= "codex-1" (:agent-id parsed)))
          (is (nil? (reg/get-agent "codex-2"))
              "reclaiming codex-1 avoids minting a confusing extra lane")
          (is (= sid (:agent/session-id agent)))
          (is (fn? (:agent/invoke-fn agent)))
          (is (= true (get-in agent [:agent/metadata :auto-registered?])))
          (is (nil? (get-in agent [:agent/metadata :remote?]))
              "local registration must clear stale remote metadata")
          (is (= sid (some-> session-file slurp str/trim))))
        (finally
          (if (some? backup)
            (spit session-file backup)
            (when (.exists session-file)
              (.delete session-file))))))))

(deftest zai-auto-register-seeds-session-id
  (testing "POST /api/alpha/agents/auto creates a fresh zai lane with its own session file"
    (let [handler (make-handler {:evidence-store (atom {:entries {} :order []})})
          session-file (io/file "/tmp/futon-zai-session-id-zai-1")
          backup (when (.exists session-file) (slurp session-file))
          sid "sess-auto-zai-register"
          cwd "/home/joe/code"
          body (json/generate-string {"type" "zai"
                                      "session-id" sid
                                      "cwd" cwd
                                      "memory-domain" "mathematics"})]
      (try
        (when (.exists session-file)
          (.delete session-file))
        (let [response (post handler "/api/alpha/agents/auto" body)
              parsed (parse-body response)
              agent (reg/get-agent "zai-1")]
          (is (= 201 (:status response)))
          (is (true? (:ok parsed)))
          (is (= "zai-1" (:agent-id parsed)))
          (is (= sid (:session-id parsed)))
          (is (= (.getPath session-file) (:session-file parsed)))
          (is (= cwd (:cwd parsed)))
          (is (= "mathematics" (:memory-domain parsed)))
          (is (= :zai (:agent/type agent)))
          (is (= :mathematics
                 (get-in agent [:agent/metadata :memory-domain])))
          (is (fn? (:agent/invoke-fn agent)))
          (is (= sid (:agent/session-id agent)))
          (is (= sid (some-> session-file slurp str/trim))))
        (finally
          (if (some? backup)
            (spit session-file backup)
            (when (.exists session-file)
              (.delete session-file))))))))

(deftest agent-restore-registers-codex-exact-identity
  (testing "POST /api/alpha/agents/restore recreates an exact codex identity"
    (let [handler (make-handler)
          session-file (io/file "/tmp/futon-codex-session-id-codex-99")
          backup (when (.exists session-file) (slurp session-file))
          sid "sess-restore-codex-99"
          cwd "/home/joe/code"
          body (json/generate-string {"agent-id" "codex-99"
                                      "type" "codex"
                                      "session-id" sid
                                      "session-file" (.getPath session-file)
                                      "cwd" cwd})]
      (try
        (when (.exists session-file)
          (.delete session-file))
        (let [response (post handler "/api/alpha/agents/restore" body)
              parsed (parse-body response)
              agent (reg/get-agent "codex-99")]
          (is (= 201 (:status response)))
          (is (true? (:ok parsed)))
          (is (= "registered" (:action parsed)))
          (is (= "codex-99" (:agent-id parsed)))
          (is (= sid (:session-id parsed)))
          (is (= (.getPath session-file) (:session-file parsed)))
          (is (= cwd (:cwd parsed)))
          (is (= sid (:agent/session-id agent)))
          (is (= true (get-in agent [:agent/metadata :require-execution?])))
          (is (= cwd (get-in agent [:agent/metadata :cwd])))
          (is (= sid (some-> session-file slurp str/trim)))
          (is (fn? (:agent/invoke-fn agent))))
        (finally
          (if (some? backup)
            (spit session-file backup)
            (when (.exists session-file)
              (.delete session-file))))))))

(deftest agent-restore-clears-stale-remote-metadata
  (testing "POST /api/alpha/agents/restore makes a local lane stop displaying as remote"
    (let [handler (make-handler)
          session-file (io/file "/tmp/futon-codex-session-id-codex-1")
          backup (when (.exists session-file) (slurp session-file))
          sid "sess-restore-local-codex-1"
          body (json/generate-string {"agent-id" "codex-1"
                                      "type" "codex"
                                      "session-id" sid
                                      "session-file" (.getPath session-file)
                                      "cwd" "/home/joe/code"})]
      (try
        (when (.exists session-file)
          (.delete session-file))
        (reg/register-agent!
         {:agent-id {:id/value "codex-1" :id/type :continuity}
          :type :codex
          :invoke-fn nil
          :capabilities [:edit :test :coordination/execute]
          :metadata {:remote? true
                     :remote-proxy? true
                     :origin-url "http://linode.invalid:7070"
                     :note "stale remote placeholder"}})
        (let [response (post handler "/api/alpha/agents/restore" body)
              parsed (parse-body response)
              agent (reg/get-agent "codex-1")]
          (is (= 200 (:status response)))
          (is (true? (:ok parsed)))
          (is (= "updated" (:action parsed)))
          (is (= sid (:agent/session-id agent)))
          (is (fn? (:agent/invoke-fn agent)))
          (is (nil? (get-in agent [:agent/metadata :remote?])))
          (is (nil? (get-in agent [:agent/metadata :remote-proxy?])))
          (is (nil? (get-in agent [:agent/metadata :origin-url])))
          (is (nil? (get-in agent [:agent/metadata :note]))))
        (finally
          (if (some? backup)
            (spit session-file backup)
            (when (.exists session-file)
              (.delete session-file))))))))

(deftest agent-restore-existing-preserves-session-id-when-omitted
  (testing "POST /api/alpha/agents/restore refreshes invoke-fn without clearing session-id"
    (let [handler (make-handler)
          session-file (io/file "/tmp/futon-session-id-claude-88")
          backup (when (.exists session-file) (slurp session-file))
          sid "sess-restore-claude-88"
          initial-body (json/generate-string {"agent-id" "claude-88"
                                              "type" "claude"
                                              "session-id" sid
                                              "session-file" (.getPath session-file)
                                              "cwd" "/home/joe/code"})
          refresh-body (json/generate-string {"agent-id" "claude-88"
                                              "type" "claude"
                                              "session-file" (.getPath session-file)
                                              "cwd" "/home/joe/code/futon3c"})]
      (try
        (when (.exists session-file)
          (.delete session-file))
        (let [initial-response (post handler "/api/alpha/agents/restore" initial-body)
              _ (spit session-file "stale-session-from-before-restart")
              refresh-response (post handler "/api/alpha/agents/restore" refresh-body)
              parsed (parse-body refresh-response)
              agent (reg/get-agent "claude-88")]
          (is (= 201 (:status initial-response)))
          (is (= 200 (:status refresh-response)))
          (is (true? (:ok parsed)))
          (is (= "updated" (:action parsed)))
          (is (= sid (:session-id parsed)))
          (is (= sid (:agent/session-id agent)))
          (is (= sid (some-> session-file slurp str/trim)))
          (is (= "/home/joe/code/futon3c"
                 (get-in agent [:agent/metadata :cwd]))))
        (finally
          (if (some? backup)
            (spit session-file backup)
            (when (.exists session-file)
              (.delete session-file))))))))

(deftest agent-restore-uses-claude-session-cwd
  (testing "POST /api/alpha/agents/restore uses Claude transcript cwd over caller cwd"
    (let [handler (make-handler)
          projects-root (doto (io/file (System/getProperty "java.io.tmpdir")
                                       (str "claude-projects-" (java.util.UUID/randomUUID)))
                          (.mkdirs))
          transcript-dir (doto (io/file projects-root "-home-joe-code")
                           (.mkdirs))
          session-file (java.io.File/createTempFile "futon3c-http-claude-cwd-" ".sid")
          transcript-session "sess-restore-claude-cwd"
          transcript-cwd "/home/joe/code"
          wrong-cwd "/home/joe/code/futon5a/essays/ukrn-open-research-training-plos-one"
          transcript-file (io/file transcript-dir (str transcript-session ".jsonl"))
          original-claude-session-cwd (var-get #'futon3c.transport.http/claude-session-cwd)
          body (json/generate-string {"agent-id" "claude-cwd"
                                      "type" "claude"
                                      "session-id" transcript-session
                                      "session-file" (.getPath session-file)
                                      "cwd" wrong-cwd})]
      (try
        (spit transcript-file
              (json/generate-string {:type "user"
                                     :sessionId transcript-session
                                     :cwd transcript-cwd}))
        (with-redefs [futon3c.transport.http/claude-session-cwd
                      (fn
                        ([session-id]
                         (original-claude-session-cwd projects-root session-id))
                        ([root session-id]
                         (original-claude-session-cwd root session-id)))]
          (let [response (post handler "/api/alpha/agents/restore" body)
                parsed (parse-body response)
                agent (reg/get-agent "claude-cwd")]
            (is (= 201 (:status response)))
            (is (true? (:ok parsed)))
            (is (= transcript-cwd (:cwd parsed)))
            (is (= transcript-cwd (get-in agent [:agent/metadata :cwd])))))
        (finally
          (when (.exists session-file)
            (.delete session-file))
          (doseq [f (reverse (file-seq projects-root))]
            (.delete f)))))))

(deftest agent-reset-session-clears-backing-continuity
  (testing "POST /api/alpha/agents/:id/reset-session clears registry and backing session state"
    (let [handler (make-handler)
          session-file (java.io.File/createTempFile "futon3c-http-reset-" ".sid")
          sid-atom (atom "sess-http-reset")]
      (try
        (spit session-file "sess-http-reset")
        (reg/register-agent!
         {:agent-id {:id/value "codex-reset" :id/type :continuity}
          :type :codex
          :invoke-fn (fn [_ _] {:result "ok"})
          :capabilities [:edit]
          :session-id "sess-http-reset"
          :session-reset-fn (fn []
                              (reset! sid-atom nil)
                              (when (.exists session-file)
                                (.delete session-file))
                              {:ok true})})
        (let [response (post handler "/api/alpha/agents/codex-reset/reset-session" "{}")
              parsed (parse-body response)]
          (is (= 200 (:status response)))
          (is (true? (:ok parsed)))
          (is (= "sess-http-reset" (:old-session-id parsed)))
          (is (nil? @sid-atom))
          (is (false? (.exists session-file)))
          (is (nil? (:session-id (get-in (reg/registry-status) [:agents "codex-reset"])))))
        (finally
          (when (.exists session-file)
            (.delete session-file)))))))

;; =============================================================================
;; POST /api/alpha/invoke tests
;; =============================================================================

(deftest invoke-job-preclocks-payload-mission-before-turn
  (testing "payload mission-id is visible under the fallback key during the turn"
    (let [seen-during-turn (atom nil)
          job-id (#'http/create-invoke-job! {:agent-id "zai-preclock"
                                             :prompt "check context"
                                             :caller "test"
                                             :surface "emacs-repl"})]
      (with-redefs [http/invoke-agent-with-session-recovery!
                    (fn [agent-id _prompt _timeout-ms]
                      (reset! seen-during-turn
                              (clock-store/current-clock agent-id "real-session"))
                      {:ok true :result "ok" :session-id "real-session"})
                    clock-lineage/clock-dispatch!
                    (fn [agent-id session-id mission-id]
                      (clock-store/set-dispatch-mission! agent-id session-id mission-id))]
        (let [result (#'http/run-invoke-job! {:job-id job-id
                                              :agent-id "zai-preclock"
                                              :prompt "check context"
                                              :caller "test"
                                              :surface "emacs-repl"
                                              :mission-id "E-preclock"})]
          (is (:ok result))
          (is (= {:campaign-id nil :mission-id nil :excursion-id "E-preclock"}
                 @seen-during-turn))
          (is (= {:campaign-id nil :mission-id nil :excursion-id "E-preclock"}
                 (clock-store/current-clock "zai-preclock" "real-session"))))))))

(deftest claude-invoke-recovers-from-missing-conversation-session
  (testing "Claude missing-conversation resume failure clears continuity and retries once"
    (let [handler (make-handler)
          session-file (java.io.File/createTempFile "futon3c-http-claude-stale-" ".sid")
          attempts (atom [])
          sid-atom (atom "stale-claude-session")]
      (try
        (spit session-file "stale-claude-session")
        (reg/register-agent!
         {:agent-id {:id/value "claude-stale" :id/type :continuity}
          :type :claude
          :invoke-fn (fn [_prompt session-id]
                       (swap! attempts conj session-id)
                       (if (= "stale-claude-session" session-id)
                         {:error "Exit 1: No conversation found with session ID: stale-claude-session"
                          :session-id session-id}
                         {:result "fresh ok"
                          :session-id "fresh-claude-session"}))
          :capabilities [:explore]
          :session-id "stale-claude-session"
          :session-reset-fn (fn []
                              (reset! sid-atom nil)
                              (when (.exists session-file)
                                (.delete session-file))
                              {:ok true})})
        (let [response (post handler "/api/alpha/invoke"
                             (json/generate-string {"agent-id" "claude-stale"
                                                    "prompt" "hello"}))
              parsed (parse-body response)]
          (is (= 200 (:status response)))
          (is (true? (:ok parsed)))
          (is (= "fresh ok" (:result parsed)))
          (is (= "fresh-claude-session" (:session-id parsed)))
          (is (= ["stale-claude-session" nil] @attempts))
          (is (nil? @sid-atom))
          (is (= "stale-claude-session"
                 (get-in parsed [:session-recovery :old-session-id])))
          (is (= "claude-missing-conversation"
                 (get-in parsed [:session-recovery :reason]))))
        (finally
          (when (.exists session-file)
            (.delete session-file)))))))

(deftest codex-task-no-execution-detection
  (testing "task-mode codex reply without execution evidence is rejected"
    (is (true?
         (#'futon3c.transport.http/codex-task-no-execution?
         "codex-1"
         "--- CURRENT TURN ---\nSurface: irc (#math)\n---\n\n[Surface: IRC | Mode: task]\nInvestigate."
         {:ok true
          :result "captured plan in data/proof-state/FM-001-falsify-plan.md"
          :invoke-meta {:execution {:executed? false
                                    :tool-events 0
                                    :command-events 0}}}
          true))))
  (testing "planning-only task-mode reply is allowed"
    (is (false?
         (#'futon3c.transport.http/codex-task-no-execution?
          "codex-1"
         "[Surface: IRC | Mode: task]\nInvestigate."
         {:ok true
          :result "Planning-only: need clarification before executing."
          :invoke-meta {:execution {:executed? false
                                    :tool-events 0
                                    :command-events 0}}}
          true))))
  (testing "non-task prompts are not blocked by this gate"
    (is (false?
         (#'futon3c.transport.http/codex-task-no-execution?
         "codex-1"
         "hello"
         {:ok true
           :result "captured plan"
           :invoke-meta {:execution {:executed? false
                                     :tool-events 0
                                     :command-events 0}}}
          true))))
  (testing "mission/work prompts are blocked even without explicit mode marker"
    (is (true?
         (#'futon3c.transport.http/codex-task-no-execution?
          "codex-1"
         "@codex can you give me a summary of the state of play on FM-001"
         {:ok true
          :result "@joe FM-001 is in FALSIFY mode with refs"
          :invoke-meta {:execution {:executed? false
                                     :tool-events 0
                                     :command-events 0}}}
          true)))))

(deftest wrap-agent-facing-surface-includes-live-emacs-projection
  (testing "invoke prompt includes the active Emacs read/write surface contract"
    (with-redefs [reg/current-surface-projection
                  (fn [_]
                    {:surface "emacs-cursor"
                     :editor-id "editor-main"
                     :mode "follow"
                     :buffer-summary "buffer=foo.clj user=(line 7 col 2 point 101) remote=nil"
                     :write-surface "minibuffer"})]
      (let [prompt (#'futon3c.transport.http/wrap-agent-facing-surface
                    "Investigate the current form."
                    "emacs-repl"
                    "joe"
                    "codex-8")]
        (is (str/includes? prompt "Surface: emacs-repl"))
        (is (str/includes? prompt "Live surface projection:"))
        (is (str/includes? prompt "Editor: editor-main"))
        (is (str/includes? prompt "Read surface `buffer`: buffer=foo.clj"))
        (is (str/includes? prompt "Write surface `minibuffer`: emit lines `MINIBUFFER: <text-or-json>`"))
        (is (str/includes? prompt "\"command\":\"eval-sexp\""))
        (is (str/includes? prompt "\"command\":\"run-script\""))))))

(deftest maybe-route-surface-writes-strips-and-relays-minibuffer-directives
  (testing "MINIBUFFER directives are removed from visible output and relayed to Emacs"
    (let [sent (atom [])]
      (with-redefs [reg/current-surface-projection
                    (fn [_]
                      {:surface "emacs-cursor"
                       :editor-id "editor-main"})
                    futon3c.transport.peripheral-events/send-peripheral-event!
                    (fn [agent-id peripheral-id event payload]
                      (swap! sent conj {:agent-id agent-id
                                        :peripheral-id peripheral-id
                                        :event event
                                        :payload payload})
                      true)]
        (let [result (#'futon3c.transport.http/maybe-route-surface-writes
                      "codex-8"
                      {:ok true
                       :result (str "Summary complete.\n"
                                    "MINIBUFFER: {\"command\":\"refresh-context\"}\n"
                                    "MINIBUFFER: Inspect current defun")
                       :invoke-meta {:execution {:executed? true}}})]
          (is (= "Summary complete." (:result result)))
          (is (= {:minibuffer-events 2
                  :routed-events 2}
                 (get-in result [:invoke-meta :surface-write])))
          (is (= [{:agent-id "codex-8"
                   :peripheral-id :emacs-cursor
                   :event :minibuffer
                   :payload {:command "refresh-context"
                             :request-id "minibuffer-0"}}
                  {:agent-id "codex-8"
                   :peripheral-id :emacs-cursor
                   :event :minibuffer
                   :payload {:command "message"
                             :message "Inspect current defun"
                             :prompt "Inspect current defun"
                             :request-id "minibuffer-1"}}]
                 @sent)))))))

(deftest invoke-registered-codex-agent
  (testing "POST /api/alpha/invoke invokes codex agent via registry"
    (register-mock-agent! "codex-1" :codex)
    (let [handler (make-handler)
          body (json/generate-string {"agent-id" "codex-1"
                                      "prompt" "hello"})]
      (with-live-server
        handler
        (fn [base-url]
          (let [response (http-post-json base-url "/api/alpha/invoke" body)
                parsed (parse-body response)
                job-id (:job-id parsed)
                final* (wait-for-job-state handler job-id 2000)
                final-resp (:response final*)
                final-parsed (:parsed final*)
                job (:job final-parsed)]
            (is (= 200 (:status response)))
            (is (true? (:ok parsed)))
            (is (string? job-id))
            (is (= 200 (:status final-resp)))
            (is (true? (:ok final-parsed)))
            (is (= job-id (:job-id job)))
            (is (= "done" (:state job)))
            (is (= "ok" (:result-summary job)))))))))

(deftest invoke-job-query-roundtrip
  (testing "invoke response job-id can be queried via /api/alpha/invoke/jobs/:id"
    (register-mock-agent! "codex-job-1" :codex)
    (let [handler (make-handler)
          body (json/generate-string {"agent-id" "codex-job-1"
                                      "prompt" "hello from job query"})
          invoke-response (post handler "/api/alpha/invoke" body)
          invoke-parsed (parse-body invoke-response)
          job-id (:job-id invoke-parsed)
          job-response (get-req handler (str "/api/alpha/invoke/jobs/" job-id))
          job-parsed (parse-body job-response)
          job (:job job-parsed)]
      (is (= 200 (:status invoke-response)))
      (is (true? (:ok invoke-parsed)))
      (is (string? job-id))
      (is (= 200 (:status job-response)))
      (is (true? (:ok job-parsed)))
      (is (= job-id (:job-id job)))
      (is (= "done" (:state job)))
      (is (= "codex-job-1" (:agent-id job))))))

(deftest invoke-job-artifact-ref-preserves-default-futon-matching
  (testing "default futon mode does not treat mfuton gitlab issue URLs as canonical artifact refs"
    (let [handler (make-handler)
          body (json/generate-string {"agent-id" "codex-http-artifact-default"
                                      "prompt" "hello from artifact default"})]
      (register-mock-agent! "codex-http-artifact-default" :codex)
      (with-redefs [mfuton-mode/mfuton-mode (constantly "futon")
                    reg/invoke-agent! (fn [_ _ _]
                                        {:ok true
                                         :result "Blocked; see http://192.168.165.188/mfuton/-/issues/2"
                                         :session-id "sess-http-artifact-default"
                                         :invoke-meta {:invoke-trace-id "invoke-http-artifact-default-1"
                                                       :execution {:executed? true
                                                                   :tool-events 1
                                                                   :command-events 0}}})]
        (let [invoke-response (post handler "/api/alpha/invoke" body)
              invoke-parsed (parse-body invoke-response)
              job-id (:job-id invoke-parsed)
              job-response (get-req handler (str "/api/alpha/invoke/jobs/" job-id))
              job-parsed (parse-body job-response)]
          (is (= 200 (:status invoke-response)))
          (is (= 200 (:status job-response)))
          (is (nil? (get-in job-parsed [:job :artifact-ref]))))))))

(deftest invoke-job-artifact-ref-does-not-take-pr-prefix-from-prose
  (testing "ordinary prose before a commit SHA cannot masquerade as a PR ref"
    (let [handler (make-handler)
          body (json/generate-string {"agent-id" "codex-http-sha-after-prose"
                                      "prompt" "hello from artifact parser"})]
      (register-mock-agent! "codex-http-sha-after-prose" :codex)
      (with-redefs [reg/invoke-agent!
                    (fn [_ _ _]
                      {:ok true
                       :result (str "Implemented fail-closed provenance checks. "
                                    "FULL_LOOP_AUTHOR: DONE "
                                    "dd3a23c81dfd275cbe406e4315a46087a8263f13")
                       :session-id "sess-http-sha-after-prose"
                       :invoke-meta
                       {:invoke-trace-id "invoke-http-sha-after-prose-1"
                        :execution {:executed? true
                                    :tool-events 1
                                    :command-events 1}}})]
        (let [invoke-response (post handler "/api/alpha/invoke" body)
              invoke-parsed (parse-body invoke-response)
              job-id (:job-id invoke-parsed)
              job-response (get-req handler (str "/api/alpha/invoke/jobs/" job-id))
              job-parsed (parse-body job-response)]
          (is (= 200 (:status invoke-response)))
          (is (= 200 (:status job-response)))
          (is (= "dd3a23c81dfd275cbe406e4315a46087a8263f13"
                 (get-in job-parsed [:job :artifact-ref]))))))))

(deftest invoke-job-artifact-ref-mfuton-mode-preserves-generic-matching
  (testing "mfuton mode still leaves gitlab issue URLs outside the generic canonical artifact-ref slot"
    (let [handler (make-handler)
          body (json/generate-string {"agent-id" "codex-http-artifact-mfuton"
                                      "prompt" "hello from artifact mfuton"})]
      (register-mock-agent! "codex-http-artifact-mfuton" :codex)
      (with-redefs [mfuton-mode/mfuton-mode (constantly "mfuton")
                    reg/invoke-agent! (fn [_ _ _]
                                        {:ok true
                                         :result "Blocked; see http://192.168.165.188/mfuton/-/issues/2"
                                         :session-id "sess-http-artifact-mfuton"
                                         :invoke-meta {:invoke-trace-id "invoke-http-artifact-mfuton-1"
                                                       :execution {:executed? true
                                                                   :tool-events 1
                                                                   :command-events 0}}})]
        (let [invoke-response (post handler "/api/alpha/invoke" body)
              invoke-parsed (parse-body invoke-response)
              job-id (:job-id invoke-parsed)
              job-response (get-req handler (str "/api/alpha/invoke/jobs/" job-id))
              job-parsed (parse-body job-response)]
          (is (= 200 (:status invoke-response)))
          (is (= 200 (:status job-response)))
          (is (nil? (get-in job-parsed [:job :artifact-ref]))))))))

(deftest invoke-job-artifact-ref-prefers-frontiermath-local-run-bundle
  (testing "frontiermath-local receipt bundle paths become canonical artifact refs"
    (let [handler (make-handler)
          body (json/generate-string {"agent-id" "codex-http-artifact-frontiermath-run"
                                      "prompt" "hello from artifact frontiermath run"})]
      (register-mock-agent! "codex-http-artifact-frontiermath-run" :codex)
      (with-redefs [reg/invoke-agent! (fn [_ _ _]
                                        {:ok true
                                         :result (str "Blocked; see http://192.168.165.188/mfuton/-/issues/2 "
                                                      "and mfuton/data/frontiermath-local/FM-001/runs/"
                                                      "2026-03-12-live-runtime-proof-root-cycle-230802Z/")
                                         :session-id "sess-http-artifact-frontiermath-run"
                                         :invoke-meta {:invoke-trace-id "invoke-http-artifact-frontiermath-run-1"
                                                       :execution {:executed? true
                                                                   :tool-events 1
                                                                   :command-events 0}}})]
        (let [invoke-response (post handler "/api/alpha/invoke" body)
              invoke-parsed (parse-body invoke-response)
              job-id (:job-id invoke-parsed)
              job-response (get-req handler (str "/api/alpha/invoke/jobs/" job-id))
              job-parsed (parse-body job-response)]
          (is (= 200 (:status invoke-response)))
          (is (= 200 (:status job-response)))
          (is (= "mfuton/data/frontiermath-local/FM-001/runs/2026-03-12-live-runtime-proof-root-cycle-230802Z/"
                 (get-in job-parsed [:job :artifact-ref]))))))))

(deftest invoke-job-artifact-ref-prefers-frontiermath-local-active-root
  (testing "frontiermath-local active roots become canonical artifact refs"
    (let [handler (make-handler)
          body (json/generate-string {"agent-id" "codex-http-artifact-frontiermath-root"
                                      "prompt" "hello from artifact frontiermath root"})]
      (register-mock-agent! "codex-http-artifact-frontiermath-root" :codex)
      (with-redefs [reg/invoke-agent! (fn [_ _ _]
                                        {:ok true
                                         :result "Verified writable proof-state authority at mfuton/data/frontiermath-local/FM-001/active"
                                         :session-id "sess-http-artifact-frontiermath-root"
                                         :invoke-meta {:invoke-trace-id "invoke-http-artifact-frontiermath-root-1"
                                                       :execution {:executed? true
                                                                   :tool-events 1
                                                                   :command-events 0}}})]
        (let [invoke-response (post handler "/api/alpha/invoke" body)
              invoke-parsed (parse-body invoke-response)
              job-id (:job-id invoke-parsed)
              job-response (get-req handler (str "/api/alpha/invoke/jobs/" job-id))
              job-parsed (parse-body job-response)]
          (is (= 200 (:status invoke-response)))
          (is (= 200 (:status job-response)))
          (is (= "mfuton/data/frontiermath-local/FM-001/active"
                 (get-in job-parsed [:job :artifact-ref]))))))))

(deftest invoke-http-surface-auto-records-delivery-when-trace-present
  (testing "direct HTTP invoke marks delivery delivered when invoke-meta includes trace-id"
    (let [handler (make-handler)
          body (json/generate-string {"agent-id" "codex-http-delivery"
                                      "prompt" "hello from http delivery"})]
      (register-mock-agent! "codex-http-delivery" :codex)
      (with-redefs [reg/invoke-agent! (fn [_ _ _]
                                        {:ok true
                                         :result "done"
                                         :session-id "sess-http-delivery"
                                         :invoke-meta {:invoke-trace-id "invoke-http-delivery-1"
                                                       :execution {:executed? true
                                                                   :tool-events 1
                                                                   :command-events 0}}})]
        (let [invoke-response (post handler "/api/alpha/invoke" body)
              invoke-parsed (parse-body invoke-response)
              job-id (:job-id invoke-parsed)
              job-response (get-req handler (str "/api/alpha/invoke/jobs/" job-id))
              job-parsed (parse-body job-response)]
          (is (= 200 (:status invoke-response)))
          (is (= 200 (:status job-response)))
          (is (= "delivered" (get-in job-parsed [:job :delivery :status])))
          (is (= "http" (get-in job-parsed [:job :delivery :surface]))))))))

(deftest invoke-emacs-surface-auto-records-delivery-when-trace-present
  (testing "direct Emacs invoke marks delivery delivered and keeps the Emacs surface label"
    (let [handler (make-handler)
          body (json/generate-string {"agent-id" "codex-emacs-delivery"
                                      "prompt" "hello from emacs delivery"
                                      "surface" "emacs-repl"
                                      "caller" "joe"})]
      (register-mock-agent! "codex-emacs-delivery" :codex)
      (with-redefs [reg/invoke-agent! (fn [_ _ _]
                                        {:ok true
                                         :result "done"
                                         :session-id "sess-emacs-delivery"
                                         :invoke-meta {:invoke-trace-id "invoke-emacs-delivery-1"
                                                       :execution {:executed? true
                                                                   :tool-events 1
                                                                   :command-events 0}}})]
        (let [invoke-response (post handler "/api/alpha/invoke" body)
              invoke-parsed (parse-body invoke-response)
              job-id (:job-id invoke-parsed)
              job-response (get-req handler (str "/api/alpha/invoke/jobs/" job-id))
              job-parsed (parse-body job-response)]
          (is (= 200 (:status invoke-response)))
          (is (= 200 (:status job-response)))
          (is (= "delivered" (get-in job-parsed [:job :delivery :status])))
          (is (= "emacs-repl" (get-in job-parsed [:job :delivery :surface])))
          (is (= "caller joe" (get-in job-parsed [:job :delivery :destination]))))))))

(deftest invoke-job-delivery-records-on-job
  (testing "POST /api/alpha/invoke-delivery updates invoke-job delivery state via trace-id"
    (let [handler (make-handler)
          invoke-body (json/generate-string {"agent-id" "codex-delivery-job"
                                             "prompt" "ship it"})
          delivery-body (json/generate-string {"agent-id" "codex-delivery-job"
                                               "invoke-trace-id" "invoke-job-trace-1"
                                               "surface" "irc"
                                               "destination" "#futon as <codex>"
                                               "delivered" true
                                               "note" "bridge-test"})]
      (register-mock-agent! "codex-delivery-job" :codex)
      (with-redefs [reg/invoke-agent! (fn [_ _ _]
                                        {:ok true
                                         :result "done"
                                         :session-id "sess-djob"
                                         :invoke-meta {:invoke-trace-id "invoke-job-trace-1"
                                                       :execution {:executed? true
                                                                   :tool-events 1
                                                                   :command-events 0}}})
                    futon3c.transport.http/*resolve-delivery-recorder*
                    (fn []
                      (fn [_agent-id _invoke-trace-id _receipt] true))]
        (let [invoke-response (post handler "/api/alpha/invoke" invoke-body)
              invoke-parsed (parse-body invoke-response)
              job-id (:job-id invoke-parsed)
              delivery-response (post handler "/api/alpha/invoke-delivery" delivery-body)
              _delivery-parsed (parse-body delivery-response)
              job-response (get-req handler (str "/api/alpha/invoke/jobs/" job-id))
              job-parsed (parse-body job-response)]
          (is (= 200 (:status invoke-response)))
          (is (string? job-id))
          (is (= 200 (:status delivery-response)))
          (is (= 200 (:status job-response)))
          (is (= "delivered" (get-in job-parsed [:job :delivery :status])))
          (is (= "#futon as <codex>" (get-in job-parsed [:job :delivery :destination]))))))))

(deftest invoke-job-failure-is-terminal
  (testing "unknown agent invoke still creates a terminal failed job"
    (let [handler (make-handler)
          body (json/generate-string {"agent-id" "ghost-terminal"
                                      "prompt" "hello"})
          invoke-response (post handler "/api/alpha/invoke" body)
          invoke-parsed (parse-body invoke-response)
          job-id (:job-id invoke-parsed)
          job-response (get-req handler (str "/api/alpha/invoke/jobs/" job-id))
          job-parsed (parse-body job-response)]
      (is (= 404 (:status invoke-response)))
      (is (string? job-id))
      (is (= 200 (:status job-response)))
      (is (= "failed" (get-in job-parsed [:job :state])))
      (is (= "agent-not-found" (get-in job-parsed [:job :terminal-code])))
      (is (some? (get-in job-parsed [:job :finished-at]))))))

(deftest invoke-job-recovery-marks-stale-running-failed
  (testing "stale running jobs are recovered to terminal failure on restart/load"
    (with-temp-dir
      (fn [dir]
        (let [store-file (io/file dir "invoke-jobs.edn")
              preloaded {:version 1
                         :next-seq 1
                         :job-order ["job-stale-1"]
                         :trace->job {}
                         :jobs {"job-stale-1"
                                {:job-id "job-stale-1"
                                 :agent-id "codex-1"
                                 :state "running"
                                 :created-at "2026-03-07T20:00:00Z"
                                 :started-at "2026-03-07T20:00:01Z"
                                 :event-seq 1
                                 :events [{:seq 1 :type "accepted" :at "2026-03-07T20:00:00Z"}]}}}]
          (spit store-file (pr-str preloaded))
          (with-redefs [futon3c.transport.http/invoke-jobs-store-path
                        (fn [] (.getAbsolutePath store-file))]
            (http/reset-invoke-jobs!)
            (let [handler (make-handler)
                  response (get-req handler "/api/alpha/invoke/jobs/job-stale-1")
                  parsed (parse-body response)]
              (is (= 200 (:status response)))
              (is (= "failed" (get-in parsed [:job :state])))
              (is (= "worker-lost-on-restart" (get-in parsed [:job :terminal-code])))
              (is (some? (get-in parsed [:job :finished-at]))))))))))

(deftest bell-accepts-and-runs-async-job
  (testing "POST /api/alpha/bell returns accepted immediately and job reaches terminal state"
    (register-mock-agent! "codex-bell-1" :codex)
    (let [handler (make-handler)
          body (json/generate-string {"agent-id" "codex-bell-1"
                                      "prompt" "hello from bell async"})
          response (post handler "/api/alpha/bell" body)
          parsed (parse-body response)
          job-id (:job-id parsed)
          final* (wait-for-job-state handler job-id 2000)
          final (:parsed final*)]
      (is (= 202 (:status response)))
      (is (true? (:ok parsed)))
      (is (= true (:accepted parsed)))
      (is (string? job-id))
      (is (= "queued" (:state parsed)))
      (is (or (= "done" (get-in final [:job :state]))
              (= "failed" (get-in final [:job :state]))))
      (is (some? (get-in final [:job :finished-at]))))))

(deftest bell-explicit-work-mode-overrides-keyword-fallback
  (testing "an explicit client mode is stored even when the prompt has no work keywords"
    (register-mock-agent! "codex-bell-mode-work" :codex)
    (let [handler (make-handler)
          body (json/generate-string {"agent-id" "codex-bell-mode-work"
                                      "prompt" "Please investigate this unusual coordination behavior."
                                      "mode" "work"})
          response (post handler "/api/alpha/bell" body)
          parsed (parse-body response)
          final (-> (wait-for-job-state handler (:job-id parsed) 10000) :parsed)]
      (is (= 202 (:status response)))
      (is (= "work" (:mode parsed))
          "the response must not silently classify the supplied mode as brief")
      (is (= "work" (get-in final [:job :mode]))
          "the durable job record honors the client-supplied mode")
      (is (= "no-execution-evidence" (get-in final [:job :terminal-code]))
          "explicit work mode also drives work-mode execution enforcement"))))

(deftest bell-explicit-brief-mode-overrides-work-keywords
  (testing "explicit mode has precedence over prompt-text inference"
    (register-mock-agent! "codex-bell-mode-brief" :codex)
    (let [handler (make-handler)
          body (json/generate-string {"agent-id" "codex-bell-mode-brief"
                                      "prompt" "State of play on this task assignment"
                                      "mode" "brief"})
          response (post handler "/api/alpha/bell" body)
          parsed (parse-body response)
          final (-> (wait-for-job-state handler (:job-id parsed) 10000) :parsed)]
      (is (= 202 (:status response)))
      (is (= "brief" (:mode parsed)))
      (is (= "brief" (get-in final [:job :mode])))
      (is (= "done" (get-in final [:job :state]))
          "explicit brief mode disables keyword-inferred work enforcement"))))

(deftest invoke-announce-creates-canonical-queued-job
  (testing "POST /api/alpha/invoke/announce records a queued job before external acceptance"
    (register-mock-agent! "codex-announce-1" :codex)
    (let [handler (make-handler)
          body (json/generate-string {"agent-id" "codex-announce-1"
                                      "prompt" "hello from announce"
                                      "caller" "irc:joe"
                                      "surface" "irc (#math)"})
          response (post handler "/api/alpha/invoke/announce" body)
          parsed (parse-body response)
          job-id (:job-id parsed)
          job-response (get-req handler (str "/api/alpha/invoke/jobs/" job-id))
          job-parsed (parse-body job-response)]
      (is (= 202 (:status response)))
      (is (true? (:ok parsed)))
      (is (true? (:accepted parsed)))
      (is (= "queued" (:state parsed)))
      (is (= 1 (:queued-jobs parsed)))
      (is (string? job-id))
      (is (= 200 (:status job-response)))
      (is (= "queued" (get-in job-parsed [:job :state])))
      (is (= "pending" (get-in job-parsed [:job :delivery :status]))))))

(deftest invoke-announce-job-is-reused-by-direct-invoke
  (testing "announced queued jobs are reused by /api/alpha/invoke instead of duplicating ledger state"
    (register-mock-agent! "codex-announce-2" :codex)
    (let [handler (make-handler)
          announce-body (json/generate-string {"agent-id" "codex-announce-2"
                                               "prompt" "queued first"
                                               "caller" "irc:joe"
                                               "surface" "irc (#math)"})
          announce-response (post handler "/api/alpha/invoke/announce" announce-body)
          announce-parsed (parse-body announce-response)
          job-id (:job-id announce-parsed)
          invoke-body (json/generate-string {"agent-id" "codex-announce-2"
                                             "prompt" "queued first"
                                             "caller" "irc:joe"
                                             "surface" "irc (#math)"
                                             "job-id" job-id})
          invoke-response (post handler "/api/alpha/invoke" invoke-body)
          invoke-parsed (parse-body invoke-response)
          job-response (get-req handler (str "/api/alpha/invoke/jobs/" job-id))
          job-parsed (parse-body job-response)
          job (:job job-parsed)]
      (is (= 202 (:status announce-response)))
      (is (= 200 (:status invoke-response)))
      (is (= job-id (:job-id invoke-parsed)))
      (is (= 200 (:status job-response)))
      (is (= job-id (:job-id job)))
      (is (= "done" (:state job)))
      (is (= 1 (count (filter #(= "accepted" (:type %)) (:events job))))))))

(deftest bell-no-evidence-work-turn-fails-terminally
  (testing "bell work-mode invoke with no execution evidence ends as failed no-execution-evidence"
    (let [handler (make-handler)
          body (json/generate-string {"agent-id" "codex-bell-noev"
                                      "prompt" "@codex can you give me a summary of the state of play on FM-001"})]
      (register-mock-agent! "codex-bell-noev" :codex)
      (with-redefs [reg/invoke-agent! (fn [_ _ _]
                                        {:ok true
                                         :result "captured plan only"
                                         :session-id "sess-bell-noev"
                                         :invoke-meta {:execution {:executed? false
                                                                   :tool-events 0
                                                                   :command-events 0}}})]
        (let [response (post handler "/api/alpha/bell" body)
              parsed (parse-body response)
              job-id (:job-id parsed)
              final* (wait-for-job-state handler job-id 2000)
              final (:parsed final*)]
          (is (= 202 (:status response)))
          (is (string? job-id))
          (is (= "failed" (get-in final [:job :state])))
          (is (= "no-execution-evidence" (get-in final [:job :terminal-code]))))))))

(deftest invoke-missing-agent-returns-404
  (testing "POST /api/alpha/invoke returns 404 for unknown agent"
    (let [handler (make-handler)
          body (json/generate-string {"agent-id" "ghost-agent"
                                      "prompt" "hello"})]
      (with-live-server
        handler
        (fn [base-url]
          (let [response (http-post-json base-url "/api/alpha/invoke" body)
                parsed (parse-body response)]
            (is (= 404 (:status response)))
            (is (false? (:ok parsed)))
            (is (= "agent-not-found" (:error parsed)))))))))

(deftest invoke-includes-invoke-meta-when-available
  (testing "POST /api/alpha/invoke includes invoke-meta from registry invoke result"
    (reg/register-agent!
     {:agent-id {:id/value "codex-meta-http" :id/type :continuity}
      :type :codex
      :invoke-fn (fn [_prompt _session-id]
                   {:result "done"
                    :session-id "sess-meta-http"
                    :execution {:executed? true
                                :tool-events 1
                                :command-events 1}})
      :capabilities [:edit]})
    (let [handler (make-handler)
          body (json/generate-string {"agent-id" "codex-meta-http"
                                      "prompt" "ship it"})]
      (with-live-server
        handler
        (fn [base-url]
          (let [response (http-post-json base-url "/api/alpha/invoke" body)
                parsed (parse-body response)]
            (is (= 200 (:status response)))
            (is (true? (:ok parsed)))
            (is (= "done" (:result parsed)))
            (is (= "sess-meta-http" (:session-id parsed)))
            (is (= true (get-in parsed [:invoke-meta :execution :executed?])))
            (is (= 1 (get-in parsed [:invoke-meta :execution :tool-events])))
            (is (= 1 (get-in parsed [:invoke-meta :execution :command-events])))))))))

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
;; POST /api/alpha/invoke-delivery tests
;; =============================================================================

(deftest invoke-delivery-records-receipt
  (testing "POST /api/alpha/invoke-delivery records delivery metadata"
    (let [calls (atom [])
          handler (make-handler)
          body (json/generate-string {"agent-id" "codex-1"
                                      "invoke-trace-id" "invoke-123"
                                      "surface" "irc"
                                      "destination" "#futon as <codex>"
                                      "delivered" true
                                      "note" "ngircd-bridge"})]
      (with-redefs [futon3c.transport.http/*resolve-delivery-recorder*
                    (fn []
                      (fn [agent-id invoke-trace-id receipt]
                        (swap! calls conj {:agent-id agent-id
                                           :invoke-trace-id invoke-trace-id
                                           :receipt receipt})))]
        (let [response (post handler "/api/alpha/invoke-delivery" body)
              parsed (parse-body response)]
          (is (= 200 (:status response)))
          (is (true? (:ok parsed)))
          (is (true? (:recorded parsed)))
          (is (= [{:agent-id "codex-1"
                   :invoke-trace-id "invoke-123"
                   :receipt {:surface "irc"
                             :destination "#futon as <codex>"
                             :delivered? true
                             :note "ngircd-bridge"}}]
                 @calls)))))))

(deftest invoke-delivery-relays-over-ws-when-local-recorder-unavailable
  (testing "POST /api/alpha/invoke-delivery succeeds via WS relay fallback"
    (let [calls (atom [])
          handler (make-handler)
          body (json/generate-string {"agent-id" "codex-1"
                                      "invoke-trace-id" "invoke-456"
                                      "surface" "irc"
                                      "destination" "#futon as <codex>"
                                      "delivered" true
                                      "note" "ngircd-bridge"})]
      (with-redefs [futon3c.transport.http/*resolve-delivery-recorder* (fn [] nil)
                    ws-invoke/send-frame!
                    (fn [agent-id payload]
                      (swap! calls conj {:agent-id agent-id :payload payload})
                      true)]
        (let [response (post handler "/api/alpha/invoke-delivery" body)
              parsed (parse-body response)]
          (is (= 200 (:status response)))
          (is (true? (:ok parsed)))
          (is (false? (:recorded parsed)))
          (is (true? (:relayed parsed)))
          (is (= [{:agent-id "codex-1"
                   :payload {"type" "invoke_delivery"
                             "agent_id" "codex-1"
                             "invoke_trace_id" "invoke-456"
                             "surface" "irc"
                             "destination" "#futon as <codex>"
                             "delivered" true
                             "note" "ngircd-bridge"}}]
                 @calls)))))))

(deftest invoke-delivery-validates-required-fields
  (testing "POST /api/alpha/invoke-delivery rejects missing trace id"
    (let [handler (make-handler)
          body (json/generate-string {"agent-id" "codex-1"})
          response (post handler "/api/alpha/invoke-delivery" body)
          parsed (parse-body response)]
      (is (= 400 (:status response)))
      (is (= "missing-invoke-trace-id" (:err parsed))))))

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

(deftest health-includes-irc-send-hint
  (testing "GET /health includes IRC relay availability and send-base hint from config"
    (let [handler (make-handler {:irc-send-fn (fn [_channel _from _text] :ok)
                                 :irc-send-base "http://172.236.28.208:7070"})
          response (get-req handler "/health")
          parsed (parse-body response)]
      (is (= 200 (:status response)))
      (is (= "ok" (:status parsed)))
      (is (= true (:irc-relay-configured parsed)))
      (is (= "http://172.236.28.208:7070" (:irc-send-base parsed))))))

(deftest health-includes-queue-hardening-status
  (testing "GET /health includes red A3 status when one hardening gate is off"
    (with-system-properties
      {"FUTON3C_DURABLE_QUEUE" "true"
       "FUTON3C_DRAINER_V2" "false"
       "FUTON3C_REPL_THROUGH_QUEUE" "true"}
      (fn []
        (let [handler (make-handler)
              response (get-req handler "/health")
              parsed (parse-body response)
              queue-hardening (:queue-hardening parsed)]
          (is (= 200 (:status response)))
          (is (= "ok" (:status parsed)))
          (is (= false (:ok? queue-hardening)))
          (is (= false (get-in queue-hardening [:gates :drainer-v2])))
          (is (= ["drainer-v2"] (:degraded queue-hardening))))))))

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

(deftest evidence-author-filter-is-pushed-to-backend
  (testing "GET /api/alpha/evidence pushes author into the backend query"
    (let [handler (make-handler)
          seen-query (atom nil)]
      (with-redefs [estore/query* (fn [_store query]
                                    (reset! seen-query query)
                                    [])]
        (let [response (get-req-with-query handler "/api/alpha/evidence" "author=codex")]
          (is (= 200 (:status response)))
          (is (= "codex" (:query/author @seen-query))))))))

(deftest evidence-limit-is-pushed-to-backend-without-app-filters
  (testing "GET /api/alpha/evidence?limit=N pushes the limit when no app-only filters are present"
    (let [handler (make-handler)
          seen-query (atom nil)]
      (with-redefs [estore/query* (fn [_store query]
                                    (reset! seen-query query)
                                    [])]
        (let [response (get-req-with-query handler "/api/alpha/evidence" "limit=5")]
          (is (= 200 (:status response)))
          (is (= 5 (:query/limit @seen-query))))))))

(deftest evidence-session-and-pattern-filters-are-pushed-to-backend
  (testing "session-id/pattern-id requests stay bounded instead of degrading to backend params={}"
    (let [handler (make-handler)
          seen-query (atom nil)]
      (with-redefs [estore/query* (fn [_store query]
                                    (reset! seen-query query)
                                    [])]
        (let [response (get-req-with-query
                        handler
                        "/api/alpha/evidence"
                        "session-id=sess-1&pattern-id=agent%2Fpause&limit=7")]
          (is (= 200 (:status response)))
          (is (= "sess-1" (:query/session-id @seen-query)))
          (is (= :agent/pause (:query/pattern-id @seen-query)))
          (is (= 7 (:query/limit @seen-query))))))))

(deftest evidence-query-defaults-to-bounded-backend-page
  (testing "GET /api/alpha/evidence without query params pushes a default page limit"
    (let [handler (make-handler)
          seen-query (atom nil)]
      (with-redefs [estore/query* (fn [_store query]
                                    (reset! seen-query query)
                                    [])]
        (let [response (get-req handler "/api/alpha/evidence")]
          (is (= 200 (:status response)))
          (is (= 100 (:query/limit @seen-query))))))))

(deftest evidence-count-author-filter-is-pushed-to-backend
  (testing "GET /api/alpha/evidence/count pushes author into the backend count query"
    (let [handler (make-handler)
          seen-query (atom nil)]
      (with-redefs [estore/count* (fn [_store query]
                                    (reset! seen-query query)
                                    0)]
        (let [response (get-req-with-query handler "/api/alpha/evidence/count" "author=codex")]
          (is (= 200 (:status response)))
          (is (= "codex" (:query/author @seen-query))))))))

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

(deftest evidence-create-normalizes-memory-witness-enum
  (testing "JSON outcome evidence remains usable by the typed PUR guard"
    (let [handler (make-handler)
          body
          (json/generate-string
           {"subject" {"ref/type" "task" "ref/id" "phase3/http-witness"}
            "type" "pattern-outcome"
            "claim-type" "observation"
            "author" "independent-checker"
            "body"
            {"memory-outcome/witness-status" "independently-witnessed"
             "exit" 0}})
          response (post handler "/api/alpha/evidence" body)
          parsed (parse-body response)
          stored (estore/get-entry (:evidence/id parsed))]
      (is (= 201 (:status response)))
      (is (= :independently-witnessed
             (get-in stored
                     [:evidence/body
                      :memory-outcome/witness-status]))))))

(deftest evidence-create-transport-failures-are-retryable
  (testing "outbox clients receive 503, not a terminal shape-like 400"
    (is (= 400 (#'http/append-error-status :store-serialization)))
    (is (= 503 (#'http/append-error-status :store-timeout)))
    (is (= 503 (#'http/append-error-status :store-unreachable)))
    (is (= 503 (#'http/append-error-status :store-rejected)))))

(deftest evidence-create-coalesces-the-same-stable-id
  (let [in-flight (var-get #'http/!evidence-appends-in-flight)]
    (reset! in-flight #{})
    (try
      (is (#'http/claim-evidence-append! "stable-id"))
      (is (not (#'http/claim-evidence-append! "stable-id")))
      (is (#'http/claim-evidence-append! "other-id"))
      (finally
        (reset! in-flight #{})))))

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

(deftest http-kit-shutdown-race-suppression
  (testing "expected shutdown races are suppressed only after stop begins"
    (let [submit-race (java.util.concurrent.RejectedExecutionException. "executor closed")
          close-race (java.nio.channels.ClosedChannelException.)]
      (is (true? (#'http/suppress-http-kit-error?
                  :stopped
                  "failed to submit task to executor service"
                  submit-race)))
      (is (true? (#'http/suppress-http-kit-error?
                  nil
                  "increase :queue-size if this happens often"
                  submit-race)))
      (is (true? (#'http/suppress-http-kit-error?
                  :stopped
                  "accept incoming request"
                  close-race)))
      (is (false? (#'http/suppress-http-kit-error?
                   :running
                   "failed to submit task to executor service"
                   submit-race)))
      (is (false? (#'http/suppress-http-kit-error?
                   :stopped
                   "some other message"
                   submit-race))))))

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

;; =============================================================================
;; Portfolio inference endpoint tests
;; =============================================================================

(deftest portfolio-state-returns-belief-state
  (testing "GET /api/alpha/portfolio/state returns current belief state"
    (let [handler (make-handler)
          response (get-req handler "/api/alpha/portfolio/state")
          parsed (parse-body response)]
      (is (= 200 (:status response)))
      (is (true? (:ok parsed)))
      (is (map? (:state parsed)))
      (is (contains? (:state parsed) :step-count)))))

(deftest portfolio-step-returns-recommendation
  (testing "POST /api/alpha/portfolio/step runs AIF step and returns recommendation"
    (let [handler (make-handler {:evidence-store estore/!store})
          response (post handler "/api/alpha/portfolio/step"
                         (json/generate-string {:agenda-id "wm.close-s6.v1"
                                                :claim "Close S6 by stepping Portfolio Inference using THE-STACK"
                                                :observation-source {:kind "aif-stack"
                                                                     :path "futon5a/holes/stories/THE-STACK.aif.edn"}}))
          parsed (parse-body response)]
      (is (= 200 (:status response)))
      (is (true? (:ok parsed)))
      (is (string? (:recommendation parsed)))
      (is (contains? parsed :diagnostics))
      (is (contains? parsed :action))
      (is (string? (:run-id parsed)))
      (is (= "wm.close-s6.v1" (:agenda-id parsed)))
      (is (= {:before 0 :after 1} (:step-count parsed)))
      (is (= "futon5a/holes/stories/THE-STACK.aif.edn"
             (get-in parsed [:observation-source :path])))
      (is (= 4 (count (get-in parsed [:evidence :entries]))))
      (let [evidence-response (get-req-with-query handler
                                                  "/api/alpha/evidence"
                                                  "tag=portfolio,step")
            evidence-parsed (parse-body evidence-response)
            step-entry (first (:entries evidence-parsed))]
        (is (= 200 (:status evidence-response)))
        (is (= 1 (:count evidence-parsed)))
        (is (= "wm.close-s6.v1" (get-in step-entry [:evidence/body :run :agenda-id])))
        (is (= (:run-id parsed) (get-in step-entry [:evidence/body :run :run-id])))))))

(deftest aif-stack-live-rolls-forward-s6-agenda
  (testing "GET /api/alpha/aif-stack/live marks v1 rolled-forward, exposes v2 as documented-but-underspecified, and updates the S6 counter"
    (let [handler (make-handler {:evidence-store estore/!store})
          _step-response (post handler "/api/alpha/portfolio/step"
                               (json/generate-string {:agenda-id "wm.close-s6.v1"
                                                      :claim "Close S6 by stepping Portfolio Inference using THE-STACK"
                                                     :observation-source {:kind "aif-stack"
                                                                           :path "futon5a/holes/stories/THE-STACK.aif.edn"}}))
          response (get-req handler "/api/alpha/aif-stack/live")
          parsed (parse-body response)
          s6-node (first (filter #(= "S6" (:id %)) (:stack-nodes parsed)))
          queue (get parsed :candidate-queue)
          inspectability (get-in queue [:by-leaf-family :inspectability])
          unmapped (get queue :unmapped-families)]
      (is (= 200 (:status response)))
      (is (= "wm.close-s6.v1" (get-in parsed [:reading :next-move :agenda :id])))
      (is (= "rolled-forward" (get-in parsed [:reading :next-move :agenda :status])))
      (is (= "portfolio-step-evidence"
             (get-in parsed [:reading :next-move :agenda :witness :kind])))
      (is (= "wm.close-s6.v1"
             (get-in parsed [:reading :next-move :agenda :witness :run :agenda-id])))
      (is (= 0
             (get-in parsed [:reading :next-move :agenda :witness :run :step-before])))
      (is (= 1
             (get-in parsed [:reading :next-move :agenda :witness :run :step-after])))
      (is (= "stack-self-step-count"
             (get-in parsed [:reading :next-move :agenda :effect-witness :kind])))
      (is (= 1
             (get-in parsed [:reading :next-move :agenda :effect-witness :after])))
      (is (= "wm.close-s6.v2"
             (get-in parsed [:reading :next-move :agenda :successor :id])))
      (is (= "underspecified"
             (get-in parsed [:reading :next-move :agenda :successor :status])))
      (is (= false
             (get-in parsed [:reading :next-move :agenda :successor :recommendation-grade?])))
      (is (= true
             (get-in parsed [:reading :next-move :agenda :successor :documented?])))
      (is (= ["action-surface" "step-witness" "effect-witness" "successor-witness"]
             (get-in parsed [:reading :next-move :agenda :successor :missing-fields])))
      (is (= "clear the completed War Machine item and set the next item for work via the Candidate Queue invariant"
             (get-in parsed [:reading :next-move :agenda :successor :specifically])))
      (is (= 1 (:live-self-step-count s6-node)))
      (is (= "PI loop step-count = 1; HGO spec-only" (:gap s6-node)))
      (is (string? (:generated-at queue)))
      (is (pos? (:run-count queue)))
      (is (pos? (:top-rank inspectability)))
      (is (pos? (:item-count inspectability)))
      (is (some #(= "human-visible-inspectability" %) (:families inspectability)))
      (is (some #(= "strategic-closure-specification" (:family-id %)) unmapped)))))

(deftest war-machine-serves-cached-snapshot
  (testing "GET /api/alpha/war-machine returns the cached snapshot plus freshness metadata"
    (let [handler (make-handler)
          as-of (java.time.Instant/parse "2026-05-25T12:00:00Z")
          response (with-redefs [requiring-resolve
                                 (fn [sym]
                                   (case sym
                                     futon3c.wm.scheduler/ensure-started!
                                     (fn [] {:ok true})
                                     futon3c.wm.scheduler/snapshot-for-days
                                     (fn [days]
                                       (when (= 14 days)
                                         {:as-of as-of
                                          :payload {"window" {"days" 14}
                                                    "judgement"
                                                    {"mode" "steady"
                                                     "selection-gain"
                                                     {"selection-gain" 1.0}
                                                     "decision"
                                                     {"action" "abstain"
                                                      "reason" "no-action-beats-no-op"}
                                                     "ranked-actions"
                                                     [{"rank" 1
                                                       "controller-score" 1.0
                                                       "habit-prior-bias" -2.0
                                                       "action"
                                                       {"type" "advance-mission"
                                                        "target" "M-live"}}]}}}))
                                     futon3c.wm.scheduler/status
                                     (fn []
                                       {:running? true
                                        :period-seconds 300
                                        :days-windows [14 90]})
                                     futon3c.wm.scheduler/request-window!
                                     (fn [_days] {:ok true})
                                     nil))
                                 futon3c.transport.http/current-vsatarcs-status
                                 (fn []
                                   {:available? true
                                    :build {:status :violation}
                                    :stories [{:story/id "leaf-invariants"
                                               :headline "drift"
                                               :build/status :violation}]
                                    :wm-escalation {:tier :warning}})]
                     (get-req-with-query handler "/api/alpha/war-machine" "days=14"))
          parsed (parse-body response)]
      (is (= 200 (:status response)))
      (is (= "*" (get-in response [:headers "Access-Control-Allow-Origin"])))
      (is (= 14 (get-in parsed [:window :days])))
      (is (= "steady" (get-in parsed [:judgement :mode])))
      (is (= "M-live"
             (get-in parsed
                     [:live-recommendation
                      :recommendation :target])))
      (is (false?
           (get-in parsed
                   [:live-recommendation
                    :selection-boundary
                    :blocks-recommendation?])))
      (is (= true (get-in parsed [:vsatarcs-status :available?])))
      (is (= "violation" (get-in parsed [:vsatarcs-status :build :status])))
      (is (= "2026-05-25T12:00:00Z" (:as-of parsed)))
      (is (integer? (:scan-age-seconds parsed)))
      (is (= 300 (get-in parsed [:scheduler :period-seconds]))))))

(deftest r14-summary-reads-selection-gain-schema
  (testing "the WM compatibility projection reads Futon2's current selection-gain state"
    (let [summary (with-redefs [requiring-resolve
                                (fn [sym]
                                  (case sym
                                    futon2.aif.trace/latest-trace-record
                                    (fn []
                                      {:selection-gain {:selection-gain 1.25
                                                        :perf-history [0.1 0.2]
                                                        :samples 2}
                                       :realized-outcome {:tick 17}})
                                    futon2.aif.selection-gain/coerce-state
                                    identity
                                    futon2.aif.selection-gain/selection-gain-for
                                    (fn [state] (double (:selection-gain state)))
                                    futon3c.aif.calibration/load-evidence
                                    (fn [] [])
                                    futon3c.aif.calibration/calibration-report
                                    (fn [_] {:paired-count 0
                                             :independent-paired-count 0
                                             :verdict :insufficient-data})
                                    nil))]
                    (#'http/r14-gamma-summary))]
      (is (= :selection-gain (:controller-kind summary)))
      (is (= 1.25 (:gamma summary)))
      (is (= 2 (:policy-outcome-samples summary)))
      (is (= {:tick 17} (:last-realized-outcome summary)))
      (is (false? (:held-at-prior? summary)))
      (is (not (str/includes? (:status summary) "trace read failed"))))))

(deftest war-machine-returns-503-while-background-warmup-starts
  (testing "GET /api/alpha/war-machine returns 503 and requests a background warmup when no snapshot exists yet"
    (let [handler (make-handler)
          requested-days (atom [])
          response (with-redefs [requiring-resolve
                                 (fn [sym]
                                   (case sym
                                     futon3c.wm.scheduler/ensure-started!
                                     (fn [] {:ok true})
                                     futon3c.wm.scheduler/snapshot-for-days
                                     (fn [_days] nil)
                                     futon3c.wm.scheduler/status
                                     (fn []
                                       {:running? true
                                        :period-seconds 300
                                        :days-windows [14 90]})
                                     futon3c.wm.scheduler/request-window!
                                     (fn [days]
                                       (swap! requested-days conj days)
                                       {:ok true})
                                     nil))]
                     (get-req-with-query handler "/api/alpha/war-machine" "days=90"))
          parsed (parse-body response)]
      (is (= 503 (:status response)))
      (is (= "*" (get-in response [:headers "Access-Control-Allow-Origin"])))
      (is (= "60" (get-in response [:headers "Retry-After"])))
      (is (= [90] @requested-days))
      (is (= "war-machine-snapshot-unavailable" (:error parsed)))
      (is (= 90 (:days parsed)))
      (is (= 60 (:retry-after-seconds parsed))))))

(deftest portfolio-heartbeat-rejects-invalid-json
  (testing "POST /api/alpha/portfolio/heartbeat with bad JSON returns 400"
    (let [handler (make-handler)
          response (post handler "/api/alpha/portfolio/heartbeat" "{bad")]
      (is (= 400 (:status response)))
      (is (false? (:ok (parse-body response)))))))
