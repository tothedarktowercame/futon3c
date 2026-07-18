(ns futon3c.transport.execution-evidence-test
  "Tests for the false-negative fix in execution-evidence telemetry.

   Bug: claude-lane reviews that DID run tools were finalized as
   {:executed false :tool-events 0} because the claude invoke path does not
   self-report execution telemetry into :invoke-meta :execution (codex does).
   The stream recorder observed tool_use events on the same path but nothing
   counted them into the finalized evidence.

   Fix (two layers):
   1. SOURCE: enrich-result-with-stream-execution populates :invoke-meta
      :execution from recorded tool_use events before the gate check.
   2. FALLBACK: invoke-execution-evidence consults the ledger when
      self-reported evidence is absent/zero (may only UPGRADE)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [cheshire.core :as json]
            [clojure.string :as str]
            [futon3c.transport.http :as http]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.social.persist :as persist]
            [futon3c.evidence.store :as estore]
            [futon3c.agency.registry :as reg]
            [futon3c.agency.clock-store :as clock-store]
            [futon3c.portfolio.core :as portfolio]
            [futon3c.portfolio.perceive :as perceive]
            [futon3c.transport.encyclopedia :as enc]))

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
;; Private var accessors
;; =============================================================================

(def count-tool-use-events        #'http/count-tool-use-events)
(def execution-evidence-from-ledger #'http/execution-evidence-from-ledger)
(def invoke-execution-evidence    #'http/invoke-execution-evidence)
(def enrich-result-with-stream-execution #'http/enrich-result-with-stream-execution)

(def ^:private ledger-atom #'http/!invoke-jobs-ledger)

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

(defn- inject-job-with-events!
  "Put a fake job into the ledger with the given tool_use events."
  [job-id events]
  (swap! @ledger-atom
         (fn [ledger]
           (-> (or ledger {:version 1 :next-seq 0 :job-order [] :jobs {} :trace->job {}})
               (assoc-in [:jobs job-id]
                         {:job-id job-id
                          :agent-id "claude-test"
                          :state "done"
                          :events events
                          :event-seq (count events)})
               (update :job-order (fnil conj []) job-id)))))

(defn- tool-use-event
  ([tools] (tool-use-event tools nil))
  ([tools previews]
   (cond-> {:type "tool_use" :tools tools}
     previews (assoc :previews previews))))

;; =============================================================================
;; Unit tests: count-tool-use-events
;; =============================================================================

(deftest count-tool-use-events-empty
  (is (= {:tool-events 0 :command-events 0}
         (count-tool-use-events []))))

(deftest count-tool-use-events-mixed
  (testing "counts tool_use events, distinguishes Bash from non-Bash"
    (let [events [{:type "text" :text "hello"}
                  (tool-use-event ["Read" "Edit"])
                  (tool-use-event ["Bash"] ["Bash ls -la"])
                  {:type "prompt" :text "do thing"}
                  (tool-use-event ["Write"])]
          result (count-tool-use-events events)]
      (is (= 3 (:tool-events result)))
      (is (= 1 (:command-events result))))))

(deftest count-tool-use-events-bash-case-insensitive
  (testing "bash tool names are matched case-insensitively"
    (let [events [(tool-use-event ["bash"] ["bash rm -rf /tmp/x"])
                  (tool-use-event ["BASH"])]
          result (count-tool-use-events events)]
      (is (= 2 (:tool-events result)))
      (is (= 2 (:command-events result))))))

;; =============================================================================
;; Unit tests: execution-evidence-from-ledger
;; =============================================================================

(deftest ledger-evidence-nil-when-no-job
  (is (nil? (execution-evidence-from-ledger "nonexistent-job"))))

(deftest ledger-evidence-nil-when-no-tool-use
  (inject-job-with-events! "job-notools" [{:type "text" :text "hi"}])
  (is (nil? (execution-evidence-from-ledger "job-notools"))))

(deftest ledger-evidence-positive-with-tools
  (inject-job-with-events! "job-tools"
                           [(tool-use-event ["Read"])
                            (tool-use-event ["Bash"] ["Bash git status"])])
  (let [evidence (execution-evidence-from-ledger "job-tools")]
    (is (= {:executed true :tool-events 2 :command-events 1} evidence))))

;; =============================================================================
;; Unit tests: invoke-execution-evidence (with fallback)
;; =============================================================================

(deftest evidence-self-reported-positive-unchanged
  (testing "(b) result with invoke-meta execution populated is unchanged"
    (inject-job-with-events! "job-selfrep"
                             [(tool-use-event ["Bash"])])
    (let [result {:ok true
                  :result "done"
                  :invoke-meta {:execution {:executed? true
                                            :tool-events 5
                                            :command-events 3}}}
          evidence (invoke-execution-evidence result "job-selfrep")]
      (is (true? (:executed evidence)))
      (is (= 5 (:tool-events evidence)))
      (is (= 3 (:command-events evidence)))
      (is (not (= 1 (:tool-events evidence)))
          "fallback must not overwrite self-reported positive (no downgrade)"))))

(deftest evidence-absent-with-ledger-events-uses-fallback
  (testing "(a) claude-shaped result with no invoke-meta but recorded tool_use events"
    (inject-job-with-events! "job-fallback"
                             [(tool-use-event ["Read"])
                              (tool-use-event ["Bash"] ["Bash make test"])])
    (let [result {:ok true :result "review complete" :session-id "s1"}
          evidence (invoke-execution-evidence result "job-fallback")]
      (is (true? (:executed evidence)))
      (is (= 2 (:tool-events evidence)))
      (is (= 1 (:command-events evidence))))))

(deftest evidence-absent-no-events-stays-false
  (testing "(c) no invoke-meta AND no events -> executed=false as today"
    (let [result {:ok true :result "planning only" :session-id "s1"}
          evidence (invoke-execution-evidence result "job-empty")]
      (is (false? (:executed evidence)))
      (is (zero? (:tool-events evidence)))
      (is (zero? (:command-events evidence))))))

(deftest evidence-zero-self-report-with-ledger-upgrades
  (testing "self-report of zero with ledger events upgrades via fallback"
    (inject-job-with-events! "job-zero-upgrade"
                             [(tool-use-event ["Edit"])])
    (let [result {:ok true
                  :result "done"
                  :invoke-meta {:execution {:executed? false
                                            :tool-events 0
                                            :command-events 0}}}
          evidence (invoke-execution-evidence result "job-zero-upgrade")]
      (is (true? (:executed evidence)))
      (is (= 1 (:tool-events evidence))))))

(deftest evidence-command-events-counts-only-bash
  (testing "(d) command-events counts only Bash-ish tool_use payloads"
    (inject-job-with-events! "job-cmd"
                             [(tool-use-event ["Read"])
                              (tool-use-event ["bash"] ["bash ./run.sh"])
                              (tool-use-event ["Write"])])
    (let [result {:ok true :result "done"}
          evidence (invoke-execution-evidence result "job-cmd")]
      (is (= 3 (:tool-events evidence)))
      (is (= 1 (:command-events evidence))))))

;; =============================================================================
;; Unit tests: enrich-result-with-stream-execution
;; =============================================================================

(deftest enrich-populates-invoke-meta-when-missing
  (testing "enrich adds :invoke-meta :execution from ledger for claude-shaped result"
    (inject-job-with-events! "job-enrich"
                             [(tool-use-event ["Bash"] ["Bash clj-kondo src/"])])
    (let [result {:ok true :result "reviewed" :session-id "s1"}
          enriched (enrich-result-with-stream-execution "job-enrich" result)]
      (is (= true (get-in enriched [:invoke-meta :execution :executed])))
      (is (= 1 (get-in enriched [:invoke-meta :execution :tool-events])))
      (is (= 1 (get-in enriched [:invoke-meta :execution :command-events]))))))

(deftest enrich-does-not-downgrade-positive-self-report
  (testing "enrich leaves existing positive execution evidence untouched"
    (inject-job-with-events! "job-enrich-pos"
                             [(tool-use-event ["Bash"])])
    (let [result {:ok true
                  :result "done"
                  :invoke-meta {:execution {:executed? true
                                            :tool-events 7
                                            :command-events 2}}}
          enriched (enrich-result-with-stream-execution "job-enrich-pos" result)]
      (is (= 7 (get-in enriched [:invoke-meta :execution :tool-events])))
      (is (= 2 (get-in enriched [:invoke-meta :execution :command-events]))))))

(deftest enrich-noop-when-no-ledger-events
  (testing "enrich is a no-op when the ledger has no tool_use events"
    (let [result {:ok true :result "planning only"}
          enriched (enrich-result-with-stream-execution "job-enrich-empty" result)]
      (is (= result enriched)))))

;; =============================================================================
;; Integration test: bell path with claude-shaped result
;; =============================================================================

(defn- make-handler
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

(defn- parse-body [response]
  (when-let [body (:body response)]
    (when-not (str/blank? body)
      (try
        (json/parse-string body true)
        (catch Exception _ nil)))))

(defn- wait-for-job-state
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

(deftest bell-claude-review-with-tool-use-events-finalizes-executed
  (testing "review with tool_use stream events but no invoke-meta
             finalizes as executed=true (the false-negative fix)"
    (let [handler (make-handler)
          body (json/generate-string {"agent-id" "codex-review-1"
                                      "prompt" "Task assignment, mode: task — review this code"
                                      "mode" "work"})]
      ;; Register as codex-named agent to guarantee execution enforcement.
      ;; The false-negative: claude-lane results lack :invoke-meta :execution
      ;; (unlike codex), but DO record tool_use stream events. The fix
      ;; derives execution evidence from those events before the gate check.
      (register-mock-agent! "codex-review-1" :codex)
      (with-redefs [reg/invoke-agent! (fn [aid _prompt _timeout]
                                        ;; Simulate tool_use events landing in
                                        ;; the ledger via the event sink, as the
                                        ;; claude stream-json parser does.
                                        (when-let [sink (reg/get-invoke-event-sink aid)]
                                          (sink {:type "tool_use"
                                                 :tools ["Read"]
                                                 :tool_details [{:name "Read" :input {:path "src/core.clj"}}]})
                                          (sink {:type "tool_use"
                                                 :tools ["Bash"]
                                                 :tool_details [{:name "Bash"
                                                                 :input {:command "clj-kondo src/core.clj"}}]}))
                                        ;; Claude-shaped result: NO :invoke-meta :execution
                                        {:ok true
                                         :result "Reviewed the code. All looks good."
                                         :session-id "sess-claude-rev-1"})]
        (let [response (post handler "/api/alpha/bell" body)
              parsed (parse-body response)
              job-id (:job-id parsed)
              final* (wait-for-job-state handler job-id 5000)
              final (:parsed final*)
              job (:job final)
              execution (:execution job)]
          (is (= 202 (:status response)))
          (is (string? job-id))
          (is (= "done" (:state job))
              "honest review with tool evidence must NOT be marked no-execution-evidence")
          (is (true? (:executed execution))
              "execution evidence must show executed=true from stream fallback")
          (is (= 2 (:tool-events execution))
              "two tool_use events recorded")
          (is (= 1 (:command-events execution))
              "one of the two tool_use events was a Bash call"))))))

(deftest bell-claude-review-no-tools-no-events-still-fails
  (testing "review with no tools and no events still fails the gate
             (no false positives from the fallback)"
    (let [handler (make-handler)
          body (json/generate-string {"agent-id" "codex-review-2"
                                      "prompt" "Task assignment, mode: task — review this code"
                                      "mode" "work"})]
      ;; Register as codex-named agent to guarantee execution enforcement
      ;; (the fix is agent-type-agnostic; the false-negative happens for any
      ;; agent whose result lacks :invoke-meta :execution)
      (register-mock-agent! "codex-review-2" :codex)
      (with-redefs [reg/invoke-agent! (fn [_ _ _]
                                        {:ok true
                                         :result "I reviewed it mentally."
                                         :session-id "s2"})]
        (let [response (post handler "/api/alpha/bell" body)
              parsed (parse-body response)
              job-id (:job-id parsed)
              final* (wait-for-job-state handler job-id 5000)
              final (:parsed final*)
              job (:job final)]
          (is (= "failed" (:state job)))
          (is (= "no-execution-evidence" (:terminal-code job))
              "no tools + no events = still refused, no false positive"))))))
