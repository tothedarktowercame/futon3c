(ns futon3c.agency.registry-test
  "Registry unit tests — ported from futon3 + new R1-R11 compliance tests.

   Original tests from futon3: timeout enforcement, no-resurrect-after-unregister.
   New tests: single-routing-authority (R2), concurrent register/unregister (R3),
   invoke-missing → SocialError (R4), bounded-lifecycle/TTL (R5),
   typed-identifiers (R6)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [cheshire.core :as json]
            [futon3c.agency.registry :as reg]
            [futon3c.blackboard]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.transport.ws.invoke :as ws-invoke]))

(use-fixtures
  :each
  (fn [f]
    (reg/reset-registry!)
    ;; Suppress live-file pollution during tests: hop! / hop-back!
    ;; would otherwise append entries to pilot-inhabitations.edn.
    (binding [reg/*enable-hop-event-emission?* false]
      (f))))

;; =============================================================================
;; Ported from futon3: timeout enforcement
;; =============================================================================

(deftest invoke-timeout-is-enforced
  (testing "registry-level timeout returns promptly and reports timeout"
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "t-timeout")
      :type :codex
      :invoke-fn (fn [_prompt _session-id]
                   (Thread/sleep 200)
                   {:result "late" :session-id nil :exit-code 0})
      :capabilities [:edit]})
    (let [resp (reg/invoke-agent! (fix/make-agent-id "t-timeout") "hi" 50)]
      (is (false? (:ok resp)))
      (is (= :invoke-error (:error/code (:error resp)))))))

;; =============================================================================
;; Ported from futon3: no resurrect after unregister
;; =============================================================================

(deftest invoke-does-not-resurrect-after-unregister
  (testing "invoke does not re-add agent record if it is unregistered mid-call"
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "t-race")
      :type :codex
      :invoke-fn (fn [_prompt _session-id]
                   (Thread/sleep 200)
                   {:result "done" :session-id nil :exit-code 0})
      :capabilities [:edit]})
    (let [f (future (reg/invoke-agent! (fix/make-agent-id "t-race") "hi" 2000))]
      (Thread/sleep 50)
      (is (true? (:ok (reg/unregister-agent! (fix/make-agent-id "t-race")))))
      (deref f 3000 :timeout)
      (is (false? (reg/agent-registered? (fix/make-agent-id "t-race")))))))

;; =============================================================================
;; R2: Single routing authority
;; =============================================================================

(deftest single-routing-authority
  (testing "register same agent-id twice → error, not overwrite"
    (let [aid (fix/make-agent-id "dual")
          inv-fn (fn [_p _s] {:result "ok"})]
      (let [r1 (reg/register-agent! {:agent-id aid :type :codex :invoke-fn inv-fn :capabilities [:edit]})]
        (is (contains? r1 :agent/id) "first registration succeeds"))
      (let [r2 (reg/register-agent! {:agent-id aid :type :codex :invoke-fn inv-fn :capabilities [:edit]})]
        (is (= false (:ok r2)) "second registration fails")
        (is (= :duplicate-registration (:error/code (:error r2))))))))

;; =============================================================================
;; R3: Atomic state transitions
;; =============================================================================

(deftest concurrent-register-unregister
  (testing "concurrent register/unregister race doesn't corrupt registry"
    (let [aids (mapv #(fix/make-agent-id (str "race-" %) :continuity) (range 20))
          inv-fn (fn [_p _s] {:result "ok"})
          reg-futs (mapv (fn [aid]
                           (future
                             (reg/register-agent!
                              {:agent-id aid :type :mock :invoke-fn inv-fn :capabilities []})))
                         aids)]
      (doseq [f reg-futs] @f)
      (is (= 20 (:count (reg/registry-status))))
      ;; Unregister odd-indexed in parallel
      (let [unreg-futs (mapv (fn [i]
                               (future
                                 (reg/unregister-agent! (nth aids i))))
                             (filter odd? (range 20)))]
        (doseq [f unreg-futs] @f))
      (is (= 10 (:count (reg/registry-status)))))))

;; =============================================================================
;; R4: Loud failure
;; =============================================================================

(deftest invoke-missing-agent-returns-social-error
  (testing "invoke of missing agent returns SocialError map"
    (let [result (reg/invoke-agent! (fix/make-agent-id "ghost") "hello")]
      (is (= false (:ok result)))
      (is (= :registry (:error/component (:error result))))
      (is (= :agent-not-found (:error/code (:error result))))
      (is (shapes/valid? shapes/SocialError (:error result))))))

(deftest invoke-falls-back-to-ws
  (testing "invoke-agent! uses WS bridge when no invoke-fn"
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "codex-ws")
      :type :codex
      :invoke-fn nil
      :capabilities []})
    (with-redefs [ws-invoke/available? (constantly true)
                  ws-invoke/invoke! (fn [_ prompt _ _]
                                      {:result (str prompt " ack")
                                       :session-id "sess-ws"})]
      (let [result (reg/invoke-agent! (fix/make-agent-id "codex-ws") "hi" 1000)]
        (is (:ok result))
        (is (= "hi ack" (:result result)))
        (is (= "sess-ws" (:session-id result)))))))

(deftest invoke-falls-back-to-ws-with-invoke-meta
  (testing "invoke-agent! surfaces invoke-meta from WS invoke_result payload"
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "codex-ws-meta")
      :type :codex
      :invoke-fn nil
      :capabilities []})
    (with-redefs [ws-invoke/available? (constantly true)
                  ws-invoke/invoke! (fn [_ _ _ _]
                                      {:result "ok"
                                       :session-id "sess-ws-meta"
                                       :invoke-trace-id "invoke-ws-123"
                                       :execution {:executed? true
                                                   :tool-events 1
                                                   :command-events 0}})]
      (let [result (reg/invoke-agent! (fix/make-agent-id "codex-ws-meta") "hi" 1000)]
        (is (:ok result))
        (is (= "sess-ws-meta" (:session-id result)))
        (is (= {:invoke-trace-id "invoke-ws-123"
                :execution {:executed? true
                            :tool-events 1
                            :command-events 0}}
               (:invoke-meta result)))))))

(deftest invoke-surfaces-invoke-meta
  (testing "invoke-agent! returns invoke-meta when invoke-fn includes extra runtime data"
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "codex-meta")
      :type :codex
      :invoke-fn (fn [_prompt _session-id]
                   {:result "ok"
                    :session-id "sess-meta"
                    :execution {:executed? true
                                :tool-events 2
                                :command-events 1}})
      :capabilities [:edit]})
    (let [result (reg/invoke-agent! (fix/make-agent-id "codex-meta") "hi" 1000)]
      (is (:ok result))
      (is (= "ok" (:result result)))
      (is (= "sess-meta" (:session-id result)))
      (is (= {:execution {:executed? true
                          :tool-events 2
                          :command-events 1}}
             (:invoke-meta result))))))

(deftest invoke-exception-returns-social-error
  (testing "invoke-fn that throws returns SocialError, not nil"
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "boom")
      :type :mock
      :invoke-fn (fn [_p _s] (throw (ex-info "kaboom" {:reason :test})))
      :capabilities []})
    (let [result (reg/invoke-agent! (fix/make-agent-id "boom") "hello")]
      (is (= false (:ok result)))
      (is (= :invoke-exception (:error/code (:error result))))
      (is (shapes/valid? shapes/SocialError (:error result))))))

(deftest reset-session-clears-backing-continuity
  (testing "reset-session! clears registry session plus backing file/atom"
    (let [session-file (java.io.File/createTempFile "futon3c-reset-session-" ".sid")
          sid-atom (atom "sess-reset")
          reset-fn (fn []
                     (reset! sid-atom nil)
                     (when (.exists session-file)
                       (.delete session-file))
                     {:ok true})]
      (try
        (spit session-file "sess-reset")
        (reg/register-agent!
         {:agent-id (fix/make-agent-id "reset-me")
          :type :codex
          :invoke-fn (fn [_ _] {:result "ok"})
          :capabilities [:edit]
          :session-id "sess-reset"
          :session-reset-fn reset-fn})
        (let [result (reg/reset-session! (fix/make-agent-id "reset-me"))]
          (is (:ok result))
          (is (= "sess-reset" (:old-session-id result)))
          (is (nil? @sid-atom))
          (is (false? (.exists session-file)))
          (is (nil? (:session-id (get-in (reg/registry-status) [:agents "reset-me"])))))
        (finally
          (when (.exists session-file)
            (.delete session-file)))))))

(deftest reset-session-fails-loudly-when-backing-reset-fails
  (testing "reset-session! preserves continuity when backing reset fails"
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "reset-fail")
      :type :codex
      :invoke-fn (fn [_ _] {:result "ok"})
      :capabilities [:edit]
      :session-id "sess-still-live"
      :session-reset-fn (fn [] {:ok false :error "cannot clear backing continuity"})})
    (let [result (reg/reset-session! (fix/make-agent-id "reset-fail"))]
      (is (= false (:ok result)))
      (is (= "sess-still-live"
             (:session-id (get-in (reg/registry-status) [:agents "reset-fail"])))))))

(deftest unregister-missing-returns-social-error
  (testing "unregister of missing agent returns SocialError"
    (let [result (reg/unregister-agent! (fix/make-agent-id "nobody"))]
      (is (= false (:ok result)))
      (is (= :agent-not-found (:error/code (:error result))))
      (is (shapes/valid? shapes/SocialError (:error result))))))

(deftest deregister-agent-removes-from-registry
  (testing "register, verify present, deregister, verify gone"
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "remove-me")
      :type :mock
      :invoke-fn (fn [_p _s] {:result "ok"})
      :capabilities []})
    (is (true? (reg/agent-registered? (fix/make-agent-id "remove-me"))))
    (is (= {:ok true :agent-id "remove-me"}
           (reg/deregister-agent! "remove-me")))
    (is (false? (reg/agent-registered? (fix/make-agent-id "remove-me"))))
    (is (= 0 (:count (reg/registry-status))))))

(deftest deregister-unknown-agent-returns-not-found
  (testing "deregistering unknown agent returns {:ok false :error \"not-found\"}"
    (is (= {:ok false :error "not-found"}
           (reg/deregister-agent! "ghost-agent")))
    (is (= 0 (:count (reg/registry-status))))))

;; =============================================================================
;; R5: Bounded lifecycle
;; =============================================================================

(deftest agent-with-ttl-gets-reaped
  (testing "agent registered with TTL is removed after expiry"
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "ephemeral")
      :type :mock
      :invoke-fn (fn [_p _s] {:result "ok"})
      :capabilities []
      :ttl-ms 100})
    (is (some? (reg/get-agent (fix/make-agent-id "ephemeral"))))
    (Thread/sleep 200)
    (let [reaped (reg/reap-expired!)]
      (is (= 1 (count reaped)))
      (is (nil? (reg/get-agent (fix/make-agent-id "ephemeral")))))))

(deftest agent-without-ttl-not-reaped
  (testing "agent without TTL is not affected by reap"
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "permanent")
      :type :claude
      :invoke-fn (fn [_p _s] {:result "ok"})
      :capabilities [:explore]})
    (Thread/sleep 50)
    (let [reaped (reg/reap-expired!)]
      (is (empty? reaped))
      (is (some? (reg/get-agent (fix/make-agent-id "permanent")))))))

;; =============================================================================
;; R6: Typed identifiers
;; =============================================================================

(deftest agent-id-is-typed-map
  (testing "registered agent has TypedAgentId"
    (let [aid (fix/make-agent-id "typed-agent" :continuity)
          result (reg/register-agent!
                  {:agent-id aid :type :claude
                   :invoke-fn (fn [_p _s] {:result "ok"})
                   :capabilities [:explore :edit]})]
      (is (shapes/valid? shapes/TypedAgentId (:agent/id result)))
      (is (= :continuity (:id/type (:agent/id result)))))))

;; =============================================================================
;; Introspection
;; =============================================================================

(deftest registry-status-reports-count
  (testing "registry-status reflects actual agent count"
    (is (= 0 (:count (reg/registry-status))))
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "stat-1")
      :type :mock
      :invoke-fn (fn [_p _s] {:result "ok"})
      :capabilities []})
    (is (= 1 (:count (reg/registry-status))))))

(deftest registry-status-detects-external-codex-invocation
  (testing "codex agent marked invoking when matching external codex process session is active"
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "codex-ext")
      :type :codex
      :invoke-fn nil
      :capabilities [:edit]
      :session-id "019c91b2-fa9f-7080-a443-e7c882046a3c"})
    (with-redefs [reg/running-codex-session-ids
                  (constantly #{"019c91b2-fa9f-7080-a443-e7c882046a3c"})]
      (let [info (get-in (reg/registry-status) [:agents "codex-ext"])]
        (is (= :invoking (:status info)))
        (is (= "[external invoke]" (:invoke-prompt-preview info)))
        (is (= "codex exec running (external surface)"
               (:invoke-activity info)))))))

(deftest registry-status-surfaces-explicit-external-invoke-state
  (testing "explicit external invoke heartbeats mark an agent invoking even without resume process detection"
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "codex-repl")
      :type :codex
      :invoke-fn nil
      :capabilities [:edit]})
    (with-redefs [reg/running-codex-session-ids (constantly #{})
                  futon3c.transport.ws.invoke/connected-agent-ids (constantly [])
                  futon3c.blackboard/project-agents! (fn [_] nil)]
      (reg/report-external-invoke!
       "codex-repl"
       "emacs-codex-repl"
       {:status :invoking
        :session-id "019cd4ad-c5b9-76c0-af08-50d8af0803c7"
        :prompt-preview "Hi Codex please recover"
        :activity "Command Execution"})
      (let [info (get-in (reg/registry-status) [:agents "codex-repl"])]
        (is (= :invoking (:status info)))
        (is (= "019cd4ad-c5b9-76c0-af08-50d8af0803c7" (:session-id info)))
        (is (= "Hi Codex please recover" (:invoke-prompt-preview info)))
        (is (= "Command Execution" (:invoke-activity info))))
      (reg/clear-external-invoke! "codex-repl" "emacs-codex-repl")
      (is (= :idle (get-in (reg/registry-status) [:agents "codex-repl" :status]))))))

(deftest external-invoke-heartbeat-announces-uplink-roster
  (testing "externally driven REPL invokes publish to the federation uplink promptly"
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "codex-uplink")
      :type :codex
      :invoke-fn nil
      :capabilities [:edit]})
    (let [announced (promise)]
      (with-redefs [reg/running-codex-session-ids (constantly #{})
                    futon3c.transport.ws.invoke/connected-agent-ids (constantly [])
                    futon3c.blackboard/project-agents! (fn [_] nil)]
        (binding [reg/*resolve-uplink-announce*
                  (fn [] (fn [] (deliver announced :announced)))]
          (reg/report-external-invoke!
           "codex-uplink"
           "emacs-codex-repl"
           {:status :invoking
            :prompt-preview "active prompt"})))
      (is (= :announced (deref announced 1000 :timeout))))))

(deftest agents-status-broadcast-advertises-invoke-pattern
  (testing "WS agents_status carries the same invoke preview fields as registry-status"
    (let [sent (promise)]
      (try
        (reg/register-agent!
         {:agent-id (fix/make-agent-id "codex-broadcast")
          :type :codex
          :invoke-fn nil
          :capabilities [:edit]})
        (ws-invoke/register! "registry-test-hud" #(deliver sent %) {:observer? true})
        (with-redefs [reg/running-codex-session-ids (constantly #{})
                      futon3c.blackboard/project-agents! (fn [_] nil)]
          (reg/report-external-invoke!
           "codex-broadcast"
           "emacs-codex-repl"
           {:status :invoking
            :prompt-preview "--- CURRENT TURN ---\nSurface: emacs-repl\nCaller: joe"
            :activity "using bash"}))
        (let [frame (json/parse-string (deref sent 1000 "{}") true)
              agent (get-in frame [:agents :codex-broadcast])]
          (is (= "agents_status" (:type frame)))
          (is (= "invoking" (:status agent)))
          (is (= "using bash" (:invoke-activity agent)))
          (is (= "--- CURRENT TURN ---\nSurface: emacs-repl\nCaller: joe"
                 (:invoke-prompt-preview agent)))
          (is (string? (:invoke-started-at agent))))
        (finally
          (ws-invoke/unregister! "registry-test-hud"))))))

(deftest registry-status-treats-heartbeating-codex-as-idle-when-clear
  (testing "codex agent stays idle after clear even if resume process is running"
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "codex-heartbeat")
      :type :codex
      :invoke-fn nil
      :session-id "019cd4ad-c5b9-76c0-af08-50d8af0803c7"
      :capabilities [:edit]})
    (with-redefs [reg/running-codex-session-ids
                  (constantly #{"019cd4ad-c5b9-76c0-af08-50d8af0803c7"})
                  futon3c.transport.ws.invoke/connected-agent-ids (constantly [])
                  futon3c.blackboard/project-agents! (fn [_] nil)]
      ;; Start heartbeat, then clear as if invoke finished
      (reg/report-external-invoke!
       "codex-heartbeat"
       "emacs-codex-repl"
       {:status :invoking
        :session-id "019cd4ad-c5b9-76c0-af08-50d8af0803c7"
        :prompt-preview "doing work"})
      (reg/clear-external-invoke! "codex-heartbeat" "emacs-codex-repl")
      (let [info (get-in (reg/registry-status) [:agents "codex-heartbeat"])]
        (is (= :idle (:status info)))
        (is (= "019cd4ad-c5b9-76c0-af08-50d8af0803c7" (:session-id info)))))))

(deftest registry-status-surfaces-live-surface-projection
  (testing "live surface projections are queryable and exposed in registry-status"
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "codex-surface")
      :type :codex
      :invoke-fn nil
      :capabilities [:edit]})
    (with-redefs [reg/running-codex-session-ids (constantly #{})
                  futon3c.transport.ws.invoke/connected-agent-ids (constantly [])
                  futon3c.blackboard/project-agents! (fn [_] nil)]
      (reg/report-surface-projection!
       "codex-surface"
       "emacs-cursor:editor-main"
       {:surface "emacs-cursor"
        :editor-id "editor-main"
        :mode "follow"
        :buffer-summary "buffer=foo.clj user=(line 7 col 2 point 101) remote=nil"
        :write-surface "minibuffer"})
      (let [projection (reg/current-surface-projection "codex-surface")
            info (get-in (reg/registry-status) [:agents "codex-surface"])]
        (is (= "emacs-cursor" (:surface projection)))
        (is (= "editor-main" (:editor-id projection)))
        (is (= "buffer=foo.clj user=(line 7 col 2 point 101) remote=nil"
               (:buffer-summary projection)))
        (is (= {:source "emacs-cursor:editor-main"
                :surface "emacs-cursor"
                :editor-id "editor-main"
                :mode "follow"
                :buffer-summary "buffer=foo.clj user=(line 7 col 2 point 101) remote=nil"
                :write-surface "minibuffer"}
               (:surface-projection info))))
      (reg/clear-surface-projection! "codex-surface" "emacs-cursor:editor-main")
      (is (nil? (reg/current-surface-projection "codex-surface")))
      (is (nil? (get-in (reg/registry-status) [:agents "codex-surface" :surface-projection]))))))

(deftest registry-status-includes-ws-connected-unregistered
  (testing "registry-status surfaces ws-connected agent ids that are not registered locally"
    (with-redefs [reg/running-codex-session-ids (constantly #{})
                  futon3c.transport.ws.invoke/connected-agent-ids
                  (constantly ["claude-remote" "codex-1"])]
      (reg/register-agent!
       {:agent-id (fix/make-agent-id "codex-1")
        :type :codex
        :invoke-fn nil
        :capabilities [:edit]})
      (let [status (reg/registry-status)]
        (is (= ["claude-remote" "codex-1"] (:ws-connected status)))
        (is (= ["claude-remote"] (:ws-unregistered status)))))))

(deftest registry-status-merges-canonical-queued-job-counts
  (testing "registry-status surfaces queued work from the invoke ledger"
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "codex-queued")
      :type :codex
      :invoke-fn (fn [_ _] {:result "ok"})
      :capabilities [:edit]})
    (with-redefs [reg/running-codex-session-ids (constantly #{})
                  futon3c.transport.ws.invoke/connected-agent-ids (constantly [])
                  reg/*resolve-invoke-job-counts*
                  (fn []
                    (fn []
                      {"codex-queued" {:queued-jobs 1
                                       :running-jobs 0
                                       :nonterminal-jobs 1}}))]
      (let [info (get-in (reg/registry-status) [:agents "codex-queued"])]
        (is (= :idle (:status info)))
        (is (= 1 (:queued-jobs info)))
        (is (= 0 (:running-jobs info)))
        (is (= 1 (:nonterminal-jobs info)))))))

(deftest registry-status-promotes-running-jobs-to-invoking
  (testing "running jobs mark an agent invoking even if its base registry status is idle"
    (reg/register-agent!
     {:agent-id (fix/make-agent-id "codex-running")
      :type :codex
      :invoke-fn nil
      :capabilities [:edit]})
    (with-redefs [reg/running-codex-session-ids (constantly #{})
                  futon3c.transport.ws.invoke/connected-agent-ids (constantly [])
                  reg/*resolve-invoke-job-counts*
                  (fn []
                    (fn []
                      {"codex-running" {:queued-jobs 0
                                        :running-jobs 1
                                        :nonterminal-jobs 1}}))]
      (let [info (get-in (reg/registry-status) [:agents "codex-running"])]
        (is (= :invoking (:status info)))
        (is (= 1 (:running-jobs info)))
        (is (= 1 (:nonterminal-jobs info)))))))

(deftest shutdown-all-clears-registry
  (testing "shutdown-all! removes all agents"
    (doseq [i (range 5)]
      (reg/register-agent!
       {:agent-id (fix/make-agent-id (str "shutdown-" i))
        :type :mock
        :invoke-fn (fn [_p _s] {:result "ok"})
        :capabilities []}))
    (is (= 5 (:count (reg/registry-status))))
    (let [n (reg/shutdown-all!)]
      (is (= 5 n))
      (is (= 0 (:count (reg/registry-status)))))))

;; =============================================================================
;; E-pilot-hop-trigger-wiring tests
;; =============================================================================

(defn- register-agent-mock! [id]
  (reg/register-agent!
   {:agent-id (fix/make-agent-id id)
    :type :claude
    :invoke-fn (fn [_p _s] {:result "ok"})
    :capabilities []}))

(defn- register-peripheral-mock! [id]
  (reg/register-agent!
   {:agent-id (fix/make-agent-id id)
    :type :peripheral
    :invoke-fn (fn [_p _s] {:result "ok"})
    :capabilities []}))

(deftest hop-into-vacant-peripheral
  (testing "Agent can hop into a vacant peripheral; bidirectional pointers set"
    (register-agent-mock! "claude-1")
    (register-peripheral-mock! "street-sweeper")
    (let [r (reg/hop! "claude-1" "street-sweeper")]
      (is (:ok r))
      (is (= "street-sweeper" (:to r)))
      (is (nil? (:from r)))
      (is (= "street-sweeper" (reg/current-peripheral "claude-1")))
      (is (= "claude-1" (reg/current-inhabitant "street-sweeper")))
      (is (= ["claude-1"] (vec (map :agent/current-inhabitant
                                    [(reg/get-agent "street-sweeper")])))))))

(deftest hop-pushes-prev-onto-stack
  (testing "Hop pushes prev peripheral onto agent's hop-stack"
    (register-agent-mock! "claude-1")
    (register-peripheral-mock! "war-machine-pilot")
    (register-peripheral-mock! "street-sweeper")
    (reg/hop! "claude-1" "war-machine-pilot")
    (let [r (reg/hop! "claude-1" "street-sweeper")]
      (is (:ok r))
      (is (= "war-machine-pilot" (:from r)))
      (is (= "street-sweeper" (:to r)))
      (is (= ["war-machine-pilot"] (reg/hop-stack "claude-1")))
      (is (= "street-sweeper" (reg/current-peripheral "claude-1")))
      (is (= "claude-1" (reg/current-inhabitant "street-sweeper")))
      (is (nil? (reg/current-inhabitant "war-machine-pilot"))
          "Prev peripheral's inhabitant should be cleared after hop"))))

(deftest hop-back-restores-prev
  (testing "Hop-back pops stack and restores prev inhabitation; pointers reverse"
    (register-agent-mock! "claude-1")
    (register-peripheral-mock! "war-machine-pilot")
    (register-peripheral-mock! "street-sweeper")
    (reg/hop! "claude-1" "war-machine-pilot")
    (reg/hop! "claude-1" "street-sweeper")
    (let [r (reg/hop-back! "claude-1")]
      (is (:ok r))
      (is (= "street-sweeper" (:from r)))
      (is (= "war-machine-pilot" (:to r)))
      (is (= "war-machine-pilot" (reg/current-peripheral "claude-1")))
      (is (= "claude-1" (reg/current-inhabitant "war-machine-pilot")))
      (is (nil? (reg/current-inhabitant "street-sweeper")))
      (is (empty? (reg/hop-stack "claude-1"))))))

(deftest hop-rejects-occupied-peripheral
  (testing "Foreign-hop-in is rejected when peripheral is occupied by another agent"
    (register-agent-mock! "claude-1")
    (register-agent-mock! "claude-2")
    (register-peripheral-mock! "street-sweeper")
    (reg/hop! "claude-1" "street-sweeper")
    (let [r (reg/hop! "claude-2" "street-sweeper")]
      (is (false? (:ok r)))
      (is (= :peripheral-occupied (:error r)))
      (is (= "claude-1" (:by r)))
      ;; Original inhabitant unchanged
      (is (= "claude-1" (reg/current-inhabitant "street-sweeper")))
      ;; Foreign agent didn't gain a current-peripheral
      (is (nil? (reg/current-peripheral "claude-2"))))))

(deftest hop-rejects-unregistered-agent
  (testing "Hop with unregistered agent fails loudly"
    (register-peripheral-mock! "street-sweeper")
    (let [r (reg/hop! "ghost" "street-sweeper")]
      (is (false? (:ok r)))
      (is (= :agent-not-registered (:error r))))))

(deftest hop-rejects-unregistered-peripheral
  (testing "Hop into nonexistent peripheral fails loudly"
    (register-agent-mock! "claude-1")
    (let [r (reg/hop! "claude-1" "no-such-peri")]
      (is (false? (:ok r)))
      (is (= :peripheral-not-registered (:error r))))))

(deftest hop-rejects-same-peripheral
  (testing "Hopping into the peripheral the agent is already in is rejected"
    (register-agent-mock! "claude-1")
    (register-peripheral-mock! "street-sweeper")
    (reg/hop! "claude-1" "street-sweeper")
    (let [r (reg/hop! "claude-1" "street-sweeper")]
      (is (false? (:ok r)))
      (is (= :hop-to-same-peripheral (:error r))))))

(deftest hop-back-rejects-empty-stack
  (testing "Hop-back with empty stack fails loudly"
    (register-agent-mock! "claude-1")
    (let [r (reg/hop-back! "claude-1")]
      (is (false? (:ok r)))
      (is (= :hop-stack-empty (:error r))))))

(deftest bidirectional-pointer-consistency-property
  (testing "After any sequence of hop! / hop-back!, agent.current-peripheral
            == peripheral.current-inhabitant invariant holds"
    (register-agent-mock! "claude-1")
    (register-peripheral-mock! "war-machine-pilot")
    (register-peripheral-mock! "street-sweeper")
    (register-peripheral-mock! "night-shift")
    (let [ops [(fn [] (reg/hop! "claude-1" "war-machine-pilot"))
               (fn [] (reg/hop! "claude-1" "street-sweeper"))
               (fn [] (reg/hop-back! "claude-1"))
               (fn [] (reg/hop! "claude-1" "night-shift"))
               (fn [] (reg/hop-back! "claude-1"))
               (fn [] (reg/hop-back! "claude-1"))]]
      (doseq [op ops]
        (op)
        (let [agent-peri (reg/current-peripheral "claude-1")]
          (if agent-peri
            (is (= "claude-1" (reg/current-inhabitant agent-peri))
                (str "After op, agent points at " agent-peri
                     " but " agent-peri " says inhabitant = "
                     (reg/current-inhabitant agent-peri)))
            ;; If agent is not in any peripheral, no peripheral should
            ;; claim them as inhabitant.
            (doseq [p ["war-machine-pilot" "street-sweeper" "night-shift"]]
              (is (not= "claude-1" (reg/current-inhabitant p))
                  (str "Agent is in NO peripheral but " p
                       " still claims them as inhabitant")))))))))

(deftest legal-self-rehop-blocked-as-noop
  (testing "An agent attempting to hop into its own current peripheral is
            rejected with :hop-to-same-peripheral (R4: loud failure, not silent no-op)"
    (register-agent-mock! "claude-1")
    (register-peripheral-mock! "war-machine-pilot")
    (reg/hop! "claude-1" "war-machine-pilot")
    (let [r (reg/hop! "claude-1" "war-machine-pilot")]
      (is (false? (:ok r))))))
