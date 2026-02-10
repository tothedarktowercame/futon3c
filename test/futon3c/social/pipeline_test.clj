(ns futon3c.social.pipeline-test
  "M-agency-refactor Part VI integration test.

   Wires: S-presence → S-authenticate → S-dispatch → S-persist
   Full end-to-end pipeline from connection to persisted session."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.registry :as reg]
            [futon3c.social.authenticate :as auth]
            [futon3c.social.dispatch :as dispatch]
            [futon3c.social.mode :as mode]
            [futon3c.social.persist :as persist]
            [futon3c.social.presence :as presence]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]))

;; Reset registry + sessions between tests
(use-fixtures
  :each
  (fn [f]
    (reg/reset-registry!)
    (persist/reset-sessions!)
    (f)))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- setup-agent!
  "Register a test agent and return its typed ID (continuity namespace)."
  [id-value]
  (let [typed-id (fix/make-agent-id id-value :continuity)]
    (reg/register-agent!
     {:agent-id typed-id
      :type :claude
      :invoke-fn (fn [_prompt _session-id] {:result "ok" :session-id nil :exit-code 0})
      :capabilities [:explore :edit :test]})
    typed-id))

;; =============================================================================
;; Full pipeline: S-presence → S-authenticate → S-dispatch → S-persist
;; =============================================================================

(deftest pipeline-connection-to-persist
  (testing "connection → presence → identity → classify → dispatch → persist end-to-end"
    (let [registry (fix/mock-registry)
          patterns (fix/mock-patterns)
          agent-id (setup-agent! "claude-1")

          ;; Step 1: S-presence
          connection (fix/make-connection
                      {:conn/agent-id (fix/make-agent-id "claude-1" :transport)
                       :conn/metadata {:ready true}})
          presence-result (presence/verify connection registry)]
      (fix/assert-valid! shapes/PresenceRecord presence-result)
      (is (true? (:presence/ready? presence-result)))

      ;; Step 2: S-authenticate
      (let [identity-result (auth/resolve-identity presence-result registry)]
        (fix/assert-valid! shapes/AgentIdentity identity-result)
        (is (= :continuity (get-in identity-result [:identity/agent-id :id/type])))

        ;; Step 3: S-mode
        (let [message {:msg/id "msg-pipeline-1"
                       :msg/payload {:type "standup"}
                       :msg/from (:identity/agent-id identity-result)
                       :msg/to agent-id
                       :msg/at (fix/now-str)}
              classified (mode/classify message patterns)]
          (fix/assert-valid! shapes/ClassifiedMessage classified)
          (is (= :coordination (:msg/mode classified)))

          ;; Step 4: S-dispatch
          (let [receipt (dispatch/dispatch classified registry)]
            (fix/assert-valid! shapes/DispatchReceipt receipt)
            (is (= "msg-pipeline-1" (:receipt/msg-id receipt)))
            (is (= agent-id (:receipt/to receipt)))
            (is (true? (:receipt/delivered? receipt)))

            ;; Step 5: S-persist
            (let [persist-result (persist/persist-session! receipt {:session/id "sess-pipeline-1"})
                  session (:session persist-result)]
              (is (true? (:ok persist-result)))
              (fix/assert-valid! shapes/SessionRecord session)
              (is (= "sess-pipeline-1" (:session/id session)))
              (is (= agent-id (:session/agent-id session)))

              ;; Verify retrieval round-trip
              (let [retrieved (persist/get-session "sess-pipeline-1")]
                (fix/assert-valid! shapes/SessionRecord retrieved)
                (is (= session retrieved))))))))))

(deftest pipeline-unknown-agent-fails-loudly
  (testing "unknown agent produces SocialError at the correct pipeline stage"
    (let [registry (fix/mock-registry)
          connection (fix/make-connection
                      {:conn/agent-id (fix/make-agent-id "ghost" :transport)
                       :conn/metadata {:ready true}})]
      (let [result (presence/verify connection registry)]
        (fix/assert-valid! shapes/SocialError result)
        (is (= :agent-not-found (:error/code result)))))))

(deftest pipeline-no-readiness-fails-at-presence
  (testing "connection without readiness handshake fails at S-presence"
    (let [registry (fix/mock-registry)
          connection (fix/make-connection
                      {:conn/agent-id (fix/make-agent-id "claude-1" :transport)})]
      (let [result (presence/verify connection registry)]
        (fix/assert-valid! shapes/SocialError result)
        (is (= :not-ready (:error/code result)))))))

(deftest pipeline-shapes-validated-at-every-boundary
  (testing "every pipeline output conforms to its declared shape"
    (let [registry (fix/mock-registry)
          patterns (fix/mock-patterns)
          _ (setup-agent! "claude-1")
          connection (fix/make-connection
                      {:conn/agent-id (fix/make-agent-id "claude-1" :transport)
                       :conn/metadata {:ready true}})
          presence-result (presence/verify connection registry)
          identity-result (auth/resolve-identity presence-result registry)
          message {:msg/id "msg-shape-check"
                   :msg/payload "check shapes"
                   :msg/from (:identity/agent-id identity-result)
                   :msg/to (fix/make-agent-id "claude-1" :continuity)
                   :msg/at (fix/now-str)}
          classified (mode/classify message patterns)
          receipt (dispatch/dispatch classified registry)
          persist-result (persist/persist-session! receipt {:session/id "sess-shape-check"})]
      ;; Every boundary output validates
      (is (shapes/valid? shapes/PresenceRecord presence-result) "presence output valid")
      (is (shapes/valid? shapes/AgentIdentity identity-result) "identity output valid")
      (is (shapes/valid? shapes/ClassifiedMessage classified) "classified output valid")
      (is (shapes/valid? shapes/DispatchReceipt receipt) "receipt output valid")
      (is (shapes/valid? shapes/SessionRecord (:session persist-result)) "session output valid"))))

(deftest pipeline-action-message-dispatches-and-persists
  (testing "action message flows through full pipeline to persisted session"
    (let [registry (fix/mock-registry)
          patterns (fix/mock-patterns)
          agent-id (setup-agent! "claude-1")
          connection (fix/make-connection
                      {:conn/agent-id (fix/make-agent-id "claude-1" :transport)
                       :conn/metadata {:ready true}})
          presence-result (presence/verify connection registry)
          identity-result (auth/resolve-identity presence-result registry)
          message {:msg/id "msg-action-1"
                   :msg/payload "run the tests"
                   :msg/from (:identity/agent-id identity-result)
                   :msg/to agent-id
                   :msg/at (fix/now-str)}
          classified (mode/classify message patterns)
          receipt (dispatch/dispatch classified registry)]
      (is (= :action (:msg/mode classified)))
      (fix/assert-valid! shapes/DispatchReceipt receipt)
      (is (true? (:receipt/delivered? receipt)))

      ;; Persist and verify
      (let [persist-result (persist/persist-session! receipt {:session/id "sess-action-1"})
            session (:session persist-result)]
        (is (true? (:ok persist-result)))
        (fix/assert-valid! shapes/SessionRecord session)
        (is (= agent-id (:session/agent-id session)))))))

;; =============================================================================
;; S-persist integration: update + retrieval within pipeline context
;; =============================================================================

(deftest pipeline-persist-update-round-trip
  (testing "persisted session can be updated and retrieved"
    (let [registry (fix/mock-registry)
          patterns (fix/mock-patterns)
          agent-id (setup-agent! "claude-1")
          connection (fix/make-connection
                      {:conn/agent-id (fix/make-agent-id "claude-1" :transport)
                       :conn/metadata {:ready true}})
          presence-result (presence/verify connection registry)
          identity-result (auth/resolve-identity presence-result registry)
          message {:msg/id "msg-update-1"
                   :msg/payload {:type "standup"}
                   :msg/from (:identity/agent-id identity-result)
                   :msg/to agent-id
                   :msg/at (fix/now-str)}
          classified (mode/classify message patterns)
          receipt (dispatch/dispatch classified registry)]
      ;; Persist initial session
      (persist/persist-session! receipt {:session/id "sess-update-1" :status :pending})

      ;; Update session state
      (let [update-result (persist/update-session! "sess-update-1" {:status :running :step 1})]
        (is (true? (:ok update-result)))
        (fix/assert-valid! shapes/SessionRecord (:session update-result))

        ;; R8: authoritative transcript has 2 events (initial + update)
        (let [session (:session update-result)]
          (is (= 2 (count (get-in session [:session/state :events]))))
          (is (= :running (get-in session [:session/state :data :status]))))))))

(deftest pipeline-persist-undelivered-receipt-rejected
  (testing "persist rejects dispatch receipt that was not delivered"
    (let [bad-receipt {:receipt/msg-id "msg-bad"
                       :receipt/to (fix/make-agent-id "claude-1" :continuity)
                       :receipt/delivered? false
                       :receipt/at (fix/now-str)}
          result (persist/persist-session! bad-receipt {:session/id "sess-bad"})]
      (fix/assert-valid! shapes/SocialError result)
      (is (= :dispatch-not-delivered (:error/code result))))))

(deftest pipeline-list-sessions-after-pipeline
  (testing "list-sessions returns sessions persisted through the pipeline"
    (let [registry (fix/mock-registry)
          patterns (fix/mock-patterns)
          a1 (setup-agent! "claude-1")
          a2 (setup-agent! "codex-1")
          ;; Run pipeline for agent 1
          c1 (fix/make-connection {:conn/agent-id (fix/make-agent-id "claude-1" :transport)
                                   :conn/metadata {:ready true}})
          p1 (presence/verify c1 registry)
          i1 (auth/resolve-identity p1 registry)
          m1 {:msg/id "m1" :msg/payload "hello" :msg/from (:identity/agent-id i1) :msg/to a1 :msg/at (fix/now-str)}
          cl1 (mode/classify m1 patterns)
          r1 (dispatch/dispatch cl1 registry)
          ;; Run pipeline for agent 2
          c2 (fix/make-connection {:conn/agent-id (fix/make-agent-id "codex-1" :transport)
                                   :conn/metadata {:ready true}})
          p2 (presence/verify c2 registry)
          i2 (auth/resolve-identity p2 registry)
          m2 {:msg/id "m2" :msg/payload "task" :msg/from (:identity/agent-id i2) :msg/to a2 :msg/at (fix/now-str)}
          cl2 (mode/classify m2 patterns)
          r2 (dispatch/dispatch cl2 registry)]
      ;; Persist both
      (persist/persist-session! r1 {:session/id "sess-c1"})
      (persist/persist-session! r2 {:session/id "sess-x1"})
      ;; List all
      (is (= 2 (count (persist/list-sessions {}))))
      ;; Filter by agent
      (let [only-a1 (persist/list-sessions {:agent-id a1})]
        (is (= 1 (count only-a1)))
        (is (= a1 (:session/agent-id (first only-a1))))))))
