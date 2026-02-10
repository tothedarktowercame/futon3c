(ns futon3c.social.mode-integration-test
  "M-peripheral-model Part IV integration test.

   Wires S-presence → S-authenticate → S-mode pipeline segment,
   validates peripheral hops end-to-end, and exercises the mode
   transition state machine."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.registry :as reg]
            [futon3c.social.authenticate :as auth]
            [futon3c.social.mode :as mode]
            [futon3c.social.peripheral :as periph]
            [futon3c.social.presence :as presence]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]))

;; Reset registry between tests
(use-fixtures
  :each
  (fn [f]
    (reg/reset-registry!)
    (f)))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- setup-agent!
  "Register a test agent and return its typed ID."
  [id-value]
  (let [typed-id (fix/make-agent-id id-value :continuity)]
    (reg/register-agent!
     {:agent-id typed-id
      :type :claude
      :invoke-fn (fn [_prompt _session-id] {:result "ok" :session-id nil :exit-code 0})
      :capabilities [:explore :edit :test]})
    typed-id))

;; =============================================================================
;; S-presence → S-authenticate → S-mode pipeline segment
;; =============================================================================

(deftest presence-to-authenticate-to-classify
  (testing "connection → presence → identity → classified message end-to-end"
    (let [registry (fix/mock-registry)
          patterns (fix/mock-patterns)
          agent-id (fix/make-agent-id "claude-1" :transport)

          ;; Step 1: S-presence
          connection (fix/make-connection
                      {:conn/agent-id agent-id
                       :conn/metadata {:ready true}})
          presence-result (presence/verify connection registry)]
      (fix/assert-valid! shapes/PresenceRecord presence-result)
      (is (true? (:presence/ready? presence-result)))

      ;; Step 2: S-authenticate
      (let [identity-result (auth/resolve-identity presence-result registry)]
        (fix/assert-valid! shapes/AgentIdentity identity-result)
        (is (= :continuity (get-in identity-result [:identity/agent-id :id/type]))
            "R6: transport ID resolved to continuity")
        (is (= :claude (:identity/type identity-result)))

        ;; Step 3: S-mode classify
        (let [message {:msg/id "msg-1"
                       :msg/payload {:type "standup"}
                       :msg/from (:identity/agent-id identity-result)
                       :msg/at (fix/now-str)}
              classified (mode/classify message patterns)]
          (fix/assert-valid! shapes/ClassifiedMessage classified)
          (is (= :coordination (:msg/mode classified)))
          (is (= "msg-1" (:msg/id classified))))))))

(deftest presence-to-authenticate-to-classify-action
  (testing "action message flows through the pipeline correctly"
    (let [registry (fix/mock-registry)
          patterns (fix/mock-patterns)
          connection (fix/make-connection
                      {:conn/agent-id (fix/make-agent-id "claude-1" :transport)
                       :conn/metadata {:ready true}})
          presence-result (presence/verify connection registry)
          identity-result (auth/resolve-identity presence-result registry)
          message {:msg/id "msg-2"
                   :msg/payload "run the tests please"
                   :msg/from (:identity/agent-id identity-result)
                   :msg/at (fix/now-str)}
          classified (mode/classify message patterns)]
      (fix/assert-valid! shapes/ClassifiedMessage classified)
      (is (= :action (:msg/mode classified))))))

;; =============================================================================
;; Mode transition state machine
;; =============================================================================

(deftest mode-transition-full-cycle
  (testing "DISCUSS → DIAGNOSE → EXECUTE → DISCUSS completes the full cycle"
    (let [t1 (mode/validate-transition :discuss :diagnose "claude-1")
          t2 (mode/validate-transition :diagnose :execute "claude-1"
                                       :approval-token "human-approved")
          t3 (mode/validate-transition :execute :discuss "claude-1"
                                       :summary "task complete")]
      (fix/assert-valid! shapes/ModeTransition t1)
      (fix/assert-valid! shapes/ModeTransition t2)
      (fix/assert-valid! shapes/ModeTransition t3)
      (is (= :discuss (:mode/from t1)))
      (is (= :diagnose (:mode/to t1)))
      (is (= "human-approved" (:mode/approval-token t2)))
      (is (= "task complete" (:mode/summary t3))))))

(deftest mode-transition-invalid-paths-blocked
  (testing "invalid mode transitions are prevented"
    ;; DISCUSS → EXECUTE (skip diagnose)
    (let [r1 (mode/validate-transition :discuss :execute "claude-1")]
      (fix/assert-valid! shapes/SocialError r1)
      (is (= :invalid-transition (:error/code r1))))
    ;; DIAGNOSE → EXECUTE without approval
    (let [r2 (mode/validate-transition :diagnose :execute "claude-1")]
      (fix/assert-valid! shapes/SocialError r2)
      (is (= :approval-required (:error/code r2))))
    ;; no-op
    (let [r3 (mode/validate-transition :discuss :discuss "claude-1")]
      (fix/assert-valid! shapes/SocialError r3)
      (is (= :invalid-transition (:error/code r3))))))

;; =============================================================================
;; Peripheral hop end-to-end
;; =============================================================================

(deftest peripheral-hop-explore-to-edit-preserves-session
  (testing "hop explore → edit preserves session-id and transfers context"
    (let [peripherals (periph/load-peripherals "resources/peripherals.edn")
          session-id "sess-integration-1"
          hop-req (fix/make-hop-request
                   {:hop/to :edit
                    :hop/reason "found target file"
                    :hop/session-id session-id})
          hop-result (periph/validate-hop peripherals :explore hop-req)]
      (fix/assert-valid! shapes/HopResult hop-result)
      (is (= :explore (:hop/from hop-result)))
      (is (= :edit (:hop/to hop-result)))
      (is (= session-id (:hop/session-id hop-result)))
      (is (true? (:hop/success? hop-result)))

      ;; Transfer context
      (let [edit-spec (periph/get-peripheral peripherals :edit)
            source-ctx {:session-id "old-sess"
                        :target-files ["src/futon3c/social/shapes.clj"]}
            transferred (periph/transfer-context hop-result source-ctx edit-spec)]
        (is (= session-id (:session-id transferred))
            "session-id comes from hop-result, not source context")
        (is (contains? transferred :target-files)
            "edit peripheral declares :target-files in context, should transfer")))))

(deftest peripheral-hop-edit-to-test-to-deploy
  (testing "hop chain: edit → test → deploy (with explicit exit conditions)"
    (let [peripherals (periph/load-peripherals "resources/peripherals.edn")
          session-id "sess-chain-1"

          ;; edit → test
          hop1 (periph/validate-hop
                peripherals :edit
                (fix/make-hop-request
                 {:hop/to :test
                  :hop/reason "tests pass"
                  :hop/session-id session-id
                  :hop/context {:hop/exit-condition :hop-test}}))]
      (fix/assert-valid! shapes/HopResult hop1)
      (is (= :test (:hop/to hop1)))

      ;; test → deploy
      (let [hop2 (periph/validate-hop
                  peripherals :test
                  (fix/make-hop-request
                   {:hop/to :deploy
                    :hop/reason "all tests passed"
                    :hop/session-id session-id
                    :hop/context {:hop/exit-condition :hop-deploy}}))]
        (fix/assert-valid! shapes/HopResult hop2)
        (is (= :deploy (:hop/to hop2)))
        (is (= session-id (:hop/session-id hop2)))))))

(deftest peripheral-hop-invalid-path-blocked
  (testing "invalid hop paths are blocked"
    (let [peripherals (periph/load-peripherals "resources/peripherals.edn")]
      ;; deploy → edit (not allowed — edit has no :from-deploy entry)
      (let [result (periph/validate-hop
                    peripherals :deploy
                    (fix/make-hop-request
                     {:hop/to :edit
                      :hop/reason "need to fix something"
                      :hop/session-id "sess-x"
                      :hop/context {:hop/exit-condition :blocked}}))]
        (fix/assert-valid! shapes/SocialError result)
        (is (= :hop-not-allowed (:error/code result)))))))

(deftest peripheral-hop-reflect-from-any
  (testing "reflect accepts hops from any peripheral (:from-any in entry)"
    (let [peripherals (periph/load-peripherals "resources/peripherals.edn")]
      ;; explore → reflect
      (let [r1 (periph/validate-hop
                peripherals :explore
                (fix/make-hop-request
                 {:hop/to :reflect
                  :hop/reason "session closing, need PAR"
                  :hop/session-id "sess-r1"
                  :hop/context {:hop/exit-condition :hop-reflect}}))]
        (fix/assert-valid! shapes/HopResult r1)
        (is (= :reflect (:hop/to r1))))
      ;; deploy → reflect
      (let [r2 (periph/validate-hop
                peripherals :deploy
                (fix/make-hop-request
                 {:hop/to :reflect
                  :hop/reason "reflect after deploy"
                  :hop/session-id "sess-r2"
                  :hop/context {:hop/exit-condition :hop-reflect}}))]
        (fix/assert-valid! shapes/HopResult r2)
        (is (= :reflect (:hop/to r2)))))))

;; =============================================================================
;; Combined: pipeline segment + mode + peripheral
;; =============================================================================

(deftest pipeline-segment-with-mode-and-hop
  (testing "full flow: presence → authenticate → classify → mode transition → hop"
    (let [registry (fix/mock-registry)
          patterns (fix/mock-patterns)
          peripherals (periph/load-peripherals "resources/peripherals.edn")

          ;; Pipeline: connection → presence → identity
          connection (fix/make-connection
                      {:conn/agent-id (fix/make-agent-id "claude-1" :transport)
                       :conn/metadata {:ready true}})
          presence (presence/verify connection registry)
          _ (fix/assert-valid! shapes/PresenceRecord presence)
          identity (auth/resolve-identity presence registry)
          _ (fix/assert-valid! shapes/AgentIdentity identity)

          ;; Classify a coordination message
          msg {:msg/id "msg-coord-1"
               :msg/payload {:type "standup"}
               :msg/from (:identity/agent-id identity)
               :msg/at (fix/now-str)}
          classified (mode/classify msg patterns)
          _ (fix/assert-valid! shapes/ClassifiedMessage classified)

          ;; Mode transition: discuss → diagnose
          transition (mode/validate-transition :discuss :diagnose
                                               (:id/value (:identity/agent-id identity)))
          _ (fix/assert-valid! shapes/ModeTransition transition)

          ;; Peripheral hop: explore → edit
          hop (periph/validate-hop
               peripherals :explore
               (fix/make-hop-request
                {:hop/to :edit
                 :hop/reason "found target file"
                 :hop/session-id "sess-combined-1"}))]
      (fix/assert-valid! shapes/HopResult hop)
      ;; Verify the whole chain produced valid shapes at every boundary
      (is (= :coordination (:msg/mode classified)))
      (is (= :diagnose (:mode/to transition)))
      (is (= :edit (:hop/to hop))))))
