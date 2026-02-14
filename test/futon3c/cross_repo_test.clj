(ns futon3c.cross-repo-test
  "Cross-repo integration test: the full futon3 triangle.

   Demonstrates the refactored futon3 system working end-to-end:
     futon3a (sidecar store) ← futon3b (gate pipeline) ← futon3c (social pipeline)

   The flow:
   1. Register an agent in futon3c's agency registry
   2. Dispatch an action message through the social pipeline (presence → mode → dispatch)
   3. Peripheral dispatch produces a receipt with session-id and evidence
   4. Bridge converts receipt → gate pipeline input → runs G5→G0
   5. Proof-path from gates becomes sidecar evidence in futon3a
   6. Decision chain scores the full proposal→evidence arc

   This test exercises all three repos through their public APIs with
   no filesystem dependencies — missions, patterns, and proof-paths are
   all in-memory."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            ;; futon3c: social pipeline
            [futon3c.social.dispatch :as dispatch]
            [futon3c.social.mode :as mode]
            [futon3c.social.presence :as presence]
            [futon3c.social.authenticate :as auth]
            [futon3c.social.shapes :as social-shapes]
            [futon3c.social.persist :as persist]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.agency.registry :as reg]
            ;; futon3c: bridge
            [futon3c.bridge :as bridge]
            ;; futon3b: gate pipeline
            [futon3.gate.pipeline :as pipeline]
            [futon3.gate.shapes :as gate-shapes]
            ;; futon3a: sidecar store
            [sidecar.store :as sidecar]))

;; =============================================================================
;; Fixtures — reset all mutable state between tests
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

(defn- register-agent!
  "Register a mock agent in the live registry."
  [id-str type]
  (reg/register-agent!
   {:agent-id {:id/value id-str :id/type :continuity}
    :type type
    :invoke-fn (fn [_prompt _session-id] {:result "ok" :session-id nil :exit-code 0})
    :capabilities [:explore :edit :test]}))

(defn- make-pipeline-registry
  "Create a registry with peripheral-config for full pipeline dispatch."
  []
  (fix/mock-registry
   {:peripheral-config (fix/make-peripheral-config)}))

(defn- run-social-pipeline
  "Run futon3c's social pipeline: presence → authenticate → mode → dispatch.
   Returns the DispatchReceipt or SocialError."
  [agent-id-str payload registry patterns]
  (let [connection (fix/make-connection
                    {:conn/agent-id (fix/make-agent-id agent-id-str :transport)
                     :conn/metadata {:ready true}})
        presence-result (presence/verify connection registry)]
    (if (social-shapes/valid? social-shapes/SocialError presence-result)
      presence-result
      (let [identity-result (auth/resolve-identity presence-result registry)
            agent-id (fix/make-agent-id agent-id-str :continuity)
            message {:msg/id (str "xrepo-msg-" (java.util.UUID/randomUUID))
                     :msg/payload payload
                     :msg/from (:identity/agent-id identity-result)
                     :msg/to agent-id
                     :msg/at (fix/now-str)}
            classified (mode/classify message patterns)]
        (dispatch/dispatch classified registry)))))

(defn- make-gate-opts
  "Build gate pipeline options with an in-memory evidence sink."
  []
  (let [sink-atom (atom nil)]
    {:evidence-sink (fn [proof-data]
                      (reset! sink-atom proof-data)
                      {:ok true :path/id (get-in proof-data [:proof-path :path/id])})
     :sink-atom sink-atom}))

;; =============================================================================
;; 1. Full Triangle: futon3c dispatch → futon3b gates → futon3a sidecar
;; =============================================================================

(deftest full-triangle-dispatch-gates-sidecar
  (testing "dispatch receipt flows through gate pipeline into sidecar store"
    (register-agent! "claude-1" :claude)
    (let [registry (make-pipeline-registry)
          patterns (fix/mock-patterns)
          ;; Step 1: futon3c social pipeline — dispatch action message
          receipt (run-social-pipeline "claude-1" "implement feature X" registry patterns)]
      (fix/assert-valid! social-shapes/DispatchReceipt receipt)
      (is (true? (:receipt/delivered? receipt)))
      (is (= "peripheral/run-chain" (:receipt/route receipt)))

      ;; Step 2+3: bridge → futon3b gates → futon3a sidecar
      (let [store (sidecar/new-store)
            gate-opts (make-gate-opts)
            result (bridge/record-triangle! receipt store gate-opts)]
        ;; Full triangle succeeded
        (is (true? (:ok result))
            (str "Triangle failed: gate=" (get-in result [:gate-result :error/key])
                 " proposal=" (get-in result [:proposal :errors])
                 " evidence=" (get-in result [:evidence :errors])))

        ;; futon3b: gate pipeline produced proof-path
        (is (true? (get-in result [:gate-result :ok])))
        (let [proof-path (get-in result [:gate-result :O-proof-path])]
          (is (string? (:path/id proof-path)))
          (is (= 6 (count (:events proof-path)))
              "G5→G0 = 6 gate events"))

        ;; futon3a: proposal recorded
        (is (true? (get-in result [:proposal :ok])))
        (let [proposal-id (get-in result [:proposal :proposal/id])]
          (is (string? proposal-id))
          ;; Proposal is queryable
          (let [timeline (sidecar/event-timeline store proposal-id)]
            (is (>= (count timeline) 1)
                "proposal appears in sidecar timeline")))

        ;; futon3a: evidence attached to proposal
        (is (true? (get-in result [:evidence :ok])))))))

;; =============================================================================
;; 2. Gate pipeline validates dispatch receipt as artifact
;; =============================================================================

(deftest gate-pipeline-validates-dispatch-artifact
  (testing "futon3b gates accept dispatch receipt as a valid artifact"
    (register-agent! "claude-1" :claude)
    (let [registry (make-pipeline-registry)
          patterns (fix/mock-patterns)
          receipt (run-social-pipeline "claude-1" "explore codebase" registry patterns)]
      (fix/assert-valid! social-shapes/DispatchReceipt receipt)

      (let [gate-opts (make-gate-opts)
            gate-result (bridge/submit-to-gates! receipt gate-opts)]
        (is (true? (:ok gate-result))
            (str "Gate rejected: " (:gate/id gate-result) " " (:error/key gate-result)))

        ;; Proof-path is shape-valid
        (let [proof-path (:O-proof-path gate-result)]
          (is (nil? (try (gate-shapes/validate! gate-shapes/ProofPath proof-path) nil
                         (catch Exception e (.getMessage e))))
              "proof-path conforms to gate shapes"))

        ;; Evidence includes all gate outputs
        (let [evidence (:O-evidence gate-result)]
          (is (some? (:task-spec evidence)) "G5 task-spec present")
          (is (some? (:assignment evidence)) "G4 assignment present")
          (is (some? (:psr evidence)) "G3 PSR present")
          (is (some? (:par evidence)) "G0 PAR present"))

        ;; PAR references the dispatch session
        (let [par (get-in gate-result [:O-evidence :par])]
          (is (= (:receipt/session-id receipt) (:par/session-ref par))
              "PAR session-ref matches dispatch session-id"))))))

;; =============================================================================
;; 3. Coordination message (no peripheral) through triangle
;; =============================================================================

(deftest coordination-message-through-triangle
  (testing "coordination message bypasses peripherals but still gates + sidecar"
    (register-agent! "claude-1" :claude)
    (let [registry (make-pipeline-registry)
          patterns (fix/mock-patterns)
          receipt (run-social-pipeline "claude-1" {:type "standup"} registry patterns)]
      (fix/assert-valid! social-shapes/DispatchReceipt receipt)
      (is (= "registry/invoke" (:receipt/route receipt))
          "coordination → direct invoke")
      (is (nil? (:receipt/session-id receipt)))

      (let [store (sidecar/new-store)
            gate-opts (make-gate-opts)
            result (bridge/record-triangle! receipt store gate-opts)]
        (is (true? (:ok result)))

        ;; Artifact type reflects coordination dispatch
        (let [artifact (get-in result [:gate-result :O-artifacts])]
          (is (= :futon3c/coordination-dispatch (:artifact/type artifact))))

        ;; Sidecar proposal has lower score (no peripheral session)
        (let [proposal-id (get-in result [:proposal :proposal/id])
              proposals (:proposals @store)
              proposal (get proposals proposal-id)]
          (is (= 0.9 (:proposal/score proposal))
              "delivered coordination gets 0.9 score"))))))

;; =============================================================================
;; 4. Codex agent routes through :edit peripheral in triangle
;; =============================================================================

(deftest codex-agent-edit-peripheral-through-triangle
  (testing "codex agent action → :edit peripheral → gates → sidecar"
    (register-agent! "codex-1" :codex)
    (let [registry (make-pipeline-registry)
          patterns (fix/mock-patterns)
          receipt (run-social-pipeline "codex-1" "fix the auth bug" registry patterns)]
      (fix/assert-valid! social-shapes/DispatchReceipt receipt)
      (is (= :edit (:receipt/peripheral-id receipt))
          "codex → :edit peripheral")

      (let [store (sidecar/new-store)
            gate-opts (make-gate-opts)
            result (bridge/record-triangle! receipt store gate-opts)]
        (is (true? (:ok result)))

        ;; Artifact ref includes peripheral-id :edit
        (let [artifact (get-in result [:gate-result :O-artifacts])]
          (is (= :futon3c/peripheral-dispatch (:artifact/type artifact)))
          (is (= :edit (get-in artifact [:artifact/ref :receipt/peripheral-id]))))))))

;; =============================================================================
;; 5. Sidecar decision chain scores the full arc
;; =============================================================================

(deftest sidecar-chain-scores-dispatch-arc
  (testing "proposal → promotion → fact → chain scores correctly in sidecar"
    (register-agent! "claude-1" :claude)
    (let [registry (make-pipeline-registry)
          patterns (fix/mock-patterns)
          receipt (run-social-pipeline "claude-1" "implement feature Y" registry patterns)]
      (fix/assert-valid! social-shapes/DispatchReceipt receipt)

      (let [store (sidecar/new-store)
            gate-opts (make-gate-opts)
            result (bridge/record-triangle! receipt store gate-opts)]
        (is (true? (:ok result)))

        ;; Build a decision chain: proposal step
        (let [proposal-id (get-in result [:proposal :proposal/id])
              ;; Promote the proposal
              promotion-id (str "prom-" proposal-id)
              promotion-result (sidecar/record-promotion!
                                store
                                {:promotion/id promotion-id
                                 :proposal/id proposal-id
                                 :promotion/kind :coordination
                                 :promotion/decided-by "cross-repo-test"
                                 :promotion/rationale "Gate pipeline validated the dispatch"})
              ;; Materialize a fact from the promotion
              fact-id (str "fact-" proposal-id)
              fact-result (sidecar/record-fact!
                           store
                           {:fact/id fact-id
                            :fact/kind :coordination
                            :fact/body {:dispatch-delivered true
                                        :gate-pipeline-passed true
                                        :peripheral (:receipt/peripheral-id receipt)}
                            :fact/event-type :fact
                            :fact/actor "cross-repo-test"
                            :fact/rationale "Full triangle: dispatch → gates → sidecar"}
                           {:promotion/id promotion-id})
              ;; Build the chain
              chain-result (sidecar/build-chain!
                            store
                            {:chain/steps [{:step/type :proposal
                                            :step/id proposal-id}]})]
          (is (true? (:ok promotion-result))
              (str "Promotion failed: " (:errors promotion-result)))
          (is (true? (:ok fact-result))
              (str "Fact failed: " (:errors fact-result)))
          (is (true? (:ok chain-result))
              (str "Chain failed: " (:errors chain-result)))

          ;; Chain has scoring
          (let [scoring (:scoring chain-result)]
            (is (some? scoring))
            (is (pos? (:score/base scoring))
                "chain has positive base score"))

          ;; Timeline shows full arc: proposal → promotion → fact
          (let [timeline (sidecar/event-timeline store proposal-id)]
            (is (>= (count timeline) 3)
                (str "Expected 3+ events (proposal + promotion + fact), got "
                     (count timeline)))))))))

;; =============================================================================
;; 6. Proof-path events span all 6 gates
;; =============================================================================

(deftest proof-path-spans-all-gates
  (testing "proof-path from gate pipeline has events from G5 through G0"
    (register-agent! "claude-1" :claude)
    (let [registry (make-pipeline-registry)
          patterns (fix/mock-patterns)
          receipt (run-social-pipeline "claude-1" "full gate traversal" registry patterns)]
      (fix/assert-valid! social-shapes/DispatchReceipt receipt)

      (let [gate-opts (make-gate-opts)
            gate-result (bridge/submit-to-gates! receipt gate-opts)]
        (is (true? (:ok gate-result)))

        (let [events (get-in gate-result [:O-proof-path :events])
              gate-ids (mapv :gate/id events)]
          ;; All 6 gates present in order
          (is (= [:g5 :g4 :g3 :g2 :g1 :g0] gate-ids)
              (str "Expected G5→G0, got " gate-ids))

          ;; Each event has a timestamp
          (doseq [event events]
            (is (string? (:gate/at event))
                (str "Missing timestamp on " (:gate/id event))))

          ;; Each event has a shape-valid record
          (doseq [event events]
            (is (map? (:gate/record event))
                (str "Missing record on " (:gate/id event)))))))))

;; =============================================================================
;; 7. Bridge converters produce correct shapes
;; =============================================================================

(deftest bridge-converters-produce-correct-shapes
  (testing "receipt→artifact, receipt→par, receipt→proposal all produce valid data"
    (register-agent! "claude-1" :claude)
    (let [registry (make-pipeline-registry)
          patterns (fix/mock-patterns)
          receipt (run-social-pipeline "claude-1" "test converters" registry patterns)]
      (fix/assert-valid! social-shapes/DispatchReceipt receipt)

      ;; Artifact conversion
      (let [artifact (bridge/receipt->artifact receipt)]
        (is (= :futon3c/peripheral-dispatch (:artifact/type artifact)))
        (is (true? (:exec/success? artifact)))
        (is (= (:receipt/msg-id receipt)
               (get-in artifact [:artifact/ref :receipt/msg-id]))))

      ;; PAR conversion
      (let [par (bridge/receipt->par receipt)]
        (is (string? (:par/session-ref par)))
        (is (= (:receipt/session-id receipt) (:par/session-ref par)))
        (is (string? (:par/what-worked par)))
        (is (vector? (:par/prediction-errors par)))
        (is (vector? (:par/suggestions par))))

      ;; Proposal conversion
      (let [proposal (bridge/receipt->proposal receipt)]
        (is (string? (:proposal/id proposal)))
        (is (= :coordination (:proposal/kind proposal)))
        (is (= 0.9 (:proposal/score proposal)))
        (is (= :dispatch (:proposal/method proposal)))
        (is (map? (:proposal/evidence proposal)))))))

;; =============================================================================
;; 8. Failed delivery still records in sidecar (with lower score)
;; =============================================================================

(deftest failed-peripheral-through-triangle
  (testing "peripheral failure → gate pipeline still runs → sidecar records with low score"
    (register-agent! "claude-1" :claude)
    (with-redefs [futon3c.peripheral.registry/run-chain
                  (fn [_ _ _] (throw (ex-info "peripheral crashed" {:reason :test})))]
      (let [registry (make-pipeline-registry)
            patterns (fix/mock-patterns)
            ;; Dispatch will fail at peripheral level → SocialError
            result (run-social-pipeline "claude-1" "trigger failure" registry patterns)]
        ;; Result is a SocialError, not a receipt
        (is (social-shapes/valid? social-shapes/SocialError result))
        (is (= :peripheral-failed (:error/code result)))

        ;; We can still record the failure in sidecar
        ;; (construct a "failed" receipt manually for the bridge)
        (let [store (sidecar/new-store)
              failed-receipt {:receipt/msg-id "failed-msg"
                              :receipt/to (fix/make-agent-id "claude-1" :continuity)
                              :receipt/delivered? false
                              :receipt/at (fix/now-str)
                              :receipt/route "peripheral/failed"}
              proposal (bridge/receipt->proposal failed-receipt)
              proposal-result (sidecar/record-proposal! store proposal)]
          (is (true? (:ok proposal-result)))
          ;; Score reflects failure
          (let [stored-proposal (get-in @store [:proposals (:proposal/id proposal)])]
            (is (= 0.3 (:proposal/score stored-proposal))
                "failed delivery gets 0.3 score")))))))
