(ns futon3c.social.dispatch-integration-test
  "M-dispatch-peripheral-bridge Part III: integration tests.

   End-to-end scenarios that wire the full social pipeline (S-presence →
   S-authenticate → S-mode → S-dispatch) with peripheral dispatch, evidence
   threading, session persistence, and ← verification."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.registry :as reg]
            [futon3c.evidence.store :as estore]
            [futon3c.evidence.threads :as threads]
            [futon3c.peripheral.registry :as preg]
            [futon3c.peripheral.round-trip :as rt]
            [futon3c.peripheral.tools :as tools]
            [futon3c.social.authenticate :as auth]
            [futon3c.social.dispatch :as dispatch]
            [futon3c.social.mode :as mode]
            [futon3c.social.persist :as persist]
            [futon3c.social.presence :as presence]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix])
  (:import [java.time Instant]))

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
  "Register a test agent and return its typed ID."
  [id-value agent-type]
  (let [typed-id (fix/make-agent-id id-value :continuity)]
    (reg/register-agent!
     {:agent-id typed-id
      :type agent-type
      :invoke-fn (fn [_prompt _session-id] {:result "ok" :session-id nil :exit-code 0})
      :capabilities [:explore :edit :test]})
    typed-id))

(defn- make-peripheral-registry
  "Create a registry with peripheral-config for integration tests."
  ([] (make-peripheral-registry {}))
  ([config-overrides]
   (fix/mock-registry
    {:peripheral-config (fix/make-peripheral-config config-overrides)})))

(defn- run-full-pipeline
  "Run the full social pipeline: presence → authenticate → mode → dispatch.
   Returns the dispatch result."
  [agent-id-value message-payload registry patterns]
  (let [connection (fix/make-connection
                    {:conn/agent-id (fix/make-agent-id agent-id-value :transport)
                     :conn/metadata {:ready true}})
        presence-result (presence/verify connection registry)]
    (if (shapes/valid? shapes/SocialError presence-result)
      presence-result
      (let [identity-result (auth/resolve-identity presence-result registry)]
        (if (shapes/valid? shapes/SocialError identity-result)
          identity-result
          (let [agent-id (fix/make-agent-id agent-id-value :continuity)
                message {:msg/id (str "msg-" (java.util.UUID/randomUUID))
                         :msg/payload message-payload
                         :msg/from (:identity/agent-id identity-result)
                         :msg/to agent-id
                         :msg/at (fix/now-str)}
                classified (mode/classify message patterns)]
            (if (shapes/valid? shapes/SocialError classified)
              classified
              (dispatch/dispatch classified registry))))))))

;; =============================================================================
;; 1. Full pipeline → peripheral → receipt
;; =============================================================================

(deftest full-pipeline-action-to-peripheral-receipt
  (testing "action message through full pipeline → peripheral dispatch → enriched receipt"
    (let [evidence-store (atom {:entries {} :order []})
          registry (make-peripheral-registry {:evidence-store evidence-store})
          patterns (fix/mock-patterns)
          agent-id (setup-agent! "claude-1" :claude)
          result (run-full-pipeline "claude-1" "implement feature X" registry patterns)]
      ;; Should produce a valid enriched DispatchReceipt
      (fix/assert-valid! shapes/DispatchReceipt result)
      (is (true? (:receipt/delivered? result)))
      (is (= "peripheral/run-chain" (:receipt/route result)))
      (is (string? (:receipt/session-id result)))
      (is (= :explore (:receipt/peripheral-id result)))
      ;; Fruit present (explore peripheral produces fruit on stop)
      (is (some? (:receipt/fruit result))))))

;; =============================================================================
;; 2. Coordination mode bypasses peripherals
;; =============================================================================

(deftest full-pipeline-coordination-bypasses-peripherals
  (testing "coordination message through full pipeline → direct invoke, no peripheral"
    (let [registry (make-peripheral-registry)
          patterns (fix/mock-patterns)
          agent-id (setup-agent! "claude-1" :claude)
          ;; {:type "standup"} is classified as coordination by S-mode
          result (run-full-pipeline "claude-1" {:type "standup"} registry patterns)]
      (fix/assert-valid! shapes/DispatchReceipt result)
      (is (true? (:receipt/delivered? result)))
      (is (= "registry/invoke" (:receipt/route result)))
      ;; No peripheral fields
      (is (nil? (:receipt/session-id result)))
      (is (nil? (:receipt/peripheral-id result)))
      (is (nil? (:receipt/fruit result))))))

;; =============================================================================
;; 3. Evidence thread spans dispatch + peripheral
;; =============================================================================

(deftest evidence-thread-spans-dispatch-and-peripheral
  (testing "dispatch root + peripheral evidence form one projectable thread"
    (let [evidence-store (atom {:entries {} :order []})
          registry (make-peripheral-registry {:evidence-store evidence-store})
          patterns (fix/mock-patterns)
          agent-id (setup-agent! "claude-1" :claude)
          result (run-full-pipeline "claude-1" "explore the codebase" registry patterns)]
      (fix/assert-valid! shapes/DispatchReceipt result)

      (let [session-id (:receipt/session-id result)
            subj {:ref/type :session :ref/id session-id}
            tp (threads/project-thread evidence-store subj)]

        ;; Thread projection exists
        (is (some? tp) "thread projection should exist")

        (let [entries (:thread/entries tp)
              by-id (into {} (map (juxt :evidence/id identity) entries))]

          ;; Has entries from both dispatch and peripheral
          (is (>= (count entries) 3)
              (str "Expected at least 3 entries (dispatch root + start + stop), got " (count entries)))

          ;; Dispatch root entry present
          (let [dispatch-entries (filter #(= [:dispatch :session-start] (:evidence/tags %)) entries)]
            (is (= 1 (count dispatch-entries)) "exactly one dispatch root entry"))

          ;; All entries share the session subject
          (is (every? #(= subj (:evidence/subject %)) entries)
              "all entries have same session subject")

          ;; Root invariant — goal entry exists
          (is (= :goal (:evidence/claim-type (:thread/goal tp))))

          ;; Tree validity — every in-reply-to references an entry in the projection
          (doseq [e entries]
            (when-let [parent (:evidence/in-reply-to e)]
              (is (contains? by-id parent)
                  (str (:evidence/id e) " references missing " parent))))

          ;; Monotonic timestamps
          (let [instants (map #(Instant/parse (:evidence/at %)) entries)]
            (is (= instants (sort instants)) "timestamps are non-decreasing"))

          ;; Entry count consistency
          (is (= (:thread/entry-count tp) (count entries))))))))

;; =============================================================================
;; 4. Peripheral error propagates as SocialError
;; =============================================================================

(deftest peripheral-error-propagates-as-dispatch-social-error
  (testing "peripheral failure surfaces as SocialError at dispatch boundary"
    (with-redefs [futon3c.peripheral.registry/run-chain
                  (fn [_ _ _] (throw (ex-info "peripheral crashed" {:reason :test})))]
      (let [registry (make-peripheral-registry)
            patterns (fix/mock-patterns)
            agent-id (setup-agent! "claude-1" :claude)
            result (run-full-pipeline "claude-1" "trigger failure" registry patterns)]
        (fix/assert-valid! shapes/SocialError result)
        (is (= :S-dispatch (:error/component result)))
        (is (= :peripheral-failed (:error/code result)))))))

;; =============================================================================
;; 5. Session lifecycle — create at dispatch, fruit persisted
;; =============================================================================

(deftest session-lifecycle-create-and-fruit
  (testing "peripheral dispatch creates session record with fruit"
    (let [evidence-store (atom {:entries {} :order []})
          registry (make-peripheral-registry {:evidence-store evidence-store})
          patterns (fix/mock-patterns)
          agent-id (setup-agent! "claude-1" :claude)
          result (run-full-pipeline "claude-1" "implement thing" registry patterns)]
      (fix/assert-valid! shapes/DispatchReceipt result)

      (let [session-id (:receipt/session-id result)
            session-result (persist/get-session session-id)]
        ;; Session exists
        (fix/assert-valid! shapes/SessionRecord session-result)
        (is (= session-id (:session/id session-result)))
        (is (= (fix/make-agent-id "claude-1" :continuity)
               (:session/agent-id session-result)))

        ;; Session state has events (dispatch event + possibly update event)
        (let [events (get-in session-result [:session/state :events])]
          (is (>= (count events) 1) "at least one event in session transcript"))

        ;; If fruit was present, session data should have peripheral-fruit
        (when (:receipt/fruit result)
          (let [data (get-in session-result [:session/state :data])]
            (is (some? (:peripheral-fruit data))
                "session data should contain peripheral fruit")))))))

;; =============================================================================
;; 6. ← verification on dispatched peripheral evidence
;; =============================================================================

(deftest backward-verification-on-dispatched-session
  (testing "← verification passes on evidence from a dispatched peripheral session"
    (let [evidence-store (atom {:entries {} :order []})
          registry (make-peripheral-registry {:evidence-store evidence-store})
          patterns (fix/mock-patterns)
          agent-id (setup-agent! "claude-1" :claude)
          result (run-full-pipeline "claude-1" "explore and verify" registry patterns)]
      (fix/assert-valid! shapes/DispatchReceipt result)

      (let [session-id (:receipt/session-id result)
            subj {:ref/type :session :ref/id session-id}
            ;; Get all evidence entries for this session
            all-entries (estore/query* evidence-store {:query/subject subj})
            ;; Filter to just the peripheral evidence (exclude dispatch root)
            peripheral-entries (->> all-entries
                                    (remove #(= [:dispatch :session-start] (:evidence/tags %)))
                                    (sort-by #(Instant/parse (:evidence/at %)))
                                    vec)]
        ;; We have peripheral evidence
        (is (>= (count peripheral-entries) 2)
            (str "Expected at least 2 peripheral entries (start + stop), got "
                 (count peripheral-entries)))

        ;; Run ← verification on the peripheral evidence against explore spec
        (let [explore-spec (get-in (preg/load-peripherals) [:peripherals :explore])
              verification (rt/verify-constraints explore-spec peripheral-entries)]
          (is (true? (:ok verification))
              (str "← verification failed: "
                   (when (:violations verification)
                     (pr-str (:violations verification))))))))))

;; =============================================================================
;; 7. Shapes validated at every pipeline boundary
;; =============================================================================

(deftest shapes-validated-at-every-boundary-with-peripheral
  (testing "every pipeline output conforms to its shape (peripheral path)"
    (let [evidence-store (atom {:entries {} :order []})
          registry (make-peripheral-registry {:evidence-store evidence-store})
          patterns (fix/mock-patterns)
          _ (setup-agent! "claude-1" :claude)
          connection (fix/make-connection
                      {:conn/agent-id (fix/make-agent-id "claude-1" :transport)
                       :conn/metadata {:ready true}})
          presence-result (presence/verify connection registry)
          identity-result (auth/resolve-identity presence-result registry)
          message {:msg/id "msg-shape-int"
                   :msg/payload "implement feature Y"
                   :msg/from (:identity/agent-id identity-result)
                   :msg/to (fix/make-agent-id "claude-1" :continuity)
                   :msg/at (fix/now-str)}
          classified (mode/classify message patterns)
          receipt (dispatch/dispatch classified registry)]
      ;; Every boundary validates
      (is (shapes/valid? shapes/PresenceRecord presence-result) "presence output valid")
      (is (shapes/valid? shapes/AgentIdentity identity-result) "identity output valid")
      (is (shapes/valid? shapes/ClassifiedMessage classified) "classified output valid")
      (is (= :action (:msg/mode classified)) "action mode classified correctly")
      (is (shapes/valid? shapes/DispatchReceipt receipt) "receipt output valid")
      ;; Evidence entries are shape-valid
      (let [entries (vals (:entries @evidence-store))]
        (doseq [e entries]
          (is (shapes/valid? shapes/EvidenceEntry e)
              (str "invalid evidence: " (:evidence/id e))))))))

;; =============================================================================
;; 8. Agent type routing: codex → :edit peripheral
;; =============================================================================

(deftest codex-agent-routes-to-edit-peripheral
  (testing "codex agent action message → :edit peripheral via full pipeline"
    (let [registry (make-peripheral-registry)
          patterns (fix/mock-patterns)
          agent-id (setup-agent! "codex-1" :codex)
          result (run-full-pipeline "codex-1" "fix the bug in auth.clj" registry patterns)]
      (fix/assert-valid! shapes/DispatchReceipt result)
      (is (= :edit (:receipt/peripheral-id result)))
      (is (= "peripheral/run-chain" (:receipt/route result))))))
