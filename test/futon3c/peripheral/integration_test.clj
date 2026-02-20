(ns futon3c.peripheral.integration-test
  "M-peripheral-behavior Part III: end-to-end integration tests.

   Exercises the full peripheral stack: registry → chain orchestration →
   hop validation → context transfer → evidence emission → ← verification →
   thread projection → proof-tree invariants."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [futon3c.evidence.store :as store]
            [futon3c.evidence.threads :as threads]
            [futon3c.peripheral.adapter :as adapter]
            [futon3c.peripheral.common :as common]
            [futon3c.peripheral.registry :as reg]
            [futon3c.peripheral.round-trip :as rt]
            [futon3c.peripheral.tools :as tools]
            [futon3c.social.shapes :as shapes])
  (:import [java.time Instant]))

;; =============================================================================
;; Shared setup
;; =============================================================================

(def ^:private peripherals (reg/load-peripherals))

(defn- make-chain-backend
  "Mock backend with canned results for all tool types."
  []
  (tools/make-mock-backend
    {:glob      {:found ["src/futon3c/core.clj"]}
     :read      {:content "(ns futon3c.core)"}
     :grep      {:matches 3}
     :edit      {:ok true}
     :write     {:ok true}
     :bash-test {:status :pass :test-count 12}
     :bash-git  {:status :committed :sha "abc123"}
     :bash-deploy {:status :pushed}
     :musn-log  {:lines 50}}))

;; =============================================================================
;; 1. Full hop chain: explore → edit → test → deploy → reflect
;; =============================================================================

(deftest full-hop-chain-preserves-session-and-context
  (testing "run-chain across all 5 peripherals preserves session-id and transfers context"
    (let [evidence-store (atom {:entries {} :order []})
          backend (make-chain-backend)
          result (reg/run-chain
                   {:backend backend :peripherals peripherals
                    :evidence-store evidence-store}
                   {:session-id "s-e2e-1"}
                   [{:peripheral-id :explore
                     :actions [{:tool :glob :args ["**/*.clj"]}
                               {:tool :read :args ["src/futon3c/core.clj"]}]
                     :stop-reason "found target"
                     :exit-condition :found-target}
                    {:peripheral-id :edit
                     :actions [{:tool :edit :args ["src/futon3c/core.clj"]}]
                     :stop-reason "changes complete"
                     :exit-condition :hop-test}
                    {:peripheral-id :test
                     :actions [{:tool :bash-test :args ["clojure -X:test"]}]
                     :stop-reason "tests pass"
                     :exit-condition :hop-deploy}
                    {:peripheral-id :deploy
                     :actions [{:tool :bash-git :args ["commit -m \"ship it\""]}]
                     :stop-reason "deployed"
                     :exit-condition :hop-reflect}
                    {:peripheral-id :reflect
                     :actions [{:tool :read :args ["session.log"]}]
                     :stop-reason "session close"
                     :exit-condition :par-generated}])]
      ;; Chain succeeds
      (is (true? (:ok result)))

      ;; 5 fruits, one per peripheral
      (is (= 5 (count (:fruits result))))

      ;; Explore fruit contains found targets
      (is (seq (get-in (:fruits result) [0 :found])))

      ;; Edit fruit shows changes
      (is (pos? (get-in (:fruits result) [1 :changes])))

      ;; Test fruit shows pass
      (is (= :pass (get-in (:fruits result) [2 :result])))

      ;; Deploy fruit shows committed
      (is (true? (get-in (:fruits result) [3 :committed?])))

      ;; Reflect fruit contains PAR evidence entry
      (let [par (get-in (:fruits result) [4 :par])]
        (is (= :reflection (:evidence/type par)))
        (is (= :observation (:evidence/claim-type par))))

      ;; Evidence in result: 4 per explore (start+2steps+stop) + 3 per remaining = 4+3+3+3+3 = 16
      (is (= 16 (count (:evidence result))))

      ;; All evidence entries are shape-valid
      (doseq [ev (:evidence result)]
        (is (shapes/valid? shapes/EvidenceEntry ev)
            (str "invalid evidence: " (:evidence/id ev))))

      ;; All evidence entries share the same session-id
      (is (every? #(= "s-e2e-1" (:evidence/session-id %)) (:evidence result)))

      ;; Evidence store has entries (including reflect's PAR)
      (is (< (count (:evidence result))
             (count (:entries @evidence-store)))
          "store has extra PAR entry from reflect"))))

;; =============================================================================
;; 2. ← round-trip passes for all 5 peripherals
;; =============================================================================

(deftest backward-verification-all-peripherals
  (testing "run-and-verify passes ← verification for each real peripheral"
    (let [backend (make-chain-backend)
          test-cases [{:pid :explore :actions [{:tool :glob :args ["**/*.clj"]}]}
                      {:pid :edit    :actions [{:tool :edit :args ["src/a.clj"]}]}
                      {:pid :test    :actions [{:tool :bash-test :args ["clojure -X:test"]}]}
                      {:pid :deploy  :actions [{:tool :bash-git :args ["commit -m x"]}]}
                      {:pid :reflect :actions [{:tool :read :args ["session.log"]}]}]]
      (doseq [{:keys [pid actions]} test-cases]
        (let [peripheral (reg/make-peripheral pid backend)
              result (rt/run-and-verify
                       peripheral
                       {:session-id (str "s-bv-" (name pid))}
                       actions
                       "done")]
          (is (true? (:ok result))
              (str "← verification failed for " pid
                   (when (:violations result)
                     (str ": " (pr-str (:violations result))))))
          (is (seq (:evidence result))
              (str "no evidence for " pid)))))))

;; =============================================================================
;; 3. Evidence thread + proof-tree invariants
;; =============================================================================

(deftest proof-tree-invariants-on-peripheral-chain
  (testing "evidence from a full chain satisfies proof-tree invariants"
    (let [evidence-store (atom {:entries {} :order []})
          backend (make-chain-backend)
          session-id "s-invariants"
          _ (reg/run-chain
              {:backend backend :peripherals peripherals
               :evidence-store evidence-store}
              {:session-id session-id}
              [{:peripheral-id :explore
                :actions [{:tool :glob :args ["**/*.clj"]}]
                :stop-reason "found target"
                :exit-condition :found-target}
               {:peripheral-id :edit
                :actions [{:tool :edit :args ["src/a.clj"]}]
                :stop-reason "ready to test"
                :exit-condition :hop-test}])
          subj {:ref/type :session :ref/id session-id}
          tp (threads/project-thread evidence-store subj)]

      (is (some? tp) "thread projection exists")

      (let [entries (:thread/entries tp)
            by-id (into {} (map (juxt :evidence/id identity) entries))]

        ;; Invariant 1: Tree validity — every in-reply-to references an entry in projection
        (doseq [e entries]
          (when-let [parent (:evidence/in-reply-to e)]
            (is (contains? by-id parent)
                (str (:evidence/id e) " references missing " parent))))

        ;; Invariant 2: Root invariant — goal entry exists
        (is (= :goal (:evidence/claim-type (:thread/goal tp))))

        ;; Invariant 3: No orphans — all entries have the session subject
        (is (every? #(= subj (:evidence/subject %)) entries))

        ;; Invariant 4: Claim ordering — no conclusion replies to another conclusion
        (let [conclusions (filter #(= :conclusion (:evidence/claim-type %)) entries)]
          (doseq [c conclusions]
            (when-let [parent-id (:evidence/in-reply-to c)]
              (let [parent (get by-id parent-id)]
                (is (not= :conclusion (:evidence/claim-type parent))
                    (str "conclusion " (:evidence/id c) " should not reply to conclusion"))))))

        ;; Invariant 5: Author tracking
        (is (seq (:thread/participants tp)))

        ;; Invariant 6: Monotonic timestamps
        (let [instants (map #(Instant/parse (:evidence/at %)) entries)]
          (is (= instants (sort instants)) "timestamps are non-decreasing"))

        ;; Invariant 7: Entry count consistency
        (is (= (:thread/entry-count tp) (count entries)))

        ;; Thread is closed (has conclusion entries)
        (is (= :closed (:thread/status tp)))))))

;; =============================================================================
;; 4. Scope violation produces SocialError
;; =============================================================================

(deftest scope-violation-in-chain
  (testing "edit attempting out-of-scope write produces SocialError, not silent failure"
    (let [backend (tools/make-mock-backend {:glob {:found ["src/a.clj"]}
                                             :write {:ok true}})
          result (reg/run-chain
                   {:backend backend :peripherals peripherals}
                   {:session-id "s-scope"}
                   [{:peripheral-id :explore
                     :actions [{:tool :glob :args ["**/*.clj"]}]
                     :stop-reason "found target"
                     :exit-condition :found-target}
                    {:peripheral-id :edit
                     ;; test/ is outside edit's scope {:paths ["src/" "docs/" "scripts/"]}
                     :actions [{:tool :write :args ["test/evil.clj"]}]
                     :stop-reason "done"
                     :exit-condition :tests-pass}])]
      (is (false? (:ok result)))
      (is (shapes/valid? shapes/SocialError (:error result)))
      (is (= :out-of-scope (:error/code (:error result))))
      ;; Explore evidence was still collected before the failure
      (is (= 3 (count (:evidence result)))))))

;; =============================================================================
;; 5. Reframe: test fails → edit → re-test → deploy
;; =============================================================================

(deftest reframe-test-fail-edit-retest-deploy
  (testing "reframe scenario: test fails, hop to edit, fix, re-test passes, deploy"
    (let [test-call-count (atom 0)
          backend (tools/make-mock-backend
                    {:bash-test (fn [_ _]
                                  (let [n (swap! test-call-count inc)]
                                    {:ok true
                                     :result (if (= 1 n)
                                               {:status :fail :test-count 5
                                                :failures ["test/core_test.clj:42"]}
                                               {:status :pass :test-count 5})}))
                     :edit     {:ok true}
                     :bash-git {:status :committed :sha "fix-abc"}})
          result (reg/run-chain
                   {:backend backend :peripherals peripherals}
                   {:session-id "s-reframe"}
                   [;; First test run — fails
                    {:peripheral-id :test
                     :actions [{:tool :bash-test :args ["clojure -X:test"]}]
                     :stop-reason "tests failed, need fix"
                     :exit-condition :hop-edit}
                    ;; Edit — apply fix
                    {:peripheral-id :edit
                     :actions [{:tool :edit :args ["src/futon3c/core.clj"]}]
                     :stop-reason "fix applied"
                     :exit-condition :hop-test}
                    ;; Second test run — passes
                    {:peripheral-id :test
                     :actions [{:tool :bash-test :args ["clojure -X:test"]}]
                     :stop-reason "tests pass"
                     :exit-condition :hop-deploy}
                    ;; Deploy
                    {:peripheral-id :deploy
                     :actions [{:tool :bash-git :args ["commit -m \"fix\""]}]
                     :stop-reason "deployed"
                     :exit-condition :deployed}])]
      (is (true? (:ok result)))
      (is (= 4 (count (:fruits result))))

      ;; First test fruit: fail
      (is (= :fail (get-in (:fruits result) [0 :result])))

      ;; Second test fruit: pass
      (is (= :pass (get-in (:fruits result) [2 :result])))

      ;; Deploy fruit: committed
      (is (true? (get-in (:fruits result) [3 :committed?])))
      (is (= "fix-abc" (get-in (:fruits result) [3 :sha]))))))

;; =============================================================================
;; 6. Reflect produces mineable PAR
;; =============================================================================

(deftest reflect-par-mineable-by-thread-patterns
  (testing "reflect's PAR is in the evidence store and mineable by thread-patterns"
    (let [evidence-store (atom {:entries {} :order []})
          backend (tools/make-mock-backend {:read {:content "session log data"}
                                             :musn-log {:lines 80}})
          session-id "s-par-mine"
          result (reg/run-chain
                   {:backend backend :peripherals peripherals
                    :evidence-store evidence-store}
                   {:session-id session-id}
                   [{:peripheral-id :reflect
                     :actions [{:tool :read :args ["session.log"]}
                               {:tool :musn-log :args ["session.log"]}]
                     :stop-reason "session close"
                     :exit-condition :par-generated}])]
      (is (true? (:ok result)))

      ;; PAR is in the fruit
      (let [par (get-in (:fruits result) [0 :par])]
        (is (some? par))
        (is (shapes/valid? shapes/EvidenceEntry par))
        (is (= :reflection (:evidence/type par)))
        (is (= :observation (:evidence/claim-type par)))
        (is (map? (get-in par [:evidence/body :par])))
        ;; PAR body has expected structure
        (let [par-body (get-in par [:evidence/body :par])]
          (is (string? (:problem par-body)))
          (is (vector? (:approach par-body)))
          (is (string? (:result par-body)))))

      ;; PAR entry is in the evidence store
      (let [subj {:ref/type :session :ref/id session-id}
            entries (store/query* evidence-store {:query/subject subj})
            par-entries (filter #(= :observation (:evidence/claim-type %)) entries)]
        (is (= 1 (count par-entries)) "exactly one PAR in store"))

      ;; Thread projection includes PAR
      (let [subj {:ref/type :session :ref/id session-id}
            tp (threads/project-thread evidence-store subj)]
        (is (some? tp))
        ;; All entries are :reflection type (reflect peripheral)
        (is (every? #(= :reflection (:evidence/type %))
                    (:thread/entries tp)))))))

;; =============================================================================
;; 7. Context flows through chain
;; =============================================================================

(deftest context-transfers-target-files-through-hop
  (testing "explore's target-files flow through hop into edit's context"
    (let [backend (tools/make-mock-backend
                    {:glob {:found ["src/futon3c/core.clj"
                                    "src/futon3c/util.clj"]}
                     :edit {:ok true}})
          result (reg/run-chain
                   {:backend backend :peripherals peripherals}
                   {:session-id "s-ctx-flow"}
                   [{:peripheral-id :explore
                     :actions [{:tool :glob :args ["src/**/*.clj"]}]
                     :stop-reason "found targets"
                     :exit-condition :found-target}
                    {:peripheral-id :edit
                     :actions [{:tool :edit :args ["src/futon3c/core.clj"]}]
                     :stop-reason "edits complete"
                     :exit-condition :hop-test}])]
      (is (true? (:ok result)))
      ;; Edit's fruit includes changed files
      (let [edit-fruit (get (:fruits result) 1)]
        (is (pos? (:changes edit-fruit)))
        (is (seq (:changed-files edit-fruit)))))))

;; =============================================================================
;; 8. Chain rejects impossible hop with accumulated evidence
;; =============================================================================

(deftest chain-impossible-hop-returns-error-with-evidence
  (testing "chain with impossible hop fails at validation, preserving prior evidence"
    (let [backend (make-chain-backend)
          result (reg/run-chain
                   {:backend backend :peripherals peripherals}
                   {:session-id "s-bad-hop"}
                   [{:peripheral-id :explore
                     :actions [{:tool :read :args ["README.md"]}]
                     :stop-reason "found target"
                     :exit-condition :found-target}
                    ;; explore → deploy is impossible (deploy wants :from-test/:tests-passed)
                    {:peripheral-id :deploy
                     :actions [{:tool :bash-git :args ["commit"]}]
                     :stop-reason "ship"
                     :exit-condition :deployed}])]
      (is (false? (:ok result)))
      (is (shapes/valid? shapes/SocialError (:error result)))
      (is (= :hop-not-allowed (:error/code (:error result))))
      ;; Explore's evidence was collected before the hop failed
      (is (= 3 (count (:evidence result))))
      (is (= 1 (count (:fruits result)))))))

;; =============================================================================
;; 9. Adapter integration: prompts + tool mapping for real specs
;; =============================================================================

(deftest adapter-works-with-all-real-peripheral-specs
  (testing "adapter generates correct prompts and mappings for each real peripheral"
    (doseq [pid reg/peripheral-ids]
      (let [spec (common/load-spec pid)
            prompt (adapter/peripheral-prompt-section spec {:session-id "s-adapt"})
            constraints (adapter/describe-constraints spec)
            claude-mapping (adapter/tool-mapping spec)
            codex-mapping (adapter/codex-tool-mapping spec)]
        ;; Prompt mentions the peripheral name
        (is (str/includes? prompt (str/upper-case (name pid)))
            (str "prompt missing name for " pid))
        ;; Constraints have allowed tools (chat has tools=#{} — IRC relay is transport-level)
        (when-not (#{:chat} pid)
          (is (seq (:allowed-tools constraints))
              (str "no allowed tools for " pid)))
        ;; Mapping has at least one entry (except reflect/:alfworld/:chat which have tools with no Claude/Codex equivalent)
        (when-not (#{:reflect :alfworld :chat} pid)
          (is (seq claude-mapping)
              (str "empty Claude mapping for " pid)))
        (when-not (#{:alfworld :chat} pid)
          (is (seq codex-mapping)
              (str "empty Codex mapping for " pid)))))))

;; =============================================================================
;; 10. ← verification detects violations on real peripherals
;; =============================================================================

(deftest backward-verification-detects-violations
  (testing "← verification catches tool violations on real peripherals"
    (let [backend (tools/make-mock-backend)
          ;; explore with an :edit action (disallowed)
          peripheral (reg/make-peripheral :explore backend)
          result (rt/run-and-verify
                   peripheral
                   {:session-id "s-bv-fail"}
                   [{:tool :edit :args ["src/a.clj"]}]
                   "done")]
      (is (false? (:ok result)))
      (is (shapes/valid? shapes/SocialError (:error result))))))

;; =============================================================================
;; 11. P-4 structural proof: explicit hop with session-id through discipline
;; =============================================================================

(deftest p4-discipline-hop-preserves-session-and-pattern
  (testing "P-4: explore → discipline → reflect chain with explicit exit conditions and session continuity"
    (let [evidence-store (atom {:entries {} :order []})
          backend (tools/make-mock-backend
                    {:glob {:found ["src/futon3c/core.clj"]}
                     :psr-search {:patterns ["realtime/liveness-heartbeats"]}
                     :psr-select {:pattern-id "realtime/liveness-heartbeats"}
                     :pur-update {:status :ok}
                     :read {:content "(ns futon3c.core)"}
                     :musn-log {:lines 50}})
          result (reg/run-chain
                   {:backend backend :peripherals peripherals
                    :evidence-store evidence-store}
                   {:session-id "s-p4-discipline"}
                   [{:peripheral-id :explore
                     :actions [{:tool :glob :args ["**/*.clj"]}]
                     :stop-reason "found pattern to select"
                     :exit-condition :found-target}
                    {:peripheral-id :discipline
                     :actions [{:tool :psr-search :args ["liveness"]}
                               {:tool :psr-select :args ["realtime/liveness-heartbeats"]}
                               {:tool :pur-update :args ["realtime/liveness-heartbeats" :ok]}]
                     :stop-reason "PAR generated"
                     :exit-condition :hop-reflect}
                    {:peripheral-id :reflect
                     :actions [{:tool :read :args ["session.log"]}]
                     :stop-reason "session close"
                     :exit-condition :par-complete}])]
      ;; Chain completed successfully
      (is (true? (:ok result)))
      (is (= 3 (count (:fruits result))))

      ;; Session-id preserved across all three peripherals
      (is (= "s-p4-discipline" (get-in result [:final-context :session-id])))

      ;; Discipline fruit contains PSR/PUR records
      (let [disc-fruit (second (:fruits result))]
        (is (= 3 (count (:records disc-fruit))) "3 discipline actions")
        (is (= :realtime/liveness-heartbeats (:selected-pattern disc-fruit))))

      ;; Evidence was persisted across all three peripherals
      (let [entries (vals (:entries @evidence-store))]
        ;; 3 per peripheral (start/step/stop) × 3 peripherals + 2 extra steps in discipline = 11
        (is (>= (count entries) 9) "at least 9 evidence entries across 3 peripherals")))))

(deftest p4-exit-condition-blocks-invalid-discipline-exit
  (testing "P-4: discipline → deploy hop is blocked (deploy requires :tests-passed exit)"
    (let [backend (tools/make-mock-backend {:psr-select {:pattern-id "patterns/x"}})
          result (reg/run-chain
                   {:backend backend :peripherals peripherals}
                   {:session-id "s-p4-blocked"}
                   [{:peripheral-id :discipline
                     :actions [{:tool :psr-select :args ["patterns/x"]}]
                     :stop-reason "want to deploy"
                     :exit-condition :par-generated}
                    {:peripheral-id :deploy
                     :actions []
                     :stop-reason "ship"
                     :exit-condition :deployed}])]
      (is (false? (:ok result)) "discipline → deploy should be blocked")
      (is (shapes/valid? shapes/SocialError (:error result))))))
