(ns futon3c.social.dispatch-realbackend-test
  "M-futon3-last-mile Seam 1+2: end-to-end integration tests with RealBackend.

   These tests verify the full dispatch → peripheral → tool execution → evidence
   path using a real backend that reads actual files from a temp directory.
   Unlike dispatch_integration_test.clj (which uses MockBackend), these tests
   confirm that tool execution produces real results and that evidence entries
   land in the evidence store with correct reply chains."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.io :as io]
            [futon3c.agency.registry :as reg]
            [futon3c.evidence.store :as estore]
            [futon3c.evidence.threads :as threads]
            [futon3c.peripheral.real-backend :as rb]
            [futon3c.peripheral.registry :as preg]
            [futon3c.social.authenticate :as auth]
            [futon3c.social.dispatch :as dispatch]
            [futon3c.social.mode :as mode]
            [futon3c.social.persist :as persist]
            [futon3c.social.presence :as presence]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix])
  (:import [java.io File]
           [java.time Instant]
           [java.util UUID]))

(use-fixtures
  :each
  (fn [f]
    (reg/reset-registry!)
    (persist/reset-sessions!)
    (f)))

;; =============================================================================
;; Hermetic temp directory with seed files
;; =============================================================================

(defn- seed-temp-dir!
  "Create a temp dir with known test files. Returns {:dir path :files {name path}}."
  []
  (let [dir (fix/temp-dir!)
        hello-file (io/file dir "hello.txt")
        src-dir (io/file dir "src")
        clj-file (io/file src-dir "core.clj")]
    (.mkdirs src-dir)
    (spit hello-file "Hello from the real backend!")
    (spit clj-file "(ns core)\n(defn greet [] \"hello\")\n")
    {:dir dir
     :files {"hello.txt" (.getAbsolutePath hello-file)
             "src/core.clj" (.getAbsolutePath clj-file)}}))

(defn- cleanup-temp-dir! [dir]
  (doseq [f (reverse (file-seq (io/file dir)))]
    (.delete ^File f)))

;; =============================================================================
;; Factory: real peripheral config
;; =============================================================================

(defn- make-real-peripheral-config
  "Create a peripheral config with RealBackend pointed at the given cwd."
  [cwd evidence-store]
  (let [backend (rb/make-real-backend {:cwd cwd
                                       :evidence-store evidence-store
                                       :timeout-ms 10000})]
    {:backend backend
     :peripherals (preg/load-peripherals)
     :evidence-store evidence-store}))

(defn- make-real-registry
  "Build a registry with RealBackend peripheral config."
  [cwd evidence-store]
  (fix/mock-registry
   {:peripheral-config (make-real-peripheral-config cwd evidence-store)}))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- setup-agent!
  [id-value agent-type]
  (let [typed-id (fix/make-agent-id id-value :continuity)]
    (reg/register-agent!
     {:agent-id typed-id
      :type agent-type
      :invoke-fn (fn [_prompt _session-id] {:result "ok" :session-id nil :exit-code 0})
      :capabilities [:explore :edit :test]})
    typed-id))

(defn- dispatch-action-message!
  "Create and dispatch an action-mode message with the given payload."
  [agent-id-value payload registry patterns]
  (let [connection (fix/make-connection
                    {:conn/agent-id (fix/make-agent-id agent-id-value :transport)
                     :conn/metadata {:ready true}})
        presence-result (presence/verify connection registry)]
    (if (shapes/valid? shapes/SocialError presence-result)
      presence-result
      (let [identity-result (auth/resolve-identity presence-result registry)
            agent-id (fix/make-agent-id agent-id-value :continuity)
            message {:msg/id (str "msg-" (UUID/randomUUID))
                     :msg/payload payload
                     :msg/from (:identity/agent-id identity-result)
                     :msg/to agent-id
                     :msg/at (fix/now-str)}
            classified (mode/classify message patterns)]
        (dispatch/dispatch classified registry)))))

;; =============================================================================
;; 1. Real file read through explore peripheral
;; =============================================================================

(deftest real-backend-read-through-explore-peripheral
  (testing "action message with :read action → RealBackend reads actual file"
    (let [{:keys [dir files]} (seed-temp-dir!)
          evidence-store (atom {:entries {} :order []})]
      (try
        (let [registry (make-real-registry dir evidence-store)
              patterns (fix/mock-patterns)
              _ (setup-agent! "claude-1" :claude)
              hello-path (get files "hello.txt")
              result (dispatch-action-message!
                      "claude-1"
                      {:actions [{:tool :read :args [hello-path]}]}
                      registry patterns)]
          ;; Valid receipt
          (fix/assert-valid! shapes/DispatchReceipt result)
          (is (true? (:receipt/delivered? result)))
          (is (= "peripheral/run-chain" (:receipt/route result)))
          (is (= :explore (:receipt/peripheral-id result)))

          ;; Fruit present (explore produces fruit on stop)
          (is (some? (:receipt/fruit result)))
          (let [fruit (:receipt/fruit result)]
            (is (string? (:summary fruit)))
            ;; discover-targets extracts path-like strings from tool results;
            ;; a plain text file yields the content string, not the path —
            ;; so we just verify the fruit structure is correct
            (is (sequential? (:found fruit)))))
        (finally
          (cleanup-temp-dir! dir))))))

;; =============================================================================
;; 2. Real glob through explore peripheral
;; =============================================================================

(deftest real-backend-glob-through-explore-peripheral
  (testing "action message with :glob action → RealBackend finds real files"
    (let [{:keys [dir files]} (seed-temp-dir!)
          evidence-store (atom {:entries {} :order []})]
      (try
        (let [registry (make-real-registry dir evidence-store)
              patterns (fix/mock-patterns)
              _ (setup-agent! "claude-1" :claude)
              result (dispatch-action-message!
                      "claude-1"
                      {:actions [{:tool :glob :args ["*.txt"]}]}
                      registry patterns)]
          (fix/assert-valid! shapes/DispatchReceipt result)
          (is (true? (:receipt/delivered? result)))
          (is (= :explore (:receipt/peripheral-id result))))
        (finally
          (cleanup-temp-dir! dir))))))

;; =============================================================================
;; 3. Evidence chain completeness with real tool execution
;; =============================================================================

(deftest real-backend-evidence-chain-complete
  (testing "real tool execution produces complete evidence chain (dispatch root → start → step → stop)"
    (let [{:keys [dir files]} (seed-temp-dir!)
          evidence-store (atom {:entries {} :order []})]
      (try
        (let [registry (make-real-registry dir evidence-store)
              patterns (fix/mock-patterns)
              _ (setup-agent! "claude-1" :claude)
              hello-path (get files "hello.txt")
              result (dispatch-action-message!
                      "claude-1"
                      {:actions [{:tool :read :args [hello-path]}]}
                      registry patterns)
              session-id (:receipt/session-id result)
              subj {:ref/type :session :ref/id session-id}
              all-entries (estore/query* evidence-store {:query/subject subj})]

          ;; Expect at least 4 entries: dispatch-root + start + step + stop
          (is (>= (count all-entries) 4)
              (str "Expected >= 4 evidence entries, got " (count all-entries)))

          ;; Dispatch root entry (from S-dispatch)
          (let [roots (filter #(= [:dispatch :session-start] (:evidence/tags %)) all-entries)]
            (is (= 1 (count roots)) "exactly one dispatch root entry"))

          ;; Peripheral entries: start (goal), step, stop (conclusion)
          (let [periph (remove #(= [:dispatch :session-start] (:evidence/tags %)) all-entries)
                by-claim (group-by :evidence/claim-type periph)]
            (is (= 1 (count (:goal by-claim))) "one start/goal entry")
            (is (= 1 (count (:step by-claim))) "one step entry for the :read action")
            (is (= 1 (count (:conclusion by-claim))) "one stop/conclusion entry")

            ;; Step entry has real tool data
            (let [step-entry (first (:step by-claim))
                  body (:evidence/body step-entry)]
              (is (= :read (:tool body)) "step recorded :read tool")
              (is (= [hello-path] (:args body)) "step recorded correct args")
              ;; The result contains the actual file content
              (is (string? (:result body)) "step has a string result")
              (is (.contains ^String (:result body) "Hello from the real backend!")
                  "step result contains actual file content")))

          ;; Thread projection is valid
          (let [tp (threads/project-thread evidence-store subj)]
            (is (some? tp) "thread projection exists")
            (is (= :goal (:evidence/claim-type (:thread/goal tp)))
                "thread root is the goal entry")

            ;; Monotonic timestamps
            (let [instants (map #(Instant/parse (:evidence/at %)) (:thread/entries tp))]
              (is (= instants (sort instants)) "timestamps are non-decreasing"))

            ;; Reply chain integrity — every in-reply-to references an entry in the thread
            (let [id-set (set (map :evidence/id (:thread/entries tp)))]
              (doseq [e (:thread/entries tp)]
                (when-let [parent (:evidence/in-reply-to e)]
                  (is (contains? id-set parent)
                      (str (:evidence/id e) " references missing " parent)))))))
        (finally
          (cleanup-temp-dir! dir))))))

;; =============================================================================
;; 4. Multiple actions in a single dispatch
;; =============================================================================

(deftest real-backend-multiple-actions-single-dispatch
  (testing "dispatch with multiple actions → all execute and produce evidence"
    (let [{:keys [dir files]} (seed-temp-dir!)
          evidence-store (atom {:entries {} :order []})]
      (try
        (let [registry (make-real-registry dir evidence-store)
              patterns (fix/mock-patterns)
              _ (setup-agent! "claude-1" :claude)
              hello-path (get files "hello.txt")
              clj-path (get files "src/core.clj")
              result (dispatch-action-message!
                      "claude-1"
                      {:actions [{:tool :read :args [hello-path]}
                                 {:tool :read :args [clj-path]}
                                 {:tool :glob :args ["*.txt"]}]}
                      registry patterns)
              session-id (:receipt/session-id result)
              subj {:ref/type :session :ref/id session-id}
              all-entries (estore/query* evidence-store {:query/subject subj})
              step-entries (filter #(= :step (:evidence/claim-type %)) all-entries)]

          (fix/assert-valid! shapes/DispatchReceipt result)
          ;; Three step entries (one per action)
          (is (= 3 (count step-entries))
              (str "Expected 3 step entries, got " (count step-entries)))

          ;; Each step recorded its tool
          (let [tools (set (map #(get-in % [:evidence/body :tool]) step-entries))]
            (is (contains? tools :read) ":read tool recorded")
            (is (contains? tools :glob) ":glob tool recorded"))

          ;; Fruit includes files from all actions
          (let [found (get-in result [:receipt/fruit :found])]
            (is (>= (count found) 2)
                (str "Expected at least 2 found files, got " (count found)))))
        (finally
          (cleanup-temp-dir! dir))))))

;; =============================================================================
;; 5. Evidence entries are individually shape-valid
;; =============================================================================

(deftest real-backend-evidence-entries-shape-valid
  (testing "every evidence entry from real tool execution conforms to EvidenceEntry shape"
    (let [{:keys [dir files]} (seed-temp-dir!)
          evidence-store (atom {:entries {} :order []})]
      (try
        (let [registry (make-real-registry dir evidence-store)
              patterns (fix/mock-patterns)
              _ (setup-agent! "claude-1" :claude)
              hello-path (get files "hello.txt")
              _ (dispatch-action-message!
                 "claude-1"
                 {:actions [{:tool :read :args [hello-path]}]}
                 registry patterns)
              all-entries (vals (:entries @evidence-store))]
          (is (>= (count all-entries) 4)
              (str "Expected >= 4 entries, got " (count all-entries)))
          (doseq [e all-entries]
            (is (shapes/valid? shapes/EvidenceEntry e)
                (str "Invalid evidence entry: " (:evidence/id e)
                     " " (shapes/validate shapes/EvidenceEntry e)))))
        (finally
          (cleanup-temp-dir! dir))))))

;; =============================================================================
;; 6. Codex agent → edit peripheral with real backend
;; =============================================================================

(deftest codex-real-backend-routes-to-edit-peripheral
  (testing "codex agent with real backend → :edit peripheral, can read files in scope"
    (let [{:keys [dir]} (seed-temp-dir!)
          evidence-store (atom {:entries {} :order []})]
      (try
        (let [registry (make-real-registry dir evidence-store)
              patterns (fix/mock-patterns)
              _ (setup-agent! "codex-1" :codex)
              ;; Edit peripheral scope is {:paths ["src/" "docs/" "scripts/"]}
              ;; so use a relative path starting with src/
              result (dispatch-action-message!
                      "codex-1"
                      {:actions [{:tool :read :args ["src/core.clj"]}]}
                      registry patterns)]
          (fix/assert-valid! shapes/DispatchReceipt result)
          (is (= :edit (:receipt/peripheral-id result)))
          (is (= "peripheral/run-chain" (:receipt/route result)))
          (is (some? (:receipt/fruit result))))
        (finally
          (cleanup-temp-dir! dir))))))

;; =============================================================================
;; 7. Tri-agent concurrent session (Seam 5)
;; =============================================================================

(deftest tri-agent-concurrent-peripheral-sessions
  (testing "three agents (Claude/Codex/Tickle) run concurrently through their default peripherals"
    (let [{:keys [dir files]} (seed-temp-dir!)
          evidence-store (atom {:entries {} :order []})]
      (try
        (let [;; Build registry including all three agent types
              registry (fix/mock-registry
                        {:agents {"claude-1" {:capabilities [:explore :edit :test] :type :claude}
                                  "codex-1"  {:capabilities [:edit :test] :type :codex}
                                  "tickle-1" {:capabilities [:mission-control :discipline] :type :tickle}}
                         :peripheral-config (make-real-peripheral-config dir evidence-store)})
              patterns (fix/mock-patterns)
              ;; Register all three agents in the live registry
              _ (setup-agent! "claude-1" :claude)
              _ (setup-agent! "codex-1" :codex)
              _ (do (reg/register-agent!
                     {:agent-id (fix/make-agent-id "tickle-1" :continuity)
                      :type :tickle
                      :invoke-fn (fn [_ _] {:result "ok" :session-id nil :exit-code 0})
                      :capabilities [:mission-control :discipline :coordination/execute]})
                    (fix/make-agent-id "tickle-1" :continuity))
              hello-path (get files "hello.txt")

              ;; Run all three dispatches concurrently
              f-claude (future
                         (dispatch-action-message!
                          "claude-1"
                          {:actions [{:tool :read :args [hello-path]}]}
                          registry patterns))
              f-codex (future
                        (dispatch-action-message!
                         "codex-1"
                         {:actions [{:tool :read :args ["src/core.clj"]}]}
                         registry patterns))
              f-tickle (future
                         (dispatch-action-message!
                          "tickle-1"
                          {:actions [{:tool :mc-bulletin :args ["portfolio check-in"]}]}
                          registry patterns))

              ;; Collect results
              r-claude @f-claude
              r-codex @f-codex
              r-tickle @f-tickle]

          ;; All three produce valid receipts
          (fix/assert-valid! shapes/DispatchReceipt r-claude)
          (fix/assert-valid! shapes/DispatchReceipt r-codex)
          (fix/assert-valid! shapes/DispatchReceipt r-tickle)

          ;; Each routed to the correct peripheral
          (is (= :explore (:receipt/peripheral-id r-claude))
              "Claude → explore")
          (is (= :edit (:receipt/peripheral-id r-codex))
              "Codex → edit")
          (is (= :mission-control (:receipt/peripheral-id r-tickle))
              "Tickle → mission-control")

          ;; Each has a distinct session
          (let [sessions #{(:receipt/session-id r-claude)
                           (:receipt/session-id r-codex)
                           (:receipt/session-id r-tickle)}]
            (is (= 3 (count sessions))
                "three distinct session IDs"))

          ;; Evidence from all three sessions exists in the store
          (let [all-entries (vals (:entries @evidence-store))]
            ;; At least 3 agents × (dispatch root + start + stop) = 9 entries minimum
            (is (>= (count all-entries) 9)
                (str "Expected >= 9 total evidence entries, got " (count all-entries)))

            ;; Each session has evidence
            (doseq [[label result] [["claude" r-claude]
                                     ["codex" r-codex]
                                     ["tickle" r-tickle]]]
              (let [sid (:receipt/session-id result)
                    session-entries (filter #(= sid (:evidence/session-id %)) all-entries)]
                (is (>= (count session-entries) 3)
                    (str label " session should have >= 3 evidence entries, got "
                         (count session-entries)))))))
        (finally
          (cleanup-temp-dir! dir))))))
