(ns futon3c.dev.apm-conductor-v2-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.agents.apm-work-queue :as apm-queue]
            [futon3c.dev.apm-conductor-v2 :as conductor]
            [futon3c.dev.apm-frames :as frames]
            [futon3c.peripheral.proof-backend :as pb]
            [futon3c.agency.registry :as reg]))

(deftest solve-prompt-prefers-frame-local-paths
  (reset! conductor/!state
          {:current-problem {:id "a01J06" :subject :analysis :year 2001 :session :fall}
           :frame-workspace {:frame/workspace-root "/tmp/frame"
                             :frame/module-root "ApmCanaries.Frames.A01J06.Run1"
                             :frame/lean-root "/home/joe/code/apm-lean/ApmCanaries/Frames/A01J06/Run1"
                             :frame/shared-extension-root "/home/joe/code/apm-lean/ApmCanaries/Local"
                             :artifacts {:proof-plan "/tmp/frame/proof-plan.edn"
                                         :changelog "/tmp/frame/changelog.edn"
                                         :lean-main "/home/joe/code/apm-lean/ApmCanaries/Frames/A01J06/Run1/Main.lean"
                                         :lean-scratch "/home/joe/code/apm-lean/ApmCanaries/Frames/A01J06/Run1/Scratch.lean"}}})
  (let [prompt (#'conductor/make-solve-prompt
                {:id "a01J06" :subject :analysis :year 2001 :session :fall}
                "Problem body")]
    (is (str/includes? prompt "/home/joe/code/apm-lean/ApmCanaries/Frames/A01J06/Run1/Main.lean"))
    (is (str/includes? prompt "/tmp/frame/proof-plan.edn"))
    (is (str/includes? prompt "Do not use shared scratch paths like `ApmCanaries/Current.lean`"))
    (is (str/includes? prompt "/home/joe/code/proof_peripheral/worked-solutions/a02J04-full"))
    (is (str/includes? prompt ":stage-status"))
    (is (str/includes? prompt ":full-record? true"))))

(deftest start-next-problem-initializes-frame-workspace
  (let [idle-callback (atom nil)
        dispatches (atom [])
        workspace {:frame/id "apm-v2-a01J06-1"
                   :frame/workspace-root "/tmp/frame"
                   :frame/module-root "ApmCanaries.Frames.A01J06.Run1"
                   :frame/lean-root "/home/joe/code/apm-lean/ApmCanaries/Frames/A01J06/Run1"
                   :frame/shared-extension-root "/home/joe/code/apm-lean/ApmCanaries/Local"
                   :artifacts {:proof-plan "/tmp/frame/proof-plan.edn"
                               :changelog "/tmp/frame/changelog.edn"
                               :lean-main "/home/joe/code/apm-lean/ApmCanaries/Frames/A01J06/Run1/Main.lean"
                               :lean-scratch "/home/joe/code/apm-lean/ApmCanaries/Frames/A01J06/Run1/Scratch.lean"
                               :workspace-metadata "/tmp/frame/workspace.json"}}]
    (with-redefs [frames/init-frame-workspace! (fn [_] workspace)
                  frames/emit-frame-receipt! (fn [& _] "/tmp/frame.json")
                  apm-queue/load-apm-manifest (fn [] [{:id "a01J06" :subject :analysis :year 2001 :session :fall}])
                  apm-queue/load-problem-tex (fn [_] "Problem body")
                  apm-queue/emit-apm-evidence! (fn [& _] nil)
                  pb/make-proof-backend (fn [_] ::backend)
                  reg/set-on-idle! (fn [f] (reset! idle-callback f))
                  reg/invoke-agent! (fn [_ prompt _]
                                      (swap! dispatches conj prompt)
                                      {:ok true :result ""})
                  conductor/log! (fn [_] nil)]
      (conductor/start-apm-conductor-v2! nil :agent-id "codex-1" :problem-ids ["a01J06"])
      (Thread/sleep 100)
      (is (= workspace (:frame-workspace @conductor/!state)))
      (is (seq @dispatches))
      (is (str/includes? (first @dispatches) "/home/joe/code/apm-lean/ApmCanaries/Frames/A01J06/Run1/Main.lean")))))

(deftest start-apm-conductor-v2-honors-custom-problem-timeout
  (let [idle-callback (atom nil)
        timeouts (atom [])
        workspace {:frame/id "apm-v2-a01J06-1"
                   :frame/workspace-root "/tmp/frame"
                   :frame/module-root "ApmCanaries.Frames.A01J06.Run1"
                   :frame/lean-root "/home/joe/code/apm-lean/ApmCanaries/Frames/A01J06/Run1"
                   :frame/shared-extension-root "/home/joe/code/apm-lean/ApmCanaries/Local"
                   :artifacts {:proof-plan "/tmp/frame/proof-plan.edn"
                               :changelog "/tmp/frame/changelog.edn"
                               :lean-main "/home/joe/code/apm-lean/ApmCanaries/Frames/A01J06/Run1/Main.lean"
                               :lean-scratch "/home/joe/code/apm-lean/ApmCanaries/Frames/A01J06/Run1/Scratch.lean"
                               :workspace-metadata "/tmp/frame/workspace.json"}}]
    (with-redefs [frames/init-frame-workspace! (fn [_] workspace)
                  frames/emit-frame-receipt! (fn [& _] "/tmp/frame.json")
                  apm-queue/load-apm-manifest (fn [] [{:id "a01J06" :subject :analysis :year 2001 :session :fall}])
                  apm-queue/load-problem-tex (fn [_] "Problem body")
                  apm-queue/emit-apm-evidence! (fn [& _] nil)
                  pb/make-proof-backend (fn [_] ::backend)
                  reg/set-on-idle! (fn [f] (reset! idle-callback f))
                  reg/invoke-agent! (fn [_ _ timeout-ms]
                                      (swap! timeouts conj timeout-ms)
                                      {:ok true :result ""})
                  conductor/log! (fn [_] nil)]
      (conductor/start-apm-conductor-v2! nil
                                         :agent-id "codex-1"
                                         :problem-ids ["a01J06"]
                                         :problem-timeout-ms 600000)
      (Thread/sleep 100)
      (is (= [600000] @timeouts))
      (is (= 600000 (:problem-timeout-ms @conductor/!state))))))

(deftest handle-solve-return-record-kicks-when-frame-record-is-placeholder
  (let [tmp-dir (.toFile (java.nio.file.Files/createTempDirectory
                           "apm-v2-record-kick" (make-array java.nio.file.attribute.FileAttribute 0)))
        workspace-root (.getAbsolutePath tmp-dir)
        lean-dir (doto (io/file workspace-root "lean") .mkdirs)
        lean-main (.getAbsolutePath (io/file lean-dir "Main.lean"))
        execute-path (.getAbsolutePath (io/file workspace-root "execute.md"))
        plan-path (.getAbsolutePath (io/file workspace-root "proof-plan.edn"))
        changelog-path (.getAbsolutePath (io/file workspace-root "changelog.edn"))
        prompts (atom [])
        workspace {:frame/id "frame-a00J02"
                   :frame/workspace-root workspace-root
                   :frame/module-root "ApmCanaries.Frames.A00J02.Frame"
                   :frame/lean-root (.getAbsolutePath lean-dir)
                   :frame/shared-extension-root "/home/joe/code/apm-lean/ApmCanaries/Local"
                   :artifacts {:execute-notes execute-path
                               :proof-plan plan-path
                               :changelog changelog-path
                               :lean-main lean-main
                               :lean-scratch (.getAbsolutePath (io/file lean-dir "Scratch.lean"))}}]
    (spit lean-main "import Mathlib\n\ntheorem clean : True := by trivial\n")
    (spit execute-path "**Stage 1 — THE CLEAN PROOF**\n\n[Fill in the authoritative reader-facing proof.]\n")
    (spit plan-path "{:goal \"\" :terms [] :strategy [] :checkpoints [{:stage 1 :status :pending}]}\n")
    (spit changelog-path "[{:kind :workspace-initialized :summary \"seed\"}]\n")
    (reset! conductor/!state {:current-problem {:id "a00J02" :subject :analysis :year 2000 :session :fall}
                              :frame-workspace workspace
                              :backend nil
                              :accumulated-output ""
                              :sorry-kick-count 0
                              :record-kick-count 0
                              :problems-done 0
                              :batch-results []
                              :target-n 1
                              :problem-timeout-ms 600000
                              :problem-start-ms (System/currentTimeMillis)
                              :dispatch-start-ms (System/currentTimeMillis)})
    (with-redefs [conductor/log! (fn [_] nil)
                  conductor/dispatch! (fn [_ prompt _] (swap! prompts conj prompt))
                  conductor/start-next-problem! (fn [& _] (throw (ex-info "should not advance" {})))
                  conductor/stop-apm-conductor-v2! (fn [& _] (throw (ex-info "should not stop" {})))]
      (#'conductor/handle-solve-return! "codex-1" "Mathematical proof and Lean complete." nil)
      (is (= 1 (:record-kick-count @conductor/!state)))
      (is (zero? (:problems-done @conductor/!state)))
      (is (= 1 (count @prompts)))
      (is (str/includes? (first @prompts) "frame-local record for apm-a00J02 is incomplete"))
      (is (str/includes? (first @prompts) "worked-solutions/a02J04-full")))))

(deftest extraction-handles-numbered-heading-format
  (let [output "## 1. Why it's hard\n\nStudents try DCT but no dominating function.\n\n## 2. The key insight\n\nDensity of simple functions.\n\n## 3. A complete proof\n\nLet f be in L1. Done.\n\n## 4. Lean 4 formalization\n\n```lean\ntheorem main : True := by trivial\n```\n\n## 5. What connects\n\n**Connections:**\nRiemann-Lebesgue.\n\n**Classification**: proved.\n\n**Confidence Inversion**: Simpler than expected.\n\n### ArSE Questions\n\n1. *Why is this hard?*\n   **Q:** Why not DCT?\n   **A:** No dominating function.\n2. *What is the key insight?*\n   **Q:** What unlocks it?\n   **A:** Density.\n"
        extraction (#'conductor/run-extraction output)]
    (is (= 6 (:phase-count extraction)) "should find all 6 phases from numbered headings")
    (is (some? (get-in extraction [:phases :observe])))
    (is (some? (get-in extraction [:phases :execute])))
    (is (some? (get-in extraction [:phases :integrate])))
    (is (= :proved (:classification extraction)))
    (is (= 2 (:arse-count extraction)))))

(deftest triage-parser-tolerates-common-formatting-variants
  (testing "bullets, colons, and uppercase lane names still parse"
    (is (= {:id "a01J01" :lane :quick :reason "Schur test is standard"}
           (#'conductor/parse-triage-line "- a01J01 QUICK: Schur test is standard")))
    (is (= {:id "a02J04" :lane :hard :reason "needs custom decomposition lemma"}
           (#'conductor/parse-triage-line "1. a02J04 hard - needs custom decomposition lemma")))))

(deftest triage-summary-reports-against-full-manifest
  (is (= {:quick 1 :medium 1 :hard 0 :unclassified 1 :total 3}
         (conductor/triage-summary
           {"a01J01" {:lane :quick :reason "x"}
            "a01J02" {:lane :medium :reason "y"}}
           [{:id "a01J01"} {:id "a01J02"} {:id "a01J03"}]))))

(deftest unclassified-problem-ids-excludes-existing-results
  (with-redefs [conductor/load-triage (fn [] {"a01J01" {:lane :quick :reason "done"}})
                apm-queue/load-apm-manifest (fn [] [{:id "a01J01"} {:id "a01J02"} {:id "a01J03"}])]
    (is (= ["a01J02" "a01J03"]
           (conductor/unclassified-problem-ids)))))

(deftest run-triage-retries-missing-subset-and-preserves-existing-results
  (let [responses (atom [{:ok true :result "- a01J02 MEDIUM: first cut\n"}
                         {:ok true :result "a01J03 hard Needs custom API"}])
        saved (atom nil)
        logs (atom [])]
    (with-redefs [apm-queue/load-apm-manifest (fn [] [{:id "a01J01" :subject :analysis :year 2001}
                                                      {:id "a01J02" :subject :analysis :year 2001}
                                                      {:id "a01J03" :subject :analysis :year 2001}])
                  apm-queue/load-problem-tex (fn [id] (str "Problem " id))
                  conductor/load-triage (fn [] {"a01J01" {:lane :quick :reason "already done"}})
                  conductor/save-triage! (fn [m] (reset! saved m) m)
                  conductor/log! (fn [entry] (swap! logs conj entry))
                  reg/invoke-agent! (fn [_agent _prompt _timeout]
                                      (let [resp (first @responses)]
                                        (swap! responses rest)
                                        resp))]
      (let [result (conductor/run-triage! :agent-id "codex-1" :batch-size 10)]
        (is (= {:lane :quick :reason "already done"} (get result "a01J01")))
        (is (= {:lane :medium :reason "first cut"} (get result "a01J02")))
        (is (= {:lane :hard :reason "Needs custom API"} (get result "a01J03")))
        (is (= result @saved))
        (is (= 2 (count (filter #(= :triage-batch-complete (:event %)) @logs))))
        (is (= 1 (count (filter #(= :triage-batch-retry (:event %)) @logs))))))))
