(ns futon3c.dev.apm-conductor-v2-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agents.apm-work-queue :as apm-queue]
            [futon3c.dev.apm-dispatch :as dispatch]
            [futon3c.dev.apm-conductor-v2 :as conductor]
            [futon3c.dev.apm-frames :as frames]
            [futon3c.peripheral.proof-backend :as pb]
            [futon3c.agency.registry :as reg]))

(defn- cid [agent-id]
  [:apm-v2 agent-id])

(use-fixtures
  :each
  (fn [f]
    (reset! conductor/!conductor {})
    (reset! conductor/!state {})
    (reset! dispatch/!conductors {})
    (f)
    (reset! conductor/!conductor {})
    (reset! conductor/!state {})
    (reset! dispatch/!conductors {})))

(deftest solve-prompt-prefers-frame-local-paths
  (reset! conductor/!state
          {(cid "codex-1")
           {:current-problem {:id "a01J06" :subject :analysis :year 2001 :session :fall}
            :frame-workspace {:frame/workspace-root "/tmp/frame"
                              :frame/module-root "ApmCanaries.Frames.A01J06.Run1"
                              :frame/lean-root "/home/joe/code/apm-lean/ApmCanaries/Frames/A01J06/Run1"
                              :frame/shared-extension-root "/home/joe/code/apm-lean/ApmCanaries/Local"
                              :artifacts {:proof-plan "/tmp/frame/proof-plan.edn"
                                          :formal-alignment "/tmp/frame/formal-alignment.edn"
                                          :changelog "/tmp/frame/changelog.edn"
                                          :lean-main "/home/joe/code/apm-lean/ApmCanaries/Frames/A01J06/Run1/Main.lean"
                                          :lean-scratch "/home/joe/code/apm-lean/ApmCanaries/Frames/A01J06/Run1/Scratch.lean"}}}})
  (let [prompt (#'conductor/make-solve-prompt
                (cid "codex-1")
                {:id "a01J06" :subject :analysis :year 2001 :session :fall}
                "Problem body")]
    (is (str/includes? prompt "/home/joe/code/apm-lean/ApmCanaries/Frames/A01J06/Run1/Main.lean"))
    (is (str/includes? prompt "/tmp/frame/proof-plan.edn"))
    (is (str/includes? prompt "Do not use shared scratch paths like `ApmCanaries/Current.lean`"))
    (is (str/includes? prompt "/home/joe/code/proof_peripheral/worked-solutions/a02J04-full"))
    (is (str/includes? prompt ":stage-status"))
    (is (str/includes? prompt ":full-record? true"))
    (is (str/includes? prompt ":mentions-problem-objects? true"))))

(deftest start-next-problem-initializes-frame-workspace
  (let [idle-callback (atom nil)
        dispatches (atom [])
        workspace {:frame/id "apm-v2-a01J06-1"
                   :frame/workspace-root "/tmp/frame"
                   :frame/module-root "ApmCanaries.Frames.A01J06.Run1"
                   :frame/lean-root "/home/joe/code/apm-lean/ApmCanaries/Frames/A01J06/Run1"
                   :frame/shared-extension-root "/home/joe/code/apm-lean/ApmCanaries/Local"
                   :artifacts {:proof-plan "/tmp/frame/proof-plan.edn"
                               :formal-alignment "/tmp/frame/formal-alignment.edn"
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
      (is (= workspace (:frame-workspace (conductor/state-for-agent "codex-1"))))
      (is (= :target-check (:current-phase (conductor/state-for-agent "codex-1"))))
      (is (seq @dispatches))
      (is (str/includes? (first @dispatches) "You are in TARGET-CHECK."))
      (is (str/includes? (first @dispatches) "/tmp/frame/proof-plan.edn"))
      (is (str/includes? (first @dispatches) "/tmp/frame/formal-alignment.edn")))))

(deftest start-apm-conductor-v2-honors-custom-problem-timeout
  (let [idle-callback (atom nil)
        timeouts (atom [])
        workspace {:frame/id "apm-v2-a01J06-1"
                   :frame/workspace-root "/tmp/frame"
                   :frame/module-root "ApmCanaries.Frames.A01J06.Run1"
                   :frame/lean-root "/home/joe/code/apm-lean/ApmCanaries/Frames/A01J06/Run1"
                   :frame/shared-extension-root "/home/joe/code/apm-lean/ApmCanaries/Local"
                   :artifacts {:proof-plan "/tmp/frame/proof-plan.edn"
                               :formal-alignment "/tmp/frame/formal-alignment.edn"
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
      (is (= [300000] @timeouts))
      (is (= 600000 (:problem-timeout-ms (conductor/state-for-agent "codex-1")))))))

(deftest handle-target-check-return-advances-to-solve-when-artifacts-validate
  (let [tmp-dir (.toFile (java.nio.file.Files/createTempDirectory
                           "apm-v2-target-check" (make-array java.nio.file.attribute.FileAttribute 0)))
        workspace-root (.getAbsolutePath tmp-dir)
        alignment-path (.getAbsolutePath (io/file workspace-root "formal-alignment.edn"))
        plan-path (.getAbsolutePath (io/file workspace-root "proof-plan.edn"))
        changelog-path (.getAbsolutePath (io/file workspace-root "changelog.edn"))
        execute-path (.getAbsolutePath (io/file workspace-root "execute.md"))
        prompts (atom [])
        workspace {:frame/id "frame-a00J02"
                   :frame/workspace-root workspace-root
                   :frame/module-root "ApmCanaries.Frames.A00J02.Frame"
                   :frame/lean-root workspace-root
                   :frame/shared-extension-root "/home/joe/code/apm-lean/ApmCanaries/Local"
                   :artifacts {:execute-notes execute-path
                               :proof-plan plan-path
                               :formal-alignment alignment-path
                               :changelog changelog-path
                               :lean-main (.getAbsolutePath (io/file workspace-root "Main.lean"))
                               :lean-scratch (.getAbsolutePath (io/file workspace-root "Scratch.lean"))}}]
    (spit plan-path "{:goal \"Prove the claim.\" :terms [{:name \"x\" :meaning \"the main object\" :needed-because \"appears in the statement\"}] :strategy [{:id :s1 :formal-dependency \"main reduction lemma\" :informal-dependency \"reduce to a canonical estimate\" :why-this-now \"the target is direct\" :lean-target \"theorem main_target : True\" :mathlib-status \"custom\" :critical-path true}] :stage-status {:stage1 :done :stage2 :done :stage3 :pending :stage4 :pending}}\n")
    (spit alignment-path "{:main-claim {:informal-claim \"Prove the claim.\" :formal-name \"main_target\" :formal-target \"theorem main_target : True\" :sanity-check {:mentions-problem-objects? true :avoids-assuming-conclusion? true :meaningful-without-prose? true :notes \"The target theorem still states the intended claim.\"}} :alignments [{:formal-name \"main_target\" :formal-statement \"theorem main_target : True\" :informal-clause \"Main claim\" :role :main-theorem}]}\n")
    (reset! conductor/!state {(cid "codex-1")
                              {:current-problem {:id "a00J02" :subject :analysis :year 2000 :session :fall}
                               :current-phase :target-check
                               :frame-workspace workspace
                               :backend nil
                               :accumulated-output ""
                               :failure-count 0
                               :problems-done 0
                               :batch-results []
                               :target-n 1
                               :problem-timeout-ms 600000
                               :problem-start-ms (System/currentTimeMillis)
                               :dispatch-start-ms (System/currentTimeMillis)}})
    (with-redefs [conductor/log! (fn [_] nil)
                  conductor/dispatch! (fn [_ prompt _] (swap! prompts conj prompt))
                  apm-queue/load-problem-tex (fn [_] "Problem body")]
      (#'conductor/handle-target-check-return!
       (cid "codex-1") "codex-1"
       "TARGET SANITY CHECK\n- mentions-problem-objects?: yes\n- avoids-assuming-conclusion?: yes\n- meaningful-without-prose?: yes\n- notes: The target theorem still states the intended claim."
       nil)
      (is (= :solve (:current-phase (conductor/state-for-agent "codex-1"))))
      (is (= 1 (count @prompts)))
      (is (str/includes? (first @prompts) "Solve this problem completely.")))))

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
                               :formal-alignment (.getAbsolutePath (io/file workspace-root "formal-alignment.edn"))
                               :changelog changelog-path
                               :lean-main lean-main
                               :lean-scratch (.getAbsolutePath (io/file lean-dir "Scratch.lean"))}}]
    (spit lean-main "import Mathlib\n\ntheorem clean : True := by trivial\n")
    (spit execute-path "**Stage 1 — THE CLEAN PROOF**\n\n[Fill in the authoritative reader-facing proof.]\n")
    (spit plan-path "{:goal \"\" :terms [] :strategy [] :checkpoints [{:stage 1 :status :pending}]}\n")
    (spit (get-in workspace [:artifacts :formal-alignment]) "{:main-claim {:informal-claim \"\" :formal-name \"\" :formal-target \"\" :sanity-check {:mentions-problem-objects? false :avoids-assuming-conclusion? false :meaningful-without-prose? false :notes \"\"}} :alignments []}\n")
    (spit changelog-path "[{:kind :workspace-initialized :summary \"seed\"}]\n")
    (reset! conductor/!state {(cid "codex-1")
                              {:current-problem {:id "a00J02" :subject :analysis :year 2000 :session :fall}
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
                               :dispatch-start-ms (System/currentTimeMillis)}})
    (with-redefs [conductor/log! (fn [_] nil)
                  conductor/dispatch! (fn [_ prompt _] (swap! prompts conj prompt))
                  conductor/start-next-problem! (fn [& _] (throw (ex-info "should not advance" {})))
                  conductor/stop-apm-conductor-v2! (fn [& _] (throw (ex-info "should not stop" {})))]
      (#'conductor/handle-solve-return! (cid "codex-1") "codex-1" "Mathematical proof and Lean complete." nil)
      (is (= 1 (:record-kick-count (conductor/state-for-agent "codex-1"))))
      (is (zero? (:problems-done (conductor/state-for-agent "codex-1"))))
      (is (= 1 (count @prompts)))
      (is (str/includes? (first @prompts) "frame-local record for apm-a00J02 is incomplete"))
      (is (str/includes? (first @prompts) "worked-solutions/a02J04-full")))))

(deftest formalization-status-rejects-empty-or-placeholder-lean
  (let [tmp-dir (.toFile (java.nio.file.Files/createTempDirectory
                           "apm-v2-formal-status" (make-array java.nio.file.attribute.FileAttribute 0)))
        lean-main (.getAbsolutePath (io/file tmp-dir "Main.lean"))
        lean-scratch (.getAbsolutePath (io/file tmp-dir "Scratch.lean"))
        alignment {:main-claim {:informal-claim "Main claim"
                                :formal-name "main_claim"
                                :formal-target "theorem main_claim (x : ℝ) : x = x"}
                   :alignments [{:formal-name "main_claim"
                                 :formal-statement "theorem main_claim (x : ℝ) : x = x"
                                 :informal-clause "Main claim"
                                 :role :main-theorem}]}]
    (spit lean-scratch "import Mathlib\n\nnamespace Scratch\n\nend Scratch\n")
    (spit lean-main "import Mathlib\n\nnamespace Foo\n\nend Foo\n")
    (let [status (#'conductor/formalization-status [lean-main] alignment)]
      (is (false? (:meaningful? status)))
      (is (false? (:substantive? status)))
      (is (empty? (:declaration-names status))))
    (spit lean-main "import Mathlib\n\ntheorem main_claim (x : ℝ) : True := by trivial\n")
    (let [status (#'conductor/formalization-status [lean-main] alignment)]
      (is (false? (:meaningful? status)))
      (is (false? (:substantive? status)))
      (is (:main-name-present? status)))
    (spit lean-main "import Mathlib\n\ntheorem main_claim (x : ℝ) : x = x := by rfl\n")
    (let [status (#'conductor/formalization-status [lean-main] alignment)]
      (is (true? (:meaningful? status)))
      (is (true? (:substantive? status)))
      (is (:main-target-present? status))
      (is (:aligned? status)))
    (let [status (#'conductor/formalization-status [lean-main lean-scratch] alignment)]
      (is (false? (:meaningful? status)))
      (is (true? (:substantive? status)))
      (is (= #{"main_claim"} (:substantive-declaration-names status))))))

(deftest handle-solve-return-formal-kicks-when-no-theorem-and-no-trace
  (let [tmp-dir (.toFile (java.nio.file.Files/createTempDirectory
                           "apm-v2-formal-kick" (make-array java.nio.file.attribute.FileAttribute 0)))
        workspace-root (.getAbsolutePath tmp-dir)
        lean-dir (doto (io/file workspace-root "lean") .mkdirs)
        lean-main (.getAbsolutePath (io/file lean-dir "Main.lean"))
        execute-path (.getAbsolutePath (io/file workspace-root "execute.md"))
        plan-path (.getAbsolutePath (io/file workspace-root "proof-plan.edn"))
        alignment-path (.getAbsolutePath (io/file workspace-root "formal-alignment.edn"))
        changelog-path (.getAbsolutePath (io/file workspace-root "changelog.edn"))
        prompts (atom [])
        workspace {:frame/id "frame-t97J01"
                   :frame/workspace-root workspace-root
                   :frame/module-root "ApmCanaries.Frames.T97J01.Frame"
                   :frame/lean-root (.getAbsolutePath lean-dir)
                   :frame/shared-extension-root "/home/joe/code/apm-lean/ApmCanaries/Local"
                   :artifacts {:execute-notes execute-path
                               :proof-plan plan-path
                               :formal-alignment alignment-path
                               :changelog changelog-path
                               :lean-main lean-main
                               :lean-scratch (.getAbsolutePath (io/file lean-dir "Scratch.lean"))}}]
    (spit lean-main "import Mathlib\n\nnamespace Foo\n\nend Foo\n")
    (spit execute-path "**Stage 1 — THE CLEAN PROOF**\n\nA real proof.\n\n**Stage 2 — LEMMA DEPENDENCY GRAPH**\n\n1. **Main step**\n   - **Formal dependency**: theorem X.\n   - **Informal dependency**: use idea Y.\n   - **Why this becomes thinkable here**: cue Z.\n   - **Lean target/type**: `theorem main_claim (x : ℝ) : x = x`.\n   - **Mathlib status/search terms**: search `rfl`.\n   - **Critical path**: yes.\n\n**Stage 3 — LEAN FORMALIZATION**\n\nLean work done.\n\n**Stage 4 — FORMAL-TO-INFORMAL REVISION**\n\nRevision.\n")
    (spit plan-path "{:goal \"Main claim\" :terms [{:name \"x\" :meaning \"a real variable\" :needed-because \"target\"}] :strategy [{:id :s1 :formal-dependency \"theorem X\" :informal-dependency \"idea Y\" :why-this-now \"cue Z\" :lean-target \"theorem main_claim (x : ℝ) : x = x\" :mathlib-status \"existing\"}] :stage-status {:stage1 :done :stage2 :done :stage3 :in-progress :stage4 :pending}}\n")
    (spit alignment-path "{:main-claim {:informal-claim \"Main claim\" :formal-name \"main_claim\" :formal-target \"theorem main_claim (x : ℝ) : x = x\" :sanity-check {:mentions-problem-objects? true :avoids-assuming-conclusion? true :meaningful-without-prose? true :notes \"The theorem still states the real claim.\"}} :alignments [{:formal-name \"main_claim\" :formal-statement \"theorem main_claim (x : ℝ) : x = x\" :informal-clause \"Main claim\" :role :main-theorem}]}\n")
    (spit changelog-path "[{:kind :stage1-completed :summary \"Proof written\" :full-record? true :sorry? false :fully-closed? false}]\n")
    (reset! conductor/!state {(cid "codex-1")
                              {:current-problem {:id "t97J01" :subject :topology :year 1997 :session :fall}
                               :frame-workspace workspace
                               :backend nil
                               :accumulated-output ""
                               :sorry-kick-count 0
                               :record-kick-count 0
                               :formal-kick-count 0
                               :problems-done 0
                               :batch-results []
                               :target-n 1
                               :problem-timeout-ms 600000
                               :problem-start-ms (System/currentTimeMillis)
                               :dispatch-start-ms (System/currentTimeMillis)}})
    (with-redefs [conductor/log! (fn [_] nil)
                  conductor/dispatch! (fn [_ prompt _] (swap! prompts conj prompt))
                  conductor/start-next-problem! (fn [& _] (throw (ex-info "should not advance" {})))
                  conductor/stop-apm-conductor-v2! (fn [& _] (throw (ex-info "should not stop" {})))]
      (#'conductor/handle-solve-return! (cid "codex-1") "codex-1" "Real proof, no sorry in prose." nil)
      (is (= 1 (:formal-kick-count (conductor/state-for-agent "codex-1"))))
      (is (zero? (:problems-done (conductor/state-for-agent "codex-1"))))
      (is (= 1 (count @prompts)))
      (is (str/includes? (first @prompts) "does not yet certify the problem you claim to have solved"))
      (is (str/includes? (first @prompts) "formal-alignment.edn")))))

(deftest handle-solve-return-accepts-real-proof-with-alignment-warning
  (let [tmp-dir (.toFile (java.nio.file.Files/createTempDirectory
                           "apm-v2-alignment-warning" (make-array java.nio.file.attribute.FileAttribute 0)))
        workspace-root (.getAbsolutePath tmp-dir)
        lean-dir (doto (io/file workspace-root "lean") .mkdirs)
        lean-main (.getAbsolutePath (io/file lean-dir "Main.lean"))
        execute-path (.getAbsolutePath (io/file workspace-root "execute.md"))
        plan-path (.getAbsolutePath (io/file workspace-root "proof-plan.edn"))
        alignment-path (.getAbsolutePath (io/file workspace-root "formal-alignment.edn"))
        changelog-path (.getAbsolutePath (io/file workspace-root "changelog.edn"))
        logs (atom [])
        workspace {:frame/id "frame-t96J03"
                   :frame/workspace-root workspace-root
                   :frame/module-root "ApmCanaries.Frames.T96J03.Frame"
                   :frame/lean-root (.getAbsolutePath lean-dir)
                   :frame/shared-extension-root "/home/joe/code/apm-lean/ApmCanaries/Local"
                   :artifacts {:execute-notes execute-path
                               :proof-plan plan-path
                               :formal-alignment alignment-path
                               :changelog changelog-path
                               :lean-main lean-main
                               :lean-scratch (.getAbsolutePath (io/file lean-dir "Scratch.lean"))}}]
    (spit lean-main "import Mathlib\n\nlemma closest_point_exists {α : Type} [MetricSpace α] (x : α) : x = x := by rfl\n")
    (spit execute-path "**Stage 1 — THE CLEAN PROOF**\n\nA real proof.\n\n**Stage 2 — LEMMA DEPENDENCY GRAPH**\n\n1. **Main step**\n   - **Formal dependency**: theorem X.\n   - **Informal dependency**: use idea Y.\n   - **Why this becomes thinkable here**: cue Z.\n   - **Lean target/type**: `lemma closest_point_exists {α : Type} [MetricSpace α] (x : α) : x = x`.\n   - **Mathlib status/search terms**: search `rfl`.\n   - **Critical path**: yes.\n\n**Stage 3 — LEAN FORMALIZATION**\n\nClosed the main existence lemma.\n\n**Stage 4 — FORMAL-TO-INFORMAL REVISION**\n\nThe formal lemma name differs slightly; revise the prose lightly if needed.\n")
    (spit plan-path "{:goal \"Main claim\" :terms [{:name \"x\" :meaning \"a metric-space point\" :needed-because \"target\"}] :strategy [{:id :s1 :formal-dependency \"theorem X\" :informal-dependency \"idea Y\" :why-this-now \"cue Z\" :lean-target \"lemma closest_point_exists {α : Type} [MetricSpace α] (x : α) : x = x\" :mathlib-status \"existing\"}] :stage-status {:stage1 :done :stage2 :done :stage3 :done :stage4 :done}}\n")
    (spit alignment-path "{:main-claim {:informal-claim \"Main claim\" :formal-name \"exists_closest_point_closed_subset\" :formal-target \"theorem exists_closest_point_closed_subset : True\" :sanity-check {:mentions-problem-objects? true :avoids-assuming-conclusion? true :meaningful-without-prose? true :notes \"The target theorem still states the real claim.\"}} :alignments [{:formal-name \"exists_closest_point_closed_subset\" :formal-statement \"theorem exists_closest_point_closed_subset : True\" :informal-clause \"Main claim\" :role :main-theorem}]}\n")
    (spit changelog-path "[{:kind :stage1-completed :summary \"Proof written\" :full-record? true :sorry? false :fully-closed? false}\n {:kind :stage3-closed-main-lemma :summary \"Closed closest_point_exists after searching MetricSpace lemmas\" :full-record? true :sorry? false :fully-closed? true}]\n")
    (reset! conductor/!state {(cid "codex-1")
                              {:current-problem {:id "t96J03" :subject :topology :year 1996 :session :fall}
                               :frame-workspace workspace
                               :backend nil
                               :accumulated-output ""
                               :sorry-kick-count 0
                               :record-kick-count 0
                               :formal-kick-count 0
                               :problems-done 0
                               :batch-results []
                               :target-n 1
                               :problem-timeout-ms 600000
                               :problem-start-ms (System/currentTimeMillis)
                               :dispatch-start-ms (System/currentTimeMillis)}})
    (with-redefs [conductor/log! (fn [entry] (swap! logs conj entry))
                  conductor/start-next-problem! (fn [& _] (throw (ex-info "should not advance" {})))
                  conductor/stop-apm-conductor-v2! (fn [& _] nil)]
      (#'conductor/handle-solve-return! (cid "codex-1") "codex-1" "Real proof, no sorry in prose." nil)
      (is (= 1 (:problems-done (conductor/state-for-agent "codex-1"))))
      (is (= :proved (-> (conductor/state-for-agent "codex-1") :batch-results first :classification)))
      (is (some #(= :alignment-warning (:event %)) @logs))
      (is (some #(and (= :problem-complete (:event %))
                      (= "proved" (:classification %)))
                @logs)))))

(deftest handle-solve-return-accepts-attempt-trace-as-partial
  (let [tmp-dir (.toFile (java.nio.file.Files/createTempDirectory
                           "apm-v2-attempt-trace" (make-array java.nio.file.attribute.FileAttribute 0)))
        workspace-root (.getAbsolutePath tmp-dir)
        lean-dir (doto (io/file workspace-root "lean") .mkdirs)
        lean-main (.getAbsolutePath (io/file lean-dir "Main.lean"))
        execute-path (.getAbsolutePath (io/file workspace-root "execute.md"))
        plan-path (.getAbsolutePath (io/file workspace-root "proof-plan.edn"))
        alignment-path (.getAbsolutePath (io/file workspace-root "formal-alignment.edn"))
        changelog-path (.getAbsolutePath (io/file workspace-root "changelog.edn"))
        logs (atom [])
        workspace {:frame/id "frame-t97J01"
                   :frame/workspace-root workspace-root
                   :frame/module-root "ApmCanaries.Frames.T97J01.Frame"
                   :frame/lean-root (.getAbsolutePath lean-dir)
                   :frame/shared-extension-root "/home/joe/code/apm-lean/ApmCanaries/Local"
                   :artifacts {:execute-notes execute-path
                               :proof-plan plan-path
                               :formal-alignment alignment-path
                               :changelog changelog-path
                               :lean-main lean-main
                               :lean-scratch (.getAbsolutePath (io/file lean-dir "Scratch.lean"))}}]
    (spit lean-main "import Mathlib\n\nnamespace Foo\n\nend Foo\n")
    (spit execute-path "**Stage 1 — THE CLEAN PROOF**\n\nA real proof.\n\n**Stage 2 — LEMMA DEPENDENCY GRAPH**\n\n1. **Main step**\n   - **Formal dependency**: theorem X.\n   - **Informal dependency**: use idea Y.\n   - **Why this becomes thinkable here**: cue Z.\n   - **Lean target/type**: `theorem finite_cover_isHausdorff ...`.\n   - **Mathlib status/search terms**: search `covering`, `homeomorph`, `Fiber`.\n   - **Critical path**: yes.\n\n**Stage 3 — LEAN FORMALIZATION**\n\nTried to state theorem finite_cover_isHausdorff, searched Mathlib for covering/homeomorph APIs, and concluded the route is blocked because the covering-map abstractions are not present.\n\n**Stage 4 — FORMAL-TO-INFORMAL REVISION**\n\nThe informal proof stands, but the formal route is blocked by missing API.\n")
    (spit plan-path "{:goal \"Main claim\" :terms [{:name \"p\" :meaning \"covering map\" :needed-because \"target\"}] :strategy [{:id :s1 :formal-dependency \"covering map API\" :informal-dependency \"lift disjoint neighborhoods\" :why-this-now \"the target is a covering-space separation result\" :lean-target \"theorem finite_cover_isHausdorff : True\" :mathlib-status \"missing\"}] :stage-status {:stage1 :done :stage2 :done :stage3 :in-progress :stage4 :pending}}\n")
    (spit alignment-path "{:main-claim {:informal-claim \"Main claim\" :formal-name \"finite_cover_isHausdorff\" :formal-target \"theorem finite_cover_isHausdorff : True\" :sanity-check {:mentions-problem-objects? true :avoids-assuming-conclusion? true :meaningful-without-prose? true :notes \"The target theorem still states the real claim.\"}} :alignments [{:formal-name \"finite_cover_isHausdorff\" :formal-statement \"theorem finite_cover_isHausdorff : True\" :informal-clause \"Main claim\" :role :main-theorem}]}\n")
    (spit changelog-path "[{:kind :stage1-completed :summary \"Proof written\" :full-record? true :sorry? false :fully-closed? false}\n {:kind :stage3-blocked :summary \"Tried theorem finite_cover_isHausdorff; searched covering/homeomorph APIs; blocked by missing covering-map abstractions\" :full-record? true :sorry? false :fully-closed? false}]\n")
    (reset! conductor/!state {(cid "codex-1")
                              {:current-problem {:id "t97J01" :subject :topology :year 1997 :session :fall}
                               :frame-workspace workspace
                               :backend nil
                               :accumulated-output ""
                               :sorry-kick-count 0
                               :record-kick-count 0
                               :formal-kick-count 0
                               :problems-done 0
                               :batch-results []
                               :target-n 1
                               :problem-timeout-ms 600000
                               :problem-start-ms (System/currentTimeMillis)
                               :dispatch-start-ms (System/currentTimeMillis)}})
    (with-redefs [conductor/log! (fn [entry] (swap! logs conj entry))
                  conductor/start-next-problem! (fn [& _] (throw (ex-info "should not advance" {})))
                  conductor/stop-apm-conductor-v2! (fn [& _] nil)]
      (#'conductor/handle-solve-return! (cid "codex-1") "codex-1" "Real proof writeup, no Lean theorem yet." nil)
      (is (= 1 (:problems-done (conductor/state-for-agent "codex-1"))))
      (is (= :partial (-> (conductor/state-for-agent "codex-1") :batch-results first :classification)))
      (is (some #(and (= :problem-complete (:event %))
                      (= "partial" (:classification %)))
                @logs)))))

(deftest handle-solve-return-reports-substantive-partial-accurately
  (let [tmp-dir (.toFile (java.nio.file.Files/createTempDirectory
                           "apm-v2-substantive-partial" (make-array java.nio.file.attribute.FileAttribute 0)))
        workspace-root (.getAbsolutePath tmp-dir)
        lean-dir (doto (io/file workspace-root "lean") .mkdirs)
        lean-main (.getAbsolutePath (io/file lean-dir "Main.lean"))
        lean-scratch (.getAbsolutePath (io/file lean-dir "Scratch.lean"))
        execute-path (.getAbsolutePath (io/file workspace-root "execute.md"))
        plan-path (.getAbsolutePath (io/file workspace-root "proof-plan.edn"))
        alignment-path (.getAbsolutePath (io/file workspace-root "formal-alignment.edn"))
        changelog-path (.getAbsolutePath (io/file workspace-root "changelog.edn"))
        logs (atom [])
        workspace {:frame/id "frame-b94J01"
                   :frame/workspace-root workspace-root
                   :frame/module-root "ApmCanaries.Frames.B94J01.Frame"
                   :frame/lean-root (.getAbsolutePath lean-dir)
                   :frame/shared-extension-root "/home/joe/code/apm-lean/ApmCanaries/Local"
                   :artifacts {:execute-notes execute-path
                               :proof-plan plan-path
                               :formal-alignment alignment-path
                               :changelog changelog-path
                               :lean-main lean-main
                               :lean-scratch lean-scratch}}]
    (spit lean-main "import Mathlib\n\ntheorem finite_mul_subgroup_cyclic {K : Type*} [Field K] (G : Subgroup Kˣ) [Finite G] : IsCyclic G := by\n  infer_instance\n")
    (spit lean-scratch "import Mathlib\n\nnamespace Scratch\n\nend Scratch\n")
    (spit execute-path "**Stage 1 — THE CLEAN PROOF**\n\nA real proof.\n\n**Stage 2 — LEMMA DEPENDENCY GRAPH**\n\n1. **Main step**\n   - **Formal dependency**: subgroup units cyclicity.\n   - **Informal dependency**: finite subgroup argument.\n   - **Why this becomes thinkable here**: the target is about finite subgroups of Kˣ.\n   - **Lean target/type**: `theorem finite_mul_subgroup_cyclic {K : Type*} [Field K] (G : Subgroup Kˣ) [Finite G] : IsCyclic G`.\n   - **Mathlib status/search terms**: search `Subgroup.isCyclic_subgroup_units`.\n   - **Critical path**: yes.\n\n**Stage 3 — LEAN FORMALIZATION**\n\nTried the main theorem, searched Mathlib for subgroup cyclicity lemmas, and closed the finite subgroup theorem directly.\n\n**Stage 4 — FORMAL-TO-INFORMAL REVISION**\n\nThe Lean theorem certifies part (c), but the whole multipart problem is not fully aligned.\n")
    (spit plan-path "{:goal \"Solve the multipart group-theory problem.\" :terms [{:name \"finite subgroup of Kˣ\" :meaning \"a finite multiplicative subgroup\" :needed-because \"part (c)\"}] :strategy [{:id :s1 :formal-dependency \"Subgroup.isCyclic_subgroup_units\" :informal-dependency \"finite subgroup argument\" :why-this-now \"part (c) is the formalized subproblem\" :lean-target \"theorem finite_mul_subgroup_cyclic {K : Type*} [Field K] (G : Subgroup Kˣ) [Finite G] : IsCyclic G\" :mathlib-status \"available\"}] :stage-status {:stage1 :done :stage2 :done :stage3 :done :stage4 :done}}\n")
    (spit alignment-path "{:main-claim {:informal-claim \"Solve the whole multipart problem.\" :formal-name \"full_problem_main\" :formal-target \"theorem full_problem_main : True\" :sanity-check {:mentions-problem-objects? true :avoids-assuming-conclusion? true :meaningful-without-prose? true :notes \"The declared main theorem is still broader than the closed Lean subtheorem.\"}} :alignments [{:formal-name \"full_problem_main\" :formal-statement \"theorem full_problem_main : True\" :informal-clause \"Whole problem\" :role :main-theorem} {:formal-name \"finite_mul_subgroup_cyclic\" :formal-statement \"theorem finite_mul_subgroup_cyclic {K : Type*} [Field K] (G : Subgroup Kˣ) [Finite G] : IsCyclic G\" :informal-clause \"Part (c)\" :role :helper-lemma}]}\n")
    (spit changelog-path "[{:kind :stage1-completed :summary \"Proof written\" :full-record? true :sorry? false :fully-closed? false}\n {:kind :stage3-completed :summary \"Tried the full theorem, then closed the part (c) Lean theorem using subgroup cyclicity in Mathlib\" :full-record? true :sorry? false :fully-closed? true}]\n")
    (reset! conductor/!state {(cid "codex-1")
                              {:current-problem {:id "b94J01" :subject :algebra :year 1994 :session :fall}
                               :frame-workspace workspace
                               :backend nil
                               :accumulated-output ""
                               :sorry-kick-count 0
                               :record-kick-count 0
                               :formal-kick-count 0
                               :problems-done 0
                               :batch-results []
                               :target-n 1
                               :problem-timeout-ms 600000
                               :problem-start-ms (System/currentTimeMillis)
                               :dispatch-start-ms (System/currentTimeMillis)}})
    (with-redefs [conductor/log! (fn [entry] (swap! logs conj entry))
                  conductor/start-next-problem! (fn [& _] (throw (ex-info "should not advance" {})))
                  conductor/stop-apm-conductor-v2! (fn [& _] nil)]
      (#'conductor/handle-solve-return! (cid "codex-1") "codex-1" "Real proof writeup and Lean subtheorems." nil)
      (is (= 1 (:problems-done (conductor/state-for-agent "codex-1"))))
      (is (= :partial (-> (conductor/state-for-agent "codex-1") :batch-results first :classification)))
      (is (= "Recorded substantive Lean subtheorems without a fully aligned main theorem"
             (-> (conductor/state-for-agent "codex-1") :batch-results first :message)))
      (is (some #(and (= :problem-complete (:event %))
                      (= "partial" (:classification %))
                      (= "Recorded substantive Lean subtheorems without a fully aligned main theorem"
                         (:message %)))
                @logs)))))

(deftest valid-formal-alignment-rejects-target-that-assumes-conclusion
  (is (false? (apm-queue/valid-formal-alignment?
                {:main-claim {:informal-claim "Y is Hausdorff"
                              :formal-name "finiteCover_isHausdorff"
                              :formal-target "theorem finiteCover_isHausdorff {X Y : Type*} [TopologicalSpace X] [TopologicalSpace Y] [T2Space Y] : T2Space Y"
                              :sanity-check {:mentions-problem-objects? true
                                             :avoids-assuming-conclusion? true
                                             :meaningful-without-prose? true
                                             :notes "This would be wrong, because it assumes the conclusion."}}
                 :alignments [{:formal-name "finiteCover_isHausdorff"
                               :formal-statement "theorem finiteCover_isHausdorff {X Y : Type*} [TopologicalSpace X] [TopologicalSpace Y] [T2Space Y] : T2Space Y"
                               :informal-clause "Main claim"
                               :role :main-theorem}]}))))

(deftest start-apm-conductor-v2-keeps-other-agent-run-alive
  (let [idle-callback (atom nil)
        workspaces (atom [])
        invokes (atom [])]
    (with-redefs [frames/init-frame-workspace! (fn [{:keys [problem-base conductor-tag]}]
                                                 (let [root (str "/tmp/" conductor-tag "-" problem-base)]
                                                   (swap! workspaces conj [conductor-tag problem-base])
                                                   {:frame/id (str conductor-tag "-" problem-base)
                                                    :frame/workspace-root root
                                                    :frame/module-root (str "ApmCanaries.Frames." problem-base ".Run")
                                                    :frame/lean-root root
                                                    :frame/shared-extension-root "/home/joe/code/apm-lean/ApmCanaries/Local"
                                                    :artifacts {:proof-plan (str root "/proof-plan.edn")
                                                                :formal-alignment (str root "/formal-alignment.edn")
                                                                :changelog (str root "/changelog.edn")
                                                                :lean-main (str root "/Main.lean")
                                                                :lean-scratch (str root "/Scratch.lean")}}))
                  frames/emit-frame-receipt! (fn [& _] "/tmp/frame.json")
                  apm-queue/load-apm-manifest (fn [] [{:id "a00J02" :subject :analysis :year 2000 :session :fall}
                                                      {:id "a01A01" :subject :analysis :year 2001 :session :fall}])
                  apm-queue/load-problem-tex (fn [id] (str "Problem " id))
                  apm-queue/emit-apm-evidence! (fn [& _] nil)
                  pb/make-proof-backend (fn [_] ::backend)
                  reg/set-on-idle! (fn [f] (reset! idle-callback f))
                  reg/invoke-agent! (fn [agent-id prompt _]
                                      (swap! invokes conj [agent-id prompt])
                                      {:ok true :result ""})
                  conductor/log! (fn [_] nil)]
      (reset! conductor/!conductor {})
      (reset! conductor/!state {})
      (conductor/start-apm-conductor-v2! nil :agent-id "codex-1" :problem-ids ["a00J02"])
      (Thread/sleep 100)
      (conductor/start-apm-conductor-v2! nil :agent-id "claude-1" :problem-ids ["a01A01"])
      (Thread/sleep 100)
      (is (= #{"codex-1" "claude-1"} (set (map :agent-id (vals (conductor/active-conductors))))))
      (is (= "a00J02" (get-in (conductor/state-for-agent "codex-1") [:current-problem :id])))
      (is (= "a01A01" (get-in (conductor/state-for-agent "claude-1") [:current-problem :id])))
      (is (= :target-check (get-in (conductor/state-for-agent "codex-1") [:current-phase])))
      (is (= :target-check (get-in (conductor/state-for-agent "claude-1") [:current-phase])))
      (is (= #{"codex-1" "claude-1"} (set (map first @invokes)))))))

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
