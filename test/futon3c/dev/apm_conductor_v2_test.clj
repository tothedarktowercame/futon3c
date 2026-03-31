(ns futon3c.dev.apm-conductor-v2-test
  (:require [clojure.string :as str]
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
    (is (str/includes? prompt "Do not use shared scratch paths like `ApmCanaries/Current.lean`"))))

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
