(ns futon3c.wm.run4-historical-verification-test
  (:require [clojure.test :refer [deftest is]] [clojure.java.io :as io]
            [clojure.string :as str] [clojure.java.shell :as shell]
            [futon2.aif.c-fold-config :as digest]
            [futon2.aif.full-loop-runner :as full]
            [futon2.aif.repair-obligation :as repair]
            [futon3c.wm.run4-historical-action :as action]
            [futon3c.wm.run4-historical-qualification :as qualification]
            [futon3c.wm.run4-historical-verification :as v]))
(defn- tmp [] (.toFile (java.nio.file.Files/createTempDirectory "hist-v" (make-array java.nio.file.attribute.FileAttribute 0))))
(defn- write! [f x] (spit f (str (pr-str x) "\n")) f)
(defn- isolated-runner-opts [store dispatches]
  {:cohort? false
   :author "zai-2" :reviewer "codex-10" :repair-reviewer "codex-10"
   :phase-log-fn (fn [_])
   :roster-fn (fn [_] {:zai-2 {:status "idle" :invoke-ready? true}
                       :codex-10 {:status "idle" :invoke-ready? true}})
   :judge-fn (fn [_] {:judgement {:ranked-actions [] :decision {:action nil}
                                  :belief {} :belief-pre {} :observation {}
                                  :free-energy {} :prediction-errors {}
                                  :precision-state {} :micro-step-trace []}})
   :refresh-fn (fn [])
   :substrate-preflight-fn (fn [_] {:route :isolated})
   :code-state-fn (fn [] {:repo "/isolated" :git-sha "head"
                          :git-dirty? false :repo-heads {}})
   :mode-flags-fn (fn [] {})
   :version-stamp-fn identity
   :repair-open-fn #(repair/open-obligations store)
   :repair-system-record-fn #(assoc % :repair/id "isolated-followup")
   :repair-supersede-fn (fn [& _])
   :dispatch-fn (fn [& args] (swap! dispatches conj args))
   :r16-park-fn (fn [& _] {:ok true :status :parked})
   :delivery-qa-fn (fn [& _] {:morning-brief/addendum-id "isolated"})
   :queue-fn identity})
(deftest qualification-to-reviewed-awaiting-validation
  (let [root (tmp) store (doto (io/file root "store") .mkdir)
        findings (doto (io/file store "findings") .mkdir) quals (doto (io/file root "q") .mkdir)
        out (doto (io/file root "o") .mkdir)
        source (write! (io/file root "source.edn") {:source :pinned})
        finding (write! (io/file findings "repair-057.edn")
                        {:repair/id "repair-057" :repair/status :open
                         :repair/class :machine-failure :repair/schema-version 3
                         :attempt-id "repair-attempt-057-untyped-failure"})
        plan-file (io/file root "plan.edn")
        plan {:schema :wm/historical-qualification-plan-v1 :verification-id "verify-1"
              :repair-id "repair-057"
              :sources [{:path (.getPath source) :sha256 (digest/sha256 (slurp source))}]
              :checks [{:id :recovery :argv ["/bin/true"] :timeout-ms 2000}
                       {:id :exhaustion :argv ["/bin/true"] :timeout-ms 2000}]}
        _ (write! plan-file plan)
        _ (qualification/produce! {:source-root (.getPath root) :output-root (.getPath quals)
                                   :manifest-path (.getPath plan-file)
                                   :manifest-sha256 (digest/sha256 (slurp plan-file))})
        q (io/file quals "verify-1.qualification.edn")
        qsha (digest/sha256 (slurp q))
        job {:job-id "review-1" :state "done"
             :agent-id "codex-10"
             :result-summary "FULL_LOOP_REVIEW: APPROVE"
             :result (str "HISTORICAL_VERIFICATION_SHA256: " qsha)
             :execution {:executed true :tool-events 1 :command-events 1}}
        opts {:finding-root (.getPath findings) :qualification-root (.getPath quals)
              :qualification-source-root (.getPath root)
              :output-root (.getPath out) :source-repo "/home/joe/code/futon2"
              :finding-path (.getPath finding) :finding-sha256 (digest/sha256 (slurp finding))
              :qualification-path (.getPath q) :qualification-sha256 qsha
              :expected-check-ids [:recovery :exhaustion]
              :first-commit "9ab503bd61be1d63e7a24731e8e8aa285a9e44da"
              :last-commit "3bdc381e76518e69f90077397fa46495da98e61c"
              :source-head (str/trim (:out (shell/sh "git" "-C" "/home/joe/code/futon2"
                                                     "rev-parse" "HEAD")))
              :verification-id "verify-1" :author "zai-2" :reviewer "codex-10"
              :review-job-id "review-1" :review-job-reader (fn [_] job)}]
    (is (= :awaiting-validation (:state (v/admit! opts))))
    (is (false? (:repair-resolved? (v/admit! opts))))
    (let [verification-file (io/file out "verify-1.verification.edn")
          action-config {:repair-root (.getPath store) :verification-root (.getPath out)
                         :verification-path (.getPath verification-file)
                         :verification-sha256 (digest/sha256 (slurp verification-file))}
          ports (action/runner-ports action-config)
          obligation (first (repair/open-obligations (.getPath store)))
          candidate ((:historical-verification-candidate-fn ports) obligation)
          admission ((:historical-verification-execute-fn ports)
                     {:execution-identity {:kind :runner-execution
                                           :id "verification-attempt-001"}
                      :obligation obligation :candidate candidate})]
      (is (= :wm/historical-repair-admission-v1 (:schema admission)))
      (is (= :awaiting-validation (:repair/status admission)))
      (is (nil? (:repair/resolution admission)))
      (write! (io/file findings "repair-058.edn")
              {:repair/id "repair-058" :repair/status :open
               :repair/class :machine-failure :repair/schema-version 3
               :attempt-id "repair-attempt-058-untyped-failure"})
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"another stop-line"
                            (action/validate-applicable! action-config)))
      (let [dispatches (atom [])
            selected (atom nil)
            actual-candidate (:historical-verification-candidate-fn ports)
            result (full/run-opportunity!
                    (merge (isolated-runner-opts (.getPath store) dispatches)
                           ports
                           {:historical-verification-candidate-fn
                            (fn [obligation]
                              (reset! selected (:repair/id obligation))
                              (actual-candidate obligation))}))]
        (is (= "repair-058" @selected)
            "the actual runner selects the open non-environmental 058")
        (is (= "Historical candidate targets another stop-line"
               (get-in result [:data :error])))
        (is (empty? @dispatches))))
    (doseq [bad [(assoc opts :expected-check-ids [:recovery])
                 (assoc opts :expected-check-ids [:recovery :recovery])
                 (assoc opts :expected-check-ids [:recovery :foreign])
                 (assoc opts :review-job-reader (fn [_] (assoc job :state "running")))
                 (assoc opts :review-job-reader (fn [_] (assoc job :agent-id "zai-2")))
                 (assoc opts :reviewer "zai-2")]]
      (is (thrown? clojure.lang.ExceptionInfo (v/admit! bad))))
    (spit q "nil")
    (is (thrown? clojure.lang.ExceptionInfo (v/admit! opts)))))
