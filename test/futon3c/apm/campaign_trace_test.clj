(ns futon3c.apm.campaign-trace-test
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-trace :as sut]))

(def valid
  {:campaign-id "qualified-cycle" :manifest-hash "manifest-1"
   :contract-id "apm-complete-frame-cycle-v2"
   :phase-order [:preflight :solve :verify :promote-solver
                 :student-attempt-1 :guide-intervention-1
                 :student-attempt-2 :guide-intervention-2
                 :student-attempt-3 :scribe-reduce :close-frame]
   :steps
   (mapv (fn [ordinal [from to]]
           {:from from :to to
            :ledger-before (str "ledger-" ordinal)
            :ledger-after (str "ledger-" (inc ordinal))
            :receipt-id (str "receipt-" ordinal)
            :prior-receipt-id (when (pos? ordinal)
                                (str "receipt-" (dec ordinal)))
            :job-id (str "job-" ordinal)
            :activated-job-id (str "job-" ordinal)
            :activation-status 202
            :reactivated-job-id (str "job-" ordinal)
            :terminal-job-id (str "job-" ordinal)
            :command-own-exit 0
            :claim-persisted true :receipt-persisted true
            :resumed-job-id (str "job-" ordinal)
            :client-timeout-observed false
            :timeout-treated-as-success false
            :submission-registered true :submission-persisted true
            :submission-schema-valid true :submission-authority-derived true
            :conversation-used-as-receipt false
            :submission-job-id (str "job-" ordinal)})
         (range 11)
         (map vector
              [:registered :preflight :solve :verify :promote-solver
               :student-attempt-1 :guide-intervention-1 :student-attempt-2
               :guide-intervention-2 :student-attempt-3 :scribe-reduce]
              [:preflight :solve :verify :promote-solver :student-attempt-1
               :guide-intervention-1 :student-attempt-2 :guide-intervention-2
               :student-attempt-3 :scribe-reduce :close-frame]))
   :closed true :terminal-ledger-digest "ledger-11"
   :solver-snapshot-digest "snapshot-verified"
   :solver-snapshot-content-digest "snapshot-verified"
   :review-snapshots
   [{:ordinal 1 :snapshot-digest "snapshot-verified"
     :content-digest "snapshot-verified"}
    {:ordinal 2 :snapshot-digest "snapshot-verified"
     :content-digest "snapshot-verified"}]
   :review-passes
   [{:phase :promote-solver :ordinal 0 :verdicts [:reject :reject]}
    {:phase :guide-intervention-1 :ordinal 1 :verdicts [:approve]}
    {:phase :guide-intervention-2 :ordinal 2 :verdicts [:approve]}
    {:phase :scribe-reduce :ordinal 0 :verdicts [:reject]}]
   :snapshot-admitted-after-solve-verify true
   :snapshot-depositor "scribe-1" :snapshot-reviewer "proctor-1"
   :student-bindings
   [{:ordinal 1 :session-id "student-session-1"
     :snapshot-digest "snapshot-verified"}
    {:ordinal 2 :session-id "student-session-2"
     :snapshot-digest "snapshot-verified"}
    {:ordinal 3 :session-id "student-session-3"
     :snapshot-digest "snapshot-verified"}]
   :campaign-lanes
   [{:campaign-id "qualified-cycle" :regulator-id "regulator:qualified-cycle"
     :problem-buffer "*problem: qualified-cycle*"
     :continuation-session "controller-session-1"
     :analyst-session "analyst-session-1"
     :ledger-digest "ledger-11" :projection-ledger-digest "ledger-11"}
    {:campaign-id "parallel-cycle" :regulator-id "regulator:parallel-cycle"
     :problem-buffer "*problem: parallel-cycle*"
     :continuation-session "controller-session-2"
     :analyst-session "analyst-session-2"
     :ledger-digest "parallel-ledger" :projection-ledger-digest "parallel-ledger"}]
   :phase-receipt-ids (mapv #(str "receipt-" %) (range 11))
   :problem-outcome :solved :frame-result :closed
   :operational-observations
   {:progress [{:coordinator-enabled? true :elapsed-ms 1000
                :valid-external-wait? false
                :semantic-cursor-advanced? true
                :coordinator-disabled? false
                :first-violation-recorded? false}]
    :successor [{:predecessor-id "job-0"
                 :terminal-evidence-id "terminal-0"
                 :collection-evidence-id "collection-0"
                 :disposition "superseded"
                 :predecessor-persisted? true
                 :successor-announced-id "job-1"
                 :successor-activated-id "job-1"}]
    :delivery [{:terminal-job-id "job-1"
                :delivery-status "delivered"
                :inbox-file-created? true
                :registered-push-performed? false
                :polling-available? true}]}
   :analyst-wakes
   [{:frame-id "f1" :terminal true :ordinal 1 :series-input-version 1
     :append-only true :proposal-type nil :proposal-digest nil
     :successor-handoff false :mutates-in-flight false}
    {:frame-id "f2" :terminal true :ordinal 2 :series-input-version 2
     :append-only true :proposal-type "regime-proposal"
     :proposal-digest "proposal-digest-2"
     :successor-handoff true :mutates-in-flight false}]})

(deftest missing-lean-declared-producer-is-a-hard-error
  (let [missing (update valid :operational-observations dissoc :delivery)]
    (is (= :campaign-trace-producer-missing
           (:error/code (ex-data (try (sut/trace missing)
                                      (catch clojure.lang.ExceptionInfo e e))))))))

(deftest generated-schema-addition-breaks-unwired-adapter
  ;; Falsifiable mutation: this is the shape produced if Lean gains a kind.
  (let [schemas (conj (sut/observation-schemas)
                      {:kind "new-kind" :collection-field "newObservations"
                       :fields [{:wire-name "witness"
                                 :source-path ["witness"]}]})]
    (is (= :campaign-trace-producer-missing
           (:error/code
            (ex-data
             (try (sut/project-operational-observations
                   schemas (:operational-observations valid))
                  (catch clojure.lang.ExceptionInfo e e))))))))

(deftest operational-observations-come-from-durable-authorities
  (let [sources (sut/operational-sources-from-durable
                 {:watchdog-states
                  [{:watchdog/trace-observation
                    (first (get-in valid [:operational-observations :progress]))}]
                  :successor-states
                  [{:superseded-terminals
                    [{:trace/successor-observation
                      (first (get-in valid [:operational-observations
                                           :successor]))}]}]
                  :delivery-ledgers
                  [{:jobs {"job-1"
                           {:trace/delivery-observation
                            (first (get-in valid [:operational-observations
                                                 :delivery]))}}}]})]
    (is (= (:operational-observations valid) sources))
    (is (= #{"progressObservations" "successorObservations"
             "deliveryObservations"}
           (set (keys (sut/project-operational-observations
                       (sut/require-complete-operational-sources sources))))))))

(deftest historical-f46-f48-authorities-are-rejected-not-backfilled
  ;; Those flights predate the durable watchdog observation. Their repair
  ;; archives are useful evidence, but cannot manufacture progress evidence.
  (doseq [frame ["f46" "f48"]]
    (let [root (str "data/apm-campaigns/jit-all-open-v2/"
                    "jit-all-open-v2-" frame)
          successor-state
          (edn/read-string
           (slurp (str root "/live/student-attempt-1.edn")))
          sources (sut/operational-sources-from-durable
                   {:watchdog-states []
                    :successor-states [successor-state]
                    :delivery-ledgers []})]
      (is (= :campaign-trace-observation-absent
             (:error/code
              (ex-data
               (try (sut/require-complete-operational-sources sources)
                    (catch clojure.lang.ExceptionInfo e e)))))
          frame))))

(deftest canonical-trace-is-deterministic-and-atomically-published
  (let [directory (.toFile (java.nio.file.Files/createTempDirectory
                            "apm-trace" (make-array java.nio.file.attribute.FileAttribute 0)))
        a (java.io.File. directory "a.json")
        b (java.io.File. directory "b.json")]
    (is (:ok (sut/emit! a valid)))
    (is (:ok (sut/emit! b valid)))
    (is (= (slurp a) (slurp b)))
    (is (.contains (slurp a) "\"promote-solver\""))
    (is (= 3 (count (get (json/parse-string (slurp a))
                         "studentBindings"))))
    (is (= "snapshot-verified"
           (get (json/parse-string (slurp a))
                "solverSnapshotContentDigest")))
    (is (= [1 2]
           (mapv #(get % "ordinal")
                 (get (json/parse-string (slurp a)) "reviewSnapshots"))))
    (is (= ["reject" "reject"]
           (get-in (json/parse-string (slurp a))
                   ["reviewPasses" 0 "verdicts"])))
    (is (= 2 (count (get (json/parse-string (slurp a))
                         "campaignLanes"))))
    (is (= (json/parse-string
            (slurp "test/resources/apm-traces/valid.json"))
           (-> (apply dissoc (json/parse-string (slurp a))
                      ["progressObservations" "successorObservations"
                       "deliveryObservations"])
               (assoc "schemaVersion" 1))))))

(deftest early-statement-refuted-trace-emits-only-executed-prefix
  (let [directory (.toFile (java.nio.file.Files/createTempDirectory
                            "apm-void-trace"
                            (make-array java.nio.file.attribute.FileAttribute 0)))
        output (java.io.File. directory "void.json")
        trace (-> valid
                  (update :steps #(subvec % 0 3))
                  (update :phase-receipt-ids #(subvec % 0 3))
                  (assoc :closed false :terminal-ledger-digest "ledger-3"
                         :problem-outcome :refuted :frame-result :void
                         :void-classification :statement-refuted))]
    (is (:ok (sut/emit! output trace)))
    (is (= (json/parse-string
            (slurp "test/resources/apm-traces/early-statement-refuted.json"))
           (-> (apply dissoc (json/parse-string (slurp output))
                      ["progressObservations" "successorObservations"
                       "deliveryObservations"])
               (assoc "schemaVersion" 1))))))

(deftest durable-state-projection-does-not-invent-job-success
  (let [step (first (:steps valid))
        projected
        (sut/from-durable-state
         {:registration (select-keys valid [:campaign-id :manifest-hash
                                             :contract-id :phase-order])
          :observations
          [{:from (:from step) :to (:to step)
            :ledger-before (:ledger-before step)
            :ledger-after (:ledger-after step)
            :claim {:persisted? true}
            :receipt {:id (:receipt-id step) :prior-id nil :persisted? true}
            :job {:announced-id (:job-id step)
                  :activated-id (:activated-job-id step)
                  :activation-status (:activation-status step)
                  :reactivated-id (:reactivated-job-id step)
                  :terminal-id (:terminal-job-id step)
                  :command-own-exit (:command-own-exit step)
                  :resumed-id (:resumed-job-id step)
                  :client-timeout-observed? true
                  :timeout-treated-as-success? false
                  :submission {:registered? true :persisted? true
                               :schema-valid? true :authority-derived? true
                               :conversation-used-as-receipt? false
                               :job-id (:job-id step)}}}]
          :closed false :terminal-ledger-digest (:ledger-after step)
          :memory {:snapshot-digest "snapshot-verified"
                   :snapshot-content-digest "snapshot-verified"
                   :review-snapshots
                   [{:ordinal 1 :snapshot-digest "snapshot-verified"
                     :content-digest "snapshot-verified"}
                    {:ordinal 2 :snapshot-digest "snapshot-verified"
                     :content-digest "snapshot-verified"}]
                   :review-passes
                   [{:phase :promote-solver :ordinal 0
                     :verdicts [:reject]}]}
          :operational-observations (:operational-observations valid)})]
    (is (= true (get-in projected ["steps" 0 "clientTimeoutObserved"])))
    (is (= false (get-in projected ["steps" 0 "timeoutTreatedAsSuccess"])))
    (is (= 202 (get-in projected ["steps" 0 "activationStatus"])))))

(deftest persisted-review-passes-retain-apparatus-failures
  (let [root "data/apm-campaigns/jit-all-open-nontopology-v1"
        expected {"f32" [0 0 0 4]
                  "f33" [4 3 0 7]
                  "f34" [0 0 0 0]
                  "f35" [5 3 4 0]}]
    (doseq [[frame counts] expected]
      (let [directory (str root "/jit-all-open-nontopology-v1-" frame)
            passes (sut/review-passes-from-live directory)]
        (is (= counts
               (mapv #(count (filter #{:cannot-judge} (:verdicts %)))
                     passes))
            frame)))))
