(ns futon3c.apm.campaign-trace-test
  (:require [cheshire.core :as json]
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
            :timeout-treated-as-success false})
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
     :ledger-digest "parallel-ledger" :projection-ledger-digest "parallel-ledger"}]})

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
    (is (= 2 (count (get (json/parse-string (slurp a))
                         "campaignLanes"))))
    (is (= (json/parse-string
            (slurp "test/resources/apm-traces/valid.json"))
           (json/parse-string (slurp a))))))

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
                  :timeout-treated-as-success? false}}]
          :closed false :terminal-ledger-digest (:ledger-after step)})]
    (is (= true (get-in projected ["steps" 0 "clientTimeoutObserved"])))
    (is (= false (get-in projected ["steps" 0 "timeoutTreatedAsSuccess"])))
    (is (= 202 (get-in projected ["steps" 0 "activationStatus"])))))
