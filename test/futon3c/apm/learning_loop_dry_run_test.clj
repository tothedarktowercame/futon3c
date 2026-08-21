(ns futon3c.apm.learning-loop-dry-run-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.learning-loop-dry-run :as sut])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def contract
  (edn/read-string
   (slurp "holes/labs/M-apm-demonstration/frame-cycle-contract-v2.edn")))

(deftest no-dispatch-learning-loop-carries-evidence-end-to-end
  (let [dir (Files/createTempDirectory "apm-learning-dry-run"
                                       (make-array FileAttribute 0))
        result (sut/dry-run!
                {:contract contract :snapshot-path (.resolve dir "eligible.edn")
                 :candidates
                 [{:memory-id "solver-memory-1" :depositor "solver"
                   :reviewer "scribe" :review-evidence-id "review-1"
                   :attachment-status :reviewed
                   :pattern-ids ["math-formalization/dry-run"]}]})
        proof (:proof result)]
    (is (:ok result))
    (is (= (get-in proof [:snapshot :snapshot/digest])
           (get-in proof [:student-request :memory-snapshot :snapshot-digest])))
    (is (= ["solver-memory-1"]
           (get-in proof [:student-request :memory-snapshot
                          :accessible-memory-ids])))
    (is (= "future-regime-1"
           (get-in proof [:analyst-final-state :analyst/regime-proposals 0
                          :proposed-regime-id])))
    (is (= "analyst-2"
           (get-in proof [:analyst-final-state :analyst/tenure :seat])))))
