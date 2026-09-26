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
                 ;; A prior memory carried into this frame. memory-snapshot's
                 ;; origin-valid? (2139100d) requires a provenance map whose
                 ;; frame-id matches the f<N> prefix of the depositor, so the
                 ;; depositing frame is named explicitly rather than implied.
                 [{:memory-id "solver-memory-1" :depositor "f1-solver"
                   :reviewer "scribe" :review-evidence-id "review-1"
                   :attachment-status :reviewed
                   :provenance {:campaign-id "dry-campaign"
                                :frame-id "f1"
                                :problem-id "dry-p0"}
                   :pattern-ids ["math-formalization/dry-run"]}]})
        proof (:proof result)]
    (is (:ok result))
    (is (= (get-in proof [:snapshot :snapshot/digest])
           (get-in proof [:student-request :memory-snapshot :snapshot-digest])))
    (is (= ["solver-memory-1"]
           (get-in proof [:student-request :memory-snapshot
                          :accessible-memory-ids])))
    (is (= {:receipt/independent-review? true
            :receipt/independence :asserted-unverified}
           (select-keys (:promotion proof)
                        [:receipt/independent-review?
                         :receipt/independence])))
    (is (= "future-regime-1"
           (get-in proof [:analyst-final-state :analyst/regime-proposals 0
                          :proposed-regime-id])))
    (is (= "analyst-2"
           (get-in proof [:analyst-final-state :analyst/tenure :seat])))))
