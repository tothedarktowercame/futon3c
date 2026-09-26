(ns futon3c.apm.checked-handoff-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.apm.checked-handoff :as checked-handoff]))

;; LIVE-PIN: both strings occur verbatim as :depositor and :reviewer in
;; data/apm-campaigns/jit-all-open-v2/jit-all-open-v2-f75/live/promote-solver.edn.
(def worker-seat "f75-scribe")
(def adjudicator-seat "f75-promotion-proctor")
(def proposal {:ref "e-apm-promotion-f75-fixture"})

(defn- event [author-seat adjudication]
  {:event :checked-handoff/verdict
   :worker-seat worker-seat
   :author-seat author-seat
   :proposal proposal
   :verdict :approved
   :adjudication adjudication})

(deftest worker-authored-is-refused-and-adjudicator-authored-is-accepted
  (let [resolve-none (constantly nil)
        refused (checked-handoff/validate-verdict-event
                 (event worker-seat {:rerun-witness :absent}) resolve-none)
        accepted (checked-handoff/validate-verdict-event
                  (event adjudicator-seat {:rerun-witness :absent}) resolve-none)]
    (is (false? (:ok refused)))
    (is (= :r9/worker-authored-verdict-refused (:error/code refused)))
    (is (:ok accepted))
    (is (= :seat-string-distinctness (:independence/grade accepted)))))

(deftest rerun-witness-must-resolve-and-match-the-proposal
  (testing "a dangling witness can never receive the top grade"
    (let [result (checked-handoff/validate-verdict-event
                  (event adjudicator-seat {:rerun-witness "e-missing"})
                  (constantly nil))]
      (is (:ok result))
      (is (= :constant-assertion (:independence/grade result)))
      (is (not= :adjudicator-rerun-witnessed
                (:independence/grade result)))
      (is (some #{:r9/rerun-witness-unresolved} (:notes result)))))
  (testing "a matching resolved witness receives the top grade"
    (let [result (checked-handoff/validate-verdict-event
                  (event adjudicator-seat {:rerun-witness "e-rerun"})
                  (fn [id] (when (= "e-rerun" id)
                             {:proposal proposal})))]
      (is (:ok result))
      (is (= :adjudicator-rerun-witnessed
             (:independence/grade result))))))

(deftest writer-supplied-grade-is-ignored
  (let [result (checked-handoff/validate-verdict-event
                (assoc (event adjudicator-seat {:rerun-witness :absent})
                       :independence/grade :adjudicator-rerun-witnessed)
                (constantly nil))]
    (is (= :seat-string-distinctness (:independence/grade result)))
    (is (not (contains? (:event result) :independence/grade)))
    (is (some #{:r9/grade-is-computed} (:notes result)))))

(deftest missing-seat-is-malformed-not-distinct
  ;; Reviewer addition (claude-2): absence must not manufacture distinctness.
  (testing "nil worker-seat refused"
    (is (= :r9/verdict-event-malformed
           (:error/code (checked-handoff/validate-verdict-event
                         (checked-handoff/verdict-event {:author-seat adjudicator-seat
                                            :proposal proposal
                                            :verdict :approve
                                            :adjudication {:rerun-witness :absent}})
                         (constantly nil))))))
  (testing "blank author-seat refused"
    (is (= :r9/verdict-event-malformed
           (:error/code (checked-handoff/validate-verdict-event
                         (checked-handoff/verdict-event {:worker-seat worker-seat
                                            :author-seat ""
                                            :proposal proposal
                                            :verdict :approve
                                            :adjudication {:rerun-witness :absent}})
                         (constantly nil)))))))

(deftest constructor-emits-exact-declared-shape
  (is (= {:event :checked-handoff/verdict
          :worker-seat worker-seat
          :author-seat adjudicator-seat
          :proposal proposal
          :verdict :approved
          :adjudication {:rerun-witness :absent}}
         (checked-handoff/verdict-event
          {:worker-seat worker-seat
           :author-seat adjudicator-seat
           :proposal proposal
           :verdict :approved
           :adjudication {:rerun-witness :absent}}))))

(deftest receipt-grades-are-derived-from-the-best-recorded-evidence
  (testing "legacy Boolean values are both ungradeable"
    (doseq [legacy-value [true false]]
      (is (= {:independence/grade :ungradeable-legacy
              :grade-source :legacy-boolean}
             (checked-handoff/grade-receipt
              {:receipt/independent-review? legacy-value})))))
  (testing "post-b persisted seats are compared"
    (is (= {:independence/grade :seat-string-distinctness
            :grade-source :persisted-seats
            :notes []}
           (checked-handoff/grade-receipt
            {:receipt/depositor-seat worker-seat
             :receipt/reviewer-seat adjudicator-seat}))))
  (testing "post-c typed assertion is passed through"
    (is (= {:independence/grade :asserted-unverified
            :grade-source :typed-field}
           (checked-handoff/grade-receipt
            {:receipt/independence :asserted-unverified}))))
  (testing "computed seats override a disagreeing assertion"
    (is (= {:independence/grade :seat-string-distinctness
            :grade-source :persisted-seats
            :notes [:r9/persisted-seats-override-typed-field]}
           (checked-handoff/grade-receipt
            {:receipt/depositor-seat worker-seat
             :receipt/reviewer-seat adjudicator-seat
             :receipt/independence :asserted-unverified}))))
  (testing "same-seat evidence cannot dignify the assertion"
    (is (= {:independence/grade :constant-assertion
            :grade-source :persisted-seats
            :notes [:r9/same-seat]}
           (checked-handoff/grade-receipt
            {:receipt/depositor-seat worker-seat
             :receipt/reviewer-seat worker-seat}))))
  (testing "unknown typed grades are refused"
    (is (= :r9/independence-grade-vocabulary-invalid
           (try
             (checked-handoff/grade-receipt
              {:receipt/independence :future-grade})
             nil
             (catch clojure.lang.ExceptionInfo e
               (:error/code (ex-data e))))))))

(deftest real-legacy-receipt-is-ungradeable-when-available
  ;; LIVE-PIN: this exact f75 receipt predates U14b/c. Campaign data is
  ;; intentionally untracked, so a checkout without it reports a visible skip.
  (let [file (io/file (str "data/apm-campaigns/jit-all-open-v2/"
                           "jit-all-open-v2-f75/live/promote-solver.edn"))]
    (if (.exists file)
      (let [receipt (:receipt (edn/read-string (slurp file)))]
        (is (= ["f75" "b94J03" true]
               [(:receipt/frame-id receipt)
                (:receipt/problem-id receipt)
                (:receipt/independent-review? receipt)]))
        (is (= {:independence/grade :ungradeable-legacy
                :grade-source :legacy-boolean}
               (checked-handoff/grade-receipt receipt))))
      (is true (str "SKIP: untracked live pin absent: " (.getPath file))))))
