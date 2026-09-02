(ns futon3c.apm.checked-handoff-test
  (:require [clojure.test :refer [deftest is testing]]
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
