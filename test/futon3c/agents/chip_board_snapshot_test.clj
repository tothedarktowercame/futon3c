(ns futon3c.agents.chip-board-snapshot-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agents.chip-board :as board]))

(def sample-board
  {:entry :s :constants {}
   :chips [{:chip/id :s :verb :sing :wires {:true :y}}
           {:chip/id :y :verb :yield}]})

(defn poison [s _ _]
  {:branch :true :effects [[:commit {:repo "counterexample-only"}]] :state' s})

(deftest digest-and-execution-use-one-snapshot
  (with-redefs [board/verb-registry (atom @board/verb-registry)]
    (let [original-digest board/verbs-digest
          expected (original-digest sample-board)
          ;; Interleave a registration at the digest boundary. All hashing,
          ;; execution and effect generation still use the real implementation.
          r (with-redefs [board/verbs-digest
                          (fn [b & captured]
                            (let [digest (apply original-digest b captured)]
                              (board/register-verb! :sing poison)
                              digest))]
              (board/run-board sample-board {} (fn [_] nil)))]
      (is (= expected (:verbs/digest r)))
      (is (= [[:report] [:yield-turn]] (mapv #(mapv first (:effects %)) (:trace r))))
      (is (= :end/yield (:end-reason r))))))

(deftest mid-run-registration-does-not-change-later-chip
  (with-redefs [board/verb-registry (atom @board/verb-registry)]
    (let [expected (board/verbs-digest sample-board)
          r (board/run-board sample-board {}
                             (fn [[kind _]]
                               (when (= :report kind)
                                 (board/register-verb! :yield poison))))]
      (is (= expected (:verbs/digest r)))
      (is (= [[:report] [:yield-turn]] (mapv #(mapv first (:effects %)) (:trace r))))
      (is (= :end/yield (:end-reason r))))))
