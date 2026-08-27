(ns futon3c.apm.open-problem-queue-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.apm.open-problem-queue :as sut]))

(deftest canonical-corpus-selection-is-pinned-and-auditable
  (let [result (sut/derive-queue "/home/joe/code/apm-lean")
        problems (:problems result)
        excluded (:excluded result)]
    (is (:ok result))
    ;; 121 = 475 corpus - 269 not-open - 79 topology - 4 defective - 2
    ;; construction-blocked. Each subtrahend is asserted below, so a future
    ;; move in this count identifies which class changed rather than being an
    ;; unexplained integer.
    (is (= 121 (count problems)))
    (is (every? #(and (= :non-excluded (:classification %))
                      (not (.startsWith ^String (:problem/id %) "t"))
                      (every? string? ((juxt :repository :base-branch :revision
                                             :path :blob) %)))
                problems))
    (testing "known statement defects are explicit exclusions"
      (is (= #{"a96A06" "b96A01" "m93J05" "m96J02"}
             (set (map :problem/id
                       (filter #(= :defective-or-invalid-statement (:reason %))
                               excluded))))))
    (testing "construction-blocked problems are explicit exclusions"
      ;; Added 2026-08-26 (6a7735e1, 57586fc6). These two moved out of the
      ;; solve queue then, which is why the count above is 121 and not 123.
      (is (= #{"a96A07" "a97J08"}
             (set (map :problem/id
                       (filter #(= :construction-blocked (:reason %))
                               excluded))))))
    (testing "topology never enters the solve queue"
      (is (every? #(not (.startsWith ^String (:problem/id %) "t")) problems)))
    (testing "every problem is either queued or excluded for a stated reason"
      (is (= 475 (+ (count problems) (count excluded))))
      (is (every? #{:not-open :topology :defective-or-invalid-statement
                    :construction-blocked}
                  (map :reason excluded))))))
