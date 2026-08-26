(ns futon3c.apm.open-problem-queue-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.apm.open-problem-queue :as sut]))

(deftest canonical-corpus-selection-is-pinned-and-auditable
  (let [result (sut/derive-queue "/home/joe/code/apm-lean")
        problems (:problems result)
        excluded (:excluded result)]
    (is (:ok result))
    (is (= 123 (count problems)))
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
    (testing "topology never enters the solve queue"
      (is (every? #(not (.startsWith ^String (:problem/id %) "t")) problems)))))
