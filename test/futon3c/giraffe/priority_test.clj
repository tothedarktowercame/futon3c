(ns futon3c.giraffe.priority-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.giraffe.priority :as priority]))

(def pass-output
  {:order [{:id :A :rank 1 :status :ready :cone 3 :unblocks 2}
           {:id :B :rank 2 :status :ready :cone 2 :unblocks 1}
           {:id :C :rank 3 :status :not-ready :cone 1 :unblocks 0}]})

(def pin {:sha256 "focus-input-sha"})

(deftest received-focus-bands-precede-pass-order
  (let [rows (priority/priority-projection
              pass-output
              {:relations {:A :useful-elsewhere :B :associated :C :focus}
               :evidence-pin pin}
              2)]
    (is (= [:C :B :A] (mapv :target rows)))
    (is (= [:focus :associated :elsewhere] (mapv :priority-band rows)))
    (is (= [:within-cutoff :within-cutoff :beyond-cutoff]
           (mapv :cutoff-status rows)))
    (is (every? #(= 2 (:cutoff %)) rows))
    (is (every? #(= pin (:evidence-pin %)) rows))))

(deftest missing-relation-is-unknown-with-reason
  (let [row (first (priority/priority-projection
                    {:order [(first (:order pass-output))]}
                    {:relations {} :evidence-pin pin}
                    1))]
    (is (= :unknown (:priority-band row)))
    (is (= :focus-relation-missing (:reason row)))
    (is (nil? (:relation row)))))
