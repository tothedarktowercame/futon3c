(ns futon3c.diagramprover.wm-wire-r6-sourced-rates-r6-cascade-lane-measurement-test
  "Rates wire, witnessed by real calls using ten real subjects admitted through the store and reader.
  See support/live-records-read for the live records lacking both ends."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-rates-support :as support]))

(defn check [] (support/observe :measurement identity))
(def wire {:wire [:r6-sourced-rates :r6-cascade-lane :measurement] :kind :witnessed-hermetically
           :test `the-writers-value-reaches-the-reader :check check
           :live-records-read support/live-records-read})

(deftest the-writers-value-reaches-the-reader
  (let [o (check)]
    (is (seq (:writer o)))
    (is (every? #(= {:false-neg {:numerator 0 :denominator 5}
                     :false-pos {:numerator 0 :denominator 5}} %)
                (vals (:writer o))) "five admitted subjects per cell")
    (is (w/received? o))))

(deftest typed-absence-carrier-does-not-witness-the-wire
  (is (not (w/received? (support/observe :measurement (constantly {:absent :not-carried}))))))

(deftest different-carrier-does-not-witness-the-wire
  (let [o (support/observe :measurement #(support/different :measurement %))]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))

(deftest live-records-lack-both-ends
  (support/assert-live-records))


(deftest reader-population-and-below-minimum-exclusion
  (let [view @support/admitted-view
        lane (support/lane view)
        rates (get-in (meta (:ranked lane)) [:cascade-scoring :precision-model :rates])]
    (is (= {:C3 10} (:subjects view)))
    (is (= 10 (count (:labels view))))
    (is (= {:alpha 1/2 :beta 1/2 :authority "A-S §2 (Jeffreys), Revision 3"} (:prior view)))
    (is (seq rates))
    (is (every? #(= {:false-neg 1/12 :false-pos 1/12} %) (vals rates))))
  (let [view (support/label-view 4)
        lane (support/lane view)]
    (is (= [{:class :C3 :excluded :below-minimum :counts {:present 4 :absent 5}}]
           (:excluded view)))
    (is (empty? (:labels view)))
    (is (not (contains? (:subjects view) :C3)))
    (is (= :absent (get (support/measurement lane) :t/wanted)))))
