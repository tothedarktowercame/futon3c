(ns futon3c.apm.countdown-control-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.countdown-control :as sut]))

(deftest replacement-registration-starts-at-f19-with-complete-cycle
  (let [body (sut/registration-body)
        units (get-in body [:block-plan 0 :units])]
    (is (= 9 (count units)))
    (is (= "f19" (:frame-id (first units))))
    (is (= "f27" (:frame-id (last units))))
    (is (= 10 (count (:phase-order body))))
    (is (= :preflight (first (:phase-order body))))
    (is (= :close-frame (last (:phase-order body))))
    (is (not-any? #(contains? % :required-receipt-kinds) units)
        "eventual close receipts must not be required at open-frame runtime")))
