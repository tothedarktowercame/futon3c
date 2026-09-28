(ns futon3c.diagramprover.wm-wire-r13-policy-depth-anticipation-r7-fold-call-horizon-steps-test
  (:require [futon3c.diagramprover.wm-wire-depth-products :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-fold-in-support :as support]))
(defn check [] (support/observe :horizon :none))
(def wire {:second-layer {:test `anticipated-depth-is-recorded-without-changing-selection :kind :record
                   :product [:policy-depth-used] :intervention :before-reader}
   :wire [:r13-policy-depth-anticipation :r7-fold-call [:horizon-steps {:record :depth-anticipation}]]
           :kind :witnessed-hermetically :test `the-judge-produces-the-received-value :check check
           :live-records-read support/live-records-read
           :note "Reader produces :policy-depth-used (3 -> 4); not cascade family T. The cascade-horizon remains sourced independently from cascade-sources."})
(deftest the-judge-produces-the-received-value
  (support/assert-live-pins)
  (let [o (check)]
    (is (nil? (get-in o [:result :wire-error])))
    (is (w/received? o))))
(deftest absence-before-the-judge-does-not-witness-the-wire
  (let [o (support/observe :horizon :absent)]
    (is (not (w/received? o)))
    (is (= 1 (:reader o)))))
(deftest different-carrier-changes-the-produced-value
  (let [o (support/observe :horizon :different)]
    (is (nil? (get-in o [:result :wire-error])))
    (is (not (w/received? o)))
    (is (= 4 (:reader o)))
    (is (= 3 (get-in o [:result :cascade-horizon :value])))))

(deftest anticipated-depth-is-recorded-without-changing-selection
  ;; judge, war_machine.clj:7451-7455,7767-7782: depth is recorded;
  ;; cascade horizon is resolved independently, not computed from this carrier.
  (let [a (products/depth-product :none) b (products/depth-product :different)]
    (is (= [3 4] [(:reader a) (:reader b)]))
    (is (= (:rank-inputs a) (:rank-inputs b)))
    (is (seq (:scores a)))
    (is (= (:scores a) (:scores b)))
    (is (seq (:posterior a)))
    (is (= (:posterior a) (:posterior b)))
    (is (= (get-in a [:result :cascade-horizon]) (get-in b [:result :cascade-horizon])))
    (prn :anticipated-depth {:record [(:reader a) (:reader b)] :G (:scores a)
                             :posterior (vals (:posterior a))})))
