(ns futon3c.diagramprover.wm-wire-construction-assemble-one-r13-family-parameters-horizon-steps-test
  (:require [futon3c.diagramprover.wm-wire-construction-products :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-construction-support :as support]))
(defn observe [mutation] (support/family :horizon-steps mutation))
(defn check [] (observe :none))
(def wire {
  :second-layer {:test 'futon3c.diagramprover.wm-wire-construction-assemble-one-r13-family-parameters-horizon-steps-test/changed-carrier-changes-the-derived-product :kind :value-varying
                  :product [:scores] :intervention :before-reader}
 :wire [:construction-assemble-one :r13-family-parameters [:horizon-steps {:record :cascade-problem}]]
           :kind :witnessed-hermetically
           :test `the-observed-handoff :check check
           :live-records-read support/live-records-read})
(deftest the-observed-handoff
  (let [o (check)]
    (is (w/received? o))
    (is (= {:beta 1 :horizon-steps 3} (:family o)))))
(deftest absence-before-reader-fails
  (is (not (w/received? (observe :absent)))))
(deftest different-value-before-reader-fails
  (let [o (observe :different)]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))

(deftest changed-carrier-changes-the-derived-product
  (let [before (products/score-product :horizon-steps :none)
        after (products/score-product :horizon-steps :different)
        v (:scores before) v-prime (:scores after)]
    (prn :wire-2l-3a :r13-family-parameters-horizon-steps :before before :after after)
    (is (< 1 (count v)) "competing scored candidates")
    (is (= (count v) (count v-prime)))
    (is (every? number? (concat v v-prime)))
    (is (not= v v-prime) "derived product changes after the carrier intervention")))
