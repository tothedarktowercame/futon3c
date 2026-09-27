(ns futon3c.diagramprover.wm-wire-construction-assemble-one-r9-decision-want-test
  (:require [futon3c.diagramprover.wm-wire-construction-products :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-construction-support :as support]))
(defn observe [mutation] (support/decision :want mutation))
(defn check [] (observe :none))
(def wire {
  :second-layer {:test 'futon3c.diagramprover.wm-wire-construction-assemble-one-r9-decision-want-test/changed-carrier-changes-the-derived-product :kind :value-varying
                  :product [:scores] :intervention :before-reader}
 :wire [:construction-assemble-one :r9-decision [:want {:record :cascade-problem}]]
           :kind :witnessed-hermetically
           :test `the-observed-handoff :check check
           :live-records-read support/live-records-read})
(deftest the-observed-handoff
  (let [o (check)]
    (is (w/received? o))
    (is (some? (get-in o [:result :decision :action])))))
(deftest absence-before-reader-fails
  (is (not (w/received? (observe :absent)))))
(deftest different-value-before-reader-fails
  (let [o (observe :different)]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))

(deftest changed-carrier-changes-the-derived-product
  (let [before (products/decision-product :want :none)
        after (products/decision-product :want :different)
        v (:scores before) v-prime (:scores after)]
    (prn :wire-2l-3a :r9-decision-want :before before :after after)
    (is (< 1 (count v)) "competing scored candidates")
    (is (= [3 2] [(count v) (count v-prime)])
        "the real decision admits a different ranked family for the changed want")
    (is (every? number? (concat v v-prime)))
    (is (not= v v-prime) "derived product changes after the carrier intervention")))
