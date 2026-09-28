(ns futon3c.diagramprover.wm-wire-construction-assemble-one-r13-family-parameters-horizon-steps-test
  (:require [clojure.test :refer [deftest is testing]] [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:construction-assemble-one :r13-family-parameters [:horizon-steps {:record :cascade-problem}]])
(def producer (delay (producer-record/record "construction-family")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn observe [mutation] (if (= mutation :none) (:primary (fields)) (get-in (fields) [:interventions mutation])))
(defn check [] (observe :none))
(def wire {:second-layer {:test `changed-carrier-changes-the-derived-product :kind :value-varying
                          :product [:scores] :intervention :before-reader}
           :wire wire-id :kind :witnessed-hermetically :test `the-observed-handoff
           :check check :live-records-read []})
(deftest the-observed-handoff
  (let [o (check)]
    (is (w/received? o) (str "writer-reader " (pr-str o)))
    (is (= {:beta 1 :horizon-steps 3} (:family o)))))
(deftest absence-before-reader-fails
  (is (not (w/received? (observe :absent)))))
(deftest different-value-before-reader-fails
  (let [o (observe :different)]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))
(deftest changed-carrier-changes-the-derived-product
  (let [r (:second-layer (fields))
        v (get-in r [:before :scores]) v-prime (get-in r [:after :scores])]
    (doseq [k [:competing-before? :scores-numeric? :scores-differ?]]
      (testing (name k) (is (true? (get r k)))))
    (is (< 1 (count v)) "competing scored candidates")
    (is (= (count v) (count v-prime)))
    (is (every? number? (concat v v-prime)))
    (is (not= v v-prime) "derived product changes after the carrier intervention")))
