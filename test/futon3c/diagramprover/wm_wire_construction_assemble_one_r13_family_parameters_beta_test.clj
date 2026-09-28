(ns futon3c.diagramprover.wm-wire-construction-assemble-one-r13-family-parameters-beta-test
  (:require [clojure.test :refer [deftest is]] [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:construction-assemble-one :r13-family-parameters [:beta {:record :cascade-problem}]])
(def producer (delay (producer-record/record "construction-family")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn observe [mutation] (if (= mutation :none) (:primary (fields)) (get-in (fields) [:interventions mutation])))
(defn check [] (observe :none))
(def wire {:second-layer {:test `beta-changes-the-joint-posterior :kind :value-varying
                          :product [:posterior] :intervention :before-reader}
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
(deftest beta-changes-the-joint-posterior
  (let [r (:second-layer (fields)) before (:before r) after (:after r)
        p1 (:posterior before) p3 (:posterior after)]
    (is (true? (:scores-equal? r)))
    (is (true? (:posteriors-differ? r)))
    (is (= [1 3] [(:beta before) (:beta after)]))
    (is (= #{:A :B} (set (keys p1)) (set (keys p3))))
    (is (= (:scores before) (:scores after)))
    (is (< (get-in before [:scores :A]) (get-in before [:scores :B])))
    (is (not= p1 p3))
    ;; beta is temperature: -G/beta, NOT inverse temperature -beta*G.
    (is (< 0.5 (:A p3) (:A p1)))
    (is (< (:B p1) (:B p3) 0.5))))
