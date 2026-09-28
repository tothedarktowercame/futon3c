(ns futon3c.diagramprover.wm-wire-construction-assemble-one-r4-kernel-want-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:construction-assemble-one :r4-kernel [:want {:record :cascade-spec}]])
(def producer (delay (producer-record/record "construction-kernel")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn observe [mutation]
  (let [o (if (= mutation :none) (:primary (fields))
              (get-in (fields) [:interventions mutation]))]
    {:writer (:writer o) :reader (:reader o)
     :ranked (when (:ranked-present? o) [{}])}))
(defn check [] (observe :none))
(def wire {
  :second-layer {:test 'futon3c.diagramprover.wm-wire-construction-assemble-one-r4-kernel-want-test/changed-carrier-changes-the-derived-product :kind :value-varying
                  :product [:scores] :intervention :before-reader}
 :wire wire-id
           :kind :witnessed-hermetically
           :test `the-observed-handoff :check check
           :live-records-read []})
(deftest the-observed-handoff
  (let [o (check)]
    (is (w/received? o))
    (is (seq (:ranked o)))))
(deftest absence-before-reader-fails
  (is (not (w/received? (observe :absent)))))
(deftest different-value-before-reader-fails
  (let [o (observe :different)]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))

(deftest changed-carrier-changes-the-derived-product
  (let [{:keys [before-scores after-scores competing? same-count? numeric? changed?]}
        (:second-layer (fields))
        v before-scores v-prime after-scores]
    (is (< 1 (count v)) "competing scored candidates")
    (is (= (count v) (count v-prime)))
    (is (every? number? (concat v v-prime)))
    (is (not= v v-prime) "derived product changes after the carrier intervention")
    (is (and competing? same-count? numeric? changed?))))
