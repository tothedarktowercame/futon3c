(ns futon3c.diagramprover.wm-wire-r6-sourced-rates-r6-cascade-lane-measurement-test
  "Rates wire, witnessed by real calls using ten real subjects admitted through the store and reader.
  See support/live-records-read for the live records lacking both ends.

  Rates wire, read from the content-addressed rates-observe-g28 producer
  record. The producer ran the real cascade-lane using ten real subjects
  admitted through the store and reader; this reader loads no product code.
  See :live-records-read for the live records lacking both ends, re-verified
  live below."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def wire-id [:r6-sourced-rates :r6-cascade-lane :measurement])
(def stem "rates-observe-g28")
(def producer (delay (producer-record/record stem)))
(defn- fields [] (get-in @producer [:wires wire-id]))
(def live-records-read (delay (:live-records-read @producer)))
(defn observe [mutation]
  (if (= mutation :none) (:primary (fields)) (get-in (fields) [:interventions mutation])))
(defn check [] (observe :none))
(def wire {:second-layer {:test 'futon3c.diagramprover.wm-wire-r6-sourced-rates-r6-cascade-lane-measurement-test/reader-record-retains-the-intervened-carrier
                         :kind :record :product [:cascade-scoring :rates-provenance :measurement]
                         :intervention :before-reader}
           :wire wire-id :kind :witnessed-hermetically
           :test `the-writers-value-reaches-the-reader :check check
           :live-records-read @live-records-read})

(deftest the-writers-value-reaches-the-reader
  (let [o (check)]
    (is (seq (:writer o)) "[:primary :writer]")
    (is (every? #(= {:false-neg {:numerator 0 :denominator 5}
                     :false-pos {:numerator 0 :denominator 5}} %)
                (vals (:writer o)))
        "[:primary :writer] five admitted subjects per cell")
    (is (w/received? o) "[:primary] the writer's value reached the reader")))

(deftest typed-absence-carrier-does-not-witness-the-wire
  (is (not (w/received? (observe :absent))) "[:interventions :absent] not received"))

(deftest different-carrier-does-not-witness-the-wire
  (let [o (observe :different)]
    (is (some? (:reader o)) "[:interventions :different :reader]")
    (is (not (w/received? o)) "[:interventions :different] not received")))

(deftest live-records-lack-both-ends
  (doseq [{:keys [path sha256]} @live-records-read]
    (let [actual (w/sha256-file path)]
      (is (= sha256 actual) path)
      (when-not (= sha256 actual) (throw (ex-info "Moved live pin" {:path path})))
      (let [r (w/read-record path)]
        (is (not-any? #(and (map? %) (or (contains? % :measurement)
                                        (contains? % :adjudication-rates)))
                      (tree-seq coll? seq r))
            path)))))

(deftest reader-population-and-below-minimum-exclusion
  (let [admitted (get-in @producer [:population :admitted])]
    (is (= {:C3 10} (:subjects admitted)) "[:population :admitted :subjects]")
    (is (= 10 (:label-count admitted)) "[:population :admitted :label-count]")
    (is (= {:alpha 1/2 :beta 1/2 :authority "A-S §2 (Jeffreys), Revision 3"} (:prior admitted))
        "[:population :admitted :prior]")
    (is (seq (:rates admitted)) "[:population :admitted :rates]")
    (is (every? #(= {:false-neg 1/12 :false-pos 1/12} %) (vals (:rates admitted)))
        "[:population :admitted :rates] Jeffreys-smoothed rates"))
  (let [below (get-in @producer [:population :below-minimum])]
    (is (= [{:class :C3 :excluded :below-minimum :counts {:present 4 :absent 5}}]
           (:excluded below))
        "[:population :below-minimum :excluded]")
    (is (true? (:labels-empty? below)) "[:population :below-minimum :labels-empty?]")
    (is (true? (:subjects-c3-absent? below)) "[:population :below-minimum :subjects-c3-absent?]")
    (is (= :absent (:t-wanted below)) "[:population :below-minimum :t-wanted]")))

(deftest reader-record-retains-the-intervened-carrier
  (let [r (:second-layer (fields)) before (:before r) after (:after r)]
    (is (seq (:writer before)) "[:second-layer :before :writer]")
    (is (true? (:writer-unchanged? r)) "[:second-layer :writer-unchanged?]")
    (is (true? (:writer-is-carrier-before? r)) "[:second-layer :writer-is-carrier-before?]")
    (is (true? (:carrier-is-recorded-after? r)) "[:second-layer :carrier-is-recorded-after?]")
    (is (= 2 (get-in (:recorded after) [:t/wanted :false-neg :numerator]))
        "[:second-layer :after :recorded :t/wanted :false-neg :numerator]")
    (is (true? (:recorded-changed? r)) "[:second-layer :recorded-changed?]")
    (is (true? (:g-efe-present? r)) "[:second-layer :g-efe-present?]")
    (is (true? (:g-efe-numeric? r)) "[:second-layer :g-efe-numeric?]")
    (is (true? (:g-efe-unchanged? r)) "[:second-layer :g-efe-unchanged?]")
    (println :recorded-measurement (:recorded before) :after (:recorded after)
             :unchanged-G (:G-efe before))))
