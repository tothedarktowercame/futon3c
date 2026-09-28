(ns futon3c.diagramprover.wm-wire-r6-cascade-lane-r4-kernel-adjudication-rates-test
  "Rates wire, read from the content-addressed rates-observe producer record.
  The producer ran the real calls using ten real subjects admitted through the
  store and reader; this reader loads no product code."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def wire-id [:r6-cascade-lane :r4-kernel :adjudication-rates])
(def producer (delay (producer-record/record "rates-observe")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(def live-records-read (delay (:live-records-read @producer)))
(defn observe [mutation]
  (if (= mutation :none) (:primary (fields)) (get-in (fields) [:interventions mutation])))
(defn check [] (observe :none))
(def wire {:second-layer {:test 'futon3c.diagramprover.wm-wire-r6-cascade-lane-r4-kernel-adjudication-rates-test/reader-product-changes-at-the-carrier :kind :value-varying
                   :product [:G-efe] :intervention :before-reader}
   :wire wire-id :kind :witnessed-hermetically
           :test `the-writers-value-reaches-the-reader :check check
           :live-records-read @live-records-read})

(deftest the-writers-value-reaches-the-reader
  (let [o (check)]
    (is (seq (:writer o)))
    (is (w/received? o))))

(deftest typed-absence-carrier-does-not-witness-the-wire
  (is (not (w/received? (observe :absent)))))

(deftest different-carrier-does-not-witness-the-wire
  (let [o (observe :different)]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))

(deftest live-records-lack-both-ends
  (doseq [{:keys [path sha256]} @live-records-read]
    (let [actual (w/sha256-file path)]
      (is (= sha256 actual))
      (when-not (= sha256 actual) (throw (ex-info "Moved live pin" {:path path})))
      (let [r (w/read-record path)]
        (is (not-any? #(and (map? %) (or (contains? % :measurement)
                                        (contains? % :adjudication-rates)))
                      (tree-seq coll? seq r)))))))

(deftest reader-product-changes-at-the-carrier
  (let [r (:second-layer (fields)) before (:before r) after (:after r)]
    (is (seq (:G-efe before)))
    (is (every? number? (concat (:G-efe before) (:G-efe after))))
    (is (= (count (:G-efe before)) (count (:G-efe after))))
    (is (not= (:G-efe before) (:G-efe after)))
    (println :rates-product :adjudication-rates before :after after)))
