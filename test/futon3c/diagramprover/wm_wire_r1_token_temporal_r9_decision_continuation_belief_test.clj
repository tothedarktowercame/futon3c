(ns futon3c.diagramprover.wm-wire-r1-token-temporal-r9-decision-continuation-belief-test
  (:require [clojure.data :as data]
            [futon3c.diagramprover.wm-wire-token-continuation-products :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-token-input-support :as support]))

(defn check [] (support/observe :temporal-decision :none))
(def wire
  {:second-layer {:test 'futon3c.diagramprover.wm-wire-r1-token-temporal-r9-decision-continuation-belief-test/changed-continuation-product :kind :value-varying
                   :product [:calls 0 :scores] :intervention :before-reader}
   :wire [:r1-token-temporal :r9-decision [:continuation-belief {:record :token-belief-input}]]
   :kind :witnessed-hermetically :test `the-real-reader-produces-the-writers-belief :check check
   :live-records-read support/live-records-read
   :note "Real writer and reader; the value is read from the reader's returned receipt or its scoring evaluation, never from the wrapper argument. The helper carry witness uses the retaining branch; overrides have distinct output scopes."})

(deftest the-real-reader-produces-the-writers-belief
  (let [r (check)]
    (is (some? (:writer r)))
    (is (w/received? r) (pr-str (dissoc r :product)))))

(deftest an-absent-carrier-is-not-the-writers-belief
  (let [r (support/observe :temporal-decision :absent)]
    (is (some? (:writer r)))
    (is (not (w/received? r)) (pr-str (dissoc r :product)))))

(deftest a-different-carrier-is-not-the-writers-belief
  (let [r (support/observe :temporal-decision :different)]
    (is (not= {#{} 1} (:writer r)))
    (is (not (w/received? r)) (pr-str (dissoc r :product)))))

(deftest pinned-live-records-do-not-record-both-scoped-endpoints
  (is (support/live-reader-absent?)))

(deftest changed-continuation-product
  (let [a (products/decision-product :none)
        b (products/decision-product :different)]
    (prn :wire-2l-4b :temporal-decision :before (dissoc a :calls :decision-inputs)
         :after (dissoc b :calls :decision-inputs)
         :scores-before (mapv :scores (:calls a)) :scores-after (mapv :scores (:calls b)))
    (is (= (:decision-inputs a) (:decision-inputs b)))
    (is (nil? (:exception a))) (is (nil? (:exception b)))
    (is (= 1 (count (:calls a)) (count (:calls b))))
    (is (= (mapv :other-inputs (:calls a)) (mapv :other-inputs (:calls b)))
        (pr-str (take 2 (data/diff (mapv :other-inputs (:calls a)) (mapv :other-inputs (:calls b))))))
    (is (= (:supplied a) (:incoming a)))
    (is (= (:supplied b) (:incoming b)))
    (is (not= (mapv :scores (:calls a)) (mapv :scores (:calls b))))))
