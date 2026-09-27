(ns futon3c.diagramprover.wm-wire-r1-token-legacy-r1-token-initialization-continuation-belief-test
  (:require [futon3c.diagramprover.wm-wire-token-continuation-products :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-token-input-support :as support]))

(defn check [] (support/observe :legacy-initialization :none))
(def wire
  {:second-layer {:test 'futon3c.diagramprover.wm-wire-r1-token-legacy-r1-token-initialization-continuation-belief-test/changed-continuation-product :kind :record
                   :product [:belief] :intervention :before-reader}
   :wire [:r1-token-legacy :r1-token-initialization [:continuation-belief {:record :legacy-token-belief-input}]]
   :kind :witnessed-hermetically :test `the-real-reader-produces-the-writers-belief :check check
   :live-records-read support/live-records-read
   :note "Real writer and reader; the value is read from the reader's returned receipt or its scoring evaluation, never from the wrapper argument. The helper carry witness uses the retaining branch; overrides have distinct output scopes."})

(deftest the-real-reader-produces-the-writers-belief
  (let [r (check)]
    (is (some? (:writer r)))
    (is (w/received? r) (pr-str (dissoc r :product)))))

(deftest an-absent-carrier-is-not-the-writers-belief
  (let [r (support/observe :legacy-initialization :absent)]
    (is (some? (:writer r)))
    (is (not (w/received? r)) (pr-str (dissoc r :product)))))

(deftest a-different-carrier-is-not-the-writers-belief
  (let [r (support/observe :legacy-initialization :different)]
    (is (not= {#{} 1} (:writer r)))
    (is (not (w/received? r)) (pr-str (dissoc r :product)))))

(deftest pinned-live-records-do-not-record-both-scoped-endpoints
  (is (support/live-reader-absent?)))

(deftest changed-continuation-product
  (let [a (products/receipt-product :legacy-initialization :none)
        b (products/receipt-product :legacy-initialization :different)]
    (prn :wire-2l-4b :legacy-initialization :before (dissoc a :inputs :other-fields)
         :after (dissoc b :inputs :other-fields))
    (is (= (:inputs a) (:inputs b)))
    (is (= (:other-fields a) (:other-fields b)))
    (is (= (:supplied a) (:belief a)))
    (is (= (:supplied b) (:belief b)))
    (is (not= (:belief a) (:belief b)))
    ;; Retaining branch: no posterior computation is driven by this field.
    (is (= [] (:updates a) (:updates b)))))
