(ns futon3c.diagramprover.wm-wire-r9-decision-r4-rank-dispatch-cascade-belief-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-token-input-support :as support]))

(defn check [] (support/observe :decision-dispatch :none))
(def wire
  {:wire [:r9-decision :r4-rank-dispatch [:cascade-belief {:record :rank-state}]]
   :kind :witnessed-hermetically :test `the-real-reader-produces-the-writers-belief :check check
   :live-records-read support/live-records-read
   :note "Real writer and reader; the value is read from the reader's returned receipt or its scoring evaluation, never from the wrapper argument. The helper carry witness uses the retaining branch; overrides have distinct output scopes."})

(deftest the-real-reader-produces-the-writers-belief
  (let [r (check)]
    (is (some? (:writer r)))
    (is (w/received? r) (pr-str (dissoc r :product)))))

(deftest an-absent-carrier-is-not-the-writers-belief
  (let [r (support/observe :decision-dispatch :absent)]
    (is (some? (:writer r)))
    (is (not (w/received? r)) (pr-str (dissoc r :product)))))

(deftest a-different-carrier-is-not-the-writers-belief
  (let [r (support/observe :decision-dispatch :different)]
    (is (not= {#{} 1} (:writer r)))
    (is (not (w/received? r)) (pr-str (dissoc r :product)))))

(deftest pinned-live-records-do-not-record-both-scoped-endpoints
  (is (support/live-reader-absent?)))
