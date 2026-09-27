(ns futon3c.diagramprover.wm-wire-r9-decision-r4-rank-dispatch-cascade-belief-test
  (:require [futon3c.diagramprover.wm-wire-belief-products-support :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-token-input-support :as support]))

(defn check [] (support/observe :decision-dispatch :none))
(def wire
  {:second-layer {:test 'futon3c.diagramprover.wm-wire-r9-decision-r4-rank-dispatch-cascade-belief-test/belief-intervention-changes-reader-product
                  :kind :record :product [:incoming] :intervention :before-reader}
   :wire [:r9-decision :r4-rank-dispatch [:cascade-belief {:record :rank-state}]]
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

(deftest belief-intervention-changes-reader-product
  (let [[before after] (get @products/pairs :dispatch)]
    (is (= (:writer before) (:writer after)))
    (is (= (:writer before) (:carrier before)))
    (is (not= (:carrier before) (:carrier after)))
    (is (= (:controls before) (:controls after))
        "Candidates, model/rates, horizon, beta and every other state field are held fixed")
    (is (= 3 (count (get-in before [:controls :candidates]))))
    (is (= 3 (get-in before [:controls :horizon])))
    (is (= {:value 1 :status :declared} (get-in before [:controls :beta])))
    (is (= (:carrier before) (:incoming before)))
    (is (= (:carrier after) (:incoming after)))
    (is (= 3 (count (:G before)) (count (:G after))))
    (is (every? number? (concat (:G before) (:G after))))
    (is (not= (:G before) (:G after)))
    ;; efe/rank-actions (efe.clj:1461) forwards unchanged to the kernel.
    ;; Dispatch adds no calculation: its result agrees with the kernel's
    ;; for EACH carrier, although the kernel calculates different Gs.
    (is (= (mapv :G (get @products/pairs :kernel)) [(:G before) (:G after)]))
    (println :dispatch :before-G (:G before) :after-G (:G after)
             :beta (get-in before [:controls :beta])
             :before-belief (:incoming before) :after-belief (:incoming after))))
