(ns futon3c.diagramprover.wm-wire-r1-outer-cascade-loop-plan-chosen-target-test
  "Wire [:r1-outer-cascade :loop-plan :chosen-target]. Real loop calls;
  no live record carries both ends. See support/live-records-read."
  (:require [futon3c.diagramprover.wm-wire-loop-products-12a :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-plan-support :as support]))

(defn observe
  ([] (observe identity))
  ([tamper] (support/observe :loop :chosen-target tamper)))

(defn check [] (observe))

(def wire
  {:wire [:r1-outer-cascade :loop-plan :chosen-target]
   :kind :witnessed-hermetically
   :test `the-writers-value-reaches-the-reader
   :second-layer {:test `selector-field-controls-loop-plan
                  :kind :value-varying :product [:result :plan]
                  :intervention :before-reader}
   :check check :live-records-read support/live-records-read})

(deftest the-writers-value-reaches-the-reader
  (is (w/received? (check))))

(deftest typed-absence-at-the-reader-fails
  (let [o (observe #(assoc % :chosen-target {:absent :not-carried}))]
    (is (w/typed-absence? (:reader o)))
    (is (not (w/received? o)))))

(deftest another-value-at-the-reader-fails
  (let [o (observe #(assoc % :chosen-target "M-other"))]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))

(deftest live-records-do-not-witness-this-wire
  (support/assert-live-records))

(deftest selector-field-controls-loop-plan
  (let [[a b] (products/products :chosen-target)
        pa (get-in a [:result :plan]) pb (get-in b [:result :plan])]
    (is (= (:written a) (:written b)) "Same real field, seed, and selector output.")
    (is (not= (:requisition pa) (:requisition pb)))
    (doseq [p [pa pb]]
      (let [m (first (filter #(= (:target %) (:requisition p)) products/missions))]
        (is (= (select-keys m [:repo :path]) (select-keys (:want-source p) [:repo :path])))))
    (is (not= (get-in pa [:want-source :path]) (get-in pb [:want-source :path])))
    (is (= #{3 6} (set (map #(count (get-in % [:wants :in-view])) [pa pb]))))
    (is (not= (:wants pa) (:wants pb)))
    (println :chosen-target :plans
             (mapv #(select-keys % [:requisition :want-source :wants]) [pa pb]))))

;; Until futon2 6c153ce11 the plan step handed a nil repo and path to the
;; planner for a chosen target the field did not list, and the planner
;; returned a plan with no wants. It now records a typed absence and does not
;; call the planner (LOOP-PLAN-ABSENT-I).
(deftest target-outside-field-is-a-typed-absence-and-is-not-planned
  (let [[_ b] (products/products :missing)]
    (is (= {} (:handed b)) "the planner was not called")
    (is (= {:absent :chosen-target-not-in-field :chosen-target "M-not-in-field"
            :missing [:considered-entry]}
           (get-in b [:result :plan])))
    (is (some? (get-in b [:result :target-selection])) "the selection record is kept")
    (println :missing-target :handed (:handed b)
             :plan (select-keys (get-in b [:result :plan]) [:requisition :want-source :wants]))))
