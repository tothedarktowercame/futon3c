(ns futon3c.diagramprover.wm-wire-flight-record-summary-flight-run-chosen-test
  "Real calls with IO isolated; no live record carries both ends.
  See support/live-records-read for the pinned record survey."
  (:require [futon3c.diagramprover.wm-wire-summary-products :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-small-support :as support]))
(defn check [] (support/observe :chosen (fn [v _] v)))
(def wire {:wire [:flight-record-summary :flight-run :chosen] :second-layer {:test `summary-field-is-recorded-without-changing-progress
                  :kind :record :product [:clicks 0 :chosen]
                  :intervention :before-reader}
   :kind :witnessed-hermetically
           :test `the-writer-reaches-the-reader :check check
           :live-records-read support/live-records-read})
(deftest the-writer-reaches-the-reader
  (is (w/received? (check))))
(deftest typed-absence-before-reader-fails
  (is (not (w/received? (support/observe :chosen (fn [_ _] {:status :absent :reason :not-carried}))))))
(deftest different-carrier-before-reader-fails
  (let [o (support/observe :chosen (fn [_ other] other))]
    (is (some? (:reader o)))
    (is (not (w/received? o)))))
(deftest live-records-do-not-carry-both-ends
  (support/assert-live-records))

(deftest summary-field-is-recorded-without-changing-progress
  ;; flight/record-click:426,429 stores the fields; :409-413 determines
  ;; progress from wants/before/after. run!:577 delegates that decision.
  (let [[a b] (products/products :run :chosen)
        ra (:record a) rb (:record b)]
    (is (= (dissoc (:carrier a) :chosen) (dissoc (:carrier b) :chosen)))
    (is (= (get-in a [:carrier :chosen]) (get-in ra [:clicks 0 :chosen])))
    (is (= (get-in b [:carrier :chosen]) (get-in rb [:clicks 0 :chosen])))
    (is (not= (get-in ra [:clicks 0 :chosen]) (get-in rb [:clicks 0 :chosen])))
    (is (= (update ra :clicks #(mapv (fn [c] (dissoc c :chosen)) %))
           (update rb :clicks #(mapv (fn [c] (dissoc c :chosen)) %))))
    (is (= :no-progress (:status ra) (:status rb)))
    (is (= 1 (count (:clicks ra)) (count (:clicks rb))))
    (is (= 2 (:observations a) (:observations b)))
    (is (= 1 (:click-calls a) (:click-calls b)))
    (println :summary-product :run :chosen
             (pr-str [(get-in ra [:clicks 0 :chosen]) (get-in rb [:clicks 0 :chosen])])
             :status [(:status ra) (:status rb)])))
