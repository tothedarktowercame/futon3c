(ns futon3c.diagramprover.wm-wire-r9-f-prefix-supply-r8-selection-candidate-f-prefix-test
  (:require [futon3c.diagramprover.wm-wire-selection-products-support :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-kernel-out-support :as support]))
(def positive (delay (support/prefix-read :none)))
(defn check [] @positive)
(def wire {:second-layer {:test `candidate-records-prefix-and-posterior-uses-it :kind :record
                                  :product [:f] :intervention :before-reader}
           :wire [:r9-f-prefix-supply :r8-selection-candidate [:f-prefix {:record :ranked-entry}]]
           :kind :witnessed-hermetically :test `the-reader-receives-the-kernel-value :check check
           :live-records-read support/live-records-read :note "Real persisted decision -> conditioning-step with canonical policy key -> prefix admission -> production-ranked -> selection-candidate. Computed F retained under :f-prefix and :f; no synthetic history."})
(deftest the-reader-receives-the-kernel-value
  (is (seq (support/census)))
  (is (w/received? (check))))
(deftest typed-absence-before-reader
  (is (not (w/received? (support/prefix-read :absent)))))
(deftest different-value-before-reader
  (let [o (support/prefix-read :different)]
    (is (not (w/received? o)))
    (is (not= (:writer o) (:reader o)))))
(deftest computed-prefix-changes-the-production-posterior
  (let [r (support/prefix-effect)]
    (is (= (+ 1 (:f r)) (:changed-f r)))
    (is (< (get-in r [:changed-posterior :observed]) (get-in r [:posterior :observed])))
    (is (= :computed (get-in (check) [:reader :status])))
    (is (= 1 (get-in (check) [:reader :steps])))))

(deftest candidate-records-prefix-and-posterior-uses-it
  ;; selection-candidate carries F without arithmetic; the downstream real
  ;; selection-posterior uses -F. This wire's product is therefore :record.
  (let [{:keys [index action before after candidates candidates-prime p p-prime]}
        (products/prefix-pair)
        a (nth candidates index) b (nth candidates-prime index)
        x (get p action) y (get p-prime action)]
    (is (= before (update-in after [index :f-prefix :f] dec)))
    (is (apply not= (map :g candidates)))
    (is (= :computed (:f-status a)))
    (is (= (get-in before [index :f-prefix :f]) (:f a)))
    (is (= (get-in after [index :f-prefix :f]) (:f b)))
    (is (not= (:f a) (:f b)))
    (is (= (mapv #(dissoc % :f :f-prefix :inputs) candidates)
           (mapv #(dissoc % :f :f-prefix :inputs) candidates-prime)))
    (is (= (:g a) (:g b)))
    (is (< y x))
    ;; A one-unit F increase multiplies this policy's odds by exp(-1).
    (is (< (Math/abs (- (/ y (- 1 y)) (* (Math/exp -1) (/ x (- 1 x))))) 1e-12))
    (println :prefix-record {:F [(:f a) (:f b)] :G (mapv :g candidates)
                             :posterior [x y]})))
