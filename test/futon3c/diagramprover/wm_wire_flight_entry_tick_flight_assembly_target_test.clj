(ns futon3c.diagramprover.wm-wire-flight-entry-tick-flight-assembly-target-test
  "Scoped target handoff. Negative controls change the flight before the real reader."
  (:require [futon3c.diagramprover.wm-wire-target-readers-11a :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-target-support :as support]))

(defn check [] (support/observe :tick-flight-assembly identity))
(def wire {:wire [:flight-entry :tick-flight-assembly [:target {:record :flight}]]
           :kind :witnessed-hermetically
           :test `the-target-reaches-the-reader :second-layer {:test `target-determines-reader-product
                          :kind :record :product [:outputs]
                          :intervention :before-reader}
           :check check
           :live-records-read support/live-records-read})

(deftest the-target-reaches-the-reader
  (is (w/received? (check)))
  (is (w/received? (support/observe :tick-flight-assembly identity))))

(deftest typed-absence-before-reader-fails
  (is (not (w/received? (support/observe :tick-flight-assembly (constantly {:absent :target-not-carried}))))))

(deftest different-target-before-reader-fails
  (let [o (support/observe :tick-flight-assembly (constantly "M-other-target"))]
    (is (= "M-other-target" (:reader o)) (pr-str o))
    (is (not (w/received? o)))))

(deftest target-determines-reader-product
  (let [r (products/products :assembly)
        [a b] (:outputs r)]
    (is (= (mapv #(dissoc % :target) (:flights r))
           (repeat 2 (dissoc (first (:flights r)) :target))))
    (is (= (mapv vector products/targets) (mapv :targets [a b])))
    (doseq [[r target] (map vector [a b] products/targets)]
      (is (= [:done] (get-in r [:sources :wants target])))
      (is (= (:locators products/wants) (get-in r [:sources :locators target])))
      (is (= {:done false} (get-in r [:sources :universes target])))
      (is (= :WM ((get-in r [:sources :context-of]) target))))
    (is (= (products/normal-assembly a (first products/targets))
           (products/normal-assembly b (second products/targets))))
    (println :assembly :products (mapv #(dissoc % :sources) [a b]))))
