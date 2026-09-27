(ns futon3c.diagramprover.wm-wire-flight-entry-flight-click-target-test
  "Scoped target handoff. Negative controls change the flight before the real reader."
  (:require [futon3c.diagramprover.wm-wire-target-readers-11a :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-target-support :as support]))

(defn check [] (support/observe :flight-click identity))
(def wire {:wire [:flight-entry :flight-click [:target {:record :flight}]]
           :kind :witnessed-hermetically
           :test `the-target-reaches-the-reader :second-layer {:test `target-determines-reader-product
                          :kind :value-varying :product [:outputs]
                          :intervention :before-reader}
           :check check
           :live-records-read support/live-records-read})

(deftest the-target-reaches-the-reader
  (is (w/received? (check)))
  (is (w/received? (support/observe :flight-click identity))))

(deftest typed-absence-before-reader-fails
  (is (not (w/received? (support/observe :flight-click (constantly {:absent :target-not-carried}))))))

(deftest different-target-before-reader-fails
  (let [o (support/observe :flight-click (constantly "M-other-target"))]
    (is (= "M-other-target" (:reader o)) (pr-str o))
    (is (not (w/received? o)))))

(deftest target-determines-reader-product
  (let [r (products/products :click)
        [a b] (:outputs r)]
    (is (= (mapv #(dissoc % :target) (:flights r))
           (repeat 2 (dissoc (first (:flights r)) :target))))
    (is (= products/targets (mapv #(get-in % [:sent :target]) [a b])))
    (is (= (dissoc (:sent a) :target) (dissoc (:sent b) :target)))
    (is (= [{:token :done}] (get-in a [:summary :unreached-wants])))
    (is (= [] (get-in b [:summary :unreached-wants])))
    (is (= :C1 (get-in a [:summary :chosen :candidate])))
    (is (nil? (get-in b [:summary :chosen])))
    (is (= (dissoc (:summary a) :chosen :unreached-wants)
           (dissoc (:summary b) :chosen :unreached-wants)))
    (println :click :products [a b])))
