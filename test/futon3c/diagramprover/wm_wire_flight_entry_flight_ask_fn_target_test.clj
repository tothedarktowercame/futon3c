(ns futon3c.diagramprover.wm-wire-flight-entry-flight-ask-fn-target-test
  "Scoped target handoff. Negative controls change the flight before the real reader."
  (:require [futon3c.diagramprover.wm-wire-target-readers-11a :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-target-support :as support]))

(defn check [] (support/live-check :flight-ask-fn))
(def wire {:wire [:flight-entry :flight-ask-fn [:target {:record :flight}]]
           :kind :verified
           :test `the-target-reaches-the-reader :second-layer {:test `target-determines-reader-product
                          :kind :value-varying :product [:outputs]
                          :intervention :before-reader}
           :check check
           :record support/ask-pin})

(deftest the-target-reaches-the-reader
  (is (w/received? (check)))
  (is (w/received? (support/observe :flight-ask-fn identity))))

(deftest typed-absence-before-reader-fails
  (is (not (w/received? (support/observe :flight-ask-fn (constantly {:absent :target-not-carried}))))))

(deftest different-target-before-reader-fails
  (let [o (support/observe :flight-ask-fn (constantly "M-other-target"))]
    (is (= "M-other-target" (:reader o)) (pr-str o))
    (is (not (w/received? o)))))

(deftest target-determines-reader-product
  (let [r (products/products :ask)
        [a b] (:outputs r)]
    (is (= (mapv #(dissoc % :target) (:flights r))
           (repeat 2 (dissoc (first (:flights r)) :target))))
    (is (= {:asked [] :needs []} a))
    (is (= [{:want :done :outcome :no-criterion}] (:asked b)))
    (is (= [{:kind :no-criterion :missing :interpretation :want :done}] (:needs b)))
    (println :ask :products [a b])))
