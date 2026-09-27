(ns futon3c.diagramprover.wm-wire-flight-entry-r0-enact-step-target-test
  "Scoped target handoff. Negative controls change the flight before the real reader."
  (:require [futon3c.diagramprover.wm-wire-target-products-16a :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-target-support :as support]))

(defn check [] (support/observe :r0-enact-step identity))
(def wire {:wire [:flight-entry :r0-enact-step [:target {:record :flight}]]
           :kind :witnessed-hermetically
           :test `the-target-reaches-the-reader :check check
           :second-layer {:test `target-reader-product :kind :record
                          :product [:attempts 0 :target] :intervention :before-reader}
           :live-records-read support/live-records-read})

(deftest the-target-reaches-the-reader
  (is (w/received? (check)))
  (is (w/received? (support/observe :r0-enact-step identity))))

(deftest typed-absence-before-reader-fails
  (is (not (w/received? (support/observe :r0-enact-step (constantly {:absent :target-not-carried}))))))

(deftest different-target-before-reader-fails
  (let [o (support/observe :r0-enact-step (constantly "M-other-target"))]
    (is (= "M-other-target" (:reader o)) (pr-str o))
    (is (not (w/received? o)))))

(deftest target-reader-product
  (let [{:keys [flights products]} (products/products :enact)
        [a b] products]
    (is (= (dissoc (first flights) :target) (dissoc (second flights) :target)))
    (is (= products/targets (mapv #(get-in % [:record :attempts 0 :target]) products)))
    (is (= (:path a) (:path b)))
    (is (= (dissoc (get-in a [:record :attempts 0]) :target)
           (dissoc (get-in b [:record :attempts 0]) :target)))
    (is (= [false false] (mapv #(get-in % [:record :attempts 0 :success]) products)))
    (println "target-enact" (pr-str products))))
