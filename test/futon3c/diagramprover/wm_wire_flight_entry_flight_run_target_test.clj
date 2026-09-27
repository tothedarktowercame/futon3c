(ns futon3c.diagramprover.wm-wire-flight-entry-flight-run-target-test
  "Scoped target handoff. Negative controls change the flight before the real reader."
  (:require [futon3c.diagramprover.wm-wire-target-products-16a :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-target-support :as support]))

(defn check [] (support/observe :flight-run identity))
(def wire {:wire [:flight-entry :flight-run [:target {:record :flight}]]
           :kind :witnessed-hermetically
           :test `the-target-reaches-the-reader :check check
           :second-layer {:test `target-reader-product :kind :value-varying
                          :product [:status] :intervention :before-reader}
           :live-records-read support/live-records-read})

(deftest the-target-reaches-the-reader
  (is (w/received? (check)))
  (is (w/received? (support/observe :flight-run identity))))

(deftest typed-absence-before-reader-fails
  (is (not (w/received? (support/observe :flight-run (constantly {:absent :target-not-carried}))))))

(deftest different-target-before-reader-fails
  (let [o (support/observe :flight-run (constantly "M-other-target"))]
    (is (= "M-other-target" (:reader o)) (pr-str o))
    (is (not (w/received? o)))))

(deftest target-reader-product
  (let [{:keys [flights products]} (products/products :run)
        ]
    (is (= (dissoc (first flights) :target) (dissoc (second flights) :target)))
    (is (= [:closed :no-progress] (mapv :status products)))
    (is (= [[] [:done]] (mapv #(get-in % [:clicks 0 :open-after]) products)))
    (is (= [1 1] (mapv #(count (:clicks %)) products)))
    (println "target-run" (pr-str products))))
