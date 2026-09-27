(ns futon3c.diagramprover.wm-wire-flight-entry-r9-close-cause-target-test
  "Scoped target handoff. Negative controls change the flight before the real reader."
  (:require [futon3c.diagramprover.wm-wire-target-products-16a :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-target-support :as support]))

(defn check [] (support/observe :r9-close-cause identity))
(def wire {:wire [:flight-entry :r9-close-cause [:target {:record :flight}]]
           :kind :witnessed-hermetically
           :test `the-target-reaches-the-reader :check check
           :second-layer {:test `target-reader-product :kind :record
                          :product [:targets 0 :target] :intervention :before-reader}
           :live-records-read support/live-records-read})

(deftest the-target-reaches-the-reader
  (is (w/received? (check)))
  (is (w/received? (support/observe :r9-close-cause identity))))

(deftest typed-absence-before-reader-fails
  (is (not (w/received? (support/observe :r9-close-cause (constantly {:absent :target-not-carried}))))))

(deftest different-target-before-reader-fails
  (let [o (support/observe :r9-close-cause (constantly "M-other-target"))]
    (is (= "M-other-target" (:reader o)) (pr-str o))
    (is (not (w/received? o)))))

(deftest target-reader-product
  (let [{:keys [flights products]} (products/products :close)
        [a b] products]
    (is (= (dissoc (first flights) :target) (dissoc (second flights) :target)))
    (is (= products/targets (mapv #(get-in % [:targets 0 :target]) products)))
    (is (= (update a :targets #(mapv (fn [t] (dissoc t :target)) %))
           (update b :targets #(mapv (fn [t] (dissoc t :target)) %))))
    (println "target-close" (pr-str products))))
