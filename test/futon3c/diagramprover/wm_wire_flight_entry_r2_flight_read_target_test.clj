(ns futon3c.diagramprover.wm-wire-flight-entry-r2-flight-read-target-test
  "Scoped target handoff. Negative controls change the flight before the real reader."
  (:require [futon3c.diagramprover.wm-wire-target-products-16a :as products]
            [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-target-support :as support]))

(defn check [] (support/observe :r2-flight-read identity))
(def wire {:wire [:flight-entry :r2-flight-read [:target {:record :flight}]]
           :kind :witnessed-hermetically
           :test `the-target-reaches-the-reader :check check
           :second-layer {:test `target-reader-product :kind :record
                          :product [:asked 0 :request-id] :intervention :before-reader}
           :live-records-read support/live-records-read})

(deftest the-target-reaches-the-reader
  (is (w/received? (check)))
  (is (w/received? (support/observe :r2-flight-read identity))))

(deftest typed-absence-before-reader-fails
  (is (not (w/received? (support/observe :r2-flight-read (constantly {:absent :target-not-carried}))))))

(deftest different-target-before-reader-fails
  (let [o (support/observe :r2-flight-read (constantly "M-other-target"))]
    (is (= "M-other-target" (:reader o)) (pr-str o))
    (is (not (w/received? o)))))

;; read-fn:606-619 keys the criteria request by target; the returned
;; request id changes, while mission text, extracted reading and outcome do not.
(deftest target-reader-product
  (let [{:keys [flights products]} (products/products :read)
        [a b] products]
    (is (= (dissoc (first flights) :target) (dissoc (second flights) :target)))
    (is (not= (get-in a [:asked 0 :request-id]) (get-in b [:asked 0 :request-id])))
    (is (every? string? (map #(get-in % [:asked 0 :request-id]) products)))
    (is (= (:served-by a) (:served-by b)))
    (is (= [:not-answered :not-answered] (mapv #(get-in % [:asked 0 :outcome]) products)))
    (println "target-read" (pr-str products))))
