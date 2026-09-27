(ns futon3c.diagramprover.wm-wire-r2-store-criteria-r3-store-criteria-criteria-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-store-products-6c :as products]
            [futon3c.diagramprover.wm-wire-store-support :as support]))
(defn check [] (support/observe :criteria :none))
(def wire {:wire [:r2-store-criteria :r3-store-criteria :criteria]
           :kind :witnessed-hermetically :test `published-value-reaches-reader
           :second-layer {:test `published-carrier-controls-reader-product
                          :kind :value-varying :product [:products]
                          :intervention :before-reader}
           :check check :live-records-read support/live-records-read})
(deftest published-value-reaches-reader
  (is (w/received? (check))))
(deftest removed-file-breaks-handoff
  (let [o (support/observe :criteria :remove)]
    (is (empty? (:reader o)))
    (is (not (w/received? o)))))
(deftest changed-file-breaks-handoff
  (let [o (support/observe :criteria :different)]
    (is (= (:alternative o) (:reader o)))
    (is (not (w/received? o)))))

(deftest published-carrier-controls-reader-product
  (let [r (products/products :criteria)
        [v v'] (:written r)
        [a b] (:products r)]
    (is (= 2 (count a)))
    (is (= a v))
    (is (= [(second a)] b) "Only the still-resolving criterion survives.")
    (is (= 2 (count v')))
    (is (not= a b))
    (is (apply = (:other-reader-values r)))
    (is (apply = (:other-stored-values r)))
    (println :criteria :reader-products (:products r))))
