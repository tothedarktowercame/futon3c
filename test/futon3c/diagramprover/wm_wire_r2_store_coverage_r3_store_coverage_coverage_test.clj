(ns futon3c.diagramprover.wm-wire-r2-store-coverage-r3-store-coverage-coverage-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-store-products-6c :as products]
            [futon3c.diagramprover.wm-wire-store-support :as support]))
(defn check [] (support/observe :coverage :none))
(def wire {:wire [:r2-store-coverage :r3-store-coverage :coverage]
           :kind :witnessed-hermetically :test `published-value-reaches-reader
           :second-layer {:test `published-carrier-controls-reader-product
                          :kind :value-varying :product [:products]
                          :intervention :before-reader}
           :check check :live-records-read support/live-records-read})
(deftest published-value-reaches-reader
  (is (w/received? (check))))
(deftest removed-file-breaks-handoff
  (let [o (support/observe :coverage :remove)]
    (is (empty? (:reader o)))
    (is (not (w/received? o)))))
(deftest changed-file-breaks-handoff
  (let [o (support/observe :coverage :different)]
    (is (= (:alternative o) (:reader o)))
    (is (not (w/received? o)))))

(deftest published-carrier-controls-reader-product
  (let [r (products/products :coverage)
        [v v'] (:written r)
        [a b] (:products r)]
    (is (= v a))
    (is (not= (:mission-sha v) (:mission-sha v')))
    (is (= (dissoc v :mission-sha :receipt) (dissoc v' :mission-sha :receipt)))
    (is (nil? b) "A publication for another text is excluded.")
    (is (not= a b))
    (is (apply = (:other-reader-values r)))
    (is (apply = (:other-stored-values r)))
    (println :coverage :reader-products (:products r))))
