(ns futon3c.diagramprover.wm-wire-r2-store-locators-r3-store-locators-locators-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-store-products-6c :as products]
            [futon3c.diagramprover.wm-wire-store-support :as support]))
(defn check [] (support/observe :locators :none))
(def wire {:wire [:r2-store-locators :r3-store-locators :locators]
           :kind :witnessed-hermetically :test `published-value-reaches-reader
           :second-layer {:test `published-carrier-controls-reader-product
                          :kind :record :product [:products]
                          :intervention :before-reader}
           :check check :live-records-read support/live-records-read})
(deftest published-value-reaches-reader
  (is (w/received? (check))))
(deftest removed-file-breaks-handoff
  (let [o (support/observe :locators :remove)]
    (is (empty? (:reader o)))
    (is (not (w/received? o)))))
(deftest changed-file-breaks-handoff
  (let [o (support/observe :locators :different)]
    (is (= (:alternative o) (:reader o)))
    (is (not (w/received? o)))))

(deftest published-carrier-controls-reader-product
  (let [r (products/products :locators)
        [v v'] (:written r)
        [a b] (:products r)]
    (is (= v a))
    (is (= v' b))
    (is (= "first.clj" (get-in a [:done :path])))
    (is (= "second.clj" (get-in b [:done :path])))
    (is (= (update a :done dissoc :path) (update b :done dissoc :path))
        "Locator projection computes no new verdict; other locator data is unchanged.")
    (is (not= a b))
    (is (apply = (:other-reader-values r)))
    (is (apply = (:other-stored-values r)))
    (println :locators :reader-products (:products r))))
