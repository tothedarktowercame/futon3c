(ns futon3c.diagramprover.wm-wire-r2-store-locator-declines-r3-store-locator-declines-locator-declines-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-store-products-7a :as products]
            [futon3c.diagramprover.wm-wire-store-support :as support]))
(defn check [] (support/observe :locator-declines :none))
(def wire {:wire [:r2-store-locator-declines :r3-store-locator-declines :locator-declines]
           :kind :witnessed-hermetically :test `published-value-reaches-reader
           :second-layer {:test `publication-changes-reader-product
                          :kind :value-varying :product [:reader]
                          :intervention :before-reader}
           :check check :live-records-read support/live-records-read})
(deftest published-value-reaches-reader
  (is (w/received? (check))))
(deftest removed-file-breaks-handoff
  (let [o (support/observe :locator-declines :remove)]
    (is (empty? (:reader o)))
    (is (not (w/received? o)))))
(deftest changed-file-breaks-handoff
  (let [o (support/observe :locator-declines :different)]
    (is (= (:alternative o) (:reader o)))
    (is (not (w/received? o)))))

(deftest publication-changes-reader-product
  (let [{:keys [written products other-reader-values other-stored-values]}
        (products/products :locator-declines)
        [before after] products]
    (println :locator-declines :before before :after after)
    (is (not= before after))
    (is (apply = other-reader-values))
    (is (apply = other-stored-values))
    (is (= (first written) before))
    (is (empty? after))
    (is (not= (second written) after)))
)
