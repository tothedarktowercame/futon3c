(ns futon3c.diagramprover.wm-wire-r2-store-constraints-read-r3-store-constraints-read-constraints-read-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-store-products-7a :as products]
            [futon3c.diagramprover.wm-wire-store-support :as support]))
(defn check [] (support/observe :constraints-read :none))
(def wire {:wire [:r2-store-constraints-read :r3-store-constraints-read :constraints-read]
           :kind :witnessed-hermetically :test `published-value-reaches-reader
           :second-layer {:test `publication-changes-reader-product
                          :kind :value-varying :product [:reader]
                          :intervention :before-reader}
           :check check :live-records-read support/live-records-read})
(deftest published-value-reaches-reader
  (is (w/received? (check))))
(deftest removed-file-breaks-handoff
  (let [o (support/observe :constraints-read :remove)]
    (is (empty? (:reader o)))
    (is (not (w/received? o)))))
(deftest changed-file-breaks-handoff
  (let [o (support/observe :constraints-read :different)]
    (is (= (:alternative o) (:reader o)))
    (is (not (w/received? o)))))

(deftest publication-changes-reader-product
  (let [{:keys [written products other-reader-values other-stored-values]}
        (products/products :constraints-read)
        [before after] products]
    (println :constraints-read :before before :after after)
    (is (not= before after))
    (is (apply = other-reader-values))
    (is (apply = other-stored-values))
    (is (= (first written) before))
    (is (nil? after))
    (is (not= (second written) after)))
)
