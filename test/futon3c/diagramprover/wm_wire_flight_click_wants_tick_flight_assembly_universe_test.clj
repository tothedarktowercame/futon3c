(ns futon3c.diagramprover.wm-wire-flight-click-wants-tick-flight-assembly-universe-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-click-products-8a :as products]
            [futon3c.diagramprover.wm-wire-ask-out-support :as support]))
(def positive (delay (support/click :assembly :universe :none)))
(defn check [] @positive)
(def wire {:wire [:flight-click-wants :tick-flight-assembly :universe]
           :kind :witnessed-hermetically :test `the-real-reader-receives-the-published-value :second-layer {:test `reader-carries-the-intervened-value
                          :kind :record :product [:products]
                          :intervention :before-reader}
           :check check
           :live-records-read support/live-records-read
           :note "click-wants from real A-exits text, returned map tampered before tick-flight-assembly; reads the named field from judge options into sources when assembling."})
(deftest the-real-reader-receives-the-published-value
  (is (seq (support/live-census)))
  (let [o (check)]
    (is (w/received? o))
    (is (seq (:reader o)))))
(deftest absent-carrier-before-reader-is-not-a-witness
  (let [o (support/click :assembly :universe :absent)]
    (is (not (w/received? o)))
    ))
(deftest different-carrier-changes-the-reader-product
  (let [o (support/click :assembly :universe :different)]
    (is (not (w/received? o)))
    (is (not= (:writer o) (:reader o)))
    ))

(deftest reader-carries-the-intervened-value
  (let [r (products/products :assembly :universe)
        [before after] (:products r)]
    (is (seq before))
    (is (= (:written r) (:products r)))
    (is (not= before after))
    (is (apply = (:unchanged r))
        "No score or selection is computed here; all other output and context results are unchanged.")
    (is (= [[:original :original :original] [:original :original :original]] (:context r)))
    (println :assembly :universe :products (:products r))))
