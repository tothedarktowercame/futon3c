(ns futon3c.diagramprover.wm-wire-flight-click-wants-flight-judge-opts-universe-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-ask-out-support :as support]))
(def positive (delay (support/click :opts :universe :none)))
(defn check [] @positive)
(def wire {:wire [:flight-click-wants :flight-judge-opts :universe]
           :kind :witnessed-hermetically :test `the-real-reader-receives-the-published-value :check check
           :live-records-read support/live-records-read
           :note "click-wants from real A-exits text, returned map tampered before flight-judge-opts; reads the named field from judge options into sources when assembling."})
(deftest the-real-reader-receives-the-published-value
  (is (seq (support/live-census)))
  (let [o (check)]
    (is (w/received? o))
    (is (seq (:reader o)))))
(deftest absent-carrier-before-reader-is-not-a-witness
  (let [o (support/click :opts :universe :absent)]
    (is (not (w/received? o)))
    ))
(deftest different-carrier-changes-the-reader-product
  (let [o (support/click :opts :universe :different)]
    (is (not (w/received? o)))
    (is (not= (:writer o) (:reader o)))
    ))
