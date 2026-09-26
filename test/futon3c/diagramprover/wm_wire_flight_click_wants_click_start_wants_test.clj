(ns futon3c.diagramprover.wm-wire-flight-click-wants-click-start-wants-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-ask-out-support :as support]))
(def positive (delay (support/click :http :wants :none)))
(defn check [] @positive)
(def wire {:wire [:flight-click-wants :click-start :wants]
           :kind :witnessed-hermetically :test `the-real-reader-receives-the-published-value :check check
           :live-records-read support/live-records-read
           :note "Real http-click-fn post body into direct handle-wm-click-start, request shape from flight-click-http-test. Runner IO captures handler-produced options; no click issued."})
(deftest the-real-reader-receives-the-published-value
  (is (seq (support/live-census)))
  (let [o (check)]
    (is (w/received? o))
    (is (seq (:reader o)))))
(deftest absent-carrier-before-reader-is-not-a-witness
  (let [o (support/click :http :wants :absent)]
    (is (not (w/received? o)))
    (is (nil? (:result o)))))
(deftest different-carrier-changes-the-reader-product
  (let [o (support/click :http :wants :different)]
    (is (not (w/received? o)))
    (is (not= (:writer o) (:reader o)))
    ))
