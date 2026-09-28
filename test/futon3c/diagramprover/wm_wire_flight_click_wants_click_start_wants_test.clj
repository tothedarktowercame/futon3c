(ns futon3c.diagramprover.wm-wire-flight-click-wants-click-start-wants-test
  "Flight click-wants wire, read from the content-addressed
  ask-out-live-census-g32 producer record. The producer ran the real
  (support/click :http :wants ...) calls through http-click-fn and
  handle-wm-click-start; this reader loads no product code."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def producer (delay (producer-record/record "ask-out-live-census-g32")))
(def live-census (delay (:live-census @producer)))
(def live-records-read (delay (:live-records-read @producer)))
(defn click [mutation] (get-in @producer [:cases mutation]))
(defn check [] (click :none))
(def wire {:wire [:flight-click-wants :click-start :wants]
           :kind :witnessed-hermetically :test `the-real-reader-receives-the-published-value :check check
           :live-records-read @live-records-read
           :note "Real http-click-fn post body into direct handle-wm-click-start, request shape from flight-click-http-test. Runner IO captures handler-produced options; no click issued."})
(deftest the-real-reader-receives-the-published-value
  (is (seq @live-census))
  (let [o (check)]
    (is (w/received? o))
    (is (seq (:reader o)))))
(deftest absent-carrier-before-reader-is-not-a-witness
  (let [o (click :absent)]
    (is (not (w/received? o)))
    (is (nil? (:result o)))))
(deftest different-carrier-changes-the-reader-product
  (let [o (click :different)]
    (is (not (w/received? o)))
    (is (not= (:writer o) (:reader o)))
    ))
