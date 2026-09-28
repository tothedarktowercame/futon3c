(ns futon3c.diagramprover.wm-wire-r9-close-cause-failure-cause-record-test-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:r9-close-cause :failure-cause-record-test :failure-cause])
(def producer (delay (producer-record/record "r9-run-tick")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(defn check [] (:primary (fields)))
(def wire {:wire wire-id :kind :witnessed-hermetically :test `the-close-cause-reaches-the-components-test
           :check check :live-records-read []})
(deftest the-close-cause-reaches-the-components-test
  (let [o (check)]
    (is (= {:cause [{:class "java.lang.RuntimeException" :message "beneath"}]} (:writer o)))
    (is (= (:writer o) (:stored-read o)))
    (is (w/received? o) (str "writer-reader " (pr-str o)))))
(deftest no-cause-is-a-typed-absence-and-fails-the-wire
  (let [o (:typed-absence (fields))]
    (is (= {:absent :no-cause} (:writer o))) (is (= {:absent :no-cause} (:reader o)))
    (is (not (w/received? o)))))
(deftest a-different-cause-fails-the-wire
  (let [o (:different (fields))]
    (is (= {:cause [{:class "java.lang.RuntimeException" :message "elsewhere"}]} (:reader o)))
    (is (not (w/received? o)))))
(deftest the-live-findings-carry-no-failure-cause
  (let [x (get-in @producer [:live-controls :finding])]
    (is (:sha-ok? x)) (is (:cause-key-absent? x))
    (is (= {:absent :cause-not-on-record} (:cause-read x)))))
