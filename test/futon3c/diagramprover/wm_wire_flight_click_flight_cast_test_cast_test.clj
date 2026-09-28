(ns futon3c.diagramprover.wm-wire-flight-click-flight-cast-test-cast-test
  (:require [clojure.test :refer [deftest is testing]] [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def producer (delay (producer-record/record "wm-wire-flight-click-flight-cast-test-cast-test-literal-fixt")))
(defn- fields [] (:fields @producer))
(defn check [] (select-keys (fields) [:writer :reader]))
(def wire {:wire [:flight-click :flight-cast-test :cast] :kind :witnessed-hermetically
           :test `the-cast-reaches-the-components-own-test :check check :live-records-read []})
(deftest the-cast-reaches-the-components-own-test
  (let [f (fields) o (check)] (is (:writer-present? f)) (is (false? (:writer-typed-absence? f)))
    (is (:seventh-pin? f)) (is (:cast-expected? f)) (is (:writer-live? f)) (is (w/received? o) (pr-str o))))
(deftest a-missing-cast-is-a-typed-absence-and-fails-the-wire
  (let [a (:absent (fields))]
    (is (= {:absent :field-not-carried} (:reader a))) (is (:typed? a))
    (is (false? (:received? a))) (is (:legacy-absent-refused? a))))
(deftest a-different-cast-fails-the-wire
  (let [d (:different (fields))]
    (is (= {:author "claude-13" :reviewer "claude-6" :repair-reviewer {:absent :no-repair-reviewer-given}} (:reader d)))
    (is (:writer-reader-differ? d)) (is (false? (:received? d)))))
(deftest the-live-records-are-as-read
  (doseq [[k v] (:live-records (fields))] (testing (name k) (is (true? v)))))
