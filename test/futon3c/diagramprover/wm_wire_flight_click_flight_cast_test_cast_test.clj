(ns futon3c.diagramprover.wm-wire-flight-click-flight-cast-test-cast-test
  "Wire [:flight-click :flight-cast-test :cast]: the cast http-click-fn
  sent the click with reaching the component's own test
  (futon2/test/futon2/aif/flight_cast_test.clj, the :box/kind :test box),
  whose read of this field is

    (is (= {:author \"a\" :reviewer \"b\"
            :repair-reviewer {:absent :no-repair-reviewer-given}}
           (:cast entry)))

  in the-cast-goes-on-the-body-and-the-click-entry — the entry being the
  click entry record-click wrote over http-click-fn's result in a run! of
  one click. A test box has no runtime var to drive through, so the
  hermetic witness performs exactly that read over a real call of the
  writer's var: http-click-fn with stubbed ports and the pinned seventh
  run record (its read-record!), run! as the test's own `click` fn does,
  then (:cast entry).

  The seat values are the eighth flight's own (author claude-6, reviewer
  claude-13, repair-reviewer not given), so the witnessed value is the
  same map the eighth flight's click entry carries live — the writer's end
  is present live (live-records-read), but the reader is a test, whose
  read is on no record, so the wire is WITNESSED-HERMETICALLY.

  The values are read from the producer record `wm-wire-flight-click-flight-cast-test-cast-test-literal-fixt`."
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
