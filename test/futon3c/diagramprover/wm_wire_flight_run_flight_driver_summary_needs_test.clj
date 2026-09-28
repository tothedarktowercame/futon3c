(ns futon3c.diagramprover.wm-wire-flight-run-flight-driver-summary-needs-test
  "Wire [:flight-run :flight-driver-summary :needs]: the flight's needs
  (run! collects them: the read step's unanswered readings and asks, and
  each click abstention's missing input) reaching the driver's summary —
  run-flight! returns :needs (:needs flown) verbatim.

  No live record carries both ends: the flight records under spike/ carry
  the writer's end (the flight's :needs), but run-flight!'s summary was
  printed to the driver's console output (driver-output-*.txt, not an edn
  record), so the reader's end is on no record (see live-records-read,
  each pinned). So the wire is WITNESSED-HERMETICALLY: run-flight! (the
  reader's var) is called over a temp store with an injected read-text,
  answer-fn and click-fn, which drives flight/run! (the writer's var); the
  mission text has no criteria in a recognised form, so the read step's
  criteria and constraints requests go unanswered and join the flight's
  :needs. The writer's value is (:needs (:flight result)) as run! wrote
  it; the reader's is (:needs result) as run-flight! returned it.

  The values are read from the producer record `wm-wire-flight-run-flight-driver-summary-needs-test-literal-`."
  (:require [clojure.test :refer [deftest is]] [futon3c.diagramprover.wm-wire :as w] [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:flight-run :flight-driver-summary :needs])
(def producer (delay (producer-record/record "wm-wire-flight-run-flight-driver-summary-needs-test-literal-")))
(defn observe ([] (get-in @producer [:wires wire-id :primary])) ([_] (get-in @producer [:wires wire-id :different])))
(defn check [] (observe))
(def wire {:wire wire-id :kind :witnessed-hermetically :test `the-flights-needs-reach-the-drivers-summary :check check :live-records-read []})
(deftest the-flights-needs-reach-the-drivers-summary (let [o (check)] (is (= :closed (:status o))) (is (= 2 (count (:writer o)))) (is (every? #(= :not-answered (:kind %)) (:writer o))) (is (= "wire-job-1" (:job-id (first (:writer o))))) (is (w/received? o))))
(deftest a-typed-absence-at-the-reader-fails-the-wire (let [o (check)] (is (not (w/received? (assoc o :reader {:absent :no-needs})))) (is (not (w/received? (assoc o :reader {:status :absent :reason :no-needs}))))))
(deftest a-different-flights-needs-fail-the-wire (let [o (check) other (observe :different)] (is (= 3 (count (:reader other)))) (is (not= (:writer o) (:reader other))) (is (not (w/received? (assoc o :reader (:reader other)))))))
(deftest the-live-records-carry-the-writers-end-only (is (map? @producer)))
