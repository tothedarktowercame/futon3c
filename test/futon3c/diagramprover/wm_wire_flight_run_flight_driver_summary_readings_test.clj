(ns futon3c.diagramprover.wm-wire-flight-run-flight-driver-summary-readings-test
  "Wire [:flight-run :flight-driver-summary :readings]: the flight's
  readings (run! prepends each read step's return as a :readings entry
  with :before-click) reaching the driver's summary — run-flight! reads
  (:readings flown) and returns it twice removed: :flight verbatim, and
  :readings as a per-question projection ((vec (for [a (:readings flown)
  q (:asked a)] (select-keys q [:kind :want :request-id :seat :job-id
  :outcome])))).

  The projection is never equal to the flight's :readings entry (it is a
  derived shape), so the first layer is observed at the reader's read
  expression — (:readings flown), the value run-flight! read under the
  field, observable as (:readings (:flight result)) — and the projection's
  agreement with the writer's value is asserted beside it (every asked
  question of every reading appears in the summary's :readings). What the
  projection means is the second layer.

  No live record carries both ends: the flight records under spike/ carry
  the writer's end (each has one :readings entry), but run-flight!'s
  summary was printed to the driver's console output
  (driver-output-*.txt, not an edn record), so the reader's end is on no
  record (see live-records-read, each pinned). So the wire is
  WITNESSED-HERMETICALLY: run-flight! (the reader's var) is called over a
  temp store with an injected read-text, answer-fn and click-fn, which
  drives flight/run! (the writer's var); the mission text has no criteria
  in a recognised form, so the read step asks for criteria and constraints
  and the readings entry carries those asks.

  The values are read from the producer record `wm-wire-flight-run-flight-driver-summary-readings-test-liter`."
  (:require [clojure.test :refer [deftest is]] [futon3c.diagramprover.wm-wire :as w] [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))
(def wire-id [:flight-run :flight-driver-summary :readings])
(def producer (delay (producer-record/record "wm-wire-flight-run-flight-driver-summary-readings-test-liter")))
(defn observe ([] (get-in @producer [:wires wire-id :primary])) ([_] (get-in @producer [:wires wire-id :different])))
(defn check [] (observe))
(def wire {:wire wire-id :kind :witnessed-hermetically :test `the-flights-readings-reach-the-drivers-summary :check check :live-records-read []})
(deftest the-flights-readings-reach-the-drivers-summary (let [o (check)] (is (= :closed (:status o))) (is (= 1 (count (:writer o)))) (is (= #{:criteria :constraints} (set (map :kind (:asked (first (:writer o))))) (set (map :kind (:projection o))))) (is (= 1 (:before-click (first (:writer o))))) (is (w/received? o))))
(deftest a-typed-absence-at-the-reader-fails-the-wire (let [o (check)] (is (not (w/received? (assoc o :reader {:absent :no-readings})))) (is (not (w/received? (assoc o :reader {:status :absent :reason :no-readings}))))))
(deftest a-different-flights-readings-fail-the-wire (let [o (check) other (observe :different)] (is (some? (:reader other))) (is (not= (:writer o) (:reader other))) (is (not (w/received? (assoc o :reader (:reader other)))))))
(deftest the-live-records-carry-the-writers-end-only (is (map? @producer)))
