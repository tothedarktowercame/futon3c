(ns futon3c.diagramprover.wm-wire-run-chosen-summary-r0-enact-step-precedence-test
  "WIRE-23-C2 wire 1: [:run-chosen-summary :r0-enact-step [:precedence {:record :chosen}]].
  The REAL full-loop-runner/chosen-summary over a real select-action-cascades
  decision (pinned click-001 exemplar) hands its :precedence to the REAL
  flight-runner/enact-fn, whose dispatch order and :attempts are the read.
  WITNESSED-HERMETICALLY: the writer end is live on the 278b6988 tick record
  but no live record carries enact-fn's :attempts (see
  support/precedence-live-records-read)."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def wire-id [:run-chosen-summary :r0-enact-step [:precedence {:record :chosen}]])
(def producer (delay (producer-record/record "c2-chosen-precedence")))
(defn- fields [] (get-in @producer [:wires wire-id]))
(def positive (delay (:primary (fields))))
(defn check [] (let [o @positive] {:writer (:writer o) :reader (:reader o)}))

(def wire {
   :second-layer {:test 'futon3c.diagramprover.wm-wire-run-chosen-summary-r0-enact-step-precedence-test/different-value-before-reader :kind :value-varying
                  :product [:reader] :intervention :before-reader}
  :wire wire-id
           :kind :witnessed-hermetically :test `the-chosen-precedence-reaches-enact-fn
           :check check
           :live-records-read []
           :note "Writer: full_loop_runner.clj chosen-summary (:precedence of the chosen action). Reader: flight_runner.clj enact-fn (precedence drives the dispatch order and :attempts). Hermetic: no live record carries enact-fn's :attempts."})

(deftest the-chosen-precedence-reaches-enact-fn
  (let [o @positive]
    (is (seq (:writer o)))
    (is (= (:writer o) (:attempted o))
        "enact-fn dispatched exactly the chosen precedence, in order")
    (is (w/received? (check)) (pr-str (check)))))

(deftest absence-before-reader
  (let [o (get-in (fields) [:interventions :absent])]
    (is (empty? (:attempted o)) "no pattern dispatched without :precedence")
    (is (= {:absent :candidate-names-no-grain-pattern}
           (get-in o [:result :enactment :grain]))
        "the enactment records the typed absence")
    (is (not (w/received? {:writer (:writer o) :reader (:reader o)})))))

(deftest different-value-before-reader
  (let [o (get-in (fields) [:interventions :different])]
    (is (= [:different/pattern-a :different/pattern-b] (:reader o))
        "a different precedence is enacted as dispatched, not the writer's")
    (is (not (w/received? {:writer (:writer o) :reader (:reader o)})))))

(deftest live-records-lack-the-reader-end (is (seq (:live-records @producer))))
