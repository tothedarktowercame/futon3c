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
            [futon3c.diagramprover.wm-wire-c2-support :as support]))

(def positive (delay (support/chosen-precedence :none)))
(defn check [] (let [o @positive] {:writer (:writer o) :reader (:reader o)}))

(def wire {:wire [:run-chosen-summary :r0-enact-step [:precedence {:record :chosen}]]
           :kind :witnessed-hermetically :test `the-chosen-precedence-reaches-enact-fn
           :check check
           :live-records-read support/precedence-live-records-read
           :note "Writer: full_loop_runner.clj chosen-summary (:precedence of the chosen action). Reader: flight_runner.clj enact-fn (precedence drives the dispatch order and :attempts). Hermetic: no live record carries enact-fn's :attempts."})

(deftest the-chosen-precedence-reaches-enact-fn
  (let [o @positive]
    (is (seq (:writer o)))
    (is (= (:writer o) (:attempted o))
        "enact-fn dispatched exactly the chosen precedence, in order")
    (is (w/received? (check)) (pr-str (check)))))

(deftest absence-before-reader
  (let [o (support/chosen-precedence :absent)]
    (is (empty? (:attempted o)) "no pattern dispatched without :precedence")
    (is (= {:absent :candidate-names-no-grain-pattern}
           (get-in o [:result :enactment :grain]))
        "the enactment records the typed absence")
    (is (not (w/received? {:writer (:writer o) :reader (:reader o)})))))

(deftest different-value-before-reader
  (let [o (support/chosen-precedence :different)]
    (is (= [:different/pattern-a :different/pattern-b] (:reader o))
        "a different precedence is enacted as dispatched, not the writer's")
    (is (not (w/received? {:writer (:writer o) :reader (:reader o)})))))

(deftest live-records-lack-the-reader-end
  (support/assert-live-records support/precedence-live-records-read :attempts))
