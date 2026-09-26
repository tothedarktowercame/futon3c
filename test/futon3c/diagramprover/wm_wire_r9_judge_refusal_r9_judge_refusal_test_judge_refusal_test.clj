(ns futon3c.diagramprover.wm-wire-r9-judge-refusal-r9-judge-refusal-test-judge-refusal-test
  "Wire [:r9-judge-refusal :r9-judge-refusal-test :judge-refusal]: the
  :no-selection sorry cell's :judge-refusal (judge-refusal-sorry) reaching
  the component's own test, futon2/test/futon2/aif/judge_refusal_abstention_test.clj,
  whose read of the field is

    (get-in result [:checkpoints :selection :sorry :judge-refusal :kind])

  asserted equal to the refusing kind. A test box has no runtime var to
  drive through, so the hermetic witness performs exactly that read on one
  hermetic tick whose judge throws a typed \"cascade decision refused\": the
  writer's value is judge-refusal-sorry's output for that refusal, the
  reader's value the run's checkpoint cell read as the reader reads it.

  No live record carries either end (live-records-read, the same two
  records the abstention-carrier wire reads: the pre-WM-CLICK-REFUSAL-I
  abstained tick carries no :judge-refusal; the fourth flight's refused
  tick keeps no sorry cell). So the wire is WITNESSED-HERMETICALLY."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.full-loop-runner :as runner]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-r9-support :as sup]))

(def ^:private judge-refusal-sorry* @#'runner/judge-refusal-sorry)

(defn- refusal [kind]
  (ex-info "cascade decision refused" {:kind kind :target "M-t"}))

(defn observe
  "KIND's refusal through judge-refusal-sorry (writer) and through one
  hermetic tick, read as judge_refusal_abstention_test.clj reads it
  (reader). {:writer the sorry cell's :judge-refusal, :reader the run's
  checkpoint cell's :judge-refusal, or {:absent :no-judge-refusal}}."
  [kind]
  (let [je (refusal kind)
        cell (judge-refusal-sorry* (runner/judge-refusal je "M-t"))
        {:keys [result]} (sup/run-tick je)]
    {:writer (get-in cell [:sorry :judge-refusal])
     :reader (let [v (get-in result [:checkpoints :selection :sorry :judge-refusal])]
               (if (some? v) v {:absent :no-judge-refusal}))}))

(defn check [] (observe :live-c-stale))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "tick-run-record-2026-09-25-flight-ffcd772b-click-1.edn")
      :sha256 "8ab0db5d770085f17bb341293a92424149bf2388ae7a6dd3e724887c9a44eba2"
      :why "an abstained tick, but pre-WM-CLICK-REFUSAL-I: no :judge-refusal key occurs in the record"}
     {:path (p "tick-run-record-2026-09-26-flight-e70b4baf-click-1.edn")
      :sha256 "241b10a024020344eba5d444c12fb33ad6afe107724dec95c81229b422bc5feb"
      :why "the fourth flight's refused tick, pre-fix: [:decision :abstention] is a typed absence, no sorry cell kept"}]))

(def wire
  {:wire [:r9-judge-refusal :r9-judge-refusal-test :judge-refusal]
   :kind :witnessed-hermetically
   :test `the-judge-refusal-reaches-the-components-test
   :check check
   :live-records-read live-records-read})

(deftest the-judge-refusal-reaches-the-components-test
  (let [o (check)]
    (is (= {:kind :live-c-stale :target "M-t" :missing :live-c
            :data {:kind :live-c-stale :target "M-t"}}
           (:writer o)))
    (is (= :live-c-stale (get-in o [:reader :kind]))
        "the reader's own assertion: the sorry cell's :judge-refusal :kind")
    (is (w/received? o))))

(deftest no-refusal-is-a-typed-absence-and-fails-the-wire
  (let [{:keys [result]} (sup/run-tick (ex-info "boom" {:x 1}))
        v (get-in result [:checkpoints :selection :sorry :judge-refusal])]
    (is (nil? v))
    (is (not (w/received? {:writer v :reader {:absent :no-judge-refusal}})))))

(deftest a-different-refusal-fails-the-wire
  (let [a (observe :live-c-stale)
        b (observe :incommensurable-family)]
    (is (some? (:reader b)))
    (is (not= (:writer a) (:reader b)))
    (is (not (w/received? {:writer (:writer a) :reader (:reader b)})))))
