(ns futon3c.diagramprover.wm-wire-r9-judge-refusal-abstention-r9-failure-classifier-outcome-test
  "Wire [:r9-judge-refusal-abstention :r9-failure-classifier :outcome]: the
  abstention exception's {:outcome :abstained} (judge-refusal-abstention,
  WM-MAP-REPLAY-I defect G) reaching the runner's failure classifier
  (explicit-failure-kind), which reads :outcome off the ex-data anywhere in
  the cause chain. This is the read that closes a refused tick :abstained
  instead of :untyped-failure.

  No live record carries both ends: every flight predates the fix
  (321d82c8). The fourth flight's refused tick (live-records-read, pinned)
  closed :untyped-failure — the classifier read nothing of the refusal. So
  the wire is WITNESSED-HERMETICALLY: the writer's var is called and its
  thrown ex-data's :outcome observed; the reader's value is the tick's
  recorded [:data :failure-kind], which is explicit-failure-kind's read of
  :outcome (failure-kind-from consults it first), off one hermetic tick
  whose judge throws a typed refusal."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.full-loop-runner :as runner]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-r9-support :as sup]))

(def ^:private judge-refusal-abstention* @#'runner/judge-refusal-abstention)
(def ^:private explicit-failure-kind* @#'runner/explicit-failure-kind)

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "tick-run-record-2026-09-26-flight-e70b4baf-click-1.edn")
      :sha256 "241b10a024020344eba5d444c12fb33ad6afe107724dec95c81229b422bc5feb"
      :why "the fourth flight's refused tick, pre-321d82c8: no :outcome :abstained to read; the click closed :untyped-failure"}]))

(defn- refusal [kind]
  (ex-info "cascade decision refused" {:kind kind :target "M-t"}))

(defn- written-outcome
  "The writer var's :outcome for KIND's refusal, caught off the throw."
  [kind]
  (let [je (refusal kind)]
    (:outcome (ex-data (try (throw (judge-refusal-abstention* (runner/judge-refusal je "M-t") je))
                            (catch clojure.lang.ExceptionInfo x x))))))

(defn observe
  "judge-refusal-abstention thrown (writer), one hermetic tick on the same
  refusal (reader). {:writer the thrown ex-data's :outcome, :reader the
  classifier's read of it as the tick's [:data :failure-kind]}."
  [kind]
  (let [{:keys [result]} (sup/run-tick (refusal kind))]
    {:writer (written-outcome kind)
     :reader (get-in result [:data :failure-kind])}))

(defn check [] (observe :live-c-stale))

(def wire
  {:wire [:r9-judge-refusal-abstention :r9-failure-classifier :outcome]
   :kind :witnessed-hermetically
   :test `the-abstentions-outcome-reaches-the-failure-classifier
   :check check
   :live-records-read live-records-read})

(deftest the-abstentions-outcome-reaches-the-failure-classifier
  (let [o (check)]
    (is (= :abstained (:writer o)))
    (is (= :abstained (:reader o))
        "explicit-failure-kind read :outcome :abstained; the tick did not close :untyped-failure")
    (is (w/received? o))))

(deftest a-typed-absence-under-outcome-fails-the-wire
  ;; the reader's var driven on an ex-data whose :outcome is a typed absence
  (let [reader (explicit-failure-kind* (ex-info "x" {:outcome {:absent :no-outcome}}))]
    (is (w/typed-absence? reader))
    (is (not (w/received? {:writer (written-outcome :live-c-stale) :reader reader})))))

(deftest a-different-outcome-fails-the-wire
  ;; judge-refusal-abstention writes only :abstained; the classifier reading
  ;; any other :outcome is not this wire's value
  (let [reader (explicit-failure-kind* (ex-info "x" {:outcome :grounded-change}))]
    (is (= :grounded-change reader))
    (is (not (w/received? {:writer (written-outcome :live-c-stale) :reader reader})))))

(deftest the-live-record-closed-untyped
  (let [{:keys [path sha256]} (first live-records-read)]
    (is (= sha256 (w/sha256-file path)))
    (let [r (w/read-record path)]
      (is (= {:status :absent :reason :no-selection-decision-recorded}
             (get-in r [:decision :abstention]))
          "pre-fix: no abstention, so no :outcome :abstained for the classifier to read"))))
