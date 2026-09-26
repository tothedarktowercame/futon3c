(ns futon3c.diagramprover.wm-wire-r9-phase-kind-phase-kind-test-failure-kind-test
  "Wire [:r9-phase-kind :phase-kind-test :failure-kind]: the selection
  catch's re-thrown :failure-kind (phase-kind-failure, WM-PHASE-KIND-I)
  reaching the component's own test,
  futon2/test/futon2/aif/phase_kind_test.clj, whose read of the field is

    (get-in r [:result :data :failure-kind])

  asserted equal to the thrower's kind. A test box has no runtime var to
  drive through, so the hermetic witness performs exactly that read on one
  hermetic tick whose judge throws a bare-:kind exception; the writer's
  value is phase-kind-failure's re-thrown ex-data :failure-kind for the
  same exception.

  No live record carries the reader's end: every flight predates
  WM-PHASE-KIND-I (futon2 a41f4c31). The eighth flight's finding
  (live-records-read, pinned) carries the thrower's kind only as
  [:failure-data :kind] :substrate-unreachable and closed :untyped-failure.
  So the wire is WITNESSED-HERMETICALLY."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.full-loop-runner :as runner]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-r9-support :as sup]))

(def ^:private phase-kind-failure* @#'runner/phase-kind-failure)

(def live-record
  {:path (str w/spike-dir "/flight-ada87008/repair-occ-036463620c0c032c9e46aa44b6a6d6b35ed3e0ceb27dc58030f07d8fc2e6c747.edn")
   :sha256 "95bbb9c5476aedaadc50125068fff5f0a7e773c29a7a7494d84be58a57d893ad"
   :why "the eighth flight's finding, pre-WM-PHASE-KIND-I: the thrower's kind kept only as [:failure-data :kind] :substrate-unreachable; the close's :failure-kind is :untyped-failure, so the reader's end never crossed"})

(defn- kind-throw [kind]
  (ex-info "substrate-2 mission registry returned no missions" {:kind kind}))

(defn observe
  "phase-kind-failure on KIND's throw (writer), one hermetic tick whose
  judge throws it, read as phase_kind_test.clj reads :failure-kind
  (reader)."
  [kind]
  (let [e (kind-throw kind)
        rethrown (phase-kind-failure* e)
        {:keys [result]} (sup/run-tick e)]
    {:writer (:failure-kind (ex-data rethrown))
     :reader (get-in result [:data :failure-kind])}))

(defn check [] (observe :substrate-mission-registry-empty))

(def wire
  {:wire [:r9-phase-kind :phase-kind-test :failure-kind]
   :kind :witnessed-hermetically
   :test `the-phase-kind-reaches-the-components-test
   :check check
   :live-records-read [live-record]})

(deftest the-phase-kind-reaches-the-components-test
  (let [o (check)]
    (is (= :substrate-mission-registry-empty (:writer o)))
    (is (= :substrate-mission-registry-empty (:reader o))
        "the reader's own assertion: [:data :failure-kind] is the thrower's kind")
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  ;; explicit typing present (a typed absence under :failure-kind):
  ;; phase-kind-failure writes nothing and the classifier reads the typed
  ;; absence straight through. Driven at the vars: a full tick on this
  ;; throw cannot close — repair-obligation/occurrence-identity refuses a
  ;; non-keyword failure-kind (repair_obligation.clj:428), so the typed
  ;; absence never reaches a record.
  (let [e (ex-info "m" {:failure-kind {:absent :not-typed}})
        reader (@#'runner/explicit-failure-kind e)]
    (is (nil? (phase-kind-failure* e)) "the writer does not re-throw over explicit typing")
    (is (w/typed-absence? reader))
    (is (not (w/received? {:writer nil :reader reader})))))

(deftest a-different-kind-fails-the-wire
  (let [a (observe :substrate-mission-registry-empty)
        b (observe :invalid-temperature)]
    (is (= :invalid-temperature (:reader b)))
    (is (not (w/received? {:writer (:writer a) :reader (:reader b)})))))

(deftest the-live-record-closed-untyped
  (is (= (:sha256 live-record) (w/sha256-file (:path live-record))))
  (let [r (w/read-record (:path live-record))]
    (is (= {:kind :substrate-unreachable} (:failure-data r)))
    (is (= :untyped-failure (:failure-kind r))
        "pre-WM-PHASE-KIND-I: the kind stayed on the finding as data")))
