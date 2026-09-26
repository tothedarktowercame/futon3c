(ns futon3c.diagramprover.wm-wire-r9-phase-kind-r9-failure-classifier-failure-kind-test
  "Wire [:r9-phase-kind :r9-failure-classifier :failure-kind]: the
  selection catch's re-thrown :failure-kind (phase-kind-failure,
  WM-PHASE-KIND-I) reaching the runner's failure classifier
  (explicit-failure-kind), which reads :failure-kind off the ex-data
  anywhere in the cause chain. This is the read that closes a
  bare-:kind-throwing tick with its kind instead of :untyped-failure.

  No live record carries the reader's end (see the phase-kind-test wire's
  pinned live record: the eighth flight closed :untyped-failure, pre-fix).
  So the wire is WITNESSED-HERMETICALLY: the writer's var re-throws a
  bare-:kind exception carrying :failure-kind, the reader's var reads that
  very exception, and one hermetic tick whose judge throws the same
  exception closes with the kind as its recorded [:data :failure-kind]."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.full-loop-runner :as runner]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-r9-support :as sup]))

(def ^:private phase-kind-failure* @#'runner/phase-kind-failure)
(def ^:private explicit-failure-kind* @#'runner/explicit-failure-kind)

(def live-record
  {:path (str w/spike-dir "/flight-ada87008/repair-occ-036463620c0c032c9e46aa44b6a6d6b35ed3e0ceb27dc58030f07d8fc2e6c747.edn")
   :sha256 "95bbb9c5476aedaadc50125068fff5f0a7e773c29a7a7494d84be58a57d893ad"
   :why "the eighth flight's finding, pre-WM-PHASE-KIND-I: [:failure-data :kind] :substrate-unreachable carried as data, :failure-kind :untyped-failure — the classifier read nothing of the kind"})

(defn- kind-throw [kind]
  (ex-info "substrate-2 mission registry returned no missions" {:kind kind}))

(defn observe
  "phase-kind-failure re-throws KIND's throw (writer); explicit-failure-kind
  reads that very re-thrown exception (reader); one hermetic tick closes
  with it. {:writer :failure-kind the writer put on, :reader the
  classifier's read, :closed the tick's recorded [:data :failure-kind]}."
  [kind]
  (let [e (kind-throw kind)
        rethrown (phase-kind-failure* e)
        {:keys [result]} (sup/run-tick e)]
    {:writer (:failure-kind (ex-data rethrown))
     :reader (explicit-failure-kind* rethrown)
     :closed (get-in result [:data :failure-kind])}))

(defn check [] (observe :substrate-mission-registry-empty))

(def wire
  {:wire [:r9-phase-kind :r9-failure-classifier :failure-kind]
   :kind :witnessed-hermetically
   :test `the-phase-kind-reaches-the-failure-classifier
   :check check
   :live-records-read [live-record]})

(deftest the-phase-kind-reaches-the-failure-classifier
  (let [o (check)]
    (is (= :substrate-mission-registry-empty (:writer o)))
    (is (= :substrate-mission-registry-empty (:reader o)))
    (is (= (:reader o) (:closed o))
        "the tick's recorded failure-kind is the classifier's read")
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  ;; explicit typing present as a typed absence: the writer does not
  ;; re-throw, and the classifier reads the typed absence straight through.
  ;; (No full tick here: occurrence-identity refuses a non-keyword
  ;; failure-kind, repair_obligation.clj:428, so this shape never closes.)
  (let [e (ex-info "m" {:failure-kind {:absent :not-typed}})]
    (is (nil? (phase-kind-failure* e)))
    (let [reader (explicit-failure-kind* e)]
      (is (w/typed-absence? reader))
      (is (not (w/received? {:writer nil :reader reader}))))))

(deftest a-different-kind-fails-the-wire
  (let [a (observe :substrate-mission-registry-empty)
        b (observe :invalid-temperature)]
    (is (= :invalid-temperature (:reader b)))
    (is (not (w/received? {:writer (:writer a) :reader (:reader b)})))))

(deftest the-live-record-closed-untyped
  (is (= (:sha256 live-record) (w/sha256-file (:path live-record))))
  (let [r (w/read-record (:path live-record))]
    (is (= {:kind :substrate-unreachable} (:failure-data r)))
    (is (= :untyped-failure (:failure-kind r)))))
