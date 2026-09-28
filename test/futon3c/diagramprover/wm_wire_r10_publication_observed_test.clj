(ns futon3c.diagramprover.wm-wire-r10-publication-observed-test
  "Wire [:r10-observe-publication :r10-test :publication-observed]: the
  publication observation reaching row 10's own test (H-publish).

  The writer is flight-runner/observe-publication-fn; the reader is
  futon2/test/futon2/aif/publication_observed_test.clj (the :box/kind :test
  box), whose one-authority read is

    (is (= (:publication-observed record) (:publication-observed entry)))

  — the enactment record's :publication-observed is the writer's value,
  copied by enact-fn (flight_runner.clj step 12). A test box has no runtime
  var to drive through, so the hermetic witness performs exactly that read:
  the writer runs over a fixture run record, and the reader's value is the
  :publication-observed on the enactment record enact-fn writes for the
  same flight and click.

  No live record carries either end (live-records-read, each pinned and
  read): every flight record's one enactment is a typed absence, so no
  enactment record was ever written live, and the hand-authored exemplar
  enactment predates H-publish. So the wire is WITNESSED-HERMETICALLY.

  Converted to read the wire ends from the content-addressed
  wm-wire-r10-publication-observed-test-literal-fixture producer record;
  this reader loads no product code."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

(def producer (delay (producer-record/record "wm-wire-r10-publication-observed-test-literal-fixture")))

(defn check [] (get-in @producer [:cases :committed]))

(def live-records-read
  [{:path (str w/spike-dir "/flight-278b6988.edn")
    :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
    :why "its one enactment is {:absent :no-dispatch-configured}: no enactment record was written, so no :publication-observed on either end"}
   {:path "holes/labs/M-futon-seams/exemplar/click-001-enactment.edn"
    :sha256 "e51063896e2a42096718d902e0b4dfe0e4652323de0b42848c2c0cf318bf6c89"
    :why "the hand-authored exemplar enactment (claude-10, 2026-09-24) predates H-publish: it carries no :publication-observed"}])

(def wire
  {:wire [:r10-observe-publication :r10-test :publication-observed]
   :kind :witnessed-hermetically
   :test `the-observation-reaches-the-enactment-record
   :check check
   :live-records-read live-records-read})

(deftest the-observation-reaches-the-enactment-record
  (let [o (check)]
    (is (true? (get-in (:writer o) [:observed])))
    (is (= "occ-published" (get-in (:reader o) [:evidence :repair/id])))
    (is (w/received? o))))

(deftest no-repair-obligation-is-a-typed-absence-and-fails-the-wire
  (let [o (get-in @producer [:cases :no-repair])]
    (is (= :no-repair-obligation-for-target (:absent (:reader o))))
    (is (not (w/received? o)))))

(deftest a-different-observation-than-the-writers-fails-the-wire
  ;; the writer observed a committed receipt; the enactment record was
  ;; written over a run record whose only entry refused publication
  (let [o (get-in @producer [:cases :refused])]
    (is (true? (get-in (:writer o) [:observed])))
    (is (false? (get-in (:reader o) [:observed])))
    (is (some? (:reader o)))
    (is (not (w/received? o)) "present, not absent, but not the value the writer wrote")))

(deftest the-live-records-carry-no-observation
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path))
  (let [f (:flight (w/read-record (:path (first live-records-read))))]
    (is (= [{:absent :no-dispatch-configured}] (mapv :enactment (:enactments f)))))
  (is (not (contains? (w/read-record (:path (second live-records-read))) :publication-observed))))
