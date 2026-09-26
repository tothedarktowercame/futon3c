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
  enactment predates H-publish. So the wire is WITNESSED-HERMETICALLY."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon2.aif.flight :as flight]
            [futon2.aif.flight-runner :as fr]
            [futon3c.diagramprover.wm-wire :as w]))

(def run-record
  {:repair/publication [{:status :receipt-committed :repair/id "occ-published" :repair/discharged? true}
                        {:status :publication-refused :repair/id "occ-refused" :reason :publication-error}]})

(defn- writer-value
  "observe-publication-fn's value over RECORD for REPAIR-ID."
  [repair-id record]
  (:publication-observed
   ((fr/observe-publication-fn {:fetch-run-record (fn [_] record)
                                :repair-id-fn (constantly repair-id)})
    {:target "T-repair-x"} {:click-id "run-p"})))

(defn- enactment-record
  "The enactment record enact-fn writes over RECORD for REPAIR-ID, through
  a real one-click flight (publication_observed_test's one-authority run)."
  [repair-id record]
  (let [enact (fr/enact-fn {:dispatch-step! (fn [_] {:commit "c" :produced :t :check {:class :fixture}})
                            :check-fn (constantly {:observed true})
                            :interpretations (constantly {:p/a {:produces #{:t}}})
                            :fetch-run-record (constantly record)
                            :repair-id-fn (constantly repair-id)
                            :record-dir (w/tmp-dir "wire-pub")})
        f (flight/run! (flight/start {:target "T-repair-x" :chosen-because {:kind :requested}}
                                     {:kind :a-exits :repo "futon2" :path "p" :read-text (fn [& _] "")}
                                     {:id "flight-wire-pub"})
                       {:click-fn (constantly {:click-id "run-p" :chosen {:candidate :cand/p :precedence [:p/a]}})
                        :enact-fn enact
                        :observe-fn (fn [_ _] {})
                        :sources-fn (constantly {})
                        :max-clicks 1})]
    (edn/read-string (slurp (:record-path (first (:enactments f)))))))

(defn observe
  "The writer's observation over WRITER-RECORD, and the reader's read of it
  off the enactment record written over READER-RECORD (the two differ only
  in the bad case): {:writer :reader}."
  [repair-id writer-record reader-record]
  {:writer (writer-value repair-id writer-record)
   :reader (:publication-observed (enactment-record repair-id reader-record))})

(defn check [] (observe "occ-published" run-record run-record))

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
  (let [o (observe nil run-record run-record)]
    (is (= :no-repair-obligation-for-target (:absent (:reader o))))
    (is (not (w/received? o)))))

(deftest a-different-observation-than-the-writers-fails-the-wire
  ;; the writer observed a committed receipt; the enactment record was
  ;; written over a run record whose only entry refused publication
  (let [refused {:repair/publication [{:status :publication-refused :repair/id "occ-published" :reason :publication-error}]}
        o (observe "occ-published" run-record refused)]
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
