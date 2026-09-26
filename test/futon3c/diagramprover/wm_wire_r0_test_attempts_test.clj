(ns futon3c.diagramprover.wm-wire-r0-test-attempts-test
  "Wire [:r0-enact-step :r0-test :attempts]: the enactment step's attempts
  reaching the row-0 test.

  The writer is flight-runner/enact-fn (:r0-enact-step's site), which builds
  the attempts vector and writes it on the enactment record under :attempts.
  The reader is futon2.aif.flight-enact-test (:r0-test's site), whose
  enactment assertions all start from (:attempts enactment) (mapv :pattern,
  :commit, :check, :success over it).

  No live record carries the writer's end (live-records-read): every flight
  record's enactment is a typed absence, and the M-futon-seams exemplar
  enactment is hand-authored, not enact-fn's output. So the wire is
  WITNESSED-HERMETICALLY: enact-fn runs with a fixture seat over the
  flight-enact-test shape (two patterns, no grain pattern), the writer's
  value is (:attempts (:enactment out)), and the reader's is (:attempts
  record) read back from the record file the writer wrote."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-enact-driver :as d]))

(defn observe
  "The writer's attempts and the reader's, TAMPER editing the record file
  before the reader reads it (the bad cases)."
  ([] (observe identity))
  ([tamper]
   (let [{:keys [enactment record-path]} (d/enact-r0)]
     (d/rewrite record-path tamper)
     {:writer (:attempts enactment)
      :reader (:attempts (w/read-record record-path))})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "flight-278b6988.edn")
      :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
      :why "its one enactment is {:absent :no-dispatch-configured}: no seat was configured, so enact-fn never wrote an :attempts"}
     {:path (p "flight-d00574c8.edn")
      :sha256 "68e531b462d535983ea5114a8da370daec943a3b4283b6b87831e63ecf9eadd8"
      :why "its one enactment is {:absent :no-decision}: the click named no chosen action, so enact-fn wrote no record"}
     {:path (:path d/click-001-enactment)
      :sha256 (:sha256 d/click-001-enactment)
      :why "hand-authored (claude-10, 2026-09-24): schema :m-futon-seams/proof2a-enactment-v1, not enact-fn's :wm/enactment-v1; its :attempts were not written by the writer"}]))

(def wire
  {:wire [:r0-enact-step :r0-test :attempts]
   :kind :witnessed-hermetically
   :test `the-attempts-reach-the-row-0-test
   :check check
   :live-records-read live-records-read})

(deftest the-attempts-reach-the-row-0-test
  (let [o (check)]
    (is (= [:p/a :p/b] (mapv :pattern (:writer o))) "the writer's two attempts")
    (is (= 2 (count (:reader o))))
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  (let [o (observe #(assoc % :attempts {:absent :no-attempts}))]
    (is (= {:absent :no-attempts} (:reader o)))
    (is (not (w/received? o)))))

(deftest different-attempts-at-the-reader-fail-the-wire
  (let [o (observe (fn [r] (update r :attempts
                                   (fn [as] (assoc-in (vec as) [0 :commit] "c-other")))))]
    (is (some? (:reader o)))
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))

(deftest the-live-records-carry-no-enactment-attempts
  (doseq [{:keys [path sha256 why]} live-records-read]
    (is (= sha256 (w/sha256-file path)) why))
  (let [[f278 f005 ex] (map #(w/read-record (:path %)) live-records-read)]
    (is (= [{:absent :no-dispatch-configured}] (mapv :enactment (:enactments (:flight f278)))))
    (is (= [{:absent :no-decision}] (mapv :enactment (:enactments (:flight f005)))))
    (is (= :m-futon-seams/proof2a-enactment-v1 (:schema ex)))))
