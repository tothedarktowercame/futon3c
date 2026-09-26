(ns futon3c.diagramprover.wm-wire-r5-gate-grain-test
  "Wire [:r5-flight-call :r5-grain-gate :grain]: the candidate's grain
  reaching the grain gate.

  The writer is the flight's enactment call, flight-runner/enact-fn
  (:r5-flight-call's site): it takes the chosen candidate's grain (the
  interpretation that declares :grain) and hands it to grain-gate as the
  candidate argument before the grain attempt's commit is asked for; the
  same grain is written on the enactment record under :grain. The reader
  is futon2.aif.grain-gate/grain-gate (:r5-grain-gate's site), whose first
  comparison is the candidate's (:keyed-by (:grain candidate)).

  No live record carries the writer's end (live-records-read), so the wire
  is WITNESSED-HERMETICALLY: enact-fn runs with a fixture seat over the
  flight-grain-gate-test shape, the writer's value is the grain it wrote
  on the record (:grain enactment), and the reader's is the value under
  :grain in the candidate argument grain-gate actually received, captured
  at the call. Both are the role grain from the pinned exemplar record."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.grain-gate :as gate]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-enact-driver :as d]))

(def repo-root "/home/joe/code/futon3c")

(defn check []
  (let [{:keys [enactment gate-calls]} (d/enact-r5)]
    {:writer (:grain enactment)
     :reader (get-in (first gate-calls) [:candidate :grain])
     :gate-result (:grain-gate (first (:attempts enactment)))}))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "flight-278b6988.edn")
      :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
      :why "its one enactment is {:absent :no-dispatch-configured}: the flight call never handed a grain to the gate"}
     {:path (p "flight-e70b4baf.edn")
      :sha256 "10ac7cd76b2b18b40c4044bb55f0e54bc210cd908d9c4deef74875fc9d2bc88c"
      :why "its one enactment is {:absent :no-decision}: no record written"}
     {:path (:path d/click-001-enactment)
      :sha256 (:sha256 d/click-001-enactment)
      :why "hand-authored (claude-10, 2026-09-24): its :grain is the role grain this test pins and plans, but it was not written by enact-fn and no gate call is recorded"}
     {:path (:path d/click-001-outcome)
      :sha256 (:sha256 d/click-001-outcome)
      :why "hand-authored: the provider grain (the failed first attempt's), the different real value the second bad case drives the reader with"}]))

(def wire
  {:wire [:r5-flight-call :r5-grain-gate :grain]
   :kind :witnessed-hermetically
   :test `the-candidates-grain-reaches-the-gate
   :check check
   :live-records-read live-records-read})

(deftest the-candidates-grain-reaches-the-gate
  (let [o (check)]
    (is (= :role (get-in o [:writer :keyed-by])))
    (is (= {:status :pass} (:gate-result o)) "the gate compared the grain it received and passed")
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  ;; the absence is the exact one enact-fn writes when the candidate names
  ;; no grain pattern, taken from a real run; the gate refuses it typed
  (let [absence (:grain (:enactment (d/enact-r0)))
        result (gate/grain-gate {:grain absence} {:grain (d/role-grain)} repo-root)
        o {:writer (d/role-grain) :reader absence}]
    (is (= {:absent :candidate-names-no-grain-pattern} absence))
    (is (= :grain-not-declared (:reason result)) "the gate read the absence and refused")
    (is (not (w/received? o)))))

(deftest a-different-grain-at-the-reader-fails-the-wire
  ;; the provider grain from the pinned outcome record, delivered to the
  ;; reader where the writer's role grain was expected
  (let [other (d/provider-grain)
        result (gate/grain-gate {:grain other} {:grain other} repo-root)
        o {:writer (d/role-grain) :reader other}]
    (is (= :agent-id (:keyed-by other)))
    (is (map? result) "the gate read the different grain and answered")
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))

(deftest the-live-records-carry-no-gate-call
  (doseq [{:keys [path sha256 why]} live-records-read]
    (is (= sha256 (w/sha256-file path)) why))
  (let [[f278 fe70 _ex _out] (map #(w/read-record (:path %)) live-records-read)]
    (is (= [{:absent :no-dispatch-configured}] (mapv :enactment (:enactments (:flight f278)))))
    (is (= [{:absent :no-decision}] (mapv :enactment (:enactments (:flight fe70)))))))
