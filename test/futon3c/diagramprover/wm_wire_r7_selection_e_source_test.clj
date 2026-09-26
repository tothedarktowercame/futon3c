(ns futon3c.diagramprover.wm-wire-r7-selection-e-source-test
  "Wire [:r7-selection :r7-test :e-source]: the selection law's record of
  where E came from reaching row 7's own test.

  The writer is policy/select-action-cascades, which writes :e-source on
  the selection law ({:source :enactment-fold :records n :samples n
  :uniform bool}). The reader is
  futon2/test/futon2/aif/selection_reads_fold_test.clj (the :box/kind :test
  box), whose read of this field is

    (is (= {:source :enactment-fold :records 2 :samples 2 :uniform false}
           (get-in d [:selection-law :e-source])))

  The hermetic witness runs the writer over the reader's own menu fixture
  with a two-receipt fold (the reader's first deftest's setup) and
  performs that read; the bad cases read a decision that never ran
  (a typed absence) and a decision over a different fold (a different
  value).

  A live record carries the WRITER's end only (live-records-read, pinned
  and read): tick-run-record-2026-09-26-flight-278b6988-click-1.edn's
  selection law has :e-source {:records 0 :samples 0 :uniform true}. The
  reader is a test box with no runtime, so no live record can carry its
  end, and the wire is WITNESSED-HERMETICALLY."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon2.aif.cascade-habit-store :as habit]
            [futon2.aif.cascade-prior :as prior]
            [futon2.aif.enactment-habit :as eh]
            [futon2.aif.policy :as policy]
            [futon3c.diagramprover.wm-wire :as w]))

(defn- menu []
  (let [fixture (edn/read-string (slurp (io/resource "fixtures/habit-accumulation/before.edn")))
        [a b] (mapv :action (:ranked (first (:cases fixture))))]
    [a (assoc b :target "M-other")]))

(defn- ranked [[a b]] [{:action a :controller-score 0} {:action b :controller-score 0}])

(defn- receipts-for [action n verdict]
  (let [key (prior/policy-key (habit/policy-view action))]
    (for [i (range n)]
      (eh/increment {:click (str "click-" i) :candidate (:id action)
                     :attempts (mapv (fn [id] {:pattern id :success true}) (nth key 2))}
                    key verdict))))

(defn- decide [m opts]
  (policy/select-action-cascades (ranked m) (merge {:beta 2} opts)))

(defn observe
  "The writer's :e-source on a decision over a fold of N passing receipts
  for the second menu action, and the reader's read of :e-source off
  READER (a decision map; {} stands in for a decision that never ran — the
  typed absence; :same reads the writer's own decision)."
  [n reader]
  (let [[_ b :as m] (menu)
        d (decide m {:enactment-fold (eh/fold nil (receipts-for b n []))})
        r (if (= :same reader) d reader)]
    {:writer (get-in d [:selection-law :e-source])
     :reader (let [v (get-in r [:selection-law :e-source])]
               (if (some? v) v {:absent :no-selection-law}))}))

(defn check [] (observe 2 :same))

(def live-records-read
  [{:path (str w/spike-dir "/tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
    :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
    :why "the writer's end only: [:decision :selection-law :e-source] is {:records 0 :samples 0 :uniform true}; the reader is a test box, no live record carries its end"}])

(def wire
  {:wire [:r7-selection :r7-test :e-source]
   :kind :witnessed-hermetically
   :test `the-e-source-reaches-the-selection-test
   :check check
   :live-records-read live-records-read})

(deftest the-e-source-reaches-the-selection-test
  (let [o (check)]
    (is (= {:source :enactment-fold :records 2 :samples 2 :uniform false} (:writer o))
        "the reader's own expected value, selection_reads_fold_test's first deftest")
    (is (w/received? o))))

(deftest a-decision-that-never-ran-is-a-typed-absence-and-fails-the-wire
  (let [o (observe 2 {})]
    (is (= {:absent :no-selection-law} (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-e-source-than-the-writers-fails-the-wire
  (let [[_ b :as m] (menu)
        empty (decide m {})]
    (is (some? b))
    (let [o (observe 2 empty)]
      (is (= 2 (get-in (:writer o) [:records])))
      (is (= 0 (get-in (:reader o) [:records])))
      (is (not (w/received? o)) "present, not absent, but not the value the writer wrote"))))

(deftest the-live-record-carries-the-writers-end-only
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path))
  (is (= {:source :enactment-fold :records 0 :samples 0 :uniform true}
         (get-in (w/read-record (:path (first live-records-read)))
                 [:decision :selection-law :e-source]))))
