(ns futon3c.diagramprover.wm-wire-r7-increment-r7-test-delta-test
  "Wire [:r7-increment :r7-test :delta]: the increment receipt's delta
  reaching row 7's selection-reads-fold test.

  The writer is enactment-habit/increment. The reader is
  futon2/test/futon2/aif/selection_reads_fold_test.clj (the :box/kind :test
  box), which builds receipts with increment, folds them, selects, and
  reads the selection law's :e-source — :records is what the fold counted,
  which is the receipts' :delta read end-to-end (its join-unverifiable
  deftest reads (:delta receipt) directly and asserts the fold counted
  nothing). The hermetic witness performs that run over the reader's own
  menu fixture: {:writer the receipt's :delta, :reader the decision's
  :e-source :records (a run that counted nothing reads as
  {:absent :no-delta-counted})}.

  No live record carries either end (live-records-read, pinned and read):
  the one live selection's :e-source is {:records 0 :samples 0 :uniform
  true} — no delta ever counted live. So the wire is
  WITNESSED-HERMETICALLY."
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

(defn- receipt-for
  "selection_reads_fold_test's enactment-receipts: one increment receipt
  of ACTION with W_c verdict VERDICT."
  [action i verdict]
  (let [key (prior/policy-key (habit/policy-view action))]
    (eh/increment {:click (str "click-" i) :candidate (:id action)
                   :attempts (mapv (fn [id] {:pattern id :success true}) (nth key 2))}
                  key verdict)))

(defn observe
  "RECEIPT-SPEC is a seq of [action-index verdict]: the writer is the
  first receipt's :delta; the reader is the decision's :e-source :records
  after the fold reaches selection (selection_reads_fold_test's decide)."
  [receipt-spec]
  (let [m (menu)
        receipts (mapv (fn [[i [ai verdict]]] (receipt-for (nth m ai) i verdict))
                       (map-indexed vector receipt-spec))
        d (policy/select-action-cascades (ranked m)
                                         {:beta 2 :enactment-fold (eh/fold nil receipts)})
        records (get-in d [:selection-law :e-source :records])]
    {:writer (:delta (first receipts))
     :reader (if (pos? records) records {:absent :no-delta-counted})}))

(defn check [] (observe [[1 []]]))

(def live-records-read
  [{:path (str w/spike-dir "/tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
    :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
    :why "the one live selection law's :e-source is {:records 0 :samples 0 :uniform true}: no live increment receipt, so no delta ever reached a fold or a selection"}])

(def wire
  {:wire [:r7-increment :r7-test :delta]
   :kind :witnessed-hermetically
   :test `the-delta-reaches-the-selection-test
   :check check
   :live-records-read live-records-read})

(deftest the-delta-reaches-the-selection-test
  (let [o (check)]
    (is (= 1 (:writer o)))
    (is (= 1 (:reader o)) "the fold counted the delta-1 receipt and selection says so")
    (is (w/received? o))))

(deftest a-zero-delta-counts-nothing-and-fails-the-wire
  ;; selection_reads_fold_test's join-unverifiable case: the typed
  ;; non-verdict passes through increment as :delta 0 and the fold counts
  ;; nothing
  (let [o (observe [[1 {:status :join-unverifiable}]])]
    (is (= 0 (:writer o)))
    (is (= {:absent :no-delta-counted} (:reader o)))
    (is (not (w/received? o)))))

(deftest another-count-than-the-writers-fails-the-wire
  (let [o (observe [[1 []] [1 []]])]
    (is (= 1 (:writer o)))
    (is (= 2 (:reader o)))
    (is (not (w/received? o)) "present, not absent, but not the value the writer wrote")))

(deftest the-live-records-carry-no-delta
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path))
  (is (= {:source :enactment-fold :records 0 :samples 0 :uniform true}
         (get-in (w/read-record (:path (first live-records-read)))
                 [:decision :selection-law :e-source]))))
