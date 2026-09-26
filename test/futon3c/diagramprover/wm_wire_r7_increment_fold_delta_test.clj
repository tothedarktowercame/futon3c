(ns futon3c.diagramprover.wm-wire-r7-increment-fold-delta-test
  "Wire [:r7-increment :r7-fold :delta]: the increment receipt's delta
  reaching the habit fold (futon-2026 Figure 6A's E accumulation).

  The writer is enactment-habit/increment; the reader is
  enactment-habit/fold, which counts a receipt once per :record-id only
  when its :delta is 1 — the fold's read of the field manifests in the
  state's :samples (cascade-prior's total count). The hermetic witness
  folds the writer's receipt: {:writer the receipt's :delta, :reader the
  fold's :samples (a fold that counted nothing reads as
  {:absent :no-delta-counted})}.

  No live record carries either end (live-records-read, each pinned and
  read): no flight record under spike/ carries an increment receipt
  (every enactment is a typed absence), so there was never a live receipt
  to fold; the live selections' :e-source shows {:records 0 :samples 0}.
  So the wire is WITNESSED-HERMETICALLY."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.enactment-habit :as eh]
            [futon3c.diagramprover.wm-wire :as w]))

(def pkey [:pattern-cascade "M-t" [:p/a] {}])

(defn- receipt [click verdict]
  (eh/increment {:click click :candidate :cas/a :attempts [{:pattern :p/a}]}
                pkey verdict))

(defn observe
  "Fold RECEIPTS: {:writer the first receipt's :delta, :reader the fold's
  :samples, or {:absent :no-delta-counted} when the fold counted
  nothing}."
  [receipts]
  (let [st (eh/fold nil receipts)]
    {:writer (:delta (first receipts))
     :reader (if (pos? (:samples st))
               (:samples st)
               {:absent :no-delta-counted})}))

(defn check [] (observe [(receipt "click-1" [])]))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "flight-e70b4baf.edn")
      :sha256 "10ac7cd76b2b18b40c4044bb55f0e54bc210cd908d9c4deef74875fc9d2bc88c"
      :why "its one enactment is {:absent :no-decision}: no increment receipt exists on any live record, so the fold had nothing to count"}
     {:path (p "tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
      :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
      :why "the live selection's :e-source is {:records 0 :samples 0 :uniform true}: no live delta ever reached a fold"}]))

(def wire
  {:wire [:r7-increment :r7-fold :delta]
   :kind :witnessed-hermetically
   :test `the-delta-reaches-the-fold
   :check check
   :live-records-read live-records-read})

(deftest the-delta-reaches-the-fold
  (let [o (check)]
    (is (= 1 (:writer o)))
    (is (= 1 (:reader o)) "the fold counted the writer's delta-1 receipt")
    (is (w/received? o))))

(deftest a-zero-delta-counts-nothing-and-fails-the-wire
  ;; W_c fail: the receipt is written, the fold reads :delta 0 and counts
  ;; nothing — the reader's end is a typed absence
  (let [o (observe [(receipt "click-1" ["W_c: a failure"])])]
    (is (= 0 (:writer o)))
    (is (= {:absent :no-delta-counted} (:reader o)))
    (is (not (w/received? o)))))

(deftest another-receipts-count-than-the-writers-fails-the-wire
  (let [o (observe [(receipt "click-1" []) (receipt "click-2" [])])]
    (is (= 1 (:writer o)))
    (is (= 2 (:reader o)))
    (is (not (w/received? o)) "present, not absent, but not the value the writer wrote")))

(deftest the-live-records-carry-no-delta
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path))
  (let [f (:flight (w/read-record (:path (first live-records-read))))]
    (is (every? w/typed-absence? (map :enactment (:enactments f)))))
  (is (= {:source :enactment-fold :records 0 :samples 0 :uniform true}
         (get-in (w/read-record (:path (second live-records-read)))
                 [:decision :selection-law :e-source]))))
