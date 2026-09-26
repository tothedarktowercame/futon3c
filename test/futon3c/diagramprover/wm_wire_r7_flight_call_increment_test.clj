(ns futon3c.diagramprover.wm-wire-r7-flight-call-increment-test
  "Wire [:r7-flight-call :r7-fold-source :increment]: the flight's increment
  receipt, on its flight record, reaching the tick's fold source.

  The writer is flight-runner/wc-verdict-fn: its :increment receipt is
  recorded on the flight record under [:flight :enactments i :increment]
  (WM-HABIT-FOLD-CALL-I). The reader is
  enactment-fold-source/increment-receipts, which reads the flight records
  in a directory and returns those receipts. The hermetic witness writes
  the receipt through wc-verdict-fn with a fixture bb checker, puts it on
  a flight record in a temp directory (habit_fold_call_test's record
  shape), and reads the directory with increment-receipts: the receipt
  that comes back is the value on the wire.

  No live record carries either end (live-records-read, each pinned and
  read): no flight record under spike/ carries :increment on an
  enactment (every enactment is a typed absence; the one attempt
  checkpoint's :increments is the empty vector, another key entirely).
  So the wire is WITNESSED-HERMETICALLY."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [clojure.walk]
            [futon2.aif.enactment-fold-source :as efs]
            [futon2.aif.flight-runner :as fr]
            [futon3c.diagramprover.wm-wire :as w]))

(def pkey [:pattern-cascade "M-t" [:p/a :p/b] {}])

(def enactment
  {:click "run-1" :candidate :cand/x
   :attempts [{:n 1 :pattern :p/a :success true} {:n 2 :pattern :p/b :success true}]
   :conformance {:deviations []}})

(defn- writer-receipt []
  (let [checker (io/file (w/tmp-dir "wire-inc-checker") "checker.clj")
        record (io/file (w/tmp-dir "wire-inc-enactment") "enactment.edn")]
    (spit checker "(println [])\n")
    (spit record (pr-str enactment))
    (:increment ((fr/wc-verdict-fn {:checker (str checker)
                                    :click-record-path (constantly "/nonexistent/click.edn")
                                    :identity-fn (fn [_ _] pkey)})
                 {:target "M-t"} {:enactment enactment :record-path (str record)}))))

(defn- write-flight! [dir id enactment-entry]
  (let [f (io/file dir (str id ".edn"))]
    (spit f (pr-str {:plan {} :flight {:flight/id id :enactments [enactment-entry]}}))
    (str f)))

(defn observe
  "The writer's increment receipt and what increment-receipts returns for
  a directory whose one flight record carries ENTRY (a fn of the receipt;
  the bad cases carry none or another). {:writer receipt, :reader the
  receipt read back, or {:absent :no-increment-receipt-on-record}}."
  [entry-fn]
  (let [receipt (writer-receipt)
        dir (w/tmp-dir "wire-inc-flights")
        _ (write-flight! dir "flight-a" (entry-fn receipt))
        rs (:receipts (efs/increment-receipts dir))]
    {:writer receipt
     :reader (if (seq rs) (first rs) {:absent :no-increment-receipt-on-record})}))

(defn check [] (observe (fn [r] {:click-id "run-1" :wc {:verdict []} :increment r})))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "flight-e70b4baf.edn")
      :sha256 "10ac7cd76b2b18b40c4044bb55f0e54bc210cd908d9c4deef74875fc9d2bc88c"
      :why "its one enactment is {:absent :no-decision}: no increment receipt was ever written on a live flight record"}
     {:path (p "attempt-001-flight-e70b4baf/007-closed.edn")
      :sha256 "e5c545ba48faaa11bffe46e9832859a87bcca95aca4cb98207be6a8c09767c37"
      :why "the attempt checkpoint's :increments is [] (a route-attestation key, not a habit receipt); nothing to fold"}]))

(def wire
  {:wire [:r7-flight-call :r7-fold-source :increment]
   :kind :witnessed-hermetically
   :test `the-increment-receipt-reaches-the-fold-source
   :check check
   :live-records-read live-records-read})

(deftest the-increment-receipt-reaches-the-fold-source
  (let [o (check)]
    (is (= 1 (get-in (:writer o) [:delta])))
    (is (= pkey (get-in (:reader o) [:policy-key])))
    (is (w/received? o))))

(deftest a-record-with-no-increment-is-a-typed-absence-and-fails-the-wire
  (let [o (observe (fn [_] {:click-id "run-1" :wc {:verdict []}}))]
    (is (= {:absent :no-increment-receipt-on-record} (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-receipt-than-the-writers-fails-the-wire
  (let [o (observe (fn [r] {:click-id "run-2" :wc {:verdict []}
                            :increment (assoc r :record-id ["run-2" :cand/x])}))]
    (is (= ["run-1" :cand/x] (get-in (:writer o) [:record-id])))
    (is (= ["run-2" :cand/x] (get-in (:reader o) [:record-id])))
    (is (not (w/received? o)) "present, not absent, but not the value the writer wrote")))

(deftest the-live-records-carry-no-increment-receipt
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path))
  (let [f (:flight (w/read-record (:path (first live-records-read))))]
    (is (every? w/typed-absence? (map :enactment (:enactments f)))))
  (let [found (atom nil)]
    (clojure.walk/postwalk (fn [x] (when (and (map? x) (contains? x :increments))
                                     (reset! found (:increments x))) x)
                           (w/read-record (:path (second live-records-read))))
    (is (= [] @found) "the one :increments key on the checkpoint is empty")))
