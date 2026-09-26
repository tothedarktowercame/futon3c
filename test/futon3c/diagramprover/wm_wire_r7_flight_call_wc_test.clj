(ns futon3c.diagramprover.wm-wire-r7-flight-call-wc-test
  "Wire [:r7-flight-call :r7-call-test :wc]: the flight's W_c call result
  reaching the call's own test.

  The writer is flight-runner/wc-verdict-fn, which runs the W_c checker
  (proof2a_check.clj --wc --edn) on the click's run record and the
  enactment record and writes {:wc {:verdict v :click-record p} :increment
  ...}. The reader is futon2/test/futon2/aif/flight_wc_call_test.clj (the
  :box/kind :test box), whose read of this field is

    (is (= [] (get-in r [:wc :verdict])))

  — the verdict read back off the call's result, byte-identical as EDN.
  The hermetic witness drives the writer with a fixture bb checker (the
  reader's own fixture shape) and performs that read; TAMPER edits the
  result between the writer and the read (the bad case).

  No live record carries either end (live-records-read, each pinned and
  read): every flight record's one enactment is a typed absence, so
  wc-verdict-fn never ran live. So the wire is WITNESSED-HERMETICALLY."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon2.aif.flight-runner :as fr]
            [futon3c.diagramprover.wm-wire :as w]))

(def enactment
  {:click "run-1" :candidate :cand/x
   :attempts [{:n 1 :pattern :p/a :success true} {:n 2 :pattern :p/b :success true}]
   :conformance {:deviations []}})

(defn- checker-printing
  "A bb script that prints S and exits 0, whatever it is given (the
  reader's own fixture shape, flight_wc_call_test/checker-printing)."
  [s]
  (let [f (io/file (w/tmp-dir "wire-wc-checker") "checker.clj")]
    (spit f (str "(println " (pr-str s) ")\n"))
    (str f)))

(defn- call [checker]
  (let [f (io/file (w/tmp-dir "wire-wc-enactment") "enactment.edn")]
    (spit f (pr-str enactment))
    ((fr/wc-verdict-fn (cond-> {:click-record-path (constantly "/nonexistent/click.edn")}
                         checker (assoc :checker checker)))
     {:target "M-t"} {:enactment enactment :record-path (str f)})))

(defn observe
  "wc-verdict-fn over a checker printing VERDICT-EDN (nil: no checker
  configured); the writer's :wc value and the reader's read of it after
  TAMPER. {:writer (:wc result), :reader (:wc (tamper result))}."
  ([verdict-edn] (observe verdict-edn identity))
  ([verdict-edn tamper]
   (let [r (call (some-> verdict-edn checker-printing))]
     {:writer (:wc r)
      :reader (:wc (tamper r))})))

(defn check [] (observe "[]"))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "flight-278b6988.edn")
      :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
      :why "its one enactment is {:absent :no-dispatch-configured}: wc-verdict-fn never ran, no :wc written"}
     {:path (p "flight-ffcd772b.edn")
      :sha256 "998565fb0a341077ae5b9341d977a9c5bf6db6ea0cf33fead1e00c630f4e2575"
      :why "its one enactment is {:absent :no-decision}: no W_c call, no :wc"}]))

(def wire
  {:wire [:r7-flight-call :r7-call-test :wc]
   :kind :witnessed-hermetically
   :test `the-wc-verdict-reaches-the-call-test
   :check check
   :live-records-read live-records-read})

(deftest the-wc-verdict-reaches-the-call-test
  (let [o (check)]
    (is (= [] (get-in (:reader o) [:verdict])) "the reader's own assertion shape")
    (is (w/received? o))))

(deftest no-checker-is-a-typed-absence-and-fails-the-wire
  (let [o (observe nil)]
    (is (= {:absent :no-wc-checker-configured} (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-verdict-than-the-writers-fails-the-wire
  (let [o (observe "[]" #(assoc-in % [:wc :verdict] ["W_c: a different verdict"]))]
    (is (= [] (get-in (:writer o) [:verdict])))
    (is (= ["W_c: a different verdict"] (get-in (:reader o) [:verdict])))
    (is (not (w/received? o)) "present, not absent, but not the value the writer wrote")))

(deftest the-live-records-carry-no-wc
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path)
    (let [f (:flight (w/read-record path))]
      (is (every? w/typed-absence? (map :enactment (:enactments f))) path)
      (is (not-any? :wc (:enactments f)) path))))
