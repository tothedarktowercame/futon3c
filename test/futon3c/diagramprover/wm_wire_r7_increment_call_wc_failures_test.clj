(ns futon3c.diagramprover.wm-wire-r7-increment-call-wc-failures-test
  "Wire [:r7-increment :r7-call-test :wc-failures]: the W_c failure strings
  on the increment receipt reaching the W_c call's own test.

  The writer is enactment-habit/increment: a non-empty verdict vector is
  kept verbatim on the receipt as :wc-failures (with :delta 0). The reader
  is futon2/test/futon2/aif/flight_wc_call_test.clj (the :box/kind :test
  box), whose read of this field is

    (is (= v (get-in r [:increment :wc-failures])))

  — the failures read back off the receipt wc-verdict-fn produced,
  byte-identical as EDN. The hermetic witness runs the writer directly for
  the checker's verdict and performs the reader's read off the
  wc-verdict-fn result.

  No live record carries either end (live-records-read, each pinned and
  read): every flight record's one enactment is a typed absence, so
  increment never ran live. So the wire is WITNESSED-HERMETICALLY."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon2.aif.enactment-habit :as eh]
            [futon2.aif.flight-runner :as fr]
            [futon3c.diagramprover.wm-wire :as w]))

(def pkey [:pattern-cascade "M-t" [:p/a :p/b] {}])

(def enactment
  {:click "run-1" :candidate :cand/x
   :attempts [{:n 1 :pattern :p/a :success true} {:n 2 :pattern :p/b :success true}]
   :conformance {:deviations []}})

(def failures ["W_c: chosen patterns with no successful attempt: [:p/c]"])

(defn- checker-printing [s]
  (let [f (io/file (w/tmp-dir "wire-wcf-checker") "checker.clj")]
    (spit f (str "(println " (pr-str s) ")\n"))
    (str f)))

(defn- call [verdict-edn]
  (let [f (io/file (w/tmp-dir "wire-wcf-enactment") "enactment.edn")]
    (spit f (pr-str enactment))
    ((fr/wc-verdict-fn (cond-> {:click-record-path (constantly "/nonexistent/click.edn")
                                :identity-fn (fn [_ _] pkey)}
                         verdict-edn (assoc :checker (checker-printing verdict-edn))))
     {:target "M-t"} {:enactment enactment :record-path (str f)})))

(defn observe
  "The writer's :wc-failures for WRITER-VERDICT and the reader's read of
  :wc-failures off the wc-verdict-fn call whose checker printed
  READER-VERDICT-EDN. {:writer :reader; a receipt carrying no
  :wc-failures reads as {:absent :no-wc-failures-on-receipt}}."
  [writer-verdict reader-verdict-edn]
  (let [receipt (:increment (call reader-verdict-edn))]
    {:writer (:wc-failures (eh/increment enactment pkey writer-verdict))
     :reader (if (contains? receipt :wc-failures)
               (:wc-failures receipt)
               {:absent :no-wc-failures-on-receipt})}))

(defn check [] (observe failures (pr-str failures)))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "flight-278b6988.edn")
      :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
      :why "its one enactment is {:absent :no-dispatch-configured}: increment never ran, no :wc-failures written"}
     {:path (p "flight-d00574c8.edn")
      :sha256 "68e531b462d535983ea5114a8da370daec943a3b4283b6b87831e63ecf9eadd8"
      :why "its one enactment is {:absent :no-decision}: no W_c call, no increment receipt"}]))

(def wire
  {:wire [:r7-increment :r7-call-test :wc-failures]
   :kind :witnessed-hermetically
   :test `the-wc-failures-reach-the-call-test
   :check check
   :live-records-read live-records-read})

(deftest the-wc-failures-reach-the-call-test
  (let [o (check)]
    (is (= failures (:writer o)))
    (is (w/received? o))))

(deftest a-passing-verdict-carries-no-failures-and-fails-the-wire
  (let [o (observe failures "[]")]
    (is (= {:absent :no-wc-failures-on-receipt} (:reader o)))
    (is (not (w/received? o)))))

(deftest different-failures-than-the-writers-fail-the-wire
  (let [o (observe failures (pr-str ["W_c: a different failure"]))]
    (is (= failures (:writer o)))
    (is (= ["W_c: a different failure"] (:reader o)))
    (is (not (w/received? o)) "present, not absent, but not the value the writer wrote")))

(deftest the-live-records-carry-no-wc-failures
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path)
    (let [f (:flight (w/read-record path))]
      (is (every? w/typed-absence? (map :enactment (:enactments f))) path))))
