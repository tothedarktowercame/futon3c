(ns futon3c.diagramprover.wm-wire-r7-increment-call-delta-test
  "Wire [:r7-increment :r7-call-test :delta]: the habit increment's counted
  delta reaching the W_c call's own test.

  The writer is enactment-habit/increment: W_c pass (an empty verdict
  vector) gives :delta 1, anything else :delta 0. The reader is
  futon2/test/futon2/aif/flight_wc_call_test.clj (the :box/kind :test box),
  whose read of this field is

    (is (= 1 (get-in r [:increment :delta])))

  — the delta on the receipt wc-verdict-fn's call to increment produced.
  The hermetic witness runs the writer directly for the checker's verdict
  and performs the reader's read off the wc-verdict-fn result (the wire's
  carrier: wc-verdict-fn hands the verdict to increment UNCHANGED).

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

(defn- checker-printing [s]
  (let [f (io/file (w/tmp-dir "wire-delta-checker") "checker.clj")]
    (spit f (str "(println " (pr-str s) ")\n"))
    (str f)))

(defn- call
  "wc-verdict-fn with a checker printing VERDICT-EDN (nil: no checker)."
  [verdict-edn]
  (let [f (io/file (w/tmp-dir "wire-delta-enactment") "enactment.edn")]
    (spit f (pr-str enactment))
    ((fr/wc-verdict-fn (cond-> {:click-record-path (constantly "/nonexistent/click.edn")
                                :identity-fn (fn [_ _] pkey)}
                         verdict-edn (assoc :checker (checker-printing verdict-edn))))
     {:target "M-t"} {:enactment enactment :record-path (str f)})))

(defn observe
  "The writer's :delta for WRITER-VERDICT (the value increment counts)
  and the reader's read of :delta off the wc-verdict-fn call whose checker
  printed READER-VERDICT-EDN (nil: no checker configured, no increment).
  {:writer :reader}."
  [writer-verdict reader-verdict-edn]
  (let [r (call reader-verdict-edn)]
    {:writer (:delta (eh/increment enactment pkey writer-verdict))
     :reader (if (contains? r :increment)
               (get-in r [:increment :delta])
               {:absent :no-increment-receipt})}))

(defn check [] (observe [] "[]"))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "flight-278b6988.edn")
      :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
      :why "its one enactment is {:absent :no-dispatch-configured}: increment never ran, no :delta written"}
     {:path (p "flight-7f89646a.edn")
      :sha256 "d782b3830a040dca8cfe080440869ab4a08ede22fbfd9650eb488d5f7449cc26"
      :why "its one enactment is {:absent :no-decision}: no W_c call, no increment receipt"}]))

(def wire
  {:wire [:r7-increment :r7-call-test :delta]
   :kind :witnessed-hermetically
   :test `the-delta-reaches-the-call-test
   :check check
   :live-records-read live-records-read})

(deftest the-delta-reaches-the-call-test
  (let [o (check)]
    (is (= 1 (:writer o)) "W_c pass counts one")
    (is (w/received? o))))

(deftest no-checker-is-a-typed-absence-and-fails-the-wire
  (let [o (observe [] nil)]
    (is (= {:absent :no-increment-receipt} (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-delta-than-the-writers-fails-the-wire
  ;; the writer counted a pass (delta 1); the receipt the reader read was
  ;; counted from a failing verdict (delta 0)
  (let [o (observe [] (pr-str ["W_c: chosen patterns with no successful attempt: [:p/c]"]))]
    (is (= 1 (:writer o)))
    (is (= 0 (:reader o)))
    (is (not (w/received? o)) "present, not absent, but not the value the writer wrote")))

(deftest the-live-records-carry-no-delta
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path)
    (let [f (:flight (w/read-record path))]
      (is (every? w/typed-absence? (map :enactment (:enactments f))) path))))
