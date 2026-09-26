(ns futon3c.diagramprover.wm-wire-r0-r5-test-grain-gate-test
  "Wire [:r0-enact-step :r5-test :grain-gate]: the gate's result on the
  grain attempt reaching the grain-gate test.

  The writer is flight-runner/enact-fn (:r0-enact-step's site): with a
  grain pattern, it runs grain-gate before the grain attempt's commit and
  writes the result on the attempt under :grain-gate. The reader is
  futon2.aif.flight-grain-gate-test (:r5-test's site), which asserts
  (= {:status :pass} (:grain-gate a)) on the grain attempt and, in its
  mismatch case, reads (get-in a [:grain-gate :reason]).

  No live record carries the writer's end (live-records-read), so the wire
  is WITNESSED-HERMETICALLY: enact-fn runs with a fixture seat over the
  flight-grain-gate-test shape, the writer's value is the grain attempt's
  :grain-gate from the returned enactment, and the reader's is the same
  field read back from the record file the writer wrote."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-enact-driver :as d]))

(defn observe
  "The writer's grain-attempt :grain-gate and the reader's, TAMPER editing
  the record file before the reader reads it (the bad cases)."
  ([] (observe identity))
  ([tamper]
   (let [{:keys [enactment record-path]} (d/enact-r5)]
     (d/rewrite record-path tamper)
     {:writer (:grain-gate (first (:attempts enactment)))
      :reader (:grain-gate (first (:attempts (w/read-record record-path))))})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "flight-278b6988.edn")
      :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
      :why "its one enactment is {:absent :no-dispatch-configured}: enact-fn never ran the gate"}
     {:path (p "flight-e70b4baf.edn")
      :sha256 "10ac7cd76b2b18b40c4044bb55f0e54bc210cd908d9c4deef74875fc9d2bc88c"
      :why "its one enactment is {:absent :no-decision}: no record written"}
     {:path (:path d/click-001-enactment)
      :sha256 (:sha256 d/click-001-enactment)
      :why "hand-authored (claude-10, 2026-09-24): no :grain-gate key anywhere in the record (its first attempt's grain failure is prose under :evidence), and not enact-fn's output"}]))

(def wire
  {:wire [:r0-enact-step :r5-test :grain-gate]
   :kind :witnessed-hermetically
   :test `the-gate-pass-reaches-the-grain-gate-test
   :check check
   :live-records-read live-records-read})

(deftest the-gate-pass-reaches-the-grain-gate-test
  (let [o (check)]
    (is (= {:status :pass} (:writer o)) "as flight-grain-gate-test asserts it")
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  (let [o (observe (fn [r] (update r :attempts
                                   (fn [as] (assoc-in (vec as) [0 :grain-gate] {:absent :gate-not-run})))))]
    (is (= {:absent :gate-not-run} (:reader o)))
    (is (not (w/received? o)))))

(deftest a-refusal-at-the-reader-fails-the-wire
  ;; {:status :refuse :reason :grain-mismatch ...} is the gate's real answer
  ;; to the flight-grain-gate-test mismatch shape (provider grain planned
  ;; against the role grain), not an invented constant
  (let [refusal (:grain-gate (first (:attempts (:enactment
                                                (d/enact {:candidate :cand/g :precedence d/r5-precedence
                                                          :interps (d/r5-interps)
                                                          :planned-grain (d/provider-grain)})))))
        o (observe (fn [r] (update r :attempts
                                   (fn [as] (assoc-in (vec as) [0 :grain-gate] refusal)))))]
    (is (= :grain-mismatch (:reason refusal)))
    (is (= refusal (:reader o)))
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))

(deftest the-live-records-carry-no-grain-gate
  (doseq [{:keys [path sha256 why]} live-records-read]
    (is (= sha256 (w/sha256-file path)) why))
  (let [[f278 fe70 ex] (map #(w/read-record (:path %)) live-records-read)]
    (is (= [{:absent :no-dispatch-configured}] (mapv :enactment (:enactments (:flight f278)))))
    (is (= [{:absent :no-decision}] (mapv :enactment (:enactments (:flight fe70)))))
    (is (not-any? #(contains? % :grain-gate) (:attempts ex)))))
