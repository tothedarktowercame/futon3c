(ns futon3c.diagramprover.wm-wire-r9-classify-target-relation-test-class-test
  "Wire [:r9-classify-target :r9-relation-test :class]: the relation class
  reaching the box's own test, futon2/test/futon2/aif/
  relation_derivation_test.clj, whose read is (:class c) on the
  classification it asserts (e.g. m-autoclock-in-live).

  The seventh flight's run record carries the writer's end (the recorded
  classification's :class :associated) but a test box records nothing, so
  no live record carries both ends and the wire is WITNESSED-HERMETICALLY:
  the reader's own live pin is reproduced — real inputs
  (focus-receipt/read-inputs), the reader's own discovery and as-of, the
  real code root — classify-target (writer) runs, and the reader's read
  (:class c) is observed."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.focus-receipt :as fr]
            [futon3c.diagramprover.wm-wire :as w]))

;; the reader's own setup (relation_derivation_test.clj:18-26)
(def inputs (fr/read-inputs))

(def discovery
  (let [ret (last (sort (map :valid-through (:windows inputs))))
        est (fr/discover inputs ret nil)]
    (fr/discover inputs "2026-09-26T02:00:00Z" {:focus (:focus est) :as-of ret})))

(def as-of "2026-09-26T02:00:00Z")

(defn observe
  "classify-target (writer) on M-autoclock-in with the live context, then
  the reader's read (:class c) as relation-derivation-test performs it.
  TAMPER edits the classification before the read (the bad cases).
  {:writer the written :class, :reader the read :class}."
  ([] (observe identity))
  ([tamper]
   (let [c (fr/classify-target inputs discovery as-of "M-autoclock-in" {:code-root "/home/joe/code"})
         c' (tamper c)]
     {:writer (:class c)
      :reader (get c' :class {:absent :field-not-carried})
      :relation (select-keys (:relation c) [:target :relation :facet])})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
      :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
      :why "carries the writer's end: [:decision :selection-certificate :focus-receipt :candidates 0 :class] :associated for M-autoclock-in; the reader is a test and records nothing"}
     {:path (p "tick-run-record-2026-09-26-flight-e70b4baf-click-1.edn")
      :sha256 "241b10a024020344eba5d444c12fb33ad6afe107724dec95c81229b422bc5feb"
      :why "the fourth flight refused :class-unknown-no-scalar-g on this same target (relation-derivation-test's docstring); the run record carries no classification"}]))

(def wire
  {:wire [:r9-classify-target :r9-relation-test :class]
   :kind :witnessed-hermetically
   :test `the-class-reaches-the-relation-test
   :check check
   :live-records-read live-records-read})

(deftest the-class-reaches-the-relation-test
  (let [o (check)]
    (is (= :associated (:writer o)) "the reader's own live expectation (m-autoclock-in-live)")
    (is (= {:target "M-aif4iad" :relation "associated" :facet "WM"} (:relation o)))
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  (let [o (observe #(assoc % :class {:status :absent :reason :relation-not-declared}))]
    (is (w/typed-absence? (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-class-at-the-reader-fails-the-wire
  (let [o (observe #(assoc % :class :focus))]
    (is (some? (:reader o)))
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))

(deftest the-live-record-carries-the-writers-end
  (let [{:keys [path sha256]} (first live-records-read)]
    (is (= sha256 (w/sha256-file path)))
    (is (= :associated
           (get-in (w/read-record path)
                   [:decision :selection-certificate :focus-receipt :candidates 0 :class])))))
