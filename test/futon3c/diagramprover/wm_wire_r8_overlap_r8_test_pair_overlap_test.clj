(ns futon3c.diagramprover.wm-wire-r8-overlap-r8-test-pair-overlap-test
  "Wire [:r8-overlap :r8-test :pair-overlap]: the target field's pair
  overlap (M-wm-wiring row 8; target_comparison, mathlib4 759b8ca884)
  reaching the component's own test.

  The writer is target-field/with-pair-overlap, which puts :pair-overlap on
  each feasible entry: {:comparable true}, {:incommensurable
  {:shared-tokens [...]}}, or a typed absence ({:absent :no-universe} /
  {:absent :no-constructed-candidate}). The reader is
  futon2/test/futon2/aif/target_field_overlap_test.clj (a :box/kind :test
  box), whose reads of this field are

    (get-in by [\"M-a\" \"M-b\"])   ; by = target -> :pair-overlap
    (get by \"T-d\")

  over its fixture field. A test box has no runtime var to drive through,
  so the hermetic witness runs with-pair-overlap over the reader's own
  fixture and evaluates exactly those read expressions.

  No live record carries either end: nothing under
  holes/labs/M-wm-wiring/spike/ carries :pair-overlap at all (the field is
  computed by target-field/target-field, which no flight or tick ran with
  the row-8 code), and futon2's pinned live fixture
  target-field@futon2-7bd17dfb.edn (343 feasible entries) carries no
  :pair-overlap key — the reader's own live-pin test computes it in-test
  and finds every entry {:absent :no-constructed-candidate}. So the wire is
  WITNESSED-HERMETICALLY (see live-records-read, each pinned and read)."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.target-field :as tf]
            [futon3c.diagramprover.wm-wire :as w]))

(def field-input
  "The reader's own fixture field (target_field_overlap_test.clj,
  a-shared-token-is-incommensurable-disjoint-is-comparable)."
  [{:target "M-a" :constructed-candidate {:produces #{:x :y}} :universe #{:x :y :a}}
   {:target "M-b" :constructed-candidate {:produces #{:z}} :universe #{:y :b}}
   {:target "M-c" :constructed-candidate {:produces #{:w}} :universe #{:c}}
   {:target "T-d" :next-step :read-criteria}])

(defn observe
  "with-pair-overlap (writer) over the reader's fixture, then the reader's
  read expressions; TAMPER edits the written field before the reads (one
  bad case). {:writer the written [:pair-overlap of M-a][M-b], :reader the
  value the reader's first read observes, :reader-absent-read the reader's
  read on T-d (a genuine typed absence)}."
  ([] (observe identity))
  ([tamper]
   (let [w0 (tf/with-pair-overlap field-input)
         by0 (into {} (map (juxt :target :pair-overlap)) w0)
         f (tamper w0)
         by (into {} (map (juxt :target :pair-overlap)) f)]
     {:writer (get-in by0 ["M-a" "M-b"])
      :reader (if (contains? (get by "M-a") "M-b")
                (get-in by ["M-a" "M-b"])
                {:absent :field-not-carried})
      :reader-absent-read (get by "T-d")})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
      :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
      :why "no :pair-overlap anywhere in the record: the tick never computed the target field's row-8 overlap; neither end"}
     {:path (p "flight-278b6988.edn")
      :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
      :why "no :pair-overlap anywhere in the flight record; neither end"}
     {:path "/home/joe/code/futon2/test/fixtures/target-field/target-field@futon2-7bd17dfb.edn"
      :sha256 "760d5b3138dc4bf21526752f3e2f5a769f864fcea6d4204f4a46ec10db447f43"
      :why "the pinned live target field (343 feasible entries): no :pair-overlap key on any entry — the writer had not run when this was pinned; the reader's own live-pin test computes the field in-test and finds every entry {:absent :no-constructed-candidate}"}]))

(def wire
  {:wire [:r8-overlap :r8-test :pair-overlap]
   :kind :witnessed-hermetically
   :test `the-pair-overlap-reaches-the-r8-test
   :check check
   :live-records-read live-records-read})

(deftest the-pair-overlap-reaches-the-r8-test
  (let [o (check)]
    (is (= {:incommensurable {:shared-tokens [:y]}} (:writer o)))
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  ;; the reader's read on T-d, whose entry has no constructed candidate, is
  ;; the writer's own typed absence — present on the field, unreadable as a
  ;; comparison
  (let [o (check)]
    (is (= {:absent :no-constructed-candidate} (:reader-absent-read o)))
    (is (not (w/received? {:writer (:writer o) :reader (:reader-absent-read o)}))))
  ;; and a field not carried at all reads as a typed absence too
  (let [o (observe #(mapv (fn [e] (dissoc e :pair-overlap)) %))]
    (is (= {:absent :field-not-carried} (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-overlap-fails-the-wire
  (let [o (observe #(mapv (fn [e]
                            (if (= "M-a" (:target e))
                              (assoc-in e [:pair-overlap "M-b"] {:comparable true})
                              e))
                          %))]
    (is (some? (:reader o)))
    (is (not (w/received? o))
        "present, not absent, but not the value the writer wrote")))

(deftest the-live-records-carry-no-pair-overlap
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path)
    (let [r (w/read-record path)]
      (is (not-any? #(and (map? %) (contains? % :pair-overlap))
                    (tree-seq coll? seq r))
          (str path " carries no :pair-overlap")))))
