(ns futon3c.diagramprover.wm-wire-r9-classify-target-decision-class-test
  "Wire [:r9-classify-target :r9-decision :class]: the relation class
  reaching the joint decision (war_machine.clj:6587-6588 reads (:class c)
  off each target's classification and maps it through scorer-class).

  VERIFIED on the seventh flight's run record: the decision attaches the
  very target-classifications it read (focus-receipt/attach is called with
  :classifications target-classifications, war_machine.clj:6697-6700, and
  build prefers them, focus_receipt.clj:319-321), so the recorded
  classification at [:decision :selection-certificate :focus-receipt
  :candidates 0] is the map the decision classified and read. Its :class
  :associated is the writer's end; the reader's end is recoverable from the
  class observation model's :target-class {\"M-autoclock-in\" :related} on
  the same record — :related is scorer-class's image of :associated and of
  no other relation, so the value the decision read is inverted out of what
  it did with it."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]))

(def record-path
  (str w/spike-dir "/tick-run-record-2026-09-26-flight-278b6988-click-1.edn"))

(def record-sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa")

;; the decision's own relation-vocabulary -> scorer-class map,
;; war_machine.clj:6532, a constant of the writer's site (not a value)
(def scorer-class {:focus :focused :associated :related :useful-elsewhere :unrelated})

(def classification-path [:decision :selection-certificate :focus-receipt :candidates 0])
(def class-model-path [:decision :selection-law :policy-comparison :winner :computed-f :model])

(defn observe
  "Both ends off the pinned record: {:writer the recorded classification's
  :class, :reader the relation the decision must have read, inverted out of
  the class model's :target-class through the site's scorer-class}. TAMPER
  edits the record in memory before the read (the bad cases)."
  ([] (observe identity))
  ([tamper]
   (let [r (tamper (w/read-record record-path))
         cls (get-in r classification-path)
         model-tc (get-in r (into class-model-path [:target-class]))]
     {:writer (:class cls)
      :reader (if (w/typed-absence? model-tc)
                model-tc
                (some (fn [[rel sc]] (when (= sc (get model-tc (:target cls))) rel)) scorer-class))
      :target (:target cls)
      :model-target-class model-tc})))

(defn check [] (observe))

(def wire
  {:wire [:r9-classify-target :r9-decision :class]
   :kind :verified
   :test `the-relation-class-reaches-the-decision
   :check check
   :record {:path record-path :sha256 record-sha256}})

(deftest the-relation-class-reaches-the-decision
  (is (= record-sha256 (w/sha256-file record-path)) "the pinned record before it is read")
  (let [o (check)]
    (is (= "M-autoclock-in" (:target o)))
    (is (= :associated (:writer o)) "classify-target's recorded class")
    (is (= {"M-autoclock-in" :related} (:model-target-class o))
        "the decision's read, recorded one hop downstream in the class model")
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  ;; the class model carrying a typed absence instead of the mapped class
  (let [o (observe #(assoc-in % (into class-model-path [:target-class]) {:absent :no-class-read}))]
    (is (= {:absent :no-class-read} (:reader o)))
    (is (w/typed-absence? (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-class-at-the-reader-fails-the-wire
  ;; the decision read :focus (mapped :focused) where the writer wrote
  ;; :associated
  (let [o (observe #(assoc-in % (into class-model-path [:target-class "M-autoclock-in"]) :focused))]
    (is (= :focus (:reader o)))
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))
