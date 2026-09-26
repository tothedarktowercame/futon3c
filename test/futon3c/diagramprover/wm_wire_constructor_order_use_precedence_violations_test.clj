(ns futon3c.diagramprover.wm-wire-constructor-order-use-precedence-violations-test
  "Wire [:r4-constructor :r4-order-use :precedence-violations]: the
  containment order's precedence violations reaching the EFE order use.

  The writer is construction/containment-order; its :precedence-violations
  is the descent edges the candidate's :precedence reverses ([] on the live
  record). The reader is efe/order-use, which branches on it: a non-empty
  :precedence-violations sends the action to the list kernel with
  {:order-not-used {:precedence-violations n}}; only with none does the
  order itself get used (chain or co-application).

  The writer's end is LIVE: the pinned tick run record
  (flight-278b6988-click-1, sha256 below, asserted before any read) carries
  a constructed selected action whose receipt :order has
  :precedence-violations []. No live record carries the reader's end:
  order-use's output appears on no record (see live-records-read). So the
  wire is WITNESSED-HERMETICALLY with the live writer's value:
  efe/order-use is called in this JVM over the pinned record's own selected
  action; the reader's value is the :precedence-violations order-use
  branched on, with its :meta showing which way the branch went (the order
  used only when the value is the writer's [])."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.efe :as efe]
            [futon3c.diagramprover.wm-wire :as w]))

(def record
  {:path (str w/spike-dir "/tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
   :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"})

(def action-path [:decision :selection-certificate :precision-family :selected-action])

(defn- action [] (get-in (w/read-record (:path record)) action-path))

(defn observe
  "efe/order-use over the pinned record's selected action; TAMPER edits the
  action first (the bad cases). {:writer the record's order
  :precedence-violations, :reader the value order-use branched on (the
  meta's typed absence when order-use read no order), :meta order-use's
  :meta}."
  ([] (observe identity))
  ([tamper]
   (let [a (tamper (action))
         ou (efe/order-use a)
         absence (get-in ou [:meta :order])]
     {:writer (get-in (action) [:construction-receipt :order :precedence-violations])
      :reader (if (w/typed-absence? absence)
                absence
                (get-in a [:construction-receipt :order :precedence-violations]))
      :meta (:meta ou)})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [(assoc record :why "carries the writer's end (the selected action's receipt :order with :precedence-violations []); no record carries order-use's output — the reader's end")
     {:path (p "tick-run-record-2026-09-26-flight-7f89646a-click-1.edn")
      :sha256 "a8e04fb97e58808e8fabdb4ab771f3c414b4181ef82dac336729dd472a18d816"
      :why "a second constructed receipt's :order (:precedence-violations []); no order-use output"}
     {:path (p "tick-run-record-2026-09-26-flight-e70b4baf-click-1.edn")
      :sha256 "241b10a024020344eba5d444c12fb33ad6afe107724dec95c81229b422bc5feb"
      :why "no constructed candidate: no :order anywhere in the record"}
     {:paths ["holes/labs/M-futon-seams/exemplar/click-001.edn"
              "holes/labs/M-futon-seams/exemplar/click-001-enactment.edn"
              "holes/labs/M-futon-seams/exemplar/click-001-outcome.edn"]
      :why "hand-authored exemplar records; no containment order, no order-use output"}]))

(def wire
  {:wire [:r4-constructor :r4-order-use :precedence-violations]
   :kind :witnessed-hermetically
   :test `the-precedence-violations-reach-order-use
   :check check
   :live-records-read live-records-read})

(deftest the-precedence-violations-reach-order-use
  (is (= (:sha256 record) (w/sha256-file (:path record))) "the pin is the record read")
  (let [o (check)]
    (is (= [] (:writer o)) "the live order has no precedence violations")
    (is (= :co-application (:order (:meta o)))
        "with no violations the order itself was used (a non-chain: co-application)")
    (is (w/received? o))))

(deftest a-missing-order-is-a-typed-absence-and-fails-the-wire
  (let [o (observe #(dissoc % :construction-receipt))]
    (is (= {:absent :no-order-on-receipt} (:reader o)))
    (is (not (w/received? o)))))

(deftest a-nonempty-violations-fails-the-wire
  ;; a real descent pair from the record, handed to the reader as a
  ;; violation: present, not absent, but not the writer's []
  (let [pair (first (get-in (action) [:construction-receipt :order :descent]))
        o (observe #(assoc-in % [:construction-receipt :order :precedence-violations] [pair]))]
    (is (vector? pair))
    (is (= {:order-not-used {:precedence-violations 1}} (:meta o))
        "order-use read the violations and fell back to the list kernel")
    (is (= [pair] (:reader o)))
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))

(deftest the-live-records-carry-no-order-use-output
  (doseq [{:keys [path sha256]} (filter :path live-records-read)]
    (is (= sha256 (w/sha256-file path)) path)))
