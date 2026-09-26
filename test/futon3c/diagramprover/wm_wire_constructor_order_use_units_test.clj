(ns futon3c.diagramprover.wm-wire-constructor-order-use-units-test
  "Wire [:r4-constructor :r4-order-use :units]: the containment order's
  units reaching the EFE order use.

  The writer is construction/containment-order; its :units is the vector of
  {:unit u :pattern id}. The reader is efe/order-use, which reads (:units
  order) off the construction receipt's :order twice over: the unit ids
  (map :unit (:units order)) for its chain check and kernel step, and the
  {:keys [unit pattern]} pairs for the kernel step's :patterns.

  The writer's end is LIVE: the pinned tick run record
  (flight-278b6988-click-1, sha256 below, asserted before any read) carries
  a constructed selected action whose receipt :order has 8 :units. No live
  record carries the reader's end: order-use's kernel step appears on no
  record (see live-records-read). So the wire is WITNESSED-HERMETICALLY
  with the live writer's value: efe/order-use is called in this JVM over
  the pinned record's own selected action; the reader's value is the
  :units order-use consumed, with its kernel step's :units and :patterns
  showing the read."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.efe :as efe]
            [futon3c.diagramprover.wm-wire :as w]))

(def record
  {:path (str w/spike-dir "/tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
   :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"})

(def other-record
  ;; a second live tick run record whose constructed candidate's :order has
  ;; DIFFERENT (7) units, read only to supply a different real units vector
  ;; for the bad case
  {:path (str w/spike-dir "/tick-run-record-2026-09-26-flight-7f89646a-click-1.edn")
   :sha256 "a8e04fb97e58808e8fabdb4ab771f3c414b4181ef82dac336729dd472a18d816"})

(def action-path [:decision :selection-certificate :precision-family :selected-action])
(def other-candidate-path [:habit-reads :occurrences 1 :consumption :candidate-ids 0])

(defn- action [] (get-in (w/read-record (:path record)) action-path))

(defn observe
  "efe/order-use over the pinned record's selected action; TAMPER edits the
  action first (the bad cases). {:writer the record's order :units, :reader
  the :units order-use consumed (the meta's typed absence when order-use
  read no order), :kernel-step order-use's :kernel-step, :meta its :meta}."
  ([] (observe identity))
  ([tamper]
   (let [a (tamper (action))
         ou (efe/order-use a)
         absence (get-in ou [:meta :order])]
     {:writer (get-in (action) [:construction-receipt :order :units])
      :reader (if (w/typed-absence? absence)
                absence
                (get-in a [:construction-receipt :order :units]))
      :kernel-step (:kernel-step ou)
      :meta (:meta ou)})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [(assoc record :why "carries the writer's end (the selected action's receipt :order with 8 :units); no record carries order-use's kernel step — the reader's end")
     {:path (p "tick-run-record-2026-09-26-flight-e70b4baf-click-1.edn")
      :sha256 "241b10a024020344eba5d444c12fb33ad6afe107724dec95c81229b422bc5feb"
      :why "no constructed candidate: no :order with :units anywhere in the record"}
     {:path (p "tick-run-record-2026-09-25-flight-ffcd772b-click-1.edn")
      :sha256 "8ab0db5d770085f17bb341293a92424149bf2388ae7a6dd3e724887c9a44eba2"
      :why "no constructed candidate: no :order with :units anywhere in the record"}
     {:paths ["holes/labs/M-futon-seams/exemplar/click-001.edn"
              "holes/labs/M-futon-seams/exemplar/click-001-enactment.edn"
              "holes/labs/M-futon-seams/exemplar/click-001-outcome.edn"]
      :why "hand-authored exemplar records; no containment order, no order-use output"}]))

(def wire
  {:wire [:r4-constructor :r4-order-use :units]
   :kind :witnessed-hermetically
   :test `the-units-reach-order-use
   :check check
   :live-records-read live-records-read})

(deftest the-units-reach-order-use
  (is (= (:sha256 record) (w/sha256-file (:path record))) "the pin is the record read")
  (let [o (check)]
    (is (= 8 (count (:writer o))) "the live order has 8 units")
    (is (= :co-application (:order (:meta o))) "order-use used the order (a non-chain)")
    (is (= (mapv :unit (:writer o)) (get-in o [:kernel-step :co-apply :units]))
        "the kernel step's units are the writer's unit ids")
    (is (= (set (map :unit (:writer o)))
           (set (keys (get-in o [:kernel-step :co-apply :patterns]))))
        "the kernel step's patterns are keyed by the writer's units")
    (is (w/received? o))))

(deftest a-missing-order-is-a-typed-absence-and-fails-the-wire
  (let [o (observe #(dissoc % :construction-receipt))]
    (is (= {:absent :no-order-on-receipt} (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-units-fails-the-wire
  (is (= (:sha256 other-record) (w/sha256-file (:path other-record))))
  (let [live (get-in (w/read-record (:path other-record))
                     (into other-candidate-path [:construction-receipt :order :units]))
        o (observe #(assoc-in % [:construction-receipt :order :units] live))]
    (is (= 7 (count live)) "a real, different units vector from a second live record")
    (is (= live (:reader o)) "the reader consumed the units it was handed")
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))

(deftest the-live-records-carry-no-kernel-step
  (doseq [{:keys [path sha256]} (filter :path live-records-read)]
    (is (= sha256 (w/sha256-file path)) path)))
