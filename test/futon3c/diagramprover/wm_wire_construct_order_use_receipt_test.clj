(ns futon3c.diagramprover.wm-wire-construct-order-use-receipt-test
  "Wire [:construction-construct :r4-order-use :construction-receipt]: the
  constructed candidate's construction receipt reaching the EFE order use.

  The writer is interpretation-construction/construct (each constructed
  candidate's :construction-receipt; constructed-from-interpretations adds
  :unknown-read-as-not-established, a carrier). The reader is efe/order-use,
  which reads (get-in action [:construction-receipt :order]).

  The writer's end is LIVE: the pinned tick run record
  (flight-278b6988-click-1, sha256 below, asserted before any read) carries
  a constructed selected action whose :construction-receipt is construct's
  receipt (its :order, :unreached-wants, :family-searched, and the carrier's
  :unknown-read-as-not-established). No live record carries the reader's
  end: order-use's output appears on no record (grep of the spike records
  finds neither :kernel-step nor :no-order-on-receipt; see
  live-records-read). So the wire is WITNESSED-HERMETICALLY with the live
  writer's value: efe/order-use is called in this JVM over the pinned
  record's own selected action, and the reader's value is the receipt
  order-use consumed, with its :meta distinguishing the typed absence
  {:absent :no-order-on-receipt} from a receipt read."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.efe :as efe]
            [futon3c.diagramprover.wm-wire :as w]))

(def record
  {:path (str w/spike-dir "/tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
   :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"})

(def other-record
  ;; a second live tick run record carrying a DIFFERENT constructed
  ;; candidate (7 units, 2 descent edges), read only to supply a different
  ;; real receipt for the bad case
  {:path (str w/spike-dir "/tick-run-record-2026-09-26-flight-7f89646a-click-1.edn")
   :sha256 "a8e04fb97e58808e8fabdb4ab771f3c414b4181ef82dac336729dd472a18d816"})

(def action-path [:decision :selection-certificate :precision-family :selected-action])
(def other-candidate-path [:habit-reads :occurrences 1 :consumption :candidate-ids 0])

(defn- action [] (get-in (w/read-record (:path record)) action-path))

(defn observe
  "efe/order-use over the pinned record's selected action; TAMPER edits the
  action first (the bad cases). {:writer the record's receipt, :reader the
  receipt order-use consumed (its meta's typed absence when no receipt was
  read), :meta order-use's :meta, :kernel-step order-use's :kernel-step}."
  ([] (observe identity))
  ([tamper]
   (let [a (tamper (action))
         ou (efe/order-use a)
         absence (get-in ou [:meta :order])]
     {:writer (:construction-receipt (action))
      :reader (if (w/typed-absence? absence)
                absence
                (get-in a [:construction-receipt]))
      :meta (:meta ou)
      :kernel-step (:kernel-step ou)})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [(assoc record :why "carries the writer's end (the selected action's construct receipt with :order); no record carries order-use's output — the reader's end")
     {:path (p "tick-run-record-2026-09-26-flight-e70b4baf-click-1.edn")
      :sha256 "241b10a024020344eba5d444c12fb33ad6afe107724dec95c81229b422bc5feb"
      :why "no constructed candidate: no :construction-receipt with :order anywhere in the record"
      }
     {:path (p "tick-run-record-2026-09-25-flight-ffcd772b-click-1.edn")
      :sha256 "8ab0db5d770085f17bb341293a92424149bf2388ae7a6dd3e724887c9a44eba2"
      :why "no constructed candidate: no :construction-receipt with :order anywhere in the record"}
     {:paths ["holes/labs/M-futon-seams/exemplar/click-001.edn"
              "holes/labs/M-futon-seams/exemplar/click-001-enactment.edn"
              "holes/labs/M-futon-seams/exemplar/click-001-outcome.edn"]
      :why "hand-authored exemplar records; no constructed candidate, no order-use output"}]))

(def wire
  {:wire [:construction-construct :r4-order-use :construction-receipt]
   :kind :witnessed-hermetically
   :test `the-construction-receipt-reaches-order-use
   :check check
   :live-records-read live-records-read})

(deftest the-construction-receipt-reaches-order-use
  (is (= (:sha256 record) (w/sha256-file (:path record))) "the pin is the record read")
  (let [o (check)]
    (is (map? (:writer o)) "the writer's receipt is on the live record")
    (is (contains? (:writer o) :order) "construct's receipt carries the containment order")
    (is (= :co-application (:order (:meta o))) "order-use used the receipt's order (not a chain)")
    (is (= (get-in o [:writer :order :descent])
           (get-in o [:kernel-step :co-apply :descent]))
        "the reader's kernel step carries the receipt's own descent: the receipt was read")
    (is (w/received? o))))

(deftest a-missing-receipt-is-a-typed-absence-and-fails-the-wire
  (let [o (observe #(dissoc % :construction-receipt))]
    (is (= {:absent :no-order-on-receipt} (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-receipt-fails-the-wire
  (is (= (:sha256 other-record) (w/sha256-file (:path other-record))))
  (let [other (get-in (w/read-record (:path other-record))
                      (conj other-candidate-path :construction-receipt))
        o (observe #(assoc % :construction-receipt other))]
    (is (map? other))
    (is (not= (:writer o) other) "a real, different receipt from a second live record")
    (is (= other (:reader o)) "the reader consumed the receipt it was handed")
    (is (not (w/received? o)))))

(deftest the-live-records-carry-no-order-use-output
  (doseq [{:keys [path sha256]} (filter :path live-records-read)]
    (is (= sha256 (w/sha256-file path)) path))
  (let [r (w/read-record (:path record))]
    (is (map? (get-in r (conj action-path :construction-receipt :order)))
        "the pinned record carries the writer's end")))
