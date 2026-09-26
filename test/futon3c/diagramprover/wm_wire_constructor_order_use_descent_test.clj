(ns futon3c.diagramprover.wm-wire-constructor-order-use-descent-test
  "Wire [:r4-constructor :r4-order-use :descent]: the containment order's
  descent reaching the EFE order use.

  The writer is construction/containment-order; its :descent is r itself.
  The reader is efe/order-use, which reads (:descent order) off the
  construction receipt's :order and — for a non-chain order with no
  precedence violations — copies it into its kernel step {:co-apply
  {:units ... :descent ... :patterns ...}} for the rollout to score by
  cascade-model-manifest/co-apply-kernel.

  The writer's end is LIVE: the pinned tick run record
  (flight-278b6988-click-1, sha256 below, asserted before any read) carries
  a constructed selected action whose receipt :order has a 4-edge :descent
  (a non-chain). No live record carries the reader's end: order-use's
  kernel step appears on no record (grep of the spike records finds no
  :kernel-step; see live-records-read). So the wire is
  WITNESSED-HERMETICALLY with the live writer's value: efe/order-use is
  called in this JVM over the pinned record's own selected action; the
  reader's value is the :descent of its kernel step, the copy it made of
  the writer's descent."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.efe :as efe]
            [futon3c.diagramprover.wm-wire :as w]))

(def record
  {:path (str w/spike-dir "/tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
   :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"})

(def other-record
  ;; a second live tick run record whose constructed candidate's :order has
  ;; a DIFFERENT (2-edge) descent, read only to supply a different real
  ;; descent for the bad case
  {:path (str w/spike-dir "/tick-run-record-2026-09-26-flight-7f89646a-click-1.edn")
   :sha256 "a8e04fb97e58808e8fabdb4ab771f3c414b4181ef82dac336729dd472a18d816"})

(def action-path [:decision :selection-certificate :precision-family :selected-action])
(def other-candidate-path [:habit-reads :occurrences 1 :consumption :candidate-ids 0])

(defn- action [] (get-in (w/read-record (:path record)) action-path))

(defn observe
  "efe/order-use over the pinned record's selected action; TAMPER edits the
  action first (the bad cases). {:writer the record's order :descent,
  :reader the kernel step's :descent (the meta's typed absence when
  order-use read no order), :meta order-use's :meta}."
  ([] (observe identity))
  ([tamper]
   (let [a (tamper (action))
         ou (efe/order-use a)
         absence (get-in ou [:meta :order])]
     {:writer (get-in (action) [:construction-receipt :order :descent])
      :reader (cond
                (w/typed-absence? absence) absence
                (:kernel-step ou) (get-in ou [:kernel-step :co-apply :descent])
                :else {:absent :order-not-used :meta (:meta ou)})
      :meta (:meta ou)})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [(assoc record :why "carries the writer's end (the selected action's receipt :order with :descent); no record carries order-use's kernel step — the reader's end")
     {:path (p "tick-run-record-2026-09-26-flight-e70b4baf-click-1.edn")
      :sha256 "241b10a024020344eba5d444c12fb33ad6afe107724dec95c81229b422bc5feb"
      :why "no constructed candidate: no :order with :descent anywhere in the record"}
     {:path (p "tick-run-record-2026-09-25-flight-ffcd772b-click-1.edn")
      :sha256 "8ab0db5d770085f17bb341293a92424149bf2388ae7a6dd3e724887c9a44eba2"
      :why "no constructed candidate: no :order with :descent anywhere in the record"}
     {:paths ["holes/labs/M-futon-seams/exemplar/click-001.edn"
              "holes/labs/M-futon-seams/exemplar/click-001-enactment.edn"
              "holes/labs/M-futon-seams/exemplar/click-001-outcome.edn"]
      :why "hand-authored exemplar records; no containment order, no order-use output"}]))

(def wire
  {:wire [:r4-constructor :r4-order-use :descent]
   :kind :witnessed-hermetically
   :test `the-descent-reaches-order-use
   :check check
   :live-records-read live-records-read})

(deftest the-descent-reaches-order-use
  (is (= (:sha256 record) (w/sha256-file (:path record))) "the pin is the record read")
  (let [o (check)]
    (is (= 4 (count (:writer o))) "the live order's descent has 4 edges (a non-chain)")
    (is (= :co-application (:order (:meta o))) "order-use used the order")
    (is (w/received? o) "the kernel step's descent is the writer's descent")))

(deftest a-missing-order-is-a-typed-absence-and-fails-the-wire
  (let [o (observe #(dissoc % :construction-receipt))]
    (is (= {:absent :no-order-on-receipt} (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-descent-fails-the-wire
  (is (= (:sha256 other-record) (w/sha256-file (:path other-record))))
  (let [live (get-in (w/read-record (:path other-record))
                     (into other-candidate-path [:construction-receipt :order :descent]))
        o (observe #(assoc-in % [:construction-receipt :order :descent] live))]
    (is (= 2 (count live)) "a real, different descent from a second live record")
    (is (= (vec live) (:reader o)) "the kernel step carried the descent it was handed")
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))

(deftest the-live-records-carry-no-kernel-step
  (doseq [{:keys [path sha256]} (filter :path live-records-read)]
    (is (= sha256 (w/sha256-file path)) path)))
