(ns futon3c.diagramprover.wm-wire-constructor-coapply-descent-test
  "Wire [:r4-constructor :r4-coapply-test :descent]: the containment order's
  descent reaching the co-application kernel's test.

  The writer is construction/containment-order; its :descent is r itself,
  [[above below] ...]. The reader is futon2.aif.coapply-kernel-test
  (M-wm-wiring step 14), which reads (:units chain-order) and (:descent
  chain-order) off containment-order's return to build its chain-step
  {:co-apply {:units ... :descent ... :patterns ...}} and evaluates it
  through cascade-model-manifest/evaluate-state.

  No live record carries the reader's end (a test's read is recorded
  nowhere; see live-records-read — one live record does carry the writer's
  end, a constructed receipt's :order with :descent). So the wire is
  WITNESSED-HERMETICALLY: containment-order is called over the reader's own
  fixture (order-kernel-test's [:A :B] candidate, the same expression the
  reader uses), its :descent is placed in the chain-step exactly as the
  reader places it, and the step is evaluated as the reader evaluates it;
  the writer's value is the order's :descent, the reader's value is the
  :descent of the evaluated kernel step."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.cascade-model-manifest :as m]
            [futon2.aif.construction :as construction]
            [futon2.aif.order-kernel-test :as ok]
            [futon3c.diagramprover.wm-wire :as w]))

(def record
  ;; a live tick run record carrying the writer's end (a constructed
  ;; receipt's :order with a 4-edge :descent), read to supply a different
  ;; real descent for the bad case
  {:path (str w/spike-dir "/tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
   :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"})

(defn- order [] (construction/containment-order {:patterns (mapv ok/interp [:A :B])
                                                 :precedence [:A :B]}))

(defn- chain-step
  "The reader's own step construction (coapply-kernel-test's chain-step)."
  [o]
  {:co-apply {:units (mapv :unit (:units o)) :descent (:descent o)
              :patterns (into {} (for [{:keys [unit pattern]} (:units o)]
                                   [unit (ok/pat pattern)]))}})

(defn observe
  "containment-order's return through the reader's step construction and
  evaluation. ORDER-FN rewrites the order between writer and reader (the
  bad cases). {:writer the order's :descent, :reader the evaluated step's
  :descent (nil when the order carries none), :kernel the evaluation at
  #{:x}}."
  ([] (observe identity))
  ([order-fn]
   (let [o (order-fn (order))
         step (chain-step o)
         kernel (:kernel (#'m/evaluate-state step #{:x} false))]
     {:writer (:descent (order))
      :reader (get-in step [:co-apply :descent])
      :kernel kernel})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [(assoc record :why "carries the writer's end (the selected action's receipt :order with :descent); a test's read of :descent is on no record — the reader's end exists only in the test JVM")
     {:path (p "tick-run-record-2026-09-26-flight-7f89646a-click-1.edn")
      :sha256 "a8e04fb97e58808e8fabdb4ab771f3c414b4181ef82dac336729dd472a18d816"
      :why "a second constructed receipt's :order with :descent; the reader's end is on no record"}
     {:paths ["holes/labs/M-futon-seams/exemplar/click-001.edn"
              "holes/labs/M-futon-seams/exemplar/click-001-enactment.edn"
              "holes/labs/M-futon-seams/exemplar/click-001-outcome.edn"]
      :why "hand-authored exemplar records; no containment order, no kernel evaluation"}]))

(def wire
  {:wire [:r4-constructor :r4-coapply-test :descent]
   :kind :witnessed-hermetically
   :test `the-descent-reaches-the-coapplication-kernel-test
   :check check
   :live-records-read live-records-read})

(deftest the-descent-reaches-the-coapplication-kernel-test
  (let [o (check)]
    (is (= [[:A :B]] (:writer o)) "the fixture is the chain A above B")
    (is (= (m/cascade-kernel (mapv ok/pat [:A :B]) #{:x}) (:kernel o))
        "the reader's own assertion: on a chain the kernels coincide")
    (is (w/received? o))))

(deftest a-refused-order-carries-no-descent-and-fails-the-wire
  ;; a cyclic containment is refused {:kind :cyclic-containment}; the
  ;; refusal carries no :descent — a typed absence at the reader
  (let [cyc (construction/containment-order {:patterns [{:id :p1 :produces #{:a} :guard {:needs #{:b}}}
                                                        {:id :p2 :produces #{:b} :guard {:needs #{:a}}}]
                                             :precedence [:p1 :p2]})]
    (is (= :refused (:status cyc)))
    (is (= :cyclic-containment (:kind cyc)) "the cyclic refusal is real")
    (is (nil? (:descent cyc)))
    (is (not (w/received? {:writer (:descent (order)) :reader (:descent cyc)})))))

(deftest a-different-descent-fails-the-wire
  ;; a real, different descent from the live record: present, not absent,
  ;; but not the writer's [[A B]]
  (is (= (:sha256 record) (w/sha256-file (:path record))))
  (let [live (get-in (w/read-record (:path record))
                     [:decision :selection-certificate :precision-family
                      :selected-action :construction-receipt :order :descent])]
    (is (= 4 (count live)))
    (is (not= (:descent (order)) live))
    (is (not (w/received? {:writer (:descent (order)) :reader live})))))

(deftest the-live-record-carries-the-writers-end
  (is (= (:sha256 record) (w/sha256-file (:path record))))
  (is (vector? (get-in (w/read-record (:path record))
                       [:decision :selection-certificate :precision-family
                        :selected-action :construction-receipt :order :descent]))))
