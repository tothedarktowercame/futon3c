(ns futon3c.diagramprover.wm-wire-r9-decision-class-model-target-class-test
  "Wire [:r9-decision :r9-class-model :target-class]: the joint decision's
  mapped target classes reaching the class observation model
  (war_machine.clj:6583-6590 builds :target-class by mapping
  target-classifications through scorer-class and hands it to
  class-observation-model, which destructures it).

  VERIFIED on the seventh flight's run record: the reader's end is the
  class model's own :target-class {\"M-autoclock-in\" :related}, recorded
  in the scored winner's :computed-f :model. The writer's end is
  recoverable from the same record: the recorded classifications the
  decision attached (its own target-classifications) mapped through
  scorer-class, the constant of the writer's site at war_machine.clj:6532."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]))

(def record-path
  (str w/spike-dir "/tick-run-record-2026-09-26-flight-278b6988-click-1.edn"))

(def record-sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa")

;; war_machine.clj:6532, the writer's vocabulary map (a constant of the
;; site, not a value)
(def scorer-class {:focus :focused :associated :related :useful-elsewhere :unrelated})

(def classifications-path [:decision :selection-certificate :focus-receipt :candidates])
(def class-model-path [:decision :selection-law :policy-comparison :winner :computed-f :model])

(defn observe
  "Both ends off the pinned record: {:writer the decision's :target-class
  map recovered from the recorded classifications through the site's
  scorer-class, :reader the class model's recorded :target-class}. TAMPER
  edits the record in memory before the read (the bad cases)."
  ([] (observe identity))
  ([tamper]
   (let [r (tamper (w/read-record record-path))
         cls (get-in r classifications-path)
         model (get-in r class-model-path)]
     {:writer (into {} (map (fn [c] [(:target c) (get scorer-class (:class c) :unknown)]) cls))
      :reader (:target-class model)
      :model-schema (:schema model)
      :model-kind (:kind model)})))

(defn check [] (observe))

(def wire
  {:wire [:r9-decision :r9-class-model :target-class]
   :kind :verified
   :test `the-target-class-reaches-the-class-model
   :check check
   :record {:path record-path :sha256 record-sha256}})

(deftest the-target-class-reaches-the-class-model
  (is (= record-sha256 (w/sha256-file record-path)) "the pinned record before it is read")
  (let [o (check)]
    (is (= :wm/observation-model-v1 (:model-schema o)))
    (is (= :class-emission (:model-kind o)))
    (is (= {"M-autoclock-in" :related} (:writer o)))
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  (let [o (observe #(assoc-in % (conj class-model-path :target-class) {:absent :no-target-class}))]
    (is (w/typed-absence? (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-target-class-at-the-reader-fails-the-wire
  (let [o (observe #(assoc-in % (into class-model-path [:target-class "M-autoclock-in"]) :focused))]
    (is (some? (:reader o)))
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))
