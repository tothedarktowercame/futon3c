(ns futon3c.diagramprover.wm-wire-r9-judge-refusal-r9-abstention-carrier-judge-refusal-test
  "Wire [:r9-judge-refusal :r9-abstention-carrier :judge-refusal]: the
  :no-selection sorry cell's :judge-refusal (judge-refusal-sorry) reaching
  persist-run-record!'s abstention-carrier branch, which reads it into the
  run record's [:decision :abstention :targets] entry (WM-CLICK-REFUSAL-I).

  No live record carries either end (live-records-read, each pinned and
  read): the seventh flight's abstained tick predates WM-CLICK-REFUSAL-I
  (its abstention targets carry no :data and no record key :judge-refusal
  occurs), and the fourth flight's refused tick predates the fix that kept
  the abstention (its [:decision :abstention] is a typed absence). So the
  wire is WITNESSED-HERMETICALLY: one hermetic tick whose judge throws a
  typed \"cascade decision refused\"; the writer's value is the sorry cell's
  :judge-refusal off the result's checkpoints; the reader's value is the
  abstention carrier's target entry, the fields it copied of the refusal."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-r9-support :as sup]))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "tick-run-record-2026-09-25-flight-ffcd772b-click-1.edn")
      :sha256 "8ab0db5d770085f17bb341293a92424149bf2388ae7a6dd3e724887c9a44eba2"
      :why "an abstained tick, but pre-WM-CLICK-REFUSAL-I: its abstention targets carry no :data and no :judge-refusal key occurs in the record"}
     {:path (p "tick-run-record-2026-09-26-flight-e70b4baf-click-1.edn")
      :sha256 "241b10a024020344eba5d444c12fb33ad6afe107724dec95c81229b422bc5feb"
      :why "the fourth flight's refused tick, pre-fix: [:decision :abstention] is the typed absence {:status :absent :reason :no-selection-decision-recorded}"}]))

(defn- refusal [kind]
  (ex-info "cascade decision refused" {:kind kind :target "M-t"}))

(defn observe
  "One tick whose judge throws KIND's refusal: {:writer the sorry cell's
  :judge-refusal, :reader the abstention carrier's entry's copied fields,
  or {:absent :no-judge-refusal} when the carrier has no entry,
  :carrier-status the abstention's :status}."
  [kind]
  (let [{:keys [result record]} (sup/run-tick (refusal kind))
        t (get-in record [:decision :abstention :targets 0])]
    {:writer (get-in result [:checkpoints :selection :sorry :judge-refusal])
     :reader (if t
               (select-keys t [:kind :target :missing :data])
               {:absent :no-judge-refusal})
     :carrier-status (get-in record [:decision :abstention :status])}))

(defn check [] (observe :live-c-stale))

(def wire
  {:wire [:r9-judge-refusal :r9-abstention-carrier :judge-refusal]
   :kind :witnessed-hermetically
   :test `the-sorry-cells-judge-refusal-reaches-the-abstention-carrier
   :check check
   :live-records-read live-records-read})

(deftest the-sorry-cells-judge-refusal-reaches-the-abstention-carrier
  (let [o (check)]
    (is (= {:kind :live-c-stale :target "M-t" :missing :live-c
            :data {:kind :live-c-stale :target "M-t"}}
           (:writer o)))
    (is (= :abstained (:carrier-status o)))
    (is (w/received? o))))

(deftest no-refusal-is-a-typed-absence-and-fails-the-wire
  ;; an untyped judge throw: no sorry cell, no abstention entry
  (let [{:keys [result record]} (sup/run-tick (ex-info "boom" {:x 1}))
        t (get-in record [:decision :abstention :targets 0])]
    (is (nil? (get-in result [:checkpoints :selection :sorry :judge-refusal])))
    (is (nil? t))
    (is (not (w/received? {:writer (get-in result [:checkpoints :selection :sorry :judge-refusal])
                           :reader (if t (select-keys t [:kind :target :missing :data])
                                       {:absent :no-judge-refusal})})))))

(deftest a-different-refusal-fails-the-wire
  (let [a (observe :live-c-stale)
        b (observe :incommensurable-family)]
    (is (some? (:reader b)))
    (is (not (w/received? {:writer (:writer a) :reader (:reader b)}))
        "the carrier's entry for one refusal is not the other's :judge-refusal")))

(deftest the-live-records-carry-no-judge-refusal
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path))
  (let [r1 (w/read-record (:path (first live-records-read)))
        r2 (w/read-record (:path (second live-records-read)))]
    (is (= :abstained (get-in r1 [:decision :abstention :status])))
    (is (every? #(not (contains? % :data)) (get-in r1 [:decision :abstention :targets]))
        "pre-fix abstention entries carry nothing of the refusal")
    (is (not (contains? (get-in r1 [:checkpoints :selection :sorry] {}) :judge-refusal)))
    (is (= {:status :absent :reason :no-selection-decision-recorded}
           (get-in r2 [:decision :abstention])))))
