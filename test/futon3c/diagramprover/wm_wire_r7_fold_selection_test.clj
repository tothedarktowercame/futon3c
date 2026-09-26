(ns futon3c.diagramprover.wm-wire-r7-fold-selection-test
  "Wire [:r7-fold :r7-selection :enactment-records]: the habit prior E, as
  enactment-habit/fold's counted records, reaching policy selection
  (futon-2026 Figure 6A's E -> selection edge).

  No live record carries both ends (live-records-read, each with why), and
  no production caller runs the writer: nothing outside tests calls
  enactment-habit/fold, and the joint selection (war_machine.clj, the
  select-action-cascades call in the joint decision) passes no
  :enactment-fold, so selection always reads the empty fold, uniform E.
  So the wire is WITNESSED-HERMETICALLY: an increment receipt folded by
  enactment-habit/fold is passed to policy/select-action-cascades as
  :enactment-fold, and the reader's value is the fold state its habit-read
  receipt records as consumed (cascade-habit-store/attach-state, :state)."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.enactment-habit :as eh]
            [futon2.aif.policy :as policy]
            [futon2.aif.scoring-input-receipts :as receipts]
            [futon3c.diagramprover.wm-wire :as w]))

(defn- step [id] {:id id :target "M-t" :guard {:clauses [{:present #{} :absent #{}}]} :produces #{}})

(defn- ranked [id g]
  {:action {:kind :cascade-candidate :id id :target "M-t" :precedence [(step (keyword "p" (name id)))]
            :construction-receipt {:kind :fixture} :interpretation-receipts {id {:kind :fixture}}}
   :cascade true :cascade-id id :controller-score g})

(def roster [(ranked :b 1.0) (ranked :a 1.0)])

(defn- receipt-for [click]
  (eh/increment {:click click :candidate :b :attempts [{:pattern :p/b}]}
                [:pattern-cascade "M-t" [:p/b] {}] []))

(defn observe
  "Fold RECEIPTS, pass SUPPLIED (default the fold) to selection as
  :enactment-fold (:none passes no key): {:writer the fold's
  :enactment-records, :reader what the selection's habit-read receipt
  records as consumed under :enactment-records (the receipt itself when it
  is a typed absence), :e-source}."
  ([receipts] (observe receipts nil))
  ([receipts supplied]
   (let [st (eh/fold nil receipts)
         reads (atom [])
         opts (cond-> {:beta 1 :novelty-inputs {}}
                (not= :none supplied) (assoc :enactment-fold (or supplied st)))
         d (binding [receipts/*habit-reads* reads] (policy/select-action-cascades roster opts))
         r (:receipt (first @reads))]
     {:writer (:enactment-records st)
      :reader (if (w/typed-absence? r) r (get-in r [:state :enactment-records]))
      :e-source (get-in d [:selection-law :e-source])})))

(defn check [] (observe [(receipt-for "click-1")]))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
      :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
      :why "the reader's end is a typed absence: both habit reads {:status :absent :reason :no-enactment-fold}, the consumed state has no :enactment-records, :e-source {:records 0 :samples 0 :uniform true}; the writer never ran"}
     {:path (p "tick-run-record-2026-09-26-flight-7f89646a-click-1.edn")
      :sha256 "a8e04fb97e58808e8fabdb4ab771f3c414b4181ef82dac336729dd472a18d816"
      :why "habit reads {:status :absent :reason :no-enactment-fold}; the tick closed :incomplete with no selection law"}
     {:path (p "tick-run-record-2026-09-26-flight-e70b4baf-click-1.edn")
      :sha256 "241b10a024020344eba5d444c12fb33ad6afe107724dec95c81229b422bc5feb"
      :why "one habit read {:status :absent :reason :no-enactment-fold}; no selection law"}
     {:path (p "tick-run-record-2026-09-25-flight-ffcd772b-click-1.edn")
      :sha256 "8ab0db5d770085f17bb341293a92424149bf2388ae7a6dd3e724887c9a44eba2"
      :why "no habit read recorded: the tick abstained before selection read E"}
     {:path (p "flight-ada87008/tick-run-record-2026-09-26-flight-ada87008-click-1.edn")
      :sha256 "df01831c24a7042d66b6ef2c38d82cdfbd0994a03b5539f3112db7dc41894970"
      :why "no habit read recorded: selection failed on the registry read before E was read"}]))

(def wire
  {:wire [:r7-fold :r7-selection :enactment-records]
   :kind :witnessed-hermetically
   :test `the-fold-reaches-selection
   :check check
   :live-records-read live-records-read})

(deftest the-fold-reaches-selection
  (let [o (check)]
    (is (w/received? o))
    (is (= 1 (count (:writer o))))
    (is (= {:source :enactment-fold :records 1 :samples 1 :uniform false} (:e-source o))
        "the selection's own record of what it read agrees")))

(deftest no-fold-is-a-typed-absence-and-fails-the-wire
  ;; the bad case production is in: nothing passed
  (let [o (observe [(receipt-for "click-1")] :none)]
    (is (= {:status :absent :reason :no-enactment-fold}
           (select-keys (:reader o) [:status :reason])))
    (is (not (w/received? o)))))

(deftest another-fold-than-the-writers-fails-the-wire
  (let [o (observe [(receipt-for "click-1")] (eh/fold nil [(receipt-for "click-2")]))]
    (is (some? (:reader o)))
    (is (not (w/received? o)) "present, not absent, but not the value the writer wrote")))

(deftest the-live-records-carry-no-fold
  ;; each record read, pinned by sha, and the reason it witnesses nothing
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path)
    (let [r (w/read-record path)
          occ (get-in r [:habit-reads :occurrences])]
      (is (not-any? #(contains? (get-in % [:receipt :state]) :enactment-records) occ) path)
      (is (every? #(= :no-enactment-fold (get-in % [:receipt :reason])) occ) path)))
  (is (= {:source :enactment-fold :records 0 :samples 0 :uniform true}
         (get-in (w/read-record (:path (first live-records-read))) [:decision :selection-law :e-source]))))
