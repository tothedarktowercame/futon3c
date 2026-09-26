(ns futon3c.diagramprover.wm-wire-r9-selection-law-decision-per-policy-argmax-test
  "Wire [:r9-selection-law :r9-decision :per-policy-argmax]: the selection
  law's posterior mode reaching the joint decision, which reads
  [:selection-law :per-policy-argmax :action] and hands it to
  candidate-derivations as :actions (war_machine.clj:6665-6670).

  The seventh flight's run record carries the writer's end
  ([:decision :selection-law :per-policy-argmax]) and an unrefused
  :candidate-derivations — the read succeeded — but the action value itself
  is not recorded there, so no live record carries both ends
  (live-records-read). The wire is WITNESSED-HERMETICALLY:
  select-action-cascades (writer) runs over the selection-law candidate
  roster; the reader's var cascade-decision-admitted cannot be called short
  of the full joint assembly (focus inputs, theta ledger, precision carry),
  so the witness performs its read verbatim — (get-in decision
  [:selection-law :per-policy-argmax :action]) — on the writer's live
  decision and observes the action the decision would hand on."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon2.aif.policy :as policy]
            [futon3c.diagramprover.wm-wire :as w]))

;; the selection-law-candidate roster (futon2 selection_law_candidate_test.clj)
(defn- step [id target]
  {:id id :target target :guard {:clauses [{:present #{} :absent #{}}]} :produces #{}})

(defn- ranked [cascade-id target g & steps]
  {:action {:kind :cascade-candidate :id cascade-id :target target
            :precedence (vec steps)
            :construction-receipt {:kind :fixture :id cascade-id}
            :interpretation-receipts {cascade-id {:kind :fixture}}}
   :cascade true
   :cascade-id cascade-id
   :controller-score g})

(def roster
  [(ranked :cas/b "M-t" 0.5 (step :p/b "M-t"))
   (ranked :cas/a1 "M-t" 1.0 (step :p/a "M-t") (step :p/c "M-t"))
   (ranked :cas/a2 "M-t" 1.0 (step :p/a "M-t") (step :p/d "M-t"))])

(defn- decide []
  (policy/select-action-cascades
   roster {:beta 1 :novelty-inputs {}
           :cascade-habit-path (str (io/file (w/tmp-dir "row9-") "absent.edn"))}))

(defn observe
  "The writer's decision, then the reader's read of :per-policy-argmax
  (war_machine.clj:6665-6669) verbatim. TAMPER edits the decision before
  the read (the bad cases). {:writer the selection law's
  :per-policy-argmax, :reader the value at the reader's path, :action what
  the decision would hand candidate-derivations}."
  ([] (observe identity))
  ([tamper]
   (let [d (decide)
         d' (tamper d)]
     {:writer (get-in d [:selection-law :per-policy-argmax])
      :reader (let [v (get-in d' [:selection-law :per-policy-argmax])]
                (if (nil? v) {:absent :field-not-carried} v))
      :action (get-in d' [:selection-law :per-policy-argmax :action])})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
      :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
      :why "carries the writer's end ([:decision :selection-law :per-policy-argmax], action id :C1) and an unrefused :candidate-derivations — the decision's read passed the bijectivity check — but the action value the decision read is echoed nowhere, so the reader's end is not on the record"}
     {:path (p "tick-run-record-2026-09-26-flight-e70b4baf-click-1.edn")
      :sha256 "241b10a024020344eba5d444c12fb33ad6afe107724dec95c81229b422bc5feb"
      :why "no :per-policy-argmax: the tick refused in class scoring before selection"}
     {:path (p "tick-run-record-2026-09-26-flight-7f89646a-click-1.edn")
      :sha256 "a8e04fb97e58808e8fabdb4ab771f3c414b4181ef82dac336729dd472a18d816"
      :why "no :per-policy-argmax: the tick closed :incomplete with no selection law"}
     {:path (p "tick-run-record-2026-09-25-flight-ffcd772b-click-1.edn")
      :sha256 "8ab0db5d770085f17bb341293a92424149bf2388ae7a6dd3e724887c9a44eba2"
      :why "no :per-policy-argmax: the tick abstained before selection"}
     {:path (p "flight-ada87008/tick-run-record-2026-09-26-flight-ada87008-click-1.edn")
      :sha256 "df01831c24a7042d66b6ef2c38d82cdfbd0994a03b5539f3112db7dc41894970"
      :why "no :per-policy-argmax: selection failed on the registry read"}]))

(def wire
  {:wire [:r9-selection-law :r9-decision :per-policy-argmax]
   :kind :witnessed-hermetically
   :test `the-posterior-mode-reaches-the-decision
   :check check
   :live-records-read live-records-read})

(deftest the-posterior-mode-reaches-the-decision
  (let [o (check)]
    (is (= :cas/b (get-in o [:writer :action :id]))
        "the mode is :cas/b (the marginal winner is a :p/a cascade, the candidate wire's case)")
    (is (some? (:action o)) "the read's product: the action handed to candidate-derivations")
    (is (= (:action o) (get-in o [:writer :action])))
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  (let [o (observe #(assoc-in % [:selection-law :per-policy-argmax] {:absent :no-argmax}))]
    (is (w/typed-absence? (:reader o)))
    (is (nil? (:action o)))
    (is (not (w/received? o)))))

(deftest a-different-argmax-at-the-reader-fails-the-wire
  (let [o (observe #(assoc-in % [:selection-law :per-policy-argmax :action :id] :cas/other))]
    (is (some? (:reader o)))
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))

(deftest the-live-record-carries-the-writers-end
  (let [{:keys [path sha256]} (first live-records-read)
        r (w/read-record path)]
    (is (= sha256 (w/sha256-file path)))
    (is (= :C1 (get-in r [:decision :selection-law :per-policy-argmax :action :id])))
    (is (map? (get-in r [:decision :selection-certificate :candidate-derivations]))
        "unrefused: the decision's read passed, but the action is not echoed")))
