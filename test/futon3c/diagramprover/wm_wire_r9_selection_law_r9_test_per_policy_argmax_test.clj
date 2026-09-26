(ns futon3c.diagramprover.wm-wire-r9-selection-law-r9-test-per-policy-argmax-test
  "Wire [:r9-selection-law :r9-test :per-policy-argmax]: the selection
  law's posterior mode reaching the box's own test,
  futon2/test/futon2/aif/selection_law_candidate_test.clj, whose read is
  (get-in law [:per-policy-argmax :action :id]) in
  the-candidate-is-the-chosen-action-not-the-posterior-mode.

  The seventh flight's run record carries the writer's end
  ([:decision :selection-law :per-policy-argmax], action id :C1) but a test
  box records nothing, so the wire is WITNESSED-HERMETICALLY:
  select-action-cascades (writer) runs over the reader's own roster, and
  the reader's read is observed. The roster makes the mode (:cas/b) differ
  from the chosen candidate, so this wire carries a value the candidate
  wire does not."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon2.aif.policy :as policy]
            [futon3c.diagramprover.wm-wire :as w]))

;; the reader's own fixtures (selection_law_candidate_test.clj:16-33)
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

(defn observe
  "select-action-cascades (writer) over the reader's roster, then the
  reader's read (:per-policy-argmax law). TAMPER edits the selection law
  before the read (the bad cases)."
  ([] (observe identity))
  ([tamper]
   (let [d (policy/select-action-cascades
            roster {:beta 1 :novelty-inputs {}
                    :cascade-habit-path (str (io/file (w/tmp-dir "row9-") "absent.edn"))})
         law (:selection-law d)
         law' (tamper law)]
     {:writer (:per-policy-argmax law)
      :reader (let [v (:per-policy-argmax law')]
                (if (nil? v) {:absent :field-not-carried} v))})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
      :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
      :why "carries the writer's end: [:decision :selection-law :per-policy-argmax] with action id :C1; the reader is a test and records nothing"}
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
  {:wire [:r9-selection-law :r9-test :per-policy-argmax]
   :kind :witnessed-hermetically
   :test `the-posterior-mode-reaches-the-selection-test
   :check check
   :live-records-read live-records-read})

(deftest the-posterior-mode-reaches-the-selection-test
  (let [o (check)]
    (is (= :cas/b (get-in o [:writer :action :id]))
        "the reader's own expectation: the mode is :cas/b, not the chosen candidate")
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  (let [o (observe #(assoc % :per-policy-argmax {:absent :no-argmax}))]
    (is (w/typed-absence? (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-argmax-at-the-reader-fails-the-wire
  (let [o (observe #(assoc-in % [:per-policy-argmax :action :id] :cas/other))]
    (is (some? (:reader o)))
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))

(deftest the-live-record-carries-the-writers-end
  (let [{:keys [path sha256]} (first live-records-read)]
    (is (= sha256 (w/sha256-file path)))
    (is (= :C1 (get-in (w/read-record path)
                       [:decision :selection-law :per-policy-argmax :action :id])))))
