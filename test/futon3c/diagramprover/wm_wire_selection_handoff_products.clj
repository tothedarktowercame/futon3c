(ns futon3c.diagramprover.wm-wire-selection-handoff-products
  (:require [clojure.test :as t]
            [futon2.aif.cascade-problems :as cp]
            [futon2.aif.locator-fixtures :as loc]
            [futon2.aif.policy :as policy]
            [futon2.aif.enactment-habit :as habit]
            [futon2.aif.selection-reads-fold-test :as fold-test]
            [futon2.report.cascade-decision-test :as fixture]
            [futon2.report.war-machine :as wm] [futon2.aif.wm.cascade-decision :as wm-cd]))

(defn decision-product [reverse?]
  (let [select policy/select-action-cascades written (atom nil) scores (atom nil)
        assembled (cp/assemble {:targets [fixture/tick-1-target]
                                :sources (loc/locate-all fixture/tick-1-sources)})
        result (with-redefs [policy/select-action-cascades
                            (fn [ranked opts]
                              ;; Explicit unequal-G selector fixture. Keep the real
                              ;; assembled candidates and run the real law; never
                              ;; mutate its returned argmax or candidate.
                              (let [gs (if reverse? [4.0 2.0 0.0] [0.0 2.0 4.0])
                                    rs (mapv #(assoc %1 :controller-score %2) ranked gs)
                                    d (select rs opts)]
                                (reset! scores gs) (reset! written d) d))]
                 (wm-cd/cascade-decision assembled
                  (assoc fixture/live-c-opts :cascade-habit-path "resources/fixtures/d-token-carry/absent-habit.edn")))
        d (:decision result)]
    {:scores @scores
     :posterior (into (sorted-map) (map (fn [[c p]] [(:id c) p]) (get-in d [:selection-law :posterior])))
     :supplied (get-in @written [:selection-law :per-policy-argmax])
     :recorded (get-in d [:selection-law :per-policy-argmax])
     :candidate (get-in d [:selection-law :candidate])
     :derivations (get-in d [:selection-certificate :candidate-derivations])}))

(defn increment-product [reverse?]
  (let [d (decision-product reverse?)
        identity [:fixture :context [:p]]
        enactment {:click "same-click" :candidate (:candidate d)
                   :attempts [{:pattern :p :success true}]}
        receipt (habit/increment enactment identity [])]
    {:scores (:scores d) :posterior (:posterior d) :candidate (:candidate d)
     :other-inputs {:enactment (dissoc enactment :candidate) :identity identity :verdict []}
     :receipt receipt}))

(defn assertion-product [changed?]
  (let [select policy/select-action-cascades reports (atom []) supplied (atom nil)]
    (with-redefs [policy/select-action-cascades
                  (fn [& args]
                    (let [d (apply select args)
                          d (if changed? (assoc-in d [:selection-law :e-source :records] 99) d)]
                      (reset! supplied (get-in d [:selection-law :e-source])) d))
                  t/report #(swap! reports conj %)]
      (fold-test/two-passing-enactments-shift-e-toward-their-cascade))
    {:carrier @supplied :reports @reports :counts (frequencies (map :type @reports))}))
