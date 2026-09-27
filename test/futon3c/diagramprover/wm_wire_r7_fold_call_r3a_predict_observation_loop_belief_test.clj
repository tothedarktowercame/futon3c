(ns futon3c.diagramprover.wm-wire-r7-fold-call-r3a-predict-observation-loop-belief-test
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.belief :as belief]
            [futon2.report.war-machine :as wm]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-fold-out-support :as support]
            [futon3c.diagramprover.wm-wire-fold-in-support :as fold-in]
            [futon3c.diagramprover.wm-wire-measured-support :as measured]))

(defn observe [mutation]
  (let [root (w/tmp-dir "belief-prediction-wire-")
        predict belief/predict-observation
        captured (atom [])
        other (wm/apply-arena-belief-events
               (belief/initial-belief-state ["known"])
               [(assoc (first fold-in/events) :type :foreclosed)])]
    (try
      (with-redefs [belief/predict-observation
                    (fn [& args]
                      ;; Observe only judge's three-argument call. The real reader's
                      ;; recursive arity calls still run their original bodies.
                      (if (= 3 (count args))
                        (let [[value tags context] args
                              received (case mutation :none value :absent nil :different other)
                              expected (predict value tags context)
                              actual (predict received tags context)]
                          (swap! captured conj {:writer value :reader received
                                                :expected-predictions expected
                                                :predictions actual})
                          actual)
                        (apply predict args)))]
        (support/judge root {:annotation-graph {:health 0.9}}))
      (assoc (first @captured) :calls (count @captured))
      (finally (measured/cleanup root)))))

(defn check [] (observe :none))
(def wire
  {:wire [:r7-fold-call :r3a-predict-observation :loop-belief]
   :kind :witnessed-hermetically
   :test `real-judge-hands-belief-to-real-predictor
   :check check
   :live-records-read support/live-records-read
   :note "The inspected live records lack the predictor's input end. Run the real judge with hermetic external input ports; observe its loop belief at the real three-argument predictor, execute that reader, and compare both carrier and prediction. Missing/different carriers are negative controls."})

(deftest real-judge-hands-belief-to-real-predictor
  (support/assert-live-pins)
  (let [o (check)]
    (is (pos? (:calls o)))
    (is (seq (:writer o)))
    (is (w/received? o))
    (is (= (:expected-predictions o) (:predictions o)))
    (is (number? (get-in o [:predictions :annotation-health :mean])))))

(deftest missing-belief-is-not-a-wire-witness
  (let [o (observe :absent)]
    (is (pos? (:calls o)))
    (is (not (w/received? o)))
    (is (not= (:expected-predictions o) (:predictions o)))))

(deftest different-belief-changes-the-real-prediction
  (let [o (observe :different)]
    (is (pos? (:calls o)))
    (is (not (w/received? o)))
    (is (not= (get-in o [:expected-predictions :annotation-health])
              (get-in o [:predictions :annotation-health])))))
