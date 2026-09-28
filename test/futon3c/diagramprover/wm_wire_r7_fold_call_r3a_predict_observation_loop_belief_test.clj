(ns futon3c.diagramprover.wm-wire-r7-fold-call-r3a-predict-observation-loop-belief-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-producer-record :as producer-record]))

;; Converted to read the wire ends from the content-addressed
;; measured-cleanup producer record; this reader loads no product code.
(def producer (delay (producer-record/record "measured-cleanup")))

(def live-records-read (:live-records-read @producer))

(defn assert-live-pins []
  (doseq [{:keys [path sha256]} live-records-read]
    (assert (= sha256 (w/sha256-file path)))
    (let [r (w/read-record path)]
      (assert (not-any? #(and (map? %) (some (partial contains? %) [:carried-mu-post :loop-belief :channel-prediction :conditioning-steps]))
                        (tree-seq coll? seq r))))))

(defn check [] (get-in @producer [:cases :none]))
(def wire
  {
   :second-layer {:test 'futon3c.diagramprover.wm-wire-r7-fold-call-r3a-predict-observation-loop-belief-test/different-belief-changes-the-real-prediction :kind :value-varying
                  :product [:predictions :annotation-health] :intervention :before-reader}
  :wire [:r7-fold-call :r3a-predict-observation :loop-belief]
   :kind :witnessed-hermetically
   :test `real-judge-hands-belief-to-real-predictor
   :check check
   :live-records-read live-records-read
   :note "The inspected live records lack the predictor's input end. Run the real judge with hermetic external input ports; observe its loop belief at the real three-argument predictor, execute that reader, and compare both carrier and prediction. Missing/different carriers are negative controls. The values are read from the measured-cleanup producer record."})

(deftest real-judge-hands-belief-to-real-predictor
  (assert-live-pins)
  (let [o (check)]
    (is (pos? (:calls o)))
    (is (seq (:writer o)))
    (is (w/received? o))
    (is (= (:expected-predictions o) (:predictions o)))
    (is (number? (get-in o [:predictions :annotation-health :mean])))))

(deftest missing-belief-is-not-a-wire-witness
  (let [o (get-in @producer [:cases :absent])]
    (is (pos? (:calls o)))
    (is (not (w/received? o)))
    (is (not= (:expected-predictions o) (:predictions o)))))

(deftest different-belief-changes-the-real-prediction
  (let [o (get-in @producer [:cases :different])]
    (is (pos? (:calls o)))
    (is (not (w/received? o)))
    (is (not= (get-in o [:expected-predictions :annotation-health])
              (get-in o [:predictions :annotation-health])))))
