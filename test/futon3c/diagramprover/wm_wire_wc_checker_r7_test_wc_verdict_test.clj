(ns futon3c.diagramprover.wm-wire-wc-checker-r7-test-wc-verdict-test
  (:require [clojure.test :as t :refer [deftest is]]
            [futon2.aif.enactment-habit :as eh]
            [futon2.aif.selection-reads-fold-test :as fold-test]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-selection-out-support :as support]))

(defn observe [mutation]
  (let [real-checker fold-test/checker-verdict real-increment eh/increment
        printed (atom nil) observed (atom []) reports (atom [])]
    (with-redefs [fold-test/checker-verdict
                  (fn [record enactment]
                    (let [verdict (real-checker record enactment)]
                      (reset! printed verdict)
                      verdict))
                  eh/increment
                  (fn [enactment identity verdict]
                    (let [received (case mutation :none verdict :absent {:status :absent}
                                         :different ["different-checker-verdict"])
                          result (real-increment enactment identity received)]
                      (swap! observed conj {:writer @printed :reader received :result result})
                      result))
                  t/report #(swap! reports conj %)]
      (fold-test/real-checker-verdict-into-increment))
    (assoc (first @observed) :observations @observed :reports @reports)))

(def positive (delay (observe :none)))
(defn check [] @positive)
(def wire
  {:wire [:wc-checker :r7-test :wc-verdict]
   :kind :witnessed-hermetically
   :test 'futon2.aif.selection-reads-fold-test/real-checker-verdict-into-increment
   :check check :live-records-read support/live-records-read
   :note "WIRE-23-C3: the named futon2 test runs bb proof2a_check.clj --wc --edn, passes its parsed verdict through enactment-receipts to the real increment and asserts the fold count. Capture wraps the real checker call and increment; bad controls alter the verdict only at increment's door."})

(deftest real-checker-output-reaches-the-named-test
  (let [{:keys [observations reports] :as o} (check)]
    (is (w/received? o))
    (is (= [] (:writer o)))
    (is (= [1 0] (mapv #(get-in % [:result :delta]) observations)))
    (is (= 2 (count observations)))
    (is (every? w/received? observations))
    (is (seq reports))
    (is (not-any? #(#{:fail :error} (:type %)) reports))))

(deftest typed-absence-before-the-real-reader
  (let [o (observe :absent)]
    (is (not (w/received? o)))
    (is (= 0 (get-in o [:result :delta])))
    (is (some #(= :fail (:type %)) (:reports o)))))

(deftest different-verdict-before-the-real-reader
  (let [o (observe :different)]
    (is (not (w/received? o)))
    (is (= ["different-checker-verdict"] (get-in o [:result :wc-failures])))
    (is (some #(= :fail (:type %)) (:reports o)))))
