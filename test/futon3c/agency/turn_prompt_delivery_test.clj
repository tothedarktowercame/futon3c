(ns futon3c.agency.turn-prompt-delivery-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agency.turn-prompt-delivery :as delivery]
            [futon3c.transport.http :as http]))

(deftest prompt-arrives-before-terminal-bound
  (is (= "$~this-turn> "
         (delivery/await-ready! {:analysis-seat? false
                         :timeout-ms 150
                         :worker #(% "$~this-turn> ")}))))

(deftest stalled-search-does-not-hold-terminal-past-bound
  (let [started (System/nanoTime)
        result (delivery/await-ready! {:analysis-seat? false
                               :timeout-ms 150
                               :worker (fn [publish!]
                                         (Thread/sleep 1000)
                                         (publish! "$~late> "))})
        elapsed-ms (/ (- (System/nanoTime) started) 1000000.0)]
    (is (nil? result))
    (is (< elapsed-ms 260.0) (str "terminal wait was " elapsed-ms "ms"))))

(deftest analysis-seat-neither-searches-nor-publishes
  (let [called? (atom false)]
    (is (nil? (delivery/await-ready! {:analysis-seat? true
                              :timeout-ms 150
                              :worker (fn [_] (reset! called? true))})))
    (is (false? @called?))))

(deftest terminal-event-carries-only-a-ready-prompt
  (let [result {:result "reply" :session-id "session-1"}]
    (is (= "$~this-turn> "
           (:prompt-line (http/invoke-done-event result "$~this-turn> "))))
    (is (not (contains? (http/invoke-done-event result nil) :prompt-line)))))
