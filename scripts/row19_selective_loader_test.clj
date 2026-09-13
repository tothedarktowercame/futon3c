(ns row19-selective-loader-test
  (:require [clojure.test :as t] [futon3c.agency.selective-form-loader-test]))
(def target #'futon3c.agency.selective-form-loader-test/offline-selective-loader-contract)
(defn -main [& args]
  (let [induced? (some #{"--induced-failure"} args) counters (ref t/*initial-report-counters*)]
    (binding [t/*report-counters* counters] (t/test-vars [target])
      (when induced? (t/do-report {:type :fail :message "induced loader runner failure"
                                   :expected :pass :actual :induced})))
    (let [{:keys [test pass fail error]} @counters exit (if (zero? (+ fail error)) 0 1)]
      (prn {:runner :row19-selective-loader :tests test :assertions (+ pass fail error)
            :pass pass :fail fail :error error :exit exit})
      (shutdown-agents) (System/exit exit))))
