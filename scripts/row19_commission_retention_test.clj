(ns row19-commission-retention-test
  "Failure-sensitive focused runner for the Row 19 retention test."
  (:require [clojure.test :as t]
            [futon3c.transport.http-test]))

(def target
  #'futon3c.transport.http-test/invoke-request-commission-survives-hot-job-expiry-and-rejoins-digest)

(defn -main [& args]
  (let [induced? (some #{"--induced-failure"} args)
        counters (ref t/*initial-report-counters*)]
    (binding [t/*report-counters* counters]
      (t/test-vars [target])
      (when induced?
        (t/do-report {:type :fail
                      :message "induced runner exit control"
                      :expected :passing-run
                      :actual :induced-failure})))
    (let [{:keys [test pass fail error]} @counters
          exit (if (zero? (+ fail error)) 0 1)]
      (println (pr-str {:runner :row19-commission-retention
                        :tests test :assertions (+ pass fail error)
                        :pass pass :fail fail :error error :exit exit
                        :induced-failure? induced?}))
      (shutdown-agents)
      (System/exit exit))))
