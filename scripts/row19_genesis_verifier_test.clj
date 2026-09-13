(ns row19-genesis-verifier-test
  (:require [clojure.test :as t]
            [futon3c.agency.r9-genesis-test]))

(def target #'futon3c.agency.r9-genesis-test/genesis-boundary-positive-and-refusals)

(defn -main [& args]
  (let [induced? (some #{"--induced-failure"} args)
        counters (ref t/*initial-report-counters*)]
    (binding [t/*report-counters* counters]
      (t/test-vars [target])
      (when induced?
        (t/do-report {:type :fail :message "induced genesis runner control"
                      :expected :passing-run :actual :induced-failure})))
    (let [{:keys [test pass fail error]} @counters
          exit (if (zero? (+ fail error)) 0 1)]
      (println (pr-str {:runner :row19-genesis-verifier :tests test
                        :assertions (+ pass fail error) :pass pass
                        :fail fail :error error :exit exit
                        :induced-failure? induced?}))
      (shutdown-agents)
      (System/exit exit))))
