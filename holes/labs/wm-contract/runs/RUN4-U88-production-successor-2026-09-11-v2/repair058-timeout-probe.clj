(require '[futon2.aif.full-loop-runner :as r] '[babashka.http-client :as http]
         '[cheshire.core :as json] '[clojure.edn :as edn])
(let [finding (edn/read-string (slurp "data/wm-repair-obligations/findings/repair-attempt-058-untyped-failure.edn"))
      ok {:status 200 :body (json/generate-string {:ok true :selection {:status "verified-live-selection" :selected-mission-ids ["M-a"]}})}
      run (fn [mode]
            (let [calls (atom 0) sleeps (atom [])
                  out (with-redefs [http/post (fn [& _]
                                               (swap! calls inc)
                                               (if (and (= mode :recovery) (= 2 @calls)) ok
                                                 (throw (java.net.http.HttpTimeoutException. "request timed out"))))]
                        (try (r/strategic-selection! {:agency-base "http://isolated-port"
                                                     :strategic-selection-sleep-fn #(swap! sleeps conj %)}
                                                    {:scheduler-habit-ranking ["M-a"]})
                             (catch clojure.lang.ExceptionInfo e (ex-data e))))]
              {:calls @calls :sleeps @sleeps
               :result (select-keys out [:status :failure-kind :failure-detail :readiness/selection-transient :attempt-failures])}))
      recovery (run :recovery) exhausted (run :exhausted)]
 (assert (= :selection (:failure-stage finding)))
 (assert (= 2 (:calls recovery)))
 (assert (= :verified-live-selection (get-in recovery [:result :status])))
 (assert (= 3 (:calls exhausted)))
 (assert (= :transient-exhausted (get-in exhausted [:result :failure-detail])))
 (prn {:repair-id (:repair/id finding) :port-fault "java.net.http.HttpTimeoutException: request timed out"
       :recovery recovery :exhaustion exhausted :repair-resolved? false :live-attempt? false}))
(shutdown-agents)
