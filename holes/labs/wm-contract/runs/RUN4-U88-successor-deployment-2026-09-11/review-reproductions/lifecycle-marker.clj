(require '[futon3c.wm.run4-series-service-test :as fixture]
         '[futon3c.wm.runner-service :as runner]
         '[futon3c.transport.http :as http]
         '[futon2.aif.c-fold-config :as digest]
         '[clojure.java.io :as io])
((var-get (ns-resolve 'futon3c.wm.run4-series-service-test 'with-service))
 (fn [root cfg]
   (let [file (io/file root "cohort.edn")
         marker (io/file (get-in cfg [:run4 :series :controller-root]) "001-started.edn")
         raw "{:cohort/id :test}"
         _ (spit file raw)
         _ (spit marker "{:not-a-start true}")
         calls (atom 0)
         cfg (-> cfg
                 (assoc-in [:run4 :execution-cohort]
                           {:preregistration (.getCanonicalPath file)
                            :data-root (.getCanonicalPath root)
                            :cohort-id :test :sha256 (digest/sha256 raw)})
                 (assoc-in [:run4 :cohort-preflight!]
                           (fn [_ capacity?]
                             (assert (false? capacity?))
                             (io/delete-file marker)
                             {:cohort-id :test :target 1 :remaining 0})))
         handler (http/make-handler cfg)]
     (with-redefs [runner/click! (fn [_] (swap! calls inc)
                                 {:click-id "should-not-start" :started-at "2026-09-11T00:00:00Z"})]
       (let [r (handler ((var-get (ns-resolve 'futon3c.wm.run4-series-service-test 'request)) {:run4-series-ref "series.edn"} fixture/auth))]
         (println {:status (:status r) :body (:body r) :clicks @calls}))))))
