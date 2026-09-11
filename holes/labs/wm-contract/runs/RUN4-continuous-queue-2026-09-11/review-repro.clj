(require '[futon3c.wm.run4-series-queue :as q]
         '[futon3c.wm.run4-series-queue-test :as qt]
         '[futon3c.wm.run4-series-service :as s]
         '[clojure.java.io :as io])
(let [root (#'qt/temp-root) c (#'qt/fixture root "review")]
 (try
  (with-redefs [s/step! (fn [& _] {:status :awaiting-terminal-evidence :click-id "retained-click"})]
   (q/start! c) (q/tick! c)
   (println :resume (try (q/resume! c) (catch Exception e (:reason (ex-data e))))))
  (println :after-resume (select-keys (q/read-state! c) [:status :reason :in-flight]))
  (q/stop! c)
  (io/delete-file (io/file (:state-root c) "queue-state.edn"))
  (.mkdir (io/file (:state-root c) "queue-state.edn"))
  (println :directory-state (select-keys (q/read-state! c) [:status :cursor :reason]))
  (finally (#'qt/delete-tree! root))))
