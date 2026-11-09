(ns futon3c.arxana.workplan-demo
  (:require [futon3c.arxana.formal-f1-demo :as base]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.workplan :as workplan]
            [futon3c.arxana.workplan-backlinks :as backlinks])
  (:import [java.lang ProcessHandle]))

(def default-port 7201)
(defn start! [port]
  (let [h (base/start! port)
        result (workplan/materialise! #(service/dispatch (:cfg h) %) (workplan/read-slice))
        _ (backlinks/materialise-rules! #(service/dispatch (:cfg h) %) (backlinks/read-rules))
        items (mapv :item/id (get-in result [:catalog :asserted-content :items]))]
    (when (seq (:failures result))
      (base/stop! h) (throw (ex-info "workplan load failures" {:failures (:failures result)})))
    (assoc h :workplan/result result :workplan/items items)))
(def stop! base/stop!)
(defn -main [& [port-str ready-path]]
  (let [port (Long/parseLong (or port-str (str default-port))) h (start! port)
        stopped? (atom false) stop #(when (compare-and-set! stopped? false true) (stop! h))
        ready {:status :ready :pid (.pid (ProcessHandle/current)) :port port
               :item/count (count (:workplan/items h))
               :load/counts (get-in h [:workplan/result :counts])
               :load/elapsed-ms (get-in h [:workplan/result :elapsed-ms])}]
    (.addShutdownHook (Runtime/getRuntime) (Thread. stop "workplan-demo-shutdown"))
    (when ready-path (spit ready-path (pr-str ready)))
    (println (pr-str ready)) (flush) @(promise)))
