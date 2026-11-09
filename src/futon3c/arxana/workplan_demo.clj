(ns futon3c.arxana.workplan-demo
  (:require [futon3c.arxana.formal-f1-demo :as base]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.workplan :as workplan])
  (:import [java.lang ProcessHandle]))

(def default-port 7201)
(defn start! [port]
  (let [h (base/start! port)
        result (workplan/materialise! #(service/dispatch (:cfg h) %) (workplan/read-slice))
        item-id (get-in result [:by-local-id :item :asserted-content :item/id])]
    (assoc h :workplan/result result :workplan/items [item-id])))
(def stop! base/stop!)
(defn -main [& [port-str ready-path]]
  (let [port (Long/parseLong (or port-str (str default-port))) h (start! port)
        stopped? (atom false) stop #(when (compare-and-set! stopped? false true) (stop! h))
        ready {:status :ready :pid (.pid (ProcessHandle/current)) :port port :items (:workplan/items h)}]
    (.addShutdownHook (Runtime/getRuntime) (Thread. stop "workplan-demo-shutdown"))
    (when ready-path (spit ready-path (pr-str ready)))
    (println (pr-str ready)) (flush) @(promise)))
