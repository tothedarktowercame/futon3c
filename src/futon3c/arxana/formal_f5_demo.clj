(ns futon3c.arxana.formal-f5-demo
  "Private Formal Arxana F5 demonstration on loopback 7087."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.arxana.formal-f1-demo :as base]
            [futon3c.arxana.formal-f3 :as f3]
            [futon3c.arxana.formal-f5 :as f5]
            [futon3c.arxana.formal-f5-projector :as projector]
            [futon3c.arxana.json :as ajson])
  (:import [java.lang ProcessHandle]
           [java.nio.file Files]))
(def default-port 7087)
(def lean-root "/home/joe/code/futon5/holes/labs/M-formal-arxana/f5-lean")
(def kernel-resource "test/resources/arxana/formal_arxana/F5a-kernel-run.edn")
(defn prepare! [{:keys [cfg]}]
  (let [bytes (Files/readAllBytes (.toPath (io/file lean-root f5/lean-source)))
        ingested (f3/ingest-manifestations! cfg [{:artifact/id "formal-f5:lean-geometry-demo"
                                                  :media-type "text/x-lean" :bytes bytes}])
        run (edn/read-string (slurp kernel-resource))
        geometry (f5/materialise! cfg (:artifact/revision-id (first ingested)) run)
        presentation (projector/materialise! cfg)]
    (when-not (and (every? :ok (:writes geometry)) (every? :ok (:writes presentation)))
      (throw (ex-info "F5 demo fixture failed" {:geometry geometry :presentation presentation})))
    {:geometry geometry :presentation presentation}))
(defn start! ([] (start! default-port)) ([port]
  (let [h (base/start! port)]
    (try (assoc h :f5/prepared (prepare! h))
         (catch Throwable t (base/stop! h) (throw t))))))
(def stop! base/stop!)
(defn recording [{:keys [cfg] prepared :f5/prepared}]
  (:ok (projector/render! cfg (get-in prepared [:presentation :panels]))))
(defn write-recording! [h path]
  (spit path (str (ajson/encode (recording h)) "\n")))
(defn -main [& [port-str ready-path]]
  (let [port (if port-str (Long/parseLong port-str) default-port) h (start! port)
        stopped? (atom false) stop #(when (compare-and-set! stopped? false true) (stop! h))]
    (.addShutdownHook (Runtime/getRuntime) (Thread. stop "formal-f5-demo-shutdown"))
    (when ready-path (spit ready-path (pr-str {:status :ready :pid (.pid (ProcessHandle/current))
                                               :port port})))
    (println (pr-str {:status :ready :port port})) (flush) @(promise)))
