(ns futon3c.arxana.formal-f6-demo
  "Private Formal Arxana F6 demonstration on loopback."
  (:require [futon3c.arxana.formal-f1-demo :as base]
            [futon3c.arxana.formal-f5-demo :as f5-demo]
            [futon3c.arxana.formal-f6 :as f6]
            [futon3c.arxana.service :as service])
  (:import [java.lang ProcessHandle]))
(def default-port 7090)
(defn call! [cfg op body id principal]
  (service/dispatch cfg {:protocol/version "arxana.v1" :message/id id
                         :operation op :principal/id principal :body body}))
(defn prepare! [h]
  (let [f5 (f5-demo/prepare! h) cfg (:cfg h)
        p (f6/plan (get-in f5 [:geometry :plan]) (get-in f5 [:presentation :panels])
                   (get-in f5 [:geometry :kernel :attestation :id]))
        writes (f6/materialise! #(service/dispatch cfg %) p)
        aid (get-in p [:argument :id]) diagram (get-in p [:items :diagram :id])
        cube (get-in p [:items :cube-claim :id])
        move (call! cfg :formal-f6/move {:argument/id aid :item/id diagram :zone :margin :order 9
                                         :placed-at "2026-11-08T18:00:00.000Z"}
                    "f6-demo:move" "formal-f6/demo")
        support (call! cfg :formal-f6/re-support {:argument/id aid :supporting-content/id diagram
                                                  :supported-claim/id cube :reason "show cube fibre instead"
                                                  :changed-at "2026-11-08T18:00:01.000Z"}
                       "f6-demo:support" "joe")]
    (when-not (and (every? :ok writes) (:ok move) (:ok support))
      (throw (ex-info "F6 fixture failed" {:writes writes :move move :support support})))
    {:argument/id aid :f5 f5 :plan p}))
(defn start! ([] (start! default-port)) ([port]
  (let [h (base/start! port)]
    (try (assoc h :f6/prepared (prepare! h))
         (catch Throwable t (base/stop! h) (throw t))))))
(def stop! base/stop!)
(defn -main [& [port-str ready-path]]
  (let [port (if port-str (Long/parseLong port-str) default-port) h (start! port)
        stopped? (atom false) stop #(when (compare-and-set! stopped? false true) (stop! h))
        aid (get-in h [:f6/prepared :argument/id])]
    (.addShutdownHook (Runtime/getRuntime) (Thread. stop "formal-f6-demo-shutdown"))
    (when ready-path (spit ready-path (pr-str {:status :ready :pid (.pid (ProcessHandle/current))
                                               :port port :argument/id aid})))
    (println (pr-str {:status :ready :port port :argument/id aid})) (flush) @(promise)))
