(ns futon3c.arxana.formal-f3-demo
  "Private Formal Arxana F3 demonstration on loopback 7086."
  (:require [futon3c.arxana.formal-f1-demo :as base]
            [futon3c.arxana.formal-f3 :as f3]
            [futon3c.arxana.formal-f3-correspond :as f3b]
            [futon3c.arxana.formal-f3-generate :as f3c]
            [futon3c.arxana.json :as ajson]
            [futon3c.arxana.service :as service])
  (:import [java.lang ProcessHandle]))

(def default-port 7086)
(def alignment-path "/home/joe/code/storage/apm/mining/alignment/r2-judged/a93J02.judged.json")
(defn- req [id op body] {:protocol/version "arxana.v1" :message/id id :operation op
                         :principal/id "formal-f3-demo" :body body})

(defn prepare! [{:keys [cfg]}]
  (let [informal (f3/pinned-bytes "/home/joe/code/apm-lean" f3/historical-informal-commit f3/informal-path)
        lean (f3/pinned-bytes "/home/joe/code/apm-lean" f3/fixture-commit f3/lean-main-path)
        base-artifacts (f3/ingest-manifestations! cfg
                       [{:artifact/id "formal-f3:a93J02/authored-informal-demo"
                         :role :authored-informal :media-type "text/markdown" :bytes informal}
                        {:artifact/id "formal-f3:a93J02/lean-main-demo"
                         :role :readable-lean :media-type "text/x-lean" :bytes lean}])
        informal-rid (:artifact/revision-id (first base-artifacts))
        lean-rid (:artifact/revision-id (second base-artifacts))
        alignment (f3b/read-alignment alignment-path)
        plan (f3b/plan alignment informal-rid) rows (f3b/materialise! cfg plan)
        c (f3c/materialise! cfg {:lean-revision lean-rid :informal-revision informal-rid
                                 :alignment alignment})
        corr (vec (concat (for [r (:records plan)]
                            {:record/id (get-in r [:claim :id]) :step/id (:step/id r)
                             :class (if (:divergence? r) :divergence :genuine)})
                          [{:record/id (get-in c [:generated-claim :id])
                            :step/id "generated" :class :generated}
                           {:record/id (get-in c [:synthetic-claim :id])
                            :step/id "SYNTHETIC" :class :synthetic}]))
        manifestations
        (vec (concat
              (for [x base-artifacts]
                {:artifact/id (:artifact/id x) :artifact/revision-id (:artifact/revision-id x)
                 :role (:role x)
                 :record/ids (vec (cons (get-in (first (:records plan)) [:claim :id])
                                        (map :record/id corr)))})
              (for [x (:ingested c)]
                {:artifact/id (:artifact/id x) :artifact/revision-id (:artifact/revision-id x)
                 :role (if (= (:artifact/id x) f3c/synthetic-artifact-id) :synthetic-negative :generated)
                 :synthetic/label (when (= (:artifact/id x) f3c/synthetic-artifact-id)
                                    :SYNTHETIC-negative)
                 :record/ids (vec (cons (if (= (:artifact/id x) f3c/synthetic-artifact-id)
                                          (get-in c [:mutation :id]) (get-in c [:generation :id]))
                                        (map :record/id corr)))})))
        manifest {:manifest/version 1 :correspondences corr :manifestations manifestations
                  :coverage (mapv :id (get-in c [:residues :unaligned]))}
        mr (service/dispatch cfg (req "f3-demo:manifest" :revision/ingest
                                      {:artifact/id "formal-f3:demo-manifest"
                                       :bytes (pr-str manifest) :media-type "application/edn"
                                       :expected/revision-id :absent}))]
    (when-not (and (every? #(every? :ok (:writes %)) rows) (every? :ok (:writes c)) (:ok mr))
      (throw (ex-info "F3 demo fixture failed" {:rows rows :f3c c :manifest mr})))
    {:manifest/revision-id (get-in mr [:ok :artifact/revision-id])}))

(defn start! ([] (start! default-port)) ([port]
  (let [handle (base/start! port)]
    (try (assoc handle :f3/prepared (prepare! handle))
         (catch Throwable t (base/stop! handle) (throw t))))))
(def stop! base/stop!)
(defn recording [{:keys [cfg] f3-prepared :f3/prepared}]
  (let [rid (:manifest/revision-id f3-prepared)
        p #(get-in (service/dispatch cfg (req (str "f3-demo:" (name %)) %
                                              {:manifest/revision-id rid})) [:ok])]
    {:recording/version 1 :manifest/revision-id rid
     :correspondence (p :formal-f3/correspondence)
     :manifestations (p :formal-f3/manifestations)}))
(defn write-recording! [h path] (spit path (str (ajson/encode (recording h)) "\n")))
(defn -main [& [port-str ready-path]]
  (let [port (if port-str (Long/parseLong port-str) default-port) h (start! port)
        stopped? (atom false) stop #(when (compare-and-set! stopped? false true) (stop! h))]
    (.addShutdownHook (Runtime/getRuntime) (Thread. stop "formal-f3-demo-shutdown"))
    (when ready-path (spit ready-path (pr-str {:status :ready :pid (.pid (ProcessHandle/current))
                                               :port port :manifest/revision-id
                                               (get-in h [:f3/prepared :manifest/revision-id])})))
    (println (pr-str {:status :ready :port port})) (flush) @(promise)))
