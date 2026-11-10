(ns futon3c.arxana.workplan-demo
  (:require [futon3c.arxana.formal-f1-demo :as base]
            [futon3c.arxana.lexicon-views :as lexicon-views]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.workplan :as workplan]
            [futon3c.arxana.workplan-backlinks :as backlinks])
  (:import [java.lang ProcessHandle]))

(def default-port 7201)
(def default-data-dir (str (System/getProperty "user.home") "/.local/share/futon4a/workplan-demo"))
(defn- existing-load [cfg slice]
  (let [plan (workplan/plan slice) catalog (workplan/catalog-record slice plan)
        response (service/dispatch
                  cfg {:protocol/version "arxana.v1" :message/id "workplan-demo:catalog-probe"
                       :operation :entity/get :principal/id "workplan/demo"
                       :body {:entity/id (:id catalog)}})]
    (when (:ok response)
      (let [rows (:records plan)]
        (assoc plan :catalog catalog :catalog/response response :results [] :failures [] :created 0
               :counts {:records (count rows) :clusions (count (filter :type-revision rows))
                        :attestations (count (filter #(and (:subject-clusion-id %) (nil? (:verdict %))) rows))
                        :assessments (count (filter :verdict rows))
                        :items (count (filter #(= [:workplan/item 1] (:type-revision %)) rows))}
               :elapsed-ms 0.0 :load/status :already-present)))))

(defn start!
  ([port] (start! port default-data-dir))
  ([port data-dir]
  (let [h (base/start! port {:data-dir data-dir :prepare? false})
        slice (workplan/read-slice)
        result (or (existing-load (:cfg h) slice)
                   (workplan/materialise! #(service/dispatch (:cfg h) %) slice))
        ;; Rules are content-addressed and may gain new seeds independently of
        ;; the item catalog; their materialiser is idempotent and bounded.
        _ (backlinks/materialise-rules! #(service/dispatch (:cfg h) %) (backlinks/read-rules))
        items (mapv :item/id (get-in result [:catalog :asserted-content :items]))]
    (when (seq (:failures result))
      (base/stop! h) (throw (ex-info "workplan load failures" {:failures (:failures result)})))
    (assoc h :workplan/result result :workplan/items items))))
(def stop! base/stop!)
(defn -main [& [port-str ready-path data-dir]]
  (let [port (Long/parseLong (or port-str (str default-port)))
        data-dir (or data-dir default-data-dir) h (start! port data-dir)
        stopped? (atom false) stop #(when (compare-and-set! stopped? false true) (stop! h))
        ready {:status :ready :pid (.pid (ProcessHandle/current)) :port port
               :item/count (count (:workplan/items h))
               :data/dir data-dir
               :load/created (get-in h [:workplan/result :created])
               :load/status (or (get-in h [:workplan/result :load/status]) :materialised)
               :load/counts (get-in h [:workplan/result :counts])
               :load/elapsed-ms (get-in h [:workplan/result :elapsed-ms])}]
    (.addShutdownHook (Runtime/getRuntime) (Thread. stop "workplan-demo-shutdown"))
    (when ready-path (spit ready-path (pr-str ready)))
    ;; Build the lexicon review table in the background, so the first
    ;; stepper frame after a restart finds it ready or nearly so.
    (lexicon-views/start-table-build!
     (fn [op body]
       (service/dispatch (:cfg h) {:protocol/version "arxana.v1"
                                   :message/id (str "workplan-demo:lexicon-table:" (hash [op body]))
                                   :operation op :principal/id "lexicon/projector" :body body})))
    (println (pr-str ready)) (flush) @(promise)))
