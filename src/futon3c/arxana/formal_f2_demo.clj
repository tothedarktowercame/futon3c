(ns futon3c.arxana.formal-f2-demo
  "Private F2 collaborative-editing demo over the F1 conversation."
  (:require [futon1b-arxana :as f1]
            [futon1b-gates :as gates]
            [futon1b-server :as f1-server]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.formal-f1 :as material]
            [futon3c.arxana.formal-f1-export :as export]
            [futon3c.arxana.http :as http]
            [futon3c.arxana.store-http :as store-http]
            [xtdb.node :as xtn])
  (:import [java.io File]
           [java.lang ProcessHandle]))

(def default-port 7088)

(defn request [mid op body]
  {:protocol/version "arxana.v1" :message/id mid :operation op
   :principal/id "formal-f2-demo" :body body})

(defn- prepare! [cfg]
  (let [plans (material/add-operator-judgement (material/plan-fixtures))
        writes (material/materialize! cfg plans)
        exported (export/export plans)
        ingested (export/ingest! cfg exported)]
    (when-not (and (every? #(and (:ok (:ingest %)) (every? :ok (:writes %))) writes)
                   (:ok (:markdown ingested)) (:ok (:manifest ingested)))
      (throw (ex-info "Formal F2 fixture preparation failed"
                      {:writes writes :ingested ingested})))
    {:artifact/id "formal-arxana:f1-conversation"
     :head/revision-id (:markdown/revision-id exported)
     :manifest/revision-id (:manifest/revision-id exported)}))

(defn start! ([] (start! default-port)) ([port]
  (http/preflight-ports! "127.0.0.1" [["formal-f2-arxana" port]])
  (gates/seed-mission-contract!)
  (let [index-file (File/createTempFile "formal-f2-demo-index-" ".sqlite")
        _ (.delete index-file) node (xtn/start-node)
        store* (atom nil) api* (atom nil)]
    (try
      (let [store-server (f1-server/start-server!
                          {:node node :port 0 :bind-host "127.0.0.1"
                           :arxana-index-path (.getPath index-file)})
            _ (reset! store* store-server)
            store-port (.getPort (.getAddress store-server))
            provider (artifact/in-memory-provider)
            cfg {:graph-store (store-http/http-store
                               (str "http://127.0.0.1:" store-port)
                               f1/service-principal)
                 :artifact-provider provider}
            prepared (prepare! cfg)
            api (http/start! {:port port :service-cfg cfg})]
        (reset! api* api)
        {:port port :node node :store-server store-server :arxana-server api
         :index-file index-file :cfg cfg :prepared prepared})
      (catch Throwable t
        (when-let [api @api*] (api))
        (when-let [server @store*] (f1-server/stop-server! server))
        (.close node) (.delete index-file) (throw t))))))

(defn stop! [{:keys [arxana-server store-server node index-file]}]
  (when arxana-server (arxana-server))
  (when store-server (f1-server/stop-server! store-server))
  (when node (.close node))
  (when index-file (.delete ^File index-file))
  true)

(defn- write-ready! [path handle]
  (when path
    (spit path (str (pr-str (merge {:status :ready
                                    :pid (.pid (ProcessHandle/current))
                                    :port (:port handle)}
                                   (:prepared handle))) "\n"))))

(defn -main [& [port-str ready-path]]
  (let [port (if port-str (Long/parseLong port-str) default-port)
        handle (start! port) stopped? (atom false)
        shutdown #(when (compare-and-set! stopped? false true) (stop! handle))]
    (.addShutdownHook (Runtime/getRuntime) (Thread. shutdown "formal-f2-demo-shutdown"))
    (write-ready! ready-path handle)
    (println (pr-str (merge {:status :ready :port port} (:prepared handle))))
    (flush) @(promise)))
