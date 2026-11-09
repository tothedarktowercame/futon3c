(ns futon3c.arxana.formal-f1-demo
  "Private, self-contained Formal Arxana F1 demonstration stack."
  (:require [futon1b-arxana :as f1]
            [futon1b-gates :as gates]
            [futon1b-server :as f1-server]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.formal-f1 :as f1a]
            [futon3c.arxana.formal-f1-export :as export]
            [futon3c.arxana.formal-f1-relocation :as relocation]
            [futon3c.arxana.http :as http]
            [futon3c.arxana.json :as ajson]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.store-http :as store-http]
            [xtdb.api :as xt]
            [xtdb.node :as xtn])
  (:import [java.io File]
           [java.lang ProcessHandle]))

(def default-port 7085)

(defn- await-log-replay!
  "Wait until a durable XTDB node has processed every locally submitted log
  message. This is required before rebuilding a derived sidecar."
  [node]
  (let [deadline (+ (System/nanoTime) (* 60 1000000000))]
    (loop []
      (let [status (xt/status node)]
        (cond
          (= (:latest-submitted-msg-ids status) (:latest-processed-msg-ids status)) status
          (< (System/nanoTime) deadline) (do (Thread/sleep 25) (recur))
          :else (throw (ex-info "Timed out waiting for durable XTDB log replay"
                                {:submitted (:latest-submitted-msg-ids status)
                                 :processed (:latest-processed-msg-ids status)})))))))

(defn request [mid op body]
  {:protocol/version "arxana.v1" :message/id mid :operation op
   :principal/id "formal-f1-demo" :body body})

(defn project! [cfg manifest-rid op limit]
  (let [body (cond-> {:manifest/revision-id manifest-rid}
               limit (assoc :query/limit limit))
        response (service/dispatch cfg (request (str "demo:" (name op) ":" (or limit "all"))
                                                op body))]
    (or (:ok response)
        (throw (ex-info "Formal F1 projection failed" {:response response})))))

(defn- prepare! [cfg provider]
  (let [plans (f1a/add-operator-judgement (f1a/plan-fixtures))
        materialized (f1a/materialize! cfg plans)
        exported (export/export plans)
        ingested (export/ingest! cfg exported)
        entries (vec (mapcat :interpretations (get-in exported [:manifest :turns])))
        [unique ambiguous missing] (map :clusion/id (take 3 entries))
        registry (relocation/register-strategies exported)
        edited (relocation/edit-successor exported unique ambiguous missing)
        successor (relocation/ingest-successor! cfg exported edited)
        successor-rid (get-in successor [:ok :artifact/revision-id])
        relocated (relocation/relocate provider registry
                                       (:markdown/revision-id exported)
                                       successor-rid successor-rid successor-rid)
        relocation-writes (relocation/append-relocations! cfg relocated)]
    (when-not (and (every? #(and (:ok (:ingest %)) (every? :ok (:writes %))) materialized)
                   (:ok (:markdown ingested)) (:ok (:manifest ingested))
                   (:ok successor) (every? :ok relocation-writes))
      (throw (ex-info "Formal F1 fixture preparation failed"
                      {:materialized materialized :ingested ingested
                       :successor successor :relocations relocation-writes})))
    {:plans plans :exported exported :successor/revision-id successor-rid
     :manifest/revision-id (:manifest/revision-id exported)}))

(defn start!
  "Start an isolated store and the public Arxana surface. Caller owns the
  returned handle and must invoke `stop!`. Port occupancy is refused before
  any server is started."
  ([] (start! default-port))
  ([port] (start! port {}))
  ([port {:keys [data-dir prepare?] :or {prepare? true}}]
   (http/preflight-ports! "127.0.0.1" [["formal-f1-arxana" port]])
   (gates/seed-mission-contract!)
   (let [durable? (some? data-dir)
         root (when durable? (doto (File. ^String data-dir) .mkdirs))
         index-file (if durable?
                      (File. ^File root "arxana-index.sqlite")
                      (File/createTempFile "formal-f1-demo-index-" ".sqlite"))
         _ (when-not durable? (.delete index-file))
         node (if durable?
                (xtn/start-node {:log [:local {:path (.getPath (File. ^File root "log"))}]
                                 :storage [:local {:path (.getPath (File. ^File root "storage"))}]})
                (xtn/start-node))
         ;; A durable node replays its local log asynchronously. The derived
         ;; sidecar must not scan until that replay has reached quiescence.
         _ (when durable? (await-log-replay! node))
         store-server* (atom nil)
         arxana-server* (atom nil)]
     (try
       (let [store-server (f1-server/start-server!
                           {:node node :port 0 :bind-host "127.0.0.1"
                            :arxana-index-path (.getPath index-file)})
             _ (reset! store-server* store-server)
             store-port (.getPort (.getAddress store-server))
             provider (artifact/in-memory-provider)
             cfg {:graph-store (store-http/http-store
                                (str "http://127.0.0.1:" store-port)
                                f1/service-principal)
                  :artifact-provider provider}
             prepared (when prepare? (prepare! cfg provider))
             arxana-server (http/start! {:port port :service-cfg cfg})
             _ (reset! arxana-server* arxana-server)]
         {:port port :store/port store-port :node node :store-server store-server
          :arxana-server arxana-server :index-file index-file :data-dir data-dir
          :durable? durable? :cfg cfg
          :prepared prepared})
       (catch Throwable t
         (when-let [server @arxana-server*] (server))
         (when-let [server @store-server*] (f1-server/stop-server! server))
         (.close node)
         (when-not durable? (.delete index-file))
         (throw t))))))

(defn stop! [{:keys [arxana-server store-server node index-file durable?]}]
  (when arxana-server (arxana-server))
  (when store-server (f1-server/stop-server! store-server))
  (when node (.close node))
  (when (and index-file (not durable?)) (.delete ^File index-file))
  true)

(defn recording
  "Return deterministic public responses used by the Emacs fixture tests."
  [{:keys [cfg prepared]}]
  (let [rid (:manifest/revision-id prepared)]
    {:recording/version 1
     :manifest/revision-id rid
     :linear (project! cfg rid :formal-f1/linear nil)
     :outline (project! cfg rid :formal-f1/outline nil)
     ;; Limit 2 deliberately witnesses typed truncation in the client.
     :gcc (project! cfg rid :formal-f1/gcc 2)}))

(defn write-recording! [handle path]
  (spit path (str (ajson/encode (recording handle)) "\n"))
  path)

(defn- write-ready! [path handle]
  (when path
    (spit path (str (pr-str {:status :ready :pid (.pid (ProcessHandle/current))
                             :port (:port handle)
                             :manifest/revision-id
                             (get-in handle [:prepared :manifest/revision-id])}) "\n"))))

(defn -main [& [port-str ready-path]]
  (let [port (if port-str (Long/parseLong port-str) default-port)
        handle (start! port)
        stopped? (atom false)
        shutdown (fn [] (when (compare-and-set! stopped? false true) (stop! handle)))]
    (.addShutdownHook (Runtime/getRuntime) (Thread. shutdown "formal-f1-demo-shutdown"))
    (write-ready! ready-path handle)
    (println (pr-str {:status :ready :port port
                      :manifest/revision-id
                      (get-in handle [:prepared :manifest/revision-id])}))
    (flush)
    @(promise)))
