(ns futon3c.arxana.workplan-service
  "Bootstrap-owned Arxana surface. Uses the existing substrate; owns no XTDB node."
  (:require [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.http :as http]
            [futon3c.arxana.store-http :as store-http]
            [futon3c.arxana.workplan-backlinks :as backlinks]))

(defonce ^:private running (atom nil))
(defn health [] (if-let [h @running] (select-keys h [:status :port :store-url]) {:status :stopped}))

(defn start!
  "Idempotently start the in-JVM surface; refuse conflicting configuration or an
  occupied port. Does not seed data or start a private store."
  ([] (start! {}))
  ([{:keys [port store-url backlinks-index]
     :or {port 7094 store-url "http://localhost:7073"
          backlinks-index backlinks/default-index}}]
   (locking running
     (if-let [h @running]
       (if (= [port store-url backlinks-index] [(:port h) (:store-url h) (:backlinks-index h)]) h
           (throw (ex-info "Workplan service already running with different configuration" (health))))
       (let [cfg {:graph-store (store-http/http-store store-url "arxana-service")
                  :artifact-provider (artifact/in-memory-provider)
                  :workplan/backlinks-index backlinks-index}
             stop (http/start! {:port port :service-cfg cfg})
             h {:status :running :port port :store-url store-url
                :backlinks-index backlinks-index :cfg cfg :stop stop}]
         (reset! running h)
         h)))))

(defn stop! []
  (locking running
    (when-let [h @running]
      ((:stop h))
      (reset! running nil))
    true))
