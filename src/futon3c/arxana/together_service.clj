(ns futon3c.arxana.together-service
  "The one live Together hub of the futon3c JVM (M-arxana-together): loopback
  port 7096, logs under ~/storage/arxana-together. Tests use
  futon3c.arxana.together-http/start! directly with their own :dir and never
  this namespace, so a test cannot write to the live logs."
  (:require [clojure.java.io :as io]
            [futon3c.arxana.together-http :as together-http]))

(def default-port 7096)

(defn default-dir []
  (or (System/getenv "FUTON3C_ARXANA_TOGETHER_LOG_DIR")
      (some-> (System/getenv "FUTON_STORAGE_ROOT")
              (io/file "arxana-together") str)
      (str (System/getProperty "user.home") "/storage/arxana-together")))

(defonce ^:private running (atom nil))

(defn health []
  (if-let [h @running]
    {:status :running :port (:port h) :dir (:dir h)
     :documents (count @(:documents h))}
    {:status :stopped}))

(defn start!
  "Start the live hub once; a second call returns the running handle's health."
  ([] (start! {}))
  ([{:keys [port dir] :or {port default-port}}]
   (locking running
     (when-not @running
       (reset! running (together-http/start! {:port port :dir (or dir (default-dir))})))
     (health))))

(defn stop! []
  (locking running
    (when-let [h @running]
      (together-http/stop! h)
      (reset! running nil))
    (health)))
