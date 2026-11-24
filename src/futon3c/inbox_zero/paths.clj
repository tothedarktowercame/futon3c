(ns futon3c.inbox-zero.paths
  "Mutable inbox-zero paths under the serving process's storage authority."
  (:require [clojure.java.io :as io]))

(def storage-root
  (or (System/getenv "FUTON_STORAGE_ROOT")
      (str (io/file (System/getProperty "user.home") "code" "storage"))))

(defn path [& parts]
  (str (apply io/file storage-root "inbox-zero" parts)))

(def state-path (path "state.edn"))
