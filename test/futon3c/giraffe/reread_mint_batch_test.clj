(ns futon3c.giraffe.reread-mint-batch-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.giraffe.reread-mint-batch :as batch])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- state-file []
  (io/file (.toFile (Files/createTempDirectory
                     "reread-mint-batch-" (make-array FileAttribute 0)))
           "state.edn"))

(deftest landings-coalesce-for-window-and-idle-forces-final-mint
  (let [path (state-file) calls (atom 0)
        mint #(do (swap! calls inc) {:status :minted :run-id (str "run-" @calls)})
        opts {:path path :window-ms 1800000 :mint-fn mint}]
    (is (= :minted (:status (batch/step! (assoc opts :now-ms 0 :landed-count 1
                                                 :queue-idle? false)))))
    (is (= :deferred (:status (batch/step! (assoc opts :now-ms 100 :landed-count 1
                                                   :queue-idle? false)))))
    (is (= :deferred (:status (batch/step! (assoc opts :now-ms 200 :landed-count 2
                                                   :queue-idle? false)))))
    (is (= 1 @calls))
    (is (= :minted (:status (batch/step! (assoc opts :now-ms 300 :landed-count 0
                                                 :queue-idle? true)))))
    (is (= 2 @calls))
    (is (= 0 (:pending-landings (batch/read-state path))))))
