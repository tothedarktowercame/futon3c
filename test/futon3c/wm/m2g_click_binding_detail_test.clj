(ns futon3c.wm.m2g-click-binding-detail-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.wm.runner-service :as service]))

(defn- temp-dir [root child]
  (let [dir (io/file root child)]
    (.mkdirs dir)
    (.getPath dir)))

(deftest click-binding-retains-coordinator-port-detail
  (let [root (.toFile
              (java.nio.file.Files/createTempDirectory
               "m2g-click-binding"
               (make-array java.nio.file.attribute.FileAttribute 0)))]
    (binding [service/*click-run-binding-dir* (temp-dir root "bindings")
              service/*run-record-dir* (temp-dir root "runs")
              service/*run4-terminal-projection-dir* (temp-dir root "terminal")
              service/*run4-historical-projection-dir* (temp-dir root "historical")]
      (let [written (#'service/persist-click-run-binding!
                     "click-m2g"
                     {:status :refused
                      :reason :coordinator/port-threw
                      :phase :enact
                      :detail "boom-detail"})
            record (edn/read-string (slurp (:path written)))]
        (is (= "boom-detail" (get-in record [:runtime-disposition :detail])))))))
