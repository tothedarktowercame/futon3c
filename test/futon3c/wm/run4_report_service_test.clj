(ns futon3c.wm.run4-report-service-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.wm.run4-report-service :as sut]))

(deftest missing-durable-evidence-is-incomplete-not-success
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "run4-report" (make-array java.nio.file.attribute.FileAttribute 0)))
        manifest {:series-id "s" :trials []}
        mf (java.io.File. root "series.edn")]
    (try
      (spit mf (pr-str manifest))
      (let [r (sut/report! {:run4 {:acceptance {:control-map-root (.getPath root)
                                                :control-map-ref "missing.edn"}
                                   :series {:manifest-root (.getPath root)
                                            :manifest-ref "series.edn"
                                            :controller-root (.getPath root)
                                            :visibility-root (.getPath root)}}} {})]
        (is (= :incomplete-durable-evidence (:decision r)))
        (is (false? (:accepted? r))))
      (finally (doseq [f (reverse (file-seq root))] (io/delete-file f true))))))
