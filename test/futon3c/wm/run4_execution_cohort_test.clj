(ns futon3c.wm.run4-execution-cohort-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.wm.run4-execution-cohort :as sut]))

(defn- delete-tree! [root]
  (doseq [f (reverse (file-seq root))] (io/delete-file f true)))

(deftest validates-fresh-capacity-and-refuses-drift-or-exhaustion
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "run4-cohort" (make-array java.nio.file.attribute.FileAttribute 0)))
        prereg (io/file root "cohort.edn")]
    (try
      (spit prereg "{:cohort/id :run4-successor :target 3}\n")
      (let [cohort {:preregistration (.getCanonicalPath prereg)
                    :data-root (.getCanonicalPath root)
                    :cohort-id :run4-successor
                    :sha256 (digest/sha256 (slurp prereg))}]
        (is (= cohort (sut/validate-and-preflight!
                       cohort (fn [_]
                                {:snapshot {:value {:cohort/id :run4-successor}}
                                 :remaining 3}))))
        (is (= :cohort-unavailable-or-exhausted
               (:reason (try
                          (sut/validate-and-preflight!
                           cohort (fn [_] {:snapshot
                                           {:value {:cohort/id :run4-successor}}
                                             :remaining 0}))
                          nil (catch clojure.lang.ExceptionInfo e (ex-data e))))))
        (spit prereg "{:cohort/id :changed}\n")
        (is (= :cohort-preregistration-drift
               (:reason (try
                          (sut/validate-and-preflight!
                           cohort (constantly {:snapshot
                                               {:value {:cohort/id :run4-successor}}
                                               :remaining 1}))
                          nil (catch clojure.lang.ExceptionInfo e (ex-data e)))))))
      (finally (delete-tree! root)))))
