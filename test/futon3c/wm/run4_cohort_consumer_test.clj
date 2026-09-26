(ns futon3c.wm.run4-cohort-consumer-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon2.aif.c-fold-config :as digest]
            [futon2.aif.full-loop-cohort :as cohort]
            [futon3c.wm.run4-execution-cohort :as preparation]))

(deftest actual-cohort-consumer-refuses-before-and-after-capacity
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "run4-real-cohort" (make-array java.nio.file.attribute.FileAttribute 0)))
        prereg (io/file root "cohort.edn")
        proposal (edn/read-string
                  (slurp "/home/joe/code/futon2/holes/labs/wm-contract/runs/RUN4-U88-cohort-2026-09-11/cohort.edn"))]
    (try
      (spit prereg (pr-str proposal))
      (let [binding {:preregistration (.getCanonicalPath prereg)
                     :data-root (.getCanonicalPath root)
                     :cohort-id (:cohort/id proposal)
                     :sha256 (digest/sha256 (slurp prereg))}
            before (set (map #(.getPath %) (file-seq root)))]
        (is (thrown? clojure.lang.ExceptionInfo
                     (preparation/validate-and-preflight! binding cohort/execution-preflight)))
        (is (= before (set (map #(.getPath %) (file-seq root)))))
        (cohort/activate! (.getPath prereg) (.getPath root))
        (is (= binding (preparation/validate-and-preflight! binding cohort/execution-preflight)))
        (let [cell {:judgment {:opportunity-id "isolated/run4" :trigger :duree-click-on-demand
                              :machine-state {} :agent-roster [] :semantic-epoch :run4-test
                              :code-state {:git-sha "test" :git-dirty? false
                                           :resolved-mode-flags {} :configuration-digest "test"}}
                    :ground {:kind :test}}
              event (cohort/start-attempt! (.getPath prereg) (.getPath root) cell)]
          (is (= (:cohort-id binding) (:cohort/id event)))
          (is (thrown? clojure.lang.ExceptionInfo
                       (preparation/validate-and-preflight! binding cohort/execution-preflight)))
          (is (= 1 (:attempt-count (cohort/ledger (.getPath prereg) (.getPath root)))))))
      (finally
        (doseq [f (reverse (file-seq root))] (io/delete-file f true))))))
