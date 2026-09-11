(require '[clojure.java.io :as io]
         '[futon2.aif.full-loop-cohort :as cohort]
         '[futon2.aif.repair-obligation :as repair])

(let [tmp (.toFile (java.nio.file.Files/createTempDirectory
                    "qualified-finding-collision"
                    (make-array java.nio.file.attribute.FileAttribute 0)))
      source (io/file "/home/joe/code/futon2/holes/labs/wm-contract/runs/RUN4-U88-cohort-2026-09-11/cohort.edn")
      prereg (io/file tmp "cohort.edn")
      repair-root (io/file tmp "repairs")
      roots [(io/file tmp "root-a") (io/file tmp "root-b")]
      raw (slurp source)]
  (spit prereg raw)
  (doseq [root roots]
    (.mkdir root)
    (cohort/activate! (.getCanonicalPath prereg) (.getCanonicalPath root)))
  (let [events (mapv #(cohort/start-attempt! (.getCanonicalPath prereg)
                                               (.getCanonicalPath %)
                                               {:judgment {:opportunity-id (str "disposable/" (.getName %))
                                                           :trigger :duree-click-on-demand
                                                           :machine-state {:tick 1}
                                                           :agent-roster []
                                                           :code-state {:git-sha "fixture"
                                                                        :git-dirty? false
                                                                        :resolved-mode-flags {}
                                                                        :configuration-digest "fixture"}
                                                           :semantic-epoch :fixture}
                                                :ground {:kind :disposable-witness}})
                     roots)
        external-ids (mapv #(str "run4-u88-20260911-v1--" (:attempt/id %)) events)
        finding (fn [attempt-id]
                  {:attempt-id attempt-id
                   :repair-class :environmental-hold
                   :failure-stage :agent-readiness
                   :outcome :agent-unavailable
                   :failure-kind :agent-readiness-failed
                   :error "Agent readiness observation failed"
                   :backtrace {:fixture :disposable}
                   :discharge-contract {:requires [:cleared-readiness]}})]
    (repair/record-system-failure! (.getPath repair-root) (finding (first external-ids)))
    (let [collision (try
                      (repair/record-system-failure! (.getPath repair-root)
                                                     (finding (second external-ids)))
                      nil
                      (catch Throwable t t))
          init (repair/record-system-failure!
                (.getPath repair-root)
                {:attempt-id "initialization-disposable"
                 :repair-class :machine-failure
                 :failure-stage :initialization
                 :outcome :incomplete
                 :failure-kind :initialization-failed
                 :error (.getMessage collision)
                 :backtrace {:error-class (.getName (class collision))}
                 :discharge-contract {:requires [:distinct-repair-commit]}})]
      (prn {:roots-distinct? (not= (.getCanonicalPath (first roots))
                                   (.getCanonicalPath (second roots)))
            :local-attempts (mapv :attempt/id events)
            :external-ids external-ids
            :collision-class (.getName (class collision))
            :collision-target (.getMessage collision)
            :outer-finding-id (:repair/id init)}))))
