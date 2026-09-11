(require '[clojure.java.io :as io] '[clojure.edn :as edn]
         '[futon2.aif.full-loop-cohort :as cohort]
         '[futon3c.wm.run4-terminal-evidence-test :as fixture])
(let [root (.toFile (java.nio.file.Files/createTempDirectory
                     "closed-execution-review" (make-array java.nio.file.attribute.FileAttribute 0)))]
  (try
    (let [binding (#'fixture/closed-cohort! root :claimed-cohort "attempt-001" :grounded-change)
          close (io/file (:data-root binding) "claimed-cohort/attempt-001/002-closed.edn")
          value (edn/read-string (slurp close))]
      ;; Existing positive fixture has only time-step and closed, no required middle checkpoints.
      ;; Also make the close explicitly foreign while retaining the earlier time-step.
      (spit close (pr-str (assoc value :cohort/id :foreign-cohort :attempt/id "attempt-999")))
      (let [result (try (cohort/closed-execution binding "attempt-001")
                        (catch clojure.lang.ExceptionInfo e {:refused? true :reason (ex-data e)}))]
        (prn {:actual-result result :planted-close-cohort :foreign-cohort
              :planted-close-attempt "attempt-999" :required-middle-checkpoints :absent})
        (assert (:refused? result) "Foreign incomplete close must not authorize execution")))
    (finally (doseq [file (reverse (file-seq root))] (io/delete-file file true)))))
(shutdown-agents)
