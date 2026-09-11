(require '[futon2.aif.full-loop-cohort :as c] '[futon2.aif.full-loop-cohort-test :as t] '[futon2.aif.repair-obligation :as r] '[clojure.edn :as edn] '[clojure.java.io :as io])
(let [root (t/tmp-root) raw (slurp t/prereg-path)
      binding {:preregistration t/prereg-path :data-root root :cohort-id (:cohort/id (edn/read-string raw)) :sha256 (#'c/sha256 raw)}]
 (c/activate! t/prereg-path root)
 (let [a (:attempt/id (t/open! root "review/nil"))]
  (t/append-required! root a)
  (c/close-attempt! t/prereg-path root a (t/term {:outcome :agent-unavailable :grounded? false :artifact-only? false :duration-ms 1 :resource-use {:agent-turns 0}}))
  (let [f (io/file root (name (:cohort-id binding)) a "001-time-step.edn") v (edn/read-string (slurp f))]
   (spit f (pr-str (assoc-in v [:payload :judgment :execution-authority] nil)))
   (let [refusal (try (c/closed-execution binding a) nil
                      (catch clojure.lang.ExceptionInfo e e))]
     (prn {:present-nil-authority-refused? (some? refusal)
           :reason (:reason (ex-data refusal))})))))
(let [root (t/tmp-root) outside (io/file (t/tmp-root) "outside.edn")
      finding {:attempt-id "attempt-001" :repair-class :machine-failure :failure-stage :selection :outcome :incomplete :failure-kind :untyped-failure :error "fixture" :opened-at "2026-09-11T00:00:00Z"}
      record (r/record-system-failure! root finding)
      f (io/file root "findings" (str (:repair/id record) ".edn"))]
 (io/copy f outside) (io/delete-file f)
 (java.nio.file.Files/createSymbolicLink (.toPath f) (.toPath outside) (make-array java.nio.file.attribute.FileAttribute 0))
 (let [refusal (try (r/record-system-failure! root finding) nil
                    (catch clojure.lang.ExceptionInfo e e))]
   (prn {:symlink-replay-refused? (some? refusal)
         :reason (:reason (ex-data refusal))})))
