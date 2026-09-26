(require '[clojure.edn :as edn] '[clojure.java.io :as io] '[futon2.aif.full-loop-cohort :as c] '[futon2.aif.repair-obligation :as r])
(let [p "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-repair-successor-v2-selection-admission-2026-09-11/authority/holes/labs/wm-contract/runs/RUN4-repair-successor-v2-selection-admission-2026-09-11/cohort.edn"
      raw (slurp p) root (.toFile (java.nio.file.Files/createTempDirectory "identity-length-review" (make-array java.nio.file.attribute.FileAttribute 0)))
      b {:preregistration p :data-root (.getCanonicalPath root) :cohort-id (:cohort/id (edn/read-string raw)) :sha256 (#'c/sha256 raw)}]
 (try
  (c/activate! p (:data-root b))
  (let [identity (c/execution-identity (c/execution-authority b) "attempt-001")]
   (assert (#'r/execution-identity? identity))
   (prn {:cohort-id (:cohort-id b) :identity-length (count (:id identity)) :store-accepts-identity? (#'r/execution-identity? identity)}))
  (finally (doseq [f (reverse (file-seq root))] (io/delete-file f true)))))
