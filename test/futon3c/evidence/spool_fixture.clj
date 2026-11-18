(ns futon3c.evidence.spool-fixture
  "Per-test worker lifetime: a previous test must not replay through the next
  test's with-redefs HTTP stub. The test-run default separately protects live data."
  (:require [futon3c.evidence.spool :as spool]))

(defn isolated [f]
  (let [dir (java.nio.file.Files/createTempDirectory
             "futon3c-spool-fixture-" (make-array java.nio.file.attribute.FileAttribute 0))
        path (str dir "/spool.db")]
    (binding [spool/*db-path* path]
      (try (f) (finally (spool/stop! path))))))
