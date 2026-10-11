(ns futon3c.agency.session-end-test-fixture
  "Shared isolation for tests that can end registered agent sessions."
  (:require [futon3c.agency.registry :as registry])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn with-isolated-session-end-outbox
  [f]
  (let [dir (Files/createTempDirectory
             "futon3c-session-ends-" (make-array FileAttribute 0))
        path (str (.resolve dir "session-ends.jsonl"))]
    (binding [registry/*session-end-outbox* path]
      (f))))
