(ns futon3c.transport.test-run-store-paths-test
  "Under the :test aliases, stores whose default is the live Agency's state
   resolve elsewhere (2026-10-12: a suite overwrote the live ledger snapshot)."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agency.turn-queue :as turn-queue]
            [futon3c.transport.http :as http]))

(deftest default-store-paths-are-not-live-under-the-test-runner
  (is (= "true" (System/getProperty "futon3c.test-run"))
      "the :test alias must set -Dfuton3c.test-run=true")
  ;; Unconditional: an inherited env var (Codex seats carry the live Agency's
  ;; environment) must not route a test run to the live store.
  (doseq [[var path-fn] [["FUTON3C_INVOKE_JOBS_FILE" #'http/invoke-jobs-store-path]
                         ["FUTON3C_DURABLE_QUEUE_PATH" turn-queue/queue-store-path]]]
    (let [p (path-fn)]
      (is (not= "/tmp/futon3c-invoke-jobs.edn" p))
      (is (not= "/tmp/futon3c-durable-turn-queue.edn" p))
      (when-let [inherited (System/getenv var)]
        (is (not= inherited p) (str var " from the environment must not win under test-run"))))))
