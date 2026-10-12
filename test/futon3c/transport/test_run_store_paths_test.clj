(ns futon3c.transport.test-run-store-paths-test
  "Under the :test aliases, stores whose default is the live Agency's state
   resolve elsewhere (2026-10-12: a suite overwrote the live ledger snapshot)."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agency.turn-queue :as turn-queue]
            [futon3c.transport.http :as http]))

(deftest default-store-paths-are-not-live-under-the-test-runner
  (is (= "true" (System/getProperty "futon3c.test-run"))
      "the :test alias must set -Dfuton3c.test-run=true")
  (when-not (System/getenv "FUTON3C_INVOKE_JOBS_FILE")
    (is (not= "/tmp/futon3c-invoke-jobs.edn" (#'http/invoke-jobs-store-path))))
  (when-not (System/getenv "FUTON3C_DURABLE_QUEUE_PATH")
    (is (not= "/tmp/futon3c-durable-turn-queue.edn" (turn-queue/queue-store-path)))))
