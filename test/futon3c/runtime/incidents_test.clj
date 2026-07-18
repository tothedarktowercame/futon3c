(ns futon3c.runtime.incidents-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.runtime.incidents :as incidents])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-root []
  (.toFile (Files/createTempDirectory
            "jvm-incidents-test-"
            (make-array FileAttribute 0))))

(defn- delete-tree! [root]
  (doseq [file (reverse (file-seq root))]
    (io/delete-file file true)))

(deftest records-typed-bounded-incidents-newest-first
  (let [root (temp-root)]
    (try
      (with-redefs [incidents/default-root (.getPath root)]
        (let [old (ex-info "old" {:large (vec (range 2000))})
              new (RuntimeException. "new")
              old-record (incidents/record-incident! old "worker-old")]
          (Thread/sleep 2)
          (let [new-record (incidents/record-incident! new "worker-new")
                records (incidents/incidents)]
            (is (= 2 (count records)))
            (is (= (:jvm-incident/id new-record)
                   (:jvm-incident/id (first records))))
            (is (= "clojure.lang.ExceptionInfo" (:ex-class old-record)))
            (is (= "worker-old" (:thread old-record)))
            (is (<= (count (:ex-data old-record)) 2000))
            (is (<= (count (:stack old-record)) 30))
            (is (integer? (get-in old-record [:heap :used-mb])))
            (is (pos-int? (get-in old-record [:runtime :pid])))
            (is (= 1 (:jvm-incident/schema-version old-record)))
            (is (= [(:jvm-incident/id new-record)]
                   (mapv :jvm-incident/id (incidents/incidents 1)))))))
      (finally (delete-tree! root)))))

(deftest rate-limits-identical-failures
  (let [root (temp-root)
        limiter (var-get #'futon3c.runtime.incidents/!last-written)]
    (try
      (reset! limiter {})
      (with-redefs [incidents/default-root (.getPath root)]
        (is (map? (incidents/record-incident!
                   (RuntimeException. "same") "worker-1")))
        (is (nil? (incidents/record-incident!
                   (RuntimeException. "same") "worker-2")))
        (is (= 1 (count (incidents/incidents)))))
      (finally
        (reset! limiter {})
        (delete-tree! root)))))

(deftest default-handler-is-idempotent-records-and-reports
  (let [root (temp-root)
        original (Thread/getDefaultUncaughtExceptionHandler)]
    (try
      (with-redefs [incidents/default-root (.getPath root)]
        (let [handler-1 (incidents/install-default-handler!)
              handler-2 (incidents/install-default-handler!)
              thread (Thread. "incident-test-thread")]
          (is (identical? handler-1 handler-2))
          (.uncaughtException handler-1 thread (RuntimeException. "uncaught"))
          (is (= "incident-test-thread" (:thread (first (incidents/incidents)))))))
      (finally
        (Thread/setDefaultUncaughtExceptionHandler original)
        (delete-tree! root)))))
