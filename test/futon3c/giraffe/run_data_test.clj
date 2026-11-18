(ns futon3c.giraffe.run-data-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [futon3c.giraffe.run-data :as data]
            [futon3c.giraffe.object-migration :as migration]
            [futon3c.giraffe.run :as run]))

(defn- temp-root []
  (.toFile (java.nio.file.Files/createTempDirectory "giraffe-objects-test-"
             (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- legacy! [root text]
  (let [sha (data/sha256-bytes (.getBytes ^String text "UTF-8"))
        f (data/object-file root sha)]
    (io/make-parents f) (spit f text) [sha f]))

(deftest packing-roundtrip-and-idempotence
  (let [root (temp-root) [sha f] (legacy! root "{:invented 1}\n")]
    (is (= :dry-run (:status (migration/migrate! root {}))))
    (is (not (.exists (data/object-db root))))
    (is (= 1 (:deleted (migration/migrate! root {:write? true}))))
    (is (not (.exists f)))
    (is (= "{:invented 1}\n" (String. ^bytes (data/object-bytes root sha) "UTF-8")))
    (is (= 0 (:deleted (migration/migrate! root {:write? true}))))
    (with-open [c (data/connect-objects root true)]
      (dotimes [_ 2] (data/put-object-bytes! c sha (.getBytes "{:invented 1}\n" "UTF-8")))
      (with-open [s (.createStatement c) r (.executeQuery s "SELECT count(*) FROM objects")]
        (.next r) (is (= 1 (.getLong r 1)))))))

(deftest bad-source-refuses-before-write
  (let [root (temp-root) [_ f] (legacy! root "{:invented 1}\n")]
    (spit f "bad")
    (doseq [write? [false true]]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"digest mismatch"
            (migration/migrate! root {:write? write?}))))
    (is (.exists f))
    (is (not (.exists (data/object-db root))))))

(deftest bad-existing-row-and-bad-run-retain-files
  (let [root (temp-root)
        _ (with-open [c (data/connect-objects root true)] (.isClosed c))
        [sha f] (legacy! root "{:invented 1}\n")]
    (with-open [c (data/connect-objects root true)]
      (data/put-object-bytes! c sha (.getBytes "corrupt" "UTF-8")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"copy verification"
          (migration/migrate! root {:write? true})))
    (is (.exists f)))
  (let [root (temp-root) [_ f] (legacy! root "{:invented 1}\n")
        manifest (io/file root "run" "manifest.edn")]
    (io/make-parents manifest) (spit manifest "{}")
    (spit (io/file root "run" "input.edn") (pr-str {:schema data/index-schema}))
    (with-redefs [run/verify-run (fn [_] {:ok? false})]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Run verification"
            (migration/migrate! root {:write? true}))))
    (is (.exists f))))

(deftest first-writer-must-not-mask-legacy-objects
  (let [root (temp-root) [sha _] (legacy! root "{:invented 1}\n")]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Migrate legacy"
          (data/make-indexes! root {:items []} {})))
    (is (not (.exists (data/object-db root))))
    (is (some? (data/object-bytes root sha)))))

(deftest independent-connections-insert-the-same-object
  (let [root (temp-root) bytes (.getBytes "{:invented 2}\n" "UTF-8")
        sha (data/sha256-bytes bytes)
        jobs (doall (for [_ (range 4)]
                      (future (with-open [c (data/connect-objects root true)]
                                (dotimes [_ 10] (data/put-object-bytes! c sha bytes))))))]
    (doseq [job jobs] @job)
    (is (= (seq bytes) (seq (data/object-bytes root sha))))
    (with-open [c (data/connect-objects root false)
                s (.createStatement c) r (.executeQuery s "SELECT count(*) FROM objects")]
      (.next r) (is (= 1 (.getLong r 1))))))
