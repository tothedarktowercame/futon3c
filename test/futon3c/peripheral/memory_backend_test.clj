(ns futon3c.peripheral.memory-backend-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.clock-store :as clock-store]
            [futon3c.evidence.store :as estore]
            [futon3c.peripheral.memory-backend :as memory-backend]))

(use-fixtures
  :each
  (fn [f]
    (clock-store/reset-store!)
    (estore/reset-store!)
    (f)
    (clock-store/reset-store!)
    (estore/reset-store!)))

(defn- with-temp-code-repo
  [f]
  (let [root (.toFile
              (java.nio.file.Files/createTempDirectory
               (.toPath (io/file "/home/joe/code"))
               "futon3c-memory-backend-test-"
               (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try
      (f root)
      (finally
        (doseq [file (reverse (file-seq root))]
          (.delete ^java.io.File file))))))

(defn- write-doc!
  [root rel]
  (let [file (io/file root rel)]
    (.mkdirs (.getParentFile file))
    (spit file (str "# Test doc\n\n"
                    "**Status:** TEST\n\n"
                    "## Checkpoint\n\n"
                    "known checkpoint\n"))
    (.getCanonicalPath file)))

(deftest mission-context-defaults-from-excursion-clock
  (testing "an excursion-only clock is a valid default target"
    (with-temp-code-repo
      (fn [root]
        (write-doc! root "holes/excursions/E-clocked.md")
        (clock-store/set-dispatch-mission! "zai-test" "sid" "E-clocked")
        (let [resp (memory-backend/mission-context
                    {:agent-id "zai-test" :session-id "sid" :cwd (.getPath root)}
                    {:limit 5})]
          (is (true? (:ok resp)))
          (is (= {:store :mission :target "E-clocked"}
                 (get-in resp [:result :query])))
          (is (some #(str/ends-with? (str (:file %)) "E-clocked.md")
                    (get-in resp [:result :items]))))))))

(deftest mission-context-preserves-explicit-excursion-target
  (testing "explicit E-* targets are not rewritten to M-E-*"
    (with-temp-code-repo
      (fn [root]
        (write-doc! root "holes/excursions/E-explicit.md")
        (let [resp (memory-backend/mission-context
                    {:agent-id "zai-test" :session-id "sid" :cwd (.getPath root)}
                    {:target "E-explicit" :limit 5})]
          (is (true? (:ok resp)))
          (is (= "E-explicit" (get-in resp [:result :query :target])))
          (is (not= "M-E-explicit" (get-in resp [:result :query :target])))
          (is (some #(str/ends-with? (str (:file %)) "E-explicit.md")
                    (get-in resp [:result :items]))))))))

(deftest mission-context-bare-target-defaults-to-mission
  (testing "bare target shorthand still resolves as M-*"
    (with-temp-code-repo
      (fn [root]
        (write-doc! root "holes/missions/M-bare.md")
        (let [resp (memory-backend/mission-context
                    {:agent-id "zai-test" :session-id "sid" :cwd (.getPath root)}
                    {:target "bare" :limit 5})]
          (is (true? (:ok resp)))
          (is (= "M-bare" (get-in resp [:result :query :target])))
          (is (some #(str/ends-with? (str (:file %)) "M-bare.md")
                    (get-in resp [:result :items]))))))))
