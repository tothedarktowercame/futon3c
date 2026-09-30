(ns futon3c.test-registry-sqlite-cli-test
  (:require [futon3c.test-support.git-fixture :as git-fixture]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is]]
            [futon3c.evidence.http-backend :as http-backend]
            [futon3c.test-registry :as registry]
            [futon3c.test-registry.sqlite-backend :as sqlite])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir []
  (.toFile (Files/createTempDirectory "registry-cli-" (make-array FileAttribute 0))))

(defn- cleanup! [dir]
  (doseq [f (reverse (file-seq dir))] (io/delete-file f true)))

(defn- run-command! [dir & args]
  (let [result (apply shell/sh (concat args [:dir (str dir) :env (git-fixture/environment)]))]
    (when-not (zero? (:exit result))
      (throw (ex-info "fixture command failed" {:args args :result result})))
    result))

(defn- write-fixture! [dir expected]
  (let [src (io/file dir "src/demo/core.clj")
        test (io/file dir "test/demo/core_test.clj")]
    (.mkdirs (.getParentFile src))
    (.mkdirs (.getParentFile test))
    (spit (io/file dir "deps.edn")
          (pr-str {:paths ["src"]
                   :aliases {:test {:extra-paths ["test"]
                                    :extra-deps
                                    {'io.github.cognitect-labs/test-runner
                                     {:git/url "https://github.com/cognitect-labs/test-runner.git"
                                      :sha "5e91ee0989d93115cfb5e15b6f242f5ec7ab762e"}}
                                    :main-opts ["-m" "cognitect.test-runner"]}}}))
    (spit src "(ns demo.core)\n(defn answer [] 42)\n")
    (spit test (str "(ns demo.core-test\n  (:require [clojure.test :refer [deftest is]]\n"
                    "            [demo.core :as core]))\n"
                    "(deftest answer-test (is (= " expected " (core/answer))))\n"))))

(defn- commit! [dir message]
  (run-command! dir "git" "add" "--" "deps.edn" "src/demo/core.clj" "test/demo/core_test.clj")
  (run-command! dir "git" "commit" "-q" "-m" message))

(defn- fixture-options [dir db]
  {:registry-db (str db)
   :agency-url "http://127.0.0.1:9"
   :repo-root (str dir)
   :code-paths ["src/demo/core.clj"]
   :test-paths ["test/demo/core_test.clj"]
   :command ["clojure" "-M:test" "-n" "demo.core-test"]
   :author "sqlite-cli-test"
   :artifact-dir (str (io/file dir "artifacts"))
   :ledger-root (str (io/file dir "artifact-ledger"))
   :namespace-ledger-file (str (io/file dir "namespace-ledger.edn"))})

(deftest command-line-operations-use-only-the-local-registry
  (let [dir (temp-dir) db (io/file dir "registry.sqlite")]
    (try
      (write-fixture! dir 42)
      (run-command! dir "git" "init" "-q")
      (run-command! dir "git" "config" "user.email" "sqlite-cli@example.invalid")
      (run-command! dir "git" "config" "user.name" "SQLite CLI Test")
      (commit! dir "passing fixture")
      (let [options (fixture-options dir db)
            first-run (registry/run-cli-operation "run" options)
            id (:evidence/id first-run)
            check-options (assoc options :entry-id id :changed-paths [])]
        (is (true? (get-in first-run [:payload :warrant?])) (pr-str first-run))
        (is (= 2 (count (registry/read-chain!
                         (sqlite/sqlite-backend db) id))))
        (is (.isFile (io/file (:namespace-ledger-file options)))
            "run keeps the transitional namespace ledger current")
        (is (true? (:warrant? (registry/run-cli-operation "check" check-options))))

        (spit (io/file dir "src/demo/core.clj") "(ns demo.core)\n(defn answer [] 43)\n")
        (is (= :stale-sha (:reason (registry/run-cli-operation "check" check-options))))
        (spit (io/file dir "src/demo/core.clj") "(ns demo.core)\n(defn answer [] 42)\n")
        (is (true? (:warrant? (registry/run-cli-operation "check" check-options))))

        (is (= id (:evidence/id
                   (registry/run-cli-operation
                    "latest-for-namespace"
                    (assoc options :namespace "demo.core-test"
                                   :namespace-ledger-file "/must/not/be/read.edn")))))

        (write-fixture! dir 41)
        (commit! dir "failing fixture")
        (let [failed-run (registry/run-cli-operation "run" options)
              latest (registry/run-cli-operation "latest-for-namespace"
                                                 (assoc options :namespace "demo.core-test"))]
          (is (false? (get-in failed-run [:payload :warrant?])))
          (is (= (:evidence/id failed-run) (:evidence/id latest)))
          (is (= 1 (get-in latest [:payload :results :failures])))
          (is (= :local-sqlite (:resolved-by latest))))

        ;; currency: the FAILING record can be shown current (its recorded
        ;; results returned) and stale on drift, while check keeps refusing
        ;; it :not-a-warrant (claude-4 requisition 2026-09-30).
        (let [failed-id (:evidence/id
                         (registry/run-cli-operation
                          "latest-for-namespace"
                          (assoc options :namespace "demo.core-test")))
              copts (assoc options :entry-id failed-id
                           :changed-paths [])
              current (registry/run-cli-operation "currency" copts)]
          (is (true? (:current? current)) (pr-str current))
          (is (= 1 (get-in current [:results :failures])))
          (spit (io/file dir "src/demo/core.clj") "(ns demo.core)\n(defn answer [] 40)\n")
          (is (= :stale-sha (:reason (registry/run-cli-operation "currency" copts))))
          (spit (io/file dir "src/demo/core.clj") "(ns demo.core)\n(defn answer [] 42)\n")
          (is (true? (:current? (registry/run-cli-operation "currency" copts))))
          (is (= :not-a-warrant (:reason (registry/run-cli-operation "check" copts)))))

        (with-redefs [http-backend/make-http-backend
                      (fn [& _] (throw (ex-info "network backend constructed" {})))]
          (let [missing (registry/run-cli-operation
                         "latest-for-namespace"
                         (assoc options :namespace "never.registered"))]
            (is (= :none (:status missing)))
            (is (= :no-local-record (:reason missing)))
            (is (= :local-sqlite (:resolved-by missing)))))

        (let [unopenable (registry/run-cli-operation
                          "latest-for-namespace"
                          {:registry-db (str dir) :namespace "demo.core-test"})]
          (is (= :test-registry/refusal (:record/type unopenable)))
          (is (= :local-store-unavailable (:reason unopenable)))
          (is (= (str dir) (get-in unopenable [:details :path])))))
      (finally (cleanup! dir)))))
