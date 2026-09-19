(ns futon3c.test-registry.validation-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.test-registry :as registry]
            [futon3c.test-registry.validation :as validation])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.time Instant]))

(defn- temp-dir []
  (.toFile (Files/createTempDirectory "validation-service-" (make-array FileAttribute 0))))

(defn- options [dir backend]
  {:backend backend
   :index-file (str (io/file dir "subjects.ednlog"))
   :queue-file (str (io/file dir "queue.ednlog"))})

(deftest append-only-binding-and-typed-absence
  (let [dir (temp-dir) opts (options dir (atom {:entries {} :order []}))]
    (try
      (is (nil? (validation/subject-binding opts "never-checked")))
      (validation/bind-subject! opts "component/a" "warrant-1" "operator" "2026-09-19T00:00:00Z")
      (validation/bind-subject! opts "component/a" "warrant-2" "operator" "2026-09-19T00:01:00Z")
      (is (= "warrant-2" (:warrant-id (validation/subject-binding opts "component/a"))))
      (is (= 2 (count (line-seq (io/reader (:index-file opts))))))
      (finally (doseq [f (reverse (file-seq dir))] (io/delete-file f))))))

(deftest real-warrant-drift-incident-and-fresh-close
  (let [dir (temp-dir)
        backend (atom {:entries {} :order []})
        opts (options dir backend)
        root (.getCanonicalPath (io/file "."))
        source "src/futon3c/test_registry/validation.clj"
        test-file "test/futon3c/test_registry/validation_fixture_test.clj"
        run-opts {:repo-root root :code-paths [source] :test-paths [test-file]
                  :command ["clojure" "-M:test" "-n" "futon3c.test-registry.validation-fixture-test"]
                  :author "validation-test" :artifact-dir (str (io/file dir "artifacts"))
                  :ledger-root (str (io/file dir "ledger"))}
        original (slurp source)]
    (try
      (let [first-run (registry/register-run! backend run-opts)
            first-id (:evidence/id first-run)
            incident-at (str (.plusNanos (Instant/parse (get-in first-run [:payload :finished-at])) 1))]
        (validation/bind-subject! opts "generic/component" first-id "operator" "2026-09-19T00:00:00Z")
        (is (= :current (:verdict (first (validation/conformance opts)))))
        (spit source (str original "\n"))
        (let [stale (first (validation/conformance opts))]
          (is (= :stale (:verdict stale)))
          (is (= [source] (:closure-diff stale))))
        (spit source original)
        (let [opened (validation/enqueue-revalidation!
                      opts {:subject-id "generic/component"
                            :incident {:kind :error :at incident-at
                                       :source "test" :detail "observed failure"}})]
          (is (= :revalidation-open (:verdict (first (validation/conformance opts)))))
          (testing "old warrant cannot close an incident"
            (is (= :warrant-not-fresh
                   (try (validation/close-revalidation!
                         opts (:entry/id opened) first-id "reviewer" "2026-09-19T00:00:02Z")
                        (catch Exception e (:reason (ex-data e)))))))
          (let [fresh (registry/register-run! backend run-opts)
                fresh-id (:evidence/id fresh)]
            (validation/bind-subject! opts "generic/component" fresh-id "reviewer"
                                      "2026-09-19T00:00:03Z")
            (validation/close-revalidation! opts (:entry/id opened) fresh-id "reviewer"
                                            "2026-09-19T00:00:04Z")
            (is (= :current (:verdict (first (validation/conformance opts)))))
            (let [printed (with-out-str (validation/report! opts))]
              (is (re-find #"generic/component current test-registry-" printed))
              (is (re-find #"SUMMARY \{:current 1\}" printed))))))
      (finally
        (spit source original)
        (doseq [f (reverse (file-seq dir))] (io/delete-file f))))))
