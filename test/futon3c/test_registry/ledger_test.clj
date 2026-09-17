(ns futon3c.test-registry.ledger-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.test-registry.ledger :as ledger])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir [prefix]
  (str (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- file-with [dir name content]
  (let [f (io/file dir name)]
    (io/make-parents f)
    (spit f content)
    f))

(deftest an-object-is-named-by-its-own-bytes
  (let [root (temp-dir "ledger-") work (temp-dir "work-")
        sha (ledger/put! root (file-with work "a.log" "hello warrant"))]
    (is (= 64 (count sha)))
    (is (ledger/holds? root sha))
    (is (= "hello warrant" (slurp (ledger/resolve-file root sha))))
    (is (= sha (subs (.getName (ledger/resolve-file root sha)) 0 64)))))

(deftest the-same-bytes-twice-are-one-object
  (let [root (temp-dir "ledger-") work (temp-dir "work-")
        a (ledger/put! root (file-with work "one.log" "same bytes"))
        b (ledger/put! root (file-with work "two.log" "same bytes"))]
    (is (= a b))
    (is (= 1 (count (filter #(.isFile ^java.io.File %)
                            (file-seq (io/file root "objects" (subs a 0 2)))))))))

(deftest different-bytes-never-collide
  (let [root (temp-dir "ledger-") work (temp-dir "work-")]
    (is (not= (ledger/put! root (file-with work "a.log" "one"))
              (ledger/put! root (file-with work "b.log" "two"))))))

(deftest a-stored-object-is-read-only
  (let [root (temp-dir "ledger-") work (temp-dir "work-")
        sha (ledger/put! root (file-with work "a.log" "evidence"))]
    (is (not (.canWrite (ledger/resolve-file root sha))))))

(deftest an-artifact-records-the-path-the-sha-and-the-ledger
  (let [root (temp-dir "ledger-") work (temp-dir "work-")
        art (ledger/artifact root (file-with work "run.log" "results"))]
    (is (= root (:ledger art)))
    (is (ledger/holds? root (:sha256 art)))
    (is (.endsWith ^String (:path art) "run.log"))))

(deftest the-log-survives-deletion-of-the-path-it-was-written-to
  (let [root (temp-dir "ledger-") work (temp-dir "work-")
        source (file-with work "run.log" "the run said this")
        art (ledger/artifact root source)]
    (is (.delete source))
    (is (nil? (ledger/locate {:path (:path art) :sha256 "deadbeef"})))
    (let [found (ledger/locate root art)]
      (is (some? found))
      (is (= "the run said this" (slurp found))))))

(deftest a-pre-ledger-record-still-resolves-by-path
  (let [root (temp-dir "ledger-") work (temp-dir "work-")
        source (file-with work "old.log" "written before the ledger")
        art {:path (.getCanonicalPath source) :sha256 "not-in-this-ledger"}]
    (is (= "written before the ledger" (slurp (ledger/locate root art))))))

(deftest a-record-whose-path-and-ledger-are-both-gone-resolves-to-nothing
  (let [root (temp-dir "ledger-")]
    (is (nil? (ledger/locate root {:path "/nonexistent/gone.log" :sha256 "absent"})))
    (is (nil? (ledger/locate root {:path nil :sha256 nil})))))

(deftest tampering-with-the-copy-at-the-recorded-path-does-not-fool-the-ledger
  (let [root (temp-dir "ledger-") work (temp-dir "work-")
        source (file-with work "run.log" "the run said this")
        art (ledger/artifact root source)]
    (spit source "the run said something else")
    (is (= "the run said this" (slurp (ledger/locate art)))
        "locate prefers the ledger the record names, not the mutable path")))
