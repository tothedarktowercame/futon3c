(ns futon3c.diagramprover.wm-wire
  "Support for the War Machine wire tests: the first-layer check and the
  live-record reads. What a wire test is, and the three statuses, are
  defined once, in futon3c.diagramprover.wm-wire-ledger-test's docstring."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import [java.security MessageDigest]))

(def spike-dir "holes/labs/M-wm-wiring/spike")

(defn typed-absence?
  "An absence typed on the record: {:absent ...}, or the older
  {:status :absent ...} shape some records still carry."
  [v]
  (and (map? v) (or (contains? v :absent) (= :absent (:status v)))))

(defn received?
  "The first layer: the reader's value is present, is not a typed absence,
  and is the writer's value."
  [{:keys [writer reader]}]
  (and (some? reader) (not (typed-absence? reader)) (= writer reader)))

(defn sha256-file [path]
  (let [bytes (java.nio.file.Files/readAllBytes (.toPath (io/file path)))]
    (apply str (map #(format "%02x" (bit-and 0xff %))
                    (.digest (MessageDigest/getInstance "SHA-256") bytes)))))

(defn read-record [path]
  (edn/read-string {:default tagged-literal} (slurp path)))

(defn tmp-dir [prefix]
  (str (java.nio.file.Files/createTempDirectory
        prefix (make-array java.nio.file.attribute.FileAttribute 0))))
