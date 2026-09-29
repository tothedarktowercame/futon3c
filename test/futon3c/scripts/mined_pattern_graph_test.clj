(ns futon3c.scripts.mined-pattern-graph-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jsonista.core :as json]))

(defn- fixture-path [& parts]
  (.getPath (apply io/file "test" "fixtures" "mined-pattern-graph" parts)))

(defn- run-graph [live-dir out-file]
  (shell/sh "python3" "scripts/mined_pattern_graph.py"
            "--batches" (fixture-path "batches")
            "--live" live-dir
            "--library" (fixture-path "library")
            "--out" (.getPath out-file)))

(defn- read-json [file]
  (json/read-value (slurp file) json/keyword-keys-object-mapper))

(deftest live-analysis-connects-elephant-family-to-giant-component
  (let [tmp-dir (.toFile (java.nio.file.Files/createTempDirectory
                          "mined-pattern-graph" (make-array java.nio.file.attribute.FileAttribute 0)))
        empty-live (doto (io/file tmp-dir "empty-live") .mkdir)
        without-file (io/file tmp-dir "without-live.json")
        with-file (io/file tmp-dir "with-live.json")
        without-run (run-graph (.getPath empty-live) without-file)
        with-run (run-graph (fixture-path "live") with-file)
        without (read-json without-file)
        with (read-json with-file)
        live-edge (some #(when (and (= "co-cited" (:kind %))
                                    (= #{"core/c" "象/live"} #{(:a %) (:b %)}))
                           %)
                        (:edges with))]
    (testing "the batch records alone leave the elephant pattern outside the giant component"
      (is (zero? (:exit without-run)) (:err without-run))
      (is (= 2 (:records without)))
      (is (false? (get-in without [:象_family :in_giant_component])))
      (is (not= [1] (get-in without [:象_family :components]))))
    (testing "a co-citation in a live-shaped record joins it to the giant component"
      (is (zero? (:exit with-run)) (:err with-run))
      (is (= 4 (:records with)))
      (is (= [1] (get-in with [:象_family :components])))
      (is (true? (get-in with [:象_family :in_giant_component])))
      (is live-edge)
      (is (some #(str/starts-with? (:at %) "live/")
                (:evidence live-edge))))))
