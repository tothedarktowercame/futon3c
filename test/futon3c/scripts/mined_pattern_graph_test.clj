(ns futon3c.scripts.mined-pattern-graph-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [jsonista.core :as json]))

(defn- fixture-path [& parts]
  (.getPath (apply io/file "test" "fixtures" "mined-pattern-graph" parts)))

(defn- run-graph
  ([live-dir out-file]
   (let [empty-diffs (doto (io/file (.getParentFile out-file) "empty-diffs") .mkdir)]
     (run-graph live-dir out-file (.getPath empty-diffs))))
  ([live-dir out-file diffs]
   (shell/sh "python3" "scripts/mined_pattern_graph.py"
             "--batches" (fixture-path "batches")
             "--live" live-dir
             "--library" (fixture-path "library")
             "--diffs" diffs
             "--out" (.getPath out-file))))

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

(deftest applied-diffs-add-only-known-pattern-uses-and-links
  (let [tmp-dir (.toFile (java.nio.file.Files/createTempDirectory
                          "mined-pattern-diffs" (make-array java.nio.file.attribute.FileAttribute 0)))
        no-diffs (doto (io/file tmp-dir "none") .mkdir)
        applied (doto (io/file tmp-dir "applied") .mkdir)
        source (io/file (fixture-path "diffs" "2026-10-05-c9d25d6a.expected-diff.json"))
        _ (io/copy source (io/file applied "run.json"))
        without-file (io/file tmp-dir "without.json")
        with-file (io/file tmp-dir "with.json")
        without-run (run-graph "" without-file (.getPath no-diffs))
        with-run (run-graph "" with-file (.getPath applied))
        without (read-json without-file)
        with (read-json with-file)
        used (filter #(= "used-together" (:kind %)) (:edges with))]
    (is (zero? (:exit without-run)) (:err without-run))
    (is (zero? (:exit with-run)) (:err with-run))
    (is (empty? (filter #(= "used-together" (:kind %)) (:edges without))))
    (is (= 6 (count used)))
    (is (= #{"2026-10-05-c9d25d6a-f2bb-42bf-a162-2c4a000e804f"}
           (set (mapcat #(map :run (:evidence %)) used))))
    (is (= 4 (count (:uses with))))
    (is (some #(= "used-together" (:through %)) (:summary with)))
    (testing "unknown ids and all links touching them are skipped"
      (let [unknown-dir (doto (io/file tmp-dir "unknown") .mkdir)
            diff (read-json source)
            changed (-> diff
                        (assoc-in [:add_uses 0 :pattern] "missing/pattern")
                        (assoc-in [:add_edges 0 :a] "missing/pattern"))
            _ (spit (io/file unknown-dir "unknown.json")
                    (json/write-value-as-string changed))
            output (io/file tmp-dir "unknown-graph.json")
            run (run-graph "" output (.getPath unknown-dir))
            graph (read-json output)]
        (is (zero? (:exit run)) (:err run))
        (is (= 3 (count (:uses graph))))
        (is (= 5 (count (filter #(= "used-together" (:kind %)) (:edges graph)))))))))

(deftest one-run-counts-once-and-a-pattern-is-not-linked-to-itself
  (let [tmp-dir (.toFile (java.nio.file.Files/createTempDirectory
                          "mined-pattern-diffs-dup" (make-array java.nio.file.attribute.FileAttribute 0)))
        source (io/file (fixture-path "diffs" "2026-10-05-c9d25d6a.expected-diff.json"))
        twice (doto (io/file tmp-dir "twice") .mkdir)
        _ (io/copy source (io/file twice "a.json"))
        _ (io/copy source (io/file twice "b.json"))
        twice-out (io/file tmp-dir "twice.json")
        twice-run (run-graph "" twice-out (.getPath twice))
        twice-graph (read-json twice-out)
        self (doto (io/file tmp-dir "self") .mkdir)
        diff (read-json source)
        first-edge (get-in diff [:add_edges 0])
        _ (spit (io/file self "self.json")
                (json/write-value-as-string
                 (assoc diff :add_edges [(assoc first-edge :b (:a first-edge))])))
        self-out (io/file tmp-dir "self.json")
        self-run (run-graph "" self-out (.getPath self))
        self-graph (read-json self-out)]
    (is (zero? (:exit twice-run)) (:err twice-run))
    (is (= 4 (count (:uses twice-graph))) "the same run in two files is one set of uses")
    (is (= #{1} (set (map #(count (:evidence %))
                          (filter #(= "used-together" (:kind %)) (:edges twice-graph))))))
    (is (zero? (:exit self-run)) (:err self-run))
    (is (empty? (filter #(= "used-together" (:kind %)) (:edges self-graph))))))

(deftest apply-copies-once-and-refuses-invalid-diffs
  (let [tmp-dir (.toFile (java.nio.file.Files/createTempDirectory
                          "apply-pattern-diff" (make-array java.nio.file.attribute.FileAttribute 0)))
        applied (io/file tmp-dir "applied")
        source (io/file (fixture-path "diffs" "2026-10-05-c9d25d6a.expected-diff.json"))
        first-run (shell/sh "python3" "scripts/pattern_graph_diff.py" "apply"
                            (.getPath source) "--applied-dir" (.getPath applied))
        destination (first (.listFiles applied))
        second-run (shell/sh "python3" "scripts/pattern_graph_diff.py" "apply"
                             (.getPath source) "--applied-dir" (.getPath applied))
        wrong (io/file tmp-dir "wrong.json")
        nothing (io/file tmp-dir "nothing.json")]
    (spit wrong (json/write-value-as-string {:schema "wrong"}))
    (spit nothing (json/write-value-as-string
                   {:schema "pattern-graph-diff-v1" :nothing_to_add "none"
                    :source {:run "nothing"}}))
    (is (zero? (:exit first-run)) (:err first-run))
    (is (= (seq (java.nio.file.Files/readAllBytes (.toPath source)))
           (seq (java.nio.file.Files/readAllBytes (.toPath destination)))))
    (is (not (zero? (:exit second-run))))
    (is (not (zero? (:exit (shell/sh "python3" "scripts/pattern_graph_diff.py" "apply"
                                     (.getPath wrong) "--applied-dir" (.getPath applied))))))
    (is (not (zero? (:exit (shell/sh "python3" "scripts/pattern_graph_diff.py" "apply"
                                     (.getPath nothing) "--applied-dir" (.getPath applied))))))
    (let [escaping (io/file tmp-dir "escaping.json")]
      (spit escaping (json/write-value-as-string
                      {:schema "pattern-graph-diff-v1" :source {:run "../outside"}}))
      (is (not (zero? (:exit (shell/sh "python3" "scripts/pattern_graph_diff.py" "apply"
                                       (.getPath escaping) "--applied-dir" (.getPath applied))))))
      (is (not (.exists (io/file tmp-dir "outside.json")))))))
