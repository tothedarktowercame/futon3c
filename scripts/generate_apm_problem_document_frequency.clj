#!/usr/bin/env -S clojure -M

(require '[clojure.java.io :as io]
         '[clojure.pprint :as pprint]
         '[futon3c.dispatch-with-recall :as dispatch])

(let [problem-root (or (first *command-line-args*)
                       "/home/joe/code/apm-lean/problems")
      output-path (or (second *command-line-args*)
                      "resources/apm-problem-document-frequency.edn")
      files (->> (.listFiles (io/file problem-root))
                 (filter #(.isDirectory %))
                 (map #(io/file % "problem.md"))
                 (filter #(.isFile %))
                 (sort-by #(.getPath %))
                 vec)
      document-frequency
      (reduce
       (fn [counts file]
         (reduce #(update %1 %2 (fnil inc 0))
                 counts
                 (dispatch/query-keywords
                  (slurp file)
                  Integer/MAX_VALUE
                  {:document-count 1 :document-frequency {}})))
       (sorted-map)
       files)
      table {:schema-version 1
             :document-count (count files)
             :document-frequency document-frequency}]
  (with-open [writer (io/writer output-path)]
    (binding [*out* writer]
      (pprint/pprint table)))
  (println "wrote" output-path "from" (count files) "documents"))
