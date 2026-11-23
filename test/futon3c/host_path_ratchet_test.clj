(ns futon3c.host-path-ratchet-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def baseline-path "test/futon3c/host_path_ratchet_baseline.edn")
(def needle "\"/home/joe/")

(defn occurrences [root relative]
  (let [text (slurp (io/file root relative))]
    (loop [offset 0 line 1 in-string? false escaped? false comment? false found []]
      (if (= offset (count text))
        found
        (let [character (.charAt text offset)
              newline? (= character \newline)]
          (cond
            newline? (recur (inc offset) (inc line) in-string? false false found)
            comment? (recur (inc offset) line in-string? false true found)
            (and (not in-string?) (= character \;))
            (recur (inc offset) line false false true found)
            (and (not in-string?) (= character \"))
            (recur (inc offset) line true false false
                   (if (.startsWith text "/home/joe/" (inc offset)) (conj found line) found))
            (and in-string? escaped?) (recur (inc offset) line true false false found)
            (and in-string? (= character \\)) (recur (inc offset) line true true false found)
            (and in-string? (= character \")) (recur (inc offset) line false false false found)
            :else (recur (inc offset) line in-string? false false found)))))))

(defn source-files [root scopes]
  (for [scope scopes
        file (file-seq (io/file root scope))
        :when (.isFile file)]
    (str/replace (.toString (.relativize (.toPath root) (.toPath file))) "\\" "/")))

(defn ratchet-errors [root baseline]
  (let [{:keys [scopes files allowlist]} baseline
        current (into {}
                      (for [relative (source-files root scopes)
                            :let [lines (vec (occurrences root relative))
                                  allowed (set (keys (get allowlist relative {})))
                                  counted (vec (remove allowed lines))]
                            :when (seq counted)]
                        [relative counted]))]
    (vec
     (mapcat
      (fn [[relative lines]]
        (let [limit (get files relative 0)]
          (when (> (count lines) limit)
            (for [line (drop limit lines)]
              (str relative ":" line " adds a literal starting with " needle)))))
      current))))

(deftest hard-coded-host-path-count-does-not-rise
  (let [root (.getCanonicalFile (io/file "."))
        baseline (edn/read-string (slurp (io/file root baseline-path)))
        errors (ratchet-errors root baseline)
        current-total (reduce + (map #(count (occurrences root %))
                                     (source-files root (:scopes baseline))))
        baseline-total (reduce + (vals (:files baseline)))]
    (when (< current-total baseline-total)
      (println "Host-path count fell from" baseline-total "to" current-total
               "— lower" baseline-path))
    (is (empty? errors) (str "Hard-coded host-path ratchet rose:\n" (str/join "\n" errors)))))
