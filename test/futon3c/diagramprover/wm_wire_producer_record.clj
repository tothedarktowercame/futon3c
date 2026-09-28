(ns futon3c.diagramprover.wm-wire-producer-record
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.diagramprover.wm-wire :as w]))

(def fixture-root "test/fixtures/wire-producers")

(defn- candidates [stem]
  (let [dir (io/file fixture-root)
        prefix (str stem "@")]
    (->> (or (.listFiles dir) (make-array java.io.File 0))
         (filter #(and (.isFile %) (.startsWith (.getName %) prefix)
                       (.endsWith (.getName %) ".edn")))
         (sort-by #(.getName %))
         vec)))

(defn record
  [stem]
  (let [files (candidates stem)]
    (when-not (= 1 (count files))
      (throw (ex-info (str "expected one producer record for " stem
                           ", found " (count files) ": "
                           (mapv #(.getName %) files))
                      {:stem stem :files (mapv str files)})))
    (let [file (first files)
          expected (second (re-find #"@([0-9a-f]{12})\.edn$" (.getName file)))
          observed (subs (w/sha256-file (.getPath file)) 0 12)]
      (when-not (= expected observed)
        (throw (ex-info (str "producer record hash mismatch for " file
                             ": name says " expected ", bytes say " observed)
                        {:file (str file) :expected expected :observed observed})))
      (edn/read-string (slurp file)))))
