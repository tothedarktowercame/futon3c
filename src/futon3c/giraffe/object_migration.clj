(ns futon3c.giraffe.object-migration
  "Pack legacy objects. Dry-run by default; never delete before copy and run verification."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.giraffe.run-data :as data]
            [futon3c.giraffe.run :as run])
  (:import [java.nio.file Files CopyOption]))

(defn- legacy-files [root]
  (->> (file-seq (io/file root "objects"))
       (filter #(and (.isFile %) (str/ends-with? (.getName %) ".edn")))
       (sort-by str) vec))

(defn- file-sha [f] (str/replace (.getName ^java.io.File f) #"\.edn$" ""))

(defn migrate!
  "Copy and verify ALL files, verify up to three sealed runs through SQLite,
  then delete the verified legacy files. A failed gate leaves every file intact.
  Stop legacy file writers before migration; SQLite writers may run concurrently."
  [root {:keys [write?] :or {write? false}}]
  (let [files (legacy-files root)
        mismatches (vec (for [f files
                              :let [sha (file-sha f)
                                    actual (data/sha256-bytes (Files/readAllBytes (.toPath f))) ]
                              :when (not= sha actual)]
                          {:file (str f) :expected sha :actual actual}))
        summary {:objects (count files) :bytes (reduce + 0 (map #(.length %) files))
                 :mismatches mismatches}]
    (when (seq mismatches)
      (throw (ex-info "Object digest mismatch; no files copied or deleted" summary)))
    (if-not write?
      (assoc summary :status :dry-run)
      (let [existing? (.isFile (data/object-db root))
            staging (when-not existing?
                      (.toFile (Files/createTempDirectory (.toPath (io/file root))
                                ".objects-pack-" (make-array java.nio.file.attribute.FileAttribute 0))))
            target (or staging root)]
        ;; A new database is invisible to readers until the entire copy passes.
        (with-open [c (data/connect-objects target true)]
          (.setAutoCommit c false)
          (try
            (doseq [f files]
              (let [bytes (Files/readAllBytes (.toPath f)) sha (file-sha f)]
                (when-not (= sha (data/sha256-bytes bytes))
                  (throw (ex-info "Object changed during copy" {:file (str f)})))
                (data/put-object-bytes! c sha bytes)))
            (binding [data/*object-connection* c]
              (doseq [f files]
                (let [sha (file-sha f) bytes (data/object-bytes target sha)]
                  (when-not (and bytes (= sha (data/sha256-bytes bytes)))
                    (throw (ex-info "Database copy verification failed" {:sha sha}))))))
            (.commit c)
            (catch Exception e (.rollback c) (throw e)))
          (.setAutoCommit c true)
          (with-open [s (.createStatement c)] (.execute s "PRAGMA wal_checkpoint(TRUNCATE)")))
        (when staging
          ;; No replacement: refuse if a concurrent first writer created the DB.
          (Files/move (.toPath (data/object-db staging)) (.toPath (data/object-db root))
                      (make-array CopyOption 0))
          (doseq [f (reverse (file-seq staging))] (io/delete-file f true)))
        (let [samples (take 3 (sort-by str
                                      (filter (fn [dir]
                                                (and (.isFile (io/file dir "manifest.edn"))
                                                     (some (fn [name]
                                                             (let [f (io/file dir name)]
                                                               (and (.isFile f)
                                                                    (data/index? (edn/read-string (slurp f))))))
                                                           ["input.edn" "pass.edn"])))
                                              (or (.listFiles (io/file root)) []))))
              checks (mapv (fn [dir] {:run (str dir) :verification (run/verify-run dir)}) samples)]
          (when (some #(not (get-in % [:verification :ok?])) checks)
            (throw (ex-info "Run verification failed; legacy files retained" {:checks checks})))
          ;; Recheck source bytes before deleting any file (including on reruns).
          (doseq [f files]
            (when-not (= (file-sha f) (data/sha256-bytes (Files/readAllBytes (.toPath f))))
              (throw (ex-info "Object changed before deletion" {:file (str f)}))))
          (doseq [f files] (io/delete-file f))
          (doseq [d (reverse (filter #(.isDirectory %) (file-seq (io/file root "objects"))))]
            (when (empty? (.list d)) (io/delete-file d)))
          (assoc summary :status :migrated :deleted (count files) :checks checks))))))

(defn -main [& args]
  (let [[root & flags] args]
    (when (or (nil? root) (some #(not= "--write" %) flags))
      (throw (ex-info "Usage: clojure -M -m futon3c.giraffe.object-migration ROOT [--write]" {})))
    (prn (migrate! root {:write? (boolean (some #{"--write"} flags))}))))
