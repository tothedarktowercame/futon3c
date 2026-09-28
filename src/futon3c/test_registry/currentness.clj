(ns futon3c.test-registry.currentness
  "Fast current-warrant classification over retained file hashes.

  This is the Clojure counterpart of scripts/warrant_index.py/classify. It is
  deliberately narrower than check-record!: no process, Git, environment, or
  log reads occur here."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.test-registry :as registry])
  (:import [java.sql DriverManager]))

(def ^:private sha256-pattern #"[0-9a-f]{64}")

(defn- fail [reason & [data]]
  (throw (ex-info (name reason) {:reason reason :data data})))

(defn- absolute-path [repo-root path]
  (let [file (io/file path)]
    (str (.normalize (.toAbsolutePath (.toPath (if (.isAbsolute file)
                                                 file
                                                 (io/file repo-root path))))))))

(defn- recorded-files [payload repo-root]
  (when-not (and (string? repo-root) (not (str/blank? repo-root)))
    (fail :missing-repo-root))
  (let [closure (:load-closure payload)
        tests (:test-files payload)]
    (when-not (vector? closure) (fail :missing-load-closure))
    (when-not (map? tests) (fail :missing-test-files))
    (reduce
     (fn [files [path sha]]
       (when-not (and (string? path) (not (str/blank? path))
                      (string? sha) (re-matches sha256-pattern sha))
         (fail :invalid-recorded-file {:path path :sha256 sha}))
       (let [path (absolute-path repo-root path)]
         (when (and (contains? files path) (not= sha (get files path)))
           (fail :conflicting-recorded-hash {:path path}))
         (assoc files path sha)))
     (sorted-map)
     (concat (map (juxt :path :sha256) closure) tests))))

(defn- decoded [entry]
  (let [text (get-in entry [:evidence/body :payload-edn])
        recorded-sha (get-in entry [:evidence/body :sha256])
        observed-sha (when (string? text) (digest/sha256 text))]
    (when-not entry (fail :entry-not-found))
    (when-not (and (string? text)
                   (= recorded-sha observed-sha)
                   (= (:evidence/id entry) (str "test-registry-" observed-sha)))
      (fail :payload-digest-mismatch))
    (try
      (edn/read-string text)
      (catch Exception _ (fail :invalid-payload-edn)))))

(defn- indexed-warrant? [store entry-id]
  (with-open [connection (DriverManager/getConnection
                          (str "jdbc:sqlite:" (:path store)))
              statement (.prepareStatement
                         connection
                         "SELECT warrant FROM registry_runs WHERE entry_id=?")]
    (.setString statement 1 entry-id)
    (with-open [result (.executeQuery statement)]
      (when-not (.next result) (fail :missing-run-index))
      (= 1 (.getInt result "warrant")))))

(defn classify
  "Classify ENTRY at REPO-ROOT as :current, :stale, :not-passing, or
  :unverifiable. A stale result names the lexically first changed file."
  [store entry repo-root]
  (try
    (let [payload (decoded entry)
          files (recorded-files payload repo-root)
          indexed-warrant (indexed-warrant? store (:evidence/id entry))]
      (cond
        (or (not indexed-warrant) (not (true? (:warrant? payload))))
        {:class :not-passing}

        (empty? files)
        {:class :unverifiable :reason :no-recorded-files}

        :else
        (if-let [changed
                 (first
                  (keep (fn [[path expected]]
                          (let [file (io/file path)]
                            (cond
                              (not (.isFile file)) {:path path :reason :unreadable}
                              (not= expected (registry/file-sha file))
                              {:path path :reason :hash-mismatch})))
                        files))]
          {:class :stale :changed changed}
          {:class :current})))
    (catch Exception exception
      {:class :unverifiable
       :reason (or (:reason (ex-data exception)) :classification-failed)})))
