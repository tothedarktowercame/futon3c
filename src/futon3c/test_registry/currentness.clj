(ns futon3c.test-registry.currentness
  "Fast current-warrant classification over retained file hashes.

  This is the Clojure counterpart of scripts/warrant_index.py/classify. The
  file-current path reads no process, Git, environment, or log state. When a
  file changed and an identity-bound dependency record exists, it delegates
  only that definition check to warrant_reach.py."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.test-registry :as registry])
  (:import [java.sql DriverManager]
           [java.util.concurrent TimeUnit]))

(def ^:private sha256-pattern #"[0-9a-f]{64}")

(def ^:dynamic *reach-dir*
  "/home/joe/code/storage/test-registry/reach-records")

(def ^:dynamic *reach-script*
  (some-> (io/resource "futon3c/test_registry/currentness.clj")
          io/file .getParentFile .getParentFile .getParentFile .getParentFile
          (io/file "scripts/warrant_reach.py") str))

(def ^:dynamic *reach-timeout-ms* 60000)

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

(defn- registration-refusal [payload]
  (let [{:keys [exit failures errors]} (:results payload)
        green? (= [0 0 0] [exit failures errors])]
    (some (fn [check]
            (when (and green?
                       (= :test-registry/refusal (:record/type check)))
              (:reason check)))
          [(:postcheck payload) (:precheck payload)])))

(defn- changed-files [files]
  (vec
   (keep (fn [[path expected]]
           (let [file (io/file path)]
             (cond
               (not (.isFile file)) {:path path :reason :unreadable}
               (not= expected (registry/file-sha file))
               {:path path :reason :hash-mismatch})))
         files)))

(defn- file-rule-ignored [changed reason]
  {:class :stale :basis :files :changed (first changed)
   :differences changed :reach-record {:ignored reason}})

(defn- valid-reach-record [file entry-id namespace]
  (try
    (let [record (json/parse-string (slurp file) true)]
      (cond
        (not= entry-id (:entry-id record)) {:ignored :entry-id-mismatch}
        (not= namespace (:namespace record)) {:ignored :namespace-mismatch}
        :else {:record record}))
    (catch Exception _ {:ignored :invalid-json})))

(defn- run-reach-check [record-file]
  (try
    (when-not (and (string? *reach-script*) (.isFile (io/file *reach-script*)))
      (throw (ex-info "reach script absent" {})))
    (let [builder (doto (ProcessBuilder.
                         ["python3" *reach-script* "check"
                          "--record" (str record-file)
                          "--cache" (str (io/file *reach-dir* ".cache"))])
                    (.redirectErrorStream true))
          process (.start builder)
          output-future (future (slurp (.getInputStream process)))
          finished? (.waitFor process *reach-timeout-ms* TimeUnit/MILLISECONDS)]
      (when-not finished?
        (.destroyForcibly process)
        (throw (ex-info "reach check timeout" {})))
      (let [output (deref output-future 1000 "")
            result (json/parse-string output true)
            result (update result :differences
                           (fn [differences]
                             (mapv #(update % :kind keyword) (or differences []))))
            exit (.exitValue process)]
        (when-not (and (#{0 1} exit) (#{"current" "stale"} (:status result)))
          (throw (ex-info "invalid reach check result" {:exit exit :result result})))
        result))
    (catch Exception exception
      {:ignored {:reason :reach-check-failed
                 :message (ex-message exception)}})))

(defn- classify-stale [entry payload changed]
  (let [entry-id (:evidence/id entry)
        namespace (:namespace payload)
        record-file (io/file *reach-dir* (str entry-id ".json"))]
    (if-not (.isFile record-file)
      {:class :stale :basis :files :changed (first changed) :differences changed}
      (let [{:keys [ignored]} (valid-reach-record record-file entry-id namespace)]
        (if ignored
          (file-rule-ignored changed ignored)
          (let [answer (run-reach-check record-file)]
            (cond
              (:ignored answer) (file-rule-ignored changed (:ignored answer))
              (= "current" (:status answer))
              {:class :current :basis :definitions
               :files-changed-unreached (mapv :path changed)}
              :else
              {:class :stale :basis :definitions
               :changed (first (:differences answer))
               :differences (:differences answer)})))))))

(defn classify
  "Classify ENTRY at REPO-ROOT as :current, :stale, :not-passing, or
  :unverifiable. File-basis stale results name the lexically first changed
  file and retain the complete difference vector."
  [store entry repo-root]
  (try
    (let [payload (decoded entry)
          files (recorded-files payload repo-root)
          indexed-warrant (indexed-warrant? store (:evidence/id entry))]
      (cond
        (or (not indexed-warrant) (not (true? (:warrant? payload))))
        (if-let [reason (registration-refusal payload)]
          {:class :registration-refused :reason reason}
          {:class :not-passing})

        (empty? files)
        {:class :unverifiable :reason :no-recorded-files}

        :else
        (if-let [changed (seq (changed-files files))]
          (classify-stale entry payload (vec changed))
          {:class :current :basis :files})))
    (catch Exception exception
      {:class :unverifiable
       :reason (or (:reason (ex-data exception)) :classification-failed)})))
