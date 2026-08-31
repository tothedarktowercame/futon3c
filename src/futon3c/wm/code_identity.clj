(ns futon3c.wm.code-identity
  "Serving-JVM provenance for targeted Futon2 reloads.

   An entry exists only when this JVM loaded a file through
   load-file-recorded!. Absence is deliberately not inferred from the current
   checkout: disk state cannot reconstruct an already-loaded Var body."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str])
  (:import [java.security MessageDigest]
           [java.time Instant]))

(def ^:dynamic *futon2-root* "/home/joe/code/futon2")
(def production-runner "src/futon2/aif/full_loop_runner.clj")
(def schema-version 1)

(defonce ^:private !reloads (atom {}))

(defn- sha256-bytes [bytes]
  (let [digest (MessageDigest/getInstance "SHA-256")]
    (.update digest bytes)
    (format "%064x" (BigInteger. 1 (.digest digest)))))

(defn- git [repo & args]
  (let [{:keys [exit out err]} (apply shell/sh "git" "-C" repo args)]
    {:ok? (zero? exit) :value (str/trim out) :error (str/trim err)}))

(defn- relative-to [root file]
  (let [root-path (.toPath (.getCanonicalFile (io/file root)))
        file-path (.toPath (.getCanonicalFile (io/file file)))]
    (when (.startsWith file-path root-path)
      (str (.relativize root-path file-path)))))

(defn measure-file
  "Observe one canonical Futon2 source before/after a targeted reload."
  [path]
  (let [file (.getCanonicalFile (io/file path))
        relative (relative-to *futon2-root* file)
        head (git *futon2-root* "rev-parse" "HEAD")
        tree (git *futon2-root* "rev-parse" "HEAD^{tree}")
        status (git *futon2-root* "status" "--porcelain")]
    (if-not (and relative (.isFile file))
      {:readable? false :path (.getPath file) :reason :outside-canonical-futon2-source}
      {:readable? (and (:ok? head) (:ok? tree) (:ok? status))
       :path relative
       :git-head (:value head)
       :tree-sha (:value tree)
       :dirty? (boolean (seq (:value status)))
       :content-sha256 (sha256-bytes (java.nio.file.Files/readAllBytes (.toPath file)))})))

(defn load-file-recorded!
  "Targeted canonical load-file plus a before/after identity receipt.

   Throws rather than recording if the source changes during the operation."
  [path]
  (let [before (measure-file path)]
    (when-not (:readable? before)
      (throw (ex-info "reload source is not a readable canonical Futon2 file" before)))
    (when (:dirty? before)
      (throw (ex-info "refusing to reload Futon2 from a dirty repository" before)))
    (let [value (load-file path)
          after (measure-file path)
          stable? (= before after)
          record (assoc before :schema schema-version :loaded-at (str (Instant/now))
                        :stable? stable? :serving-pid
                        (-> (java.lang.management.ManagementFactory/getRuntimeMXBean) .getPid))]
      (when-not stable?
        (throw (ex-info "reload source or repository basis changed during load"
                        {:before before :after after})))
      (swap! !reloads assoc (:path before) record)
      value)))

(defn status []
  (let [record (get @!reloads production-runner)]
    {:schema schema-version
     :required-file production-runner
     :availability (if record :available :unavailable)
     :reason (when-not record :not-recorded-in-this-process-image)
     :identity record}))

(defn reset-for-test! [] (reset! !reloads {}))
(defn install-for-test! [record] (swap! !reloads assoc production-runner record))
