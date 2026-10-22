(ns futon3c.wm.code-identity
  "Serving-JVM provenance for targeted Futon2 reloads.

   An entry exists only when this JVM loaded a file through
   load-file-recorded!. Absence is deliberately not inferred from the current
   checkout: disk state cannot reconstruct an already-loaded Var body."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon2.aif.load-identity :as load-identity])
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

(defn- dirty-status?
  "True when `git status --porcelain` output shows a change that could reach
   loaded code: any tracked change, or an untracked file under src/. Untracked
   files elsewhere (lab outputs under holes/, data) cannot be loaded and do not
   count: click wm-click-57af6c46 (2026-10-22) had its reviewed repair refused
   at enact because 26 untracked holes/labs files made the shared futon2
   checkout read as dirty."
  [porcelain]
  (boolean
   (some (fn [line]
           (or (not (str/starts-with? line "?? "))
               (str/starts-with? (str/replace (subs line 3) #"^\"" "") "src/")))
         (remove str/blank? (str/split-lines porcelain)))))

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
       :dirty? (dirty-status? (:value status))
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
    (load-file path)
    (let [after (measure-file path)
          stable? (= before after)
          record (assoc before :schema schema-version :loaded-at (str (Instant/now))
                        :stable? stable? :serving-pid
                        (-> (java.lang.management.ManagementFactory/getRuntimeMXBean) .getPid))]
      (when-not stable?
        (throw (ex-info "reload source or repository basis changed during load"
                        {:before before :after after})))
      (swap! !reloads assoc (:path before) record)
      ;; F39 (click wm-click-96850427, 2026-10-20): return the reload record,
      ;; not the value of load-file. That value is the file's last Var, and
      ;; enact! put it in the port receipt's payload, where identity/digest
      ;; refuses it ("Unsupported action identity value"), so a correct enact
      ;; ended the click as :coordinator/port-threw. No caller used the value.
      record)))

(defn status []
  (let [record (get @!reloads production-runner)
        startup (get @load-identity/registry 'futon2.aif.full-loop-runner)
        startup-check (when startup
                        (load-identity/check startup
                                             (str *futon2-root* "/" production-runner)
                                             load-identity/read-bytes))
        identity (or record
                     (when startup
                       {:schema schema-version
                        :loaded-at (:captured-at startup)
                        :path production-runner
                        :content-sha256 (:sha256 startup)
                        :source :namespace-load-registration
                        :current-status (:status startup-check)
                        :disk-sha256 (:disk-sha256 startup-check)}))]
    {:schema schema-version
     :required-file production-runner
     :availability (if identity :available :unavailable)
     :reason (when-not identity :not-recorded-in-this-process-image)
     :identity identity}))

(defn reset-for-test! [] (reset! !reloads {}))
(defn install-for-test! [record] (swap! !reloads assoc production-runner record))
