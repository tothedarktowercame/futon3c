(ns futon3c.giraffe.corpus-generation
  "Immutable, content-addressed Giraffe reading corpus generations.

  The compatibility path remains the path supplied as GIRAFFE_READINGS_ROOT.
  After migration it is a symlink. Readers resolve it once to an immutable
  generation; publishers replace the symlink atomically with rename(2)."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.giraffe.missions :as missions]
            [futon3c.giraffe.readings :as readings])
  (:import [java.nio.channels FileChannel]
           [java.nio.file CopyOption Files LinkOption Path StandardCopyOption StandardOpenOption]
           [java.nio.file.attribute FileAttribute]
           [java.security MessageDigest]
           [java.util UUID]))

(defn- sha-bytes [^bytes bs]
  (let [digest (doto (MessageDigest/getInstance "SHA-256") (.update bs))]
    (apply str (map #(format "%02x" %) (.digest digest)))))

(defn file-sha [file]
  (sha-bytes (Files/readAllBytes (.toPath (io/file file)))))

(defn generation-digest
  "Digest relative file names and bytes in DIR, in lexical order."
  [dir]
  (let [root (.toPath (.getCanonicalFile (io/file dir)))
        digest (MessageDigest/getInstance "SHA-256")]
    (with-open [stream (Files/walk root (make-array java.nio.file.FileVisitOption 0))]
      (doseq [^Path path (->> (.iterator stream) iterator-seq
                              (filter #(Files/isRegularFile % (make-array LinkOption 0)))
                              (sort-by #(.toString (.relativize root %))))]
        (.update digest (.getBytes (.toString (.relativize root path)) "UTF-8"))
        (.update digest (byte 0))
        (.update digest (Files/readAllBytes path))))
    (apply str (map #(format "%02x" %) (.digest digest)))))

(defn resolve-generation
  "Resolve ROOT exactly once. The returned path remains valid after a pointer
  switch. Legacy directory roots are accepted until the one-shot migration."
  [root]
  (let [requested (.toPath (.getAbsoluteFile (io/file root)))
        resolved (.toRealPath requested (make-array LinkOption 0))]
    {:pointer (.toString requested)
     :path (.toString resolved)
     :digest (generation-digest (.toFile resolved))}))

(defn- copy-tree! [source destination]
  (let [src (.toPath (io/file source)) dst (.toPath (io/file destination))]
    (with-open [stream (Files/walk src (make-array java.nio.file.FileVisitOption 0))]
      (doseq [^Path path (->> (.iterator stream) iterator-seq
                              (sort-by #(.getNameCount ^Path %)))]
        (let [relative (.relativize src path)
              target (.resolve dst relative)]
          (if (Files/isDirectory path (make-array LinkOption 0))
            (Files/createDirectories target (make-array FileAttribute 0))
            (Files/copy path target
                        (into-array CopyOption [StandardCopyOption/COPY_ATTRIBUTES]))))))))

(defn- delete-tree! [root]
  (let [path (.toPath (io/file root))]
    (when (Files/exists path (make-array LinkOption 0))
      (with-open [stream (Files/walk path (make-array java.nio.file.FileVisitOption 0))]
        (doseq [^Path p (->> (.iterator stream) iterator-seq
                             (sort-by #(- (.getNameCount ^Path %))))]
          (Files/deleteIfExists p))))))

(defn- batch-files [root]
  (->> (file-seq (io/file root))
       (filter #(.isFile %))
       (filter #(re-matches #"batch-.*\.jsonl" (.getName %)))
       (sort-by #(.getName %))
       vec))

(defn- remove-target [text target-id]
  (->> (str/split-lines text)
       (remove str/blank?)
       (remove #(= target-id (:id (json/parse-string % true))))
       (str/join "\n")))

(defn- apply-replacement! [staging target-id replacement source]
  (doseq [batch (batch-files staging)]
    (let [retained (remove-target (slurp batch) target-id)]
      (spit batch (if (str/blank? retained) "" (str retained "\n")))))
  (spit (io/file staging "batch-reread.jsonl")
        (str (json/generate-string replacement) "\n") :append true)
  (let [members-file (io/file staging "members.json")
        members (json/parse-string (slurp members-file) true)
        found? (some #(= target-id (:id %)) members)
        members' (mapv #(if (= target-id (:id %))
                          (merge % (select-keys source [:path :sha256]))
                          %)
                       members)]
    (when-not found?
      (throw (ex-info "Replacement target is absent from members.json"
                      {:kind :giraffe-generation/member-absent :target-id target-id})))
    (spit members-file (json/generate-string members' {:pretty true}))))

(defn- validate-staging! [staging target-id]
  (let [batches (mapv #(.getAbsolutePath %) (batch-files staging))
        result (readings/consume
                {:checker (.getPath (io/file staging "check_edges.py"))
                 :members-file (.getPath (io/file staging "members.json"))
                 :batches batches :repos missions/default-repos
                 :scope :population/missions-excursions-tickets})
        rows (get-in result [:coverage :members])
        row (some #(when (= target-id (:id %)) %) rows)
        failed (filterv (complement :passed?) (:checked-records result))
        ambiguous (filterv #(= :ambiguous (:status %)) rows)]
    (when (or (not= :read (:status row)) (seq failed) (seq ambiguous))
      (throw (ex-info "Replacement did not pass corpus validation"
                      {:kind :giraffe-generation/checker-failed
                       :target-id target-id :member row
                       :failed-record-count (count failed)
                       :ambiguous-members (mapv :id ambiguous)})))
    result))

(defn- pointer-target [pointer]
  (:path (resolve-generation pointer)))

(defn- atomic-link! [pointer generation]
  (let [pointer-path (.toPath (.getAbsoluteFile (io/file pointer)))
        temp (.resolve (.getParent pointer-path)
                       (str "." (.getFileName pointer-path) ".next-" (UUID/randomUUID)))]
    (Files/createSymbolicLink temp (.toPath (.getAbsoluteFile (io/file generation)))
                              (make-array FileAttribute 0))
    (try
      (Files/move temp pointer-path
                  (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                          StandardCopyOption/REPLACE_EXISTING]))
      (finally (Files/deleteIfExists temp)))))

(defn publish-generation!
  "Publish one replacement record against POINTER's current generation.

  CLAIM is {:target-id :source-path :source-sha}. REPLACEMENT is the checked
  JSON record. Validation occurs in staging; source and base pins are checked
  immediately before the atomic pointer switch. Returns a typed result and
  removes staging on every refusal."
  [{:keys [pointer generations-dir claim replacement before-switch-fn]}]
  (let [{base-path :path base-digest :digest} (resolve-generation pointer)
        generations-dir (or generations-dir
                            (str (.getAbsolutePath (io/file pointer)) ".generations"))
        stage-parent (doto (io/file generations-dir ".staging") .mkdirs)
        staging (io/file stage-parent (str (UUID/randomUUID)))
        lock-path (.toPath (io/file generations-dir ".publish.lock"))]
    (try
      (copy-tree! base-path staging)
      (apply-replacement! staging (:target-id claim) replacement
                          {:path (:source-path claim) :sha256 (:source-sha claim)})
      (validate-staging! staging (:target-id claim))
      (when before-switch-fn (before-switch-fn))
      (when-not (= (:source-sha claim) (file-sha (:source-path claim)))
        (throw (ex-info "Claimed source changed during reading"
                        {:kind :giraffe-generation/source-changed})))
      (Files/createDirectories (.getParent lock-path) (make-array FileAttribute 0))
      (with-open [channel (FileChannel/open lock-path
                                           (into-array StandardOpenOption
                                                       [StandardOpenOption/CREATE
                                                        StandardOpenOption/WRITE]))
                  _lock (.lock channel)]
        (when-not (= base-path (pointer-target pointer))
          (throw (ex-info "Corpus pointer moved during publication"
                          {:kind :giraffe-generation/base-mismatch
                           :expected base-path :actual (pointer-target pointer)})))
        (let [digest (generation-digest staging)
              generation (io/file generations-dir digest)]
          (if (.exists generation)
            (delete-tree! staging)
            (Files/move (.toPath staging) (.toPath generation)
                        (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE])))
          (atomic-link! pointer generation)
          {:status :published :base-digest base-digest
           :digest digest :path (.getAbsolutePath generation)}))
      (catch clojure.lang.ExceptionInfo exc
        {:status :rejected :reason (:kind (ex-data exc))
         :details (dissoc (ex-data exc) :kind)})
      (catch Exception exc
        {:status :rejected :reason :giraffe-generation/checker-failed
         :details {:message (.getMessage exc)}})
      (finally (delete-tree! staging)))))

(defn migrate!
  "Plan or perform one-shot conversion of legacy corpus directory POINTER.
  Dry-run is the default and performs no writes. Actual migration must run
  between clicks."
  [{:keys [pointer generations-dir dry-run?] :or {dry-run? true}}]
  (let [pointer-file (io/file pointer)
        generations-dir (or generations-dir (str (.getAbsolutePath pointer-file) ".generations"))
        digest (generation-digest pointer-file)
        generation (io/file generations-dir digest)
        legacy (io/file (str (.getAbsolutePath pointer-file) ".legacy-" digest))
        report {:status (if dry-run? :dry-run :migrated)
                :pointer (.getAbsolutePath pointer-file)
                :generation (.getAbsolutePath generation)
                :digest digest :legacy (.getAbsolutePath legacy)}]
    (when-not dry-run?
      (when (Files/isSymbolicLink (.toPath pointer-file))
        (throw (ex-info "Corpus is already a generation pointer"
                        {:kind :giraffe-generation/already-migrated})))
      (copy-tree! pointer-file generation)
      (Files/move (.toPath pointer-file) (.toPath legacy)
                  (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE]))
      (atomic-link! pointer generation))
    report))

(defn -main [& args]
  (let [pointer (second (drop-while #(not= "--pointer" %) args))
        apply? (contains? (set args) "--apply")]
    (when-not pointer
      (throw (ex-info "--pointer is required" {:kind :giraffe-generation/pointer-required})))
    (println (pr-str (migrate! {:pointer pointer :dry-run? (not apply?)})))))
