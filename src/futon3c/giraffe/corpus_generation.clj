(ns futon3c.giraffe.corpus-generation
  "Immutable, content-addressed Giraffe reading corpus generations.

  The compatibility path remains the path supplied as GIRAFFE_READINGS_ROOT.
  After migration it is a symlink. Readers resolve it once to an immutable
  generation; publishers replace the symlink atomically with rename(2)."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
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
       (remove #(= target-id (:id (try (json/parse-string % true)
                                          ;; a malformed line is kept as it is;
                                          ;; validation reports it (claude-13)
                                          (catch Exception _ nil)))))
       (str/join "\n")))

(defn- apply-replacement! [staging target-id replacement source new-item?]
  (doseq [batch (batch-files staging)]
    (let [retained (remove-target (slurp batch) target-id)]
      (spit batch (if (str/blank? retained) "" (str retained "\n")))))
  (spit (io/file staging "batch-reread.jsonl")
        (str (json/generate-string replacement) "\n") :append true)
  (let [members-file (io/file staging "members.json")
        members (json/parse-string (slurp members-file) true)
        found? (some #(= target-id (:id %)) members)
        members' (if found?
                   (mapv #(if (= target-id (:id %))
                            (merge % (select-keys source [:path :sha256]))
                            %)
                         members)
                   (conj members (assoc (select-keys source [:path :sha256])
                                        :id target-id)))]
    (when (and (not found?) (not new-item?))
      (throw (ex-info "Replacement target is absent from members.json"
                      {:kind :giraffe-generation/member-absent :target-id target-id})))
    (spit members-file (json/generate-string members' {:pretty true}))))

(defn- new-failing-record-ids [base-failing staged-failing]
  (set/difference staged-failing base-failing))

(defn- staged-lines
  "Every JSONL line of ROOT's batches with its batch path, 1-based line
  number and parsed record (nil for an unparseable line). No checker run."
  [root]
  (mapcat (fn [batch]
            (keep-indexed (fn [i line]
                            (when-not (str/blank? line)
                              {:batch (.getAbsolutePath batch)
                               :line-number (inc i) :line line
                               :record (try (json/parse-string line true)
                                            (catch Exception _ nil))}))
                          (str/split (slurp batch) #"\n")))
          (batch-files root)))

(defn- cites-path?
  "True when RECORD's edges or unresolved entries carry evidence citing PATH."
  [record path]
  (some #(= path (get-in % [:evidence :path]))
        (concat (:edges record) (:unresolved record))))

(defn- checked-line-failures
  "Run the Python checker on exactly LINES (staged-lines entries) of ROOT and
  return [failing-id-set checked-records]. The failing set uses record ids
  (:unidentified-record for a line that does not parse). The population and
  mid-validation-change guards of a full readings/consume are retained: the
  checker's own members.json must equal ROOT's, and neither file may change
  while the checks run."
  [root lines]
  (let [checker (.getAbsolutePath (io/file root "check_edges.py"))
        members-file (.getAbsolutePath (io/file root "members.json"))
        members-text (slurp members-file)
        checker-text (slurp checker)]
    (when-not (= members-text (slurp (.getPath (io/file (.getParentFile (io/file checker))
                                                         "members.json"))))
      (throw (ex-info "Checker uses a different population"
                      {:kind :reading/population-mismatch})))
    (let [checked (mapv #(readings/check-record checker (:batch %)
                                                (:line-number %) (:line %))
                        lines)]
      (when-not (and (= checker-text (slurp checker))
                     (= members-text (slurp members-file)))
        (throw (ex-info "Checker or population changed during validation"
                        {:kind :reading/validation-input-changed})))
      [(into #{}
             (comp (filter (complement :passed?))
                   (map #(or (get-in % [:record :id]) :unidentified-record)))
             checked)
       checked])))

(defn- validate-staging!
  "Decide the staged corpus by running the Python checker ONLY on the lines
  the replacement can affect (Joe, 2026-11-15: one landing took 5-7 minutes
  because validate-staging! consumed all 654 records; measured here at 583 s).

  Argument for the narrowed check: apply-replacement! changes exactly one
  staged file line (the target's, replaced) and the target's members.json
  pin. A checker result is a function of its record line and members.json,
  so every other record's result equals its base result unless it cites the
  target's source path in its evidence — those citers are re-checked
  explicitly. An unparseable staged line is checked (and fails) rather than
  silently skipped. New failures are judged against the base's result for
  the SAME ids (run against the base's own lines), not a full base consume,
  so a publish never pays for the records it did not touch.

  Ambiguity (two records for one id) is detected over ALL staged lines by
  parsing ids, without the checker. The population and checker-changed
  guards of readings/consume are retained in checked-line-failures. The
  price is explicit: a pre-existing failure elsewhere in the corpus is no
  longer re-witnessed on every publish — it stays decided, and the
  full-corpus certificate still happens at mint time."
  [base staging target-id source-path]
  (let [lines (staged-lines staging)
        by-id (group-by (comp :id :record) lines)
        ambiguous (->> by-id
                       (filter (fn [[id _]] (and id (> (count (val (find by-id id))) 1))))
                       (map key))
        target-lines (get by-id target-id)
        citers (->> lines
                    (filter #(and (:record %)
                                  (not= target-id (:id (:record %)))
                                  (cites-path? (:record %) source-path))))
        to-check (vec (distinct (concat target-lines citers
                                        (filter #(nil? (:record %)) lines))))
        base-lines (let [base-ids (into #{} (comp (map #(get-in % [:record :id]))
                                                  (remove nil?))
                                        to-check)]
                     ;; the base's unparseable lines too: the staged side
                     ;; always checks its own, so without these a malformed
                     ;; line already in the base would count as a NEW
                     ;; failure and block every publish (claude-13 review)
                     (filter #(or (nil? (:record %))
                                  (contains? base-ids (:id (:record %))))
                             (staged-lines base)))
        [staged-failing staged-checked] (checked-line-failures staging to-check)
        target-passed? (and (= 1 (count target-lines))
                            (boolean (some #(and (= target-id (get-in % [:record :id]))
                                                 (:passed? %))
                                           staged-checked)))
        [base-failing _base-checked] (checked-line-failures base base-lines)
        new-failing (new-failing-record-ids base-failing staged-failing)]
    (when (or (not target-passed?) (seq new-failing) (seq ambiguous))
      (throw (ex-info "Replacement did not pass corpus validation"
                      {:kind :giraffe-generation/checker-failed
                       :target-id target-id
                       :member (when (seq target-lines)
                                 {:id target-id :status (if target-passed? :read :unread)})
                       :failed-record-count (count staged-failing)
                       :new-failing-record-ids (vec (sort-by str new-failing))
                       :ambiguous-members (vec ambiguous)})))
    {:checked-lines (count to-check)}))

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

(defn- publish-generation-attempt!
  [{:keys [pointer generations-dir claim replacement before-switch-fn]}]
  (let [{base-path :path base-digest :digest} (resolve-generation pointer)
        stage-parent (doto (io/file generations-dir ".staging") .mkdirs)
        staging (io/file stage-parent (str (UUID/randomUUID)))
        lock-path (.toPath (io/file generations-dir ".publish.lock"))]
    (try
      ;; Cheap check first (claude-13, 2026-11-15): a reading of a source that
      ;; has since changed is superseded, not a checker failure. Checked only
      ;; after validation, 25 stale readings were reported :checker-failed
      ;; after a full-corpus check each (the 74-document status edit,
      ;; 0292cd2b6). The check after validation stays for the race.
      (when-not (= (:source-sha claim) (file-sha (:source-path claim)))
        (throw (ex-info "Claimed source changed during reading"
                        {:kind :giraffe-generation/source-changed})))
      (copy-tree! base-path staging)
      (apply-replacement! staging (:target-id claim) replacement
                          {:path (:source-path claim) :sha256 (:source-sha claim)}
                          (nil? (:old-sha claim)))
      ;; Validation is deliberately repeated against this attempt's base. A
      ;; successful retry must never carry forward validation from a stale base.
      (validate-staging! base-path staging (:target-id claim) (:source-path claim))
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

(def max-race-attempts
  "Tries before a base-pointer race is returned as :base-mismatch. Three was
  too few once a second lane (second-reading on two Kimi seats) published
  beside the two first readers: 3 of 10 results after the race fix were still
  base-mismatch (2026-11-06 17:52-18:50Z)."
  8)

(defn publish-generation!
  "Publish one replacement record against POINTER's current generation.

  CLAIM is {:target-id :source-path :source-sha}. REPLACEMENT is the checked
  JSON record. Validation occurs in staging; source and base pins are checked
  immediately before the atomic pointer switch. Returns a typed result and
  removes staging on every refusal."
  [{:keys [pointer generations-dir] :as opts}]
  (let [generations-dir (or generations-dir
                            (str (.getAbsolutePath (io/file pointer)) ".generations"))]
    (loop [attempt 1]
      (let [result (publish-generation-attempt!
                    (assoc opts :generations-dir generations-dir))]
        (if (and (= :giraffe-generation/base-mismatch (:reason result))
                 (< attempt max-race-attempts))
          (do (Thread/sleep (long (rand-int (* 500 attempt))))
              (recur (inc attempt)))
          (assoc result :attempts attempt))))))

(defn- retirement-members!
  "Check every requested identity and missing source against the staged inventory."
  [staging targets]
  (let [members (json/parse-string (slurp (io/file staging "members.json")) true)
        by-id (group-by :id members)
        bad (filterv (fn [id]
                       (let [entries (get by-id id)
                             path (:path (first entries))]
                         (or (not= 1 (count entries))
                             (not (string? path)) (str/blank? path)
                             (.isFile (io/file path))))) targets)]
    (when (seq bad)
      (throw (ex-info "Retirement requires existing members with missing source files"
                      {:kind :giraffe-generation/retirement-precondition :targets bad})))
    members))

(defn retire-readings!
  "Retire missing-source readings, preserving member identities for incoming edges.
   Rechecks all targets in staging and before switching under the publication lock.
   Unlike replacement's affected-line validation, checks EVERY remaining line with
   checked-line-failures (the generation's own check_edges.py), refusing failures
   not already present in the base generation, and duplicate ids. Throws ex-info on refusal; never partially publishes.
   An already-retired target is unchanged. BEFORE-SWITCH-FN is a test race hook."
  [{:keys [pointer generations-dir targets reason now-ms before-switch-fn]}]
  (when-not (and (= :source-missing reason) (integer? now-ms)
                 (vector? targets) (every? #(and (string? %) (not (str/blank? %))) targets))
    (throw (ex-info "Invalid retirement request"
                    {:kind :giraffe-generation/invalid-retirement})))
  (let [{base-path :path base-digest :digest} (resolve-generation pointer)
        generations-dir (or generations-dir (str (.getAbsolutePath (io/file pointer)) ".generations"))
        stage-parent (doto (io/file generations-dir ".staging") .mkdirs)
        staging (io/file stage-parent (str (UUID/randomUUID)))
        lock-path (.toPath (io/file generations-dir ".publish.lock"))
        targets (vec (distinct targets))]
    (try
      (copy-tree! base-path staging)
      (let [members (retirement-members! staging targets)
            retiring (filterv #(and (some #{(:id %)} targets) (nil? (:retired %))) members)
            ids (set (map :id retiring))]
        (when (seq ids)
          (doseq [batch (batch-files staging)]
            (let [text (reduce remove-target (slurp batch) (sort ids))]
              (spit batch (if (str/blank? text) "" (str text "\n")))))
          (spit (io/file staging "members.json")
                (json/generate-string
                 (mapv #(if (contains? ids (:id %))
                          (assoc % :retired {:reason "source-missing" :at_ms now-ms
                                            :last_sha256 (:sha256 %)}) %) members)
                 {:pretty true}))
          (doseq [member retiring]
            (spit (io/file staging "retired.jsonl")
                  (str (json/generate-string
                        {:id (:id member) :reason "source-missing" :at_ms now-ms
                         :source (select-keys member [:path :sha256])
                         :base_digest base-digest}) "\n") :append true))
          (let [lines (staged-lines staging)
                ambiguous (->> lines (group-by #(get-in % [:record :id]))
                               (keep (fn [[id rows]] (when (and id (> (count rows) 1)) id))))
                [staged-failing _] (checked-line-failures staging lines)
                ;; Only NEW failures refuse: the live corpus carries stale
                ;; readings that already fail the checker, and retiring other
                ;; members must not depend on them (claude-13 review, as in
                ;; validate-staging!).
                base (io/file base-path)
                [base-failing _] (checked-line-failures base (staged-lines base))
                failed (new-failing-record-ids base-failing staged-failing)]
            (when (or (seq failed) (seq ambiguous))
              (throw (ex-info "Retirement did not pass corpus validation"
                              {:kind :giraffe-generation/checker-failed
                               :failed-record-ids (vec (sort-by str failed))
                               :ambiguous-members (vec ambiguous)})))))
        (when before-switch-fn (before-switch-fn))
        (with-open [channel (FileChannel/open lock-path
                                             (into-array StandardOpenOption
                                                         [StandardOpenOption/CREATE StandardOpenOption/WRITE]))
                    _lock (.lock channel)]
          (when-not (= base-path (pointer-target pointer))
            (throw (ex-info "Corpus pointer moved during retirement"
                            {:kind :giraffe-generation/base-mismatch
                             :expected base-path :actual (pointer-target pointer)})))
          (retirement-members! staging targets)
          (if (empty? ids)
            {:status :unchanged :base-digest base-digest :digest base-digest :path base-path}
            (let [digest (generation-digest staging)
                  generation (io/file generations-dir digest)]
              (if (.exists generation)
                (delete-tree! staging)
                (Files/move (.toPath staging) (.toPath generation)
                            (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE])))
              (atomic-link! pointer generation)
              {:status :published :base-digest base-digest :digest digest
               :path (.getAbsolutePath generation) :retired (vec (sort ids))}))))
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
