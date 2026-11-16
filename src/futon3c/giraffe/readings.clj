(ns futon3c.giraffe.readings
  "Consume checked per-member readings; never infer coverage from absent edges."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.giraffe.missions :as missions]))

(defn sha256 [text]
  (format "%064x" (java.math.BigInteger. 1
                    (.digest (java.security.MessageDigest/getInstance "SHA-256")
                             (.getBytes ^String text "UTF-8")))))

(defn- scrubbed-result
  "The checker is invoked on a per-record temp file whose random name must not
  enter the recorded output.  Coverage certificates -- and so the adapter
  input.edn and its sha256, from which the run id is minted -- embed
  :checker-results; a random /tmp path inside them makes the input digest
  differ on every mint over an identical corpus, which keeps the freshness
  probe permanently stale (2026-10-31: two mints in one click launch, each
  re-probe 'fresh: false').  Replace the temp path with a stable token."
  [result path]
  (into {} (map (fn [[k v]]
                  [k (if (#{:out :err} k)
                       (str/replace (str v) path "giraffe-reading-line")
                       v)]))
        (select-keys result [:exit :out :err])))

(defn- checked-record [checker batch line-number line env]
  (let [file (java.io.File/createTempFile "giraffe-reading-" ".jsonl")
        pin {:sha256 (sha256 line) :batch (str batch) :line line-number}]
    (try
      (spit file (str line "\n"))
      (let [;; :checker-env is merged OVER the process environment (never
            ;; replaces it), so a fixture can point the checker's code-root
            ;; override at its invented root while PATH etc. survive.
            result (if env
                     (shell/sh "python3" "-I" (str checker) (.getPath file)
                               :env (merge (into {} (System/getenv)) env))
                     (shell/sh "python3" "-I" (str checker) (.getPath file)))
            record (try (json/parse-string line true) (catch Exception _ nil))]
        {:record record :record-pin pin
         :checker-result (scrubbed-result result (.getPath file))
         :passed? (and (zero? (:exit result)) (map? record))})
      (finally (.delete file)))))

(defn check-record
  "Public single-line checker for individual callers and equivalence checks.
  consume and publication validation use check-records to batch their lines."
  [checker batch line-number line]
  (checked-record checker batch line-number line nil))

(defn check-records
  "Check entries {:batch :line-number :line} in one Python process. The
  generation's own checker still runs main once per record; no evidence or
  target-index caching. Driver failures and incomplete output throw."
  ([checker entries] (check-records checker entries nil))
  ([checker entries env]
   (let [entries (vec entries)]
     (if (empty? entries)
       []
       (let [file (java.io.File/createTempFile "giraffe-reading-batch-" ".jsonl")]
         (try
           (spit file (str (str/join "\n" (map :line entries)) "\n"))
           (let [args ["python3" "-I" "scripts/giraffe_check_batch.py" (str checker) (.getPath file)]
                 result (apply shell/sh (cond-> args env
                                          (into [:env (merge (into {} (System/getenv)) env)])))
                 fail #(throw (ex-info "Batch checker failed"
                                      {:kind :reading/batch-checker-error :result result}))
                 _ (when-not (zero? (:exit result)) (fail))
                 results (try (mapv #(json/parse-string % true) (str/split-lines (:out result)))
                              (catch Exception _ (fail)))]
             (when-not (and (= (count entries) (count results))
                            (every? true? (map-indexed
                             (fn [i r] (and (= (inc i) (:line r))
                                            (integer? (:exit r))
                                            (string? (:out r)) (string? (:err r)))) results)))
               (fail))
             (mapv (fn [{:keys [batch line-number line]} result]
                     (let [record (try (json/parse-string line true) (catch Exception _ nil))]
                       {:record record
                        :record-pin {:sha256 (sha256 line) :batch (str batch) :line line-number}
                        :checker-result (select-keys result [:exit :out :err])
                        :passed? (and (zero? (:exit result)) (map? record))}))
                   entries results))
           (finally (.delete file))))))))

(def ^:private reading-classes #{"closure" "artifact" "alternative"})

(defn- edge-class
  "The class the reader stated (intended_class), else :closure.  Before
  2026-11-10 every reading became :closure, so M-formal-arxana's five
  :artifact needs (codex-2's strategic-needs reading) could not land."
  [edge]
  (let [c (some-> (or (:intended_class edge) (:intended-class edge)) name)]
    (if (contains? reading-classes c) (keyword c) :closure)))

(defn- convert-edge [repos ids owner edge]
  (let [reverse? (= "unblocks" (:kind edge))
        prerequisite? (contains? #{"depends-on" "unblocks"} (:kind edge))
        from (if reverse? (:to edge) owner)
        to (if reverse? owner (:to edge))
        evidence (:evidence edge)
        path (.toPath (.getAbsoluteFile (io/file (:path evidence))))
        root (some #(let [p (.toPath (.getAbsoluteFile (io/file %)))]
                      (when (.startsWith path p) p)) repos)
        retained (assoc edge :reading-member owner :from from :to to)
        scoped? (and (contains? ids (keyword from)) (contains? ids (keyword to)))]
    (cond
      (not prerequisite?) {:link retained}
      (not scoped?) {:out-of-scope retained}
      (nil? root) {:rejected (assoc retained :reason :edge/file-not-resolvable)}
      :else
      (let [file (str (.getFileName root) "/" (.relativize root path))
            converted (missions/edge->pass-edge
                       (into {} (map #(vector (.getName (io/file %)) %) repos)) ids
                       {:from from :to to :kind "needs" :class (edge-class edge)
                        :file file :line (:line evidence) :quote (:quote evidence)
                        :note (:note edge)})]
        (if-let [pass-edge (:edge converted)]
          {:edge (assoc pass-edge :basis (keyword (:basis edge))
                                  :reading-kind (keyword (:kind edge))
                                  :reading-member owner :evidence-pin evidence)}
          {:rejected (assoc retained :reason (get-in converted [:reject :reject]))})))))

(defn consume
  "One process invokes the exact checker for each record. Duplicate members
  are ambiguous even when each individual record passes. Certificate hashes bind checker, member
  inventory and the exact JSONL line bytes (excluding LF delimiter).
  Retired inventory entries without records are :retired; with any record they
  are :ambiguous (contradictory current/retired evidence), never admitted.
  Coverage consumers: excursions.clj:98 treats non-:read as :unread-new;
  :122-158 excludes those open items and reports their status; :242 treats
  them as unread. reread_queue.clj:133-154 queues unread-new and :188 only
  uses an old sha for :read. consume itself admits only :read below and counts
  retired members as incomplete, not silently read.
  Optional :target-id checks only that member, retaining duplicate detection;
  it is a per-item certificate, never a complete population certificate.
  Optional :checker-env is merged over the process environment for every
  checker invocation (e.g. {\"GIRAFFE_CHECK_CODE_ROOT\" ...} for a fixture
  code root); omitted, the checker sees the process environment unchanged."
  [{:keys [checker members-file batches repos scope target-id checker-env]}]
  (let [checker-text (slurp checker)
        members-text (slurp members-file)
        checker-members (io/file (.getParentFile (io/file checker)) "members.json")
        _ (when-not (= members-text (slurp checker-members))
            (throw (ex-info "Checker uses a different population" {:kind :reading/population-mismatch})))
        members (json/parse-string members-text true)
        ids (set (map (comp keyword :id) members))
        checked (check-records checker
                  (mapcat (fn [batch]
                            (keep-indexed (fn [i line]
                                            (when (and (not (str/blank? line))
                                                       (or (nil? target-id)
                                                           (= target-id (:id (json/parse-string line true)))))
                                              {:batch batch :line-number (inc i) :line line}))
                                          (str/split (slurp batch) #"\n"))) batches)
                  checker-env)
        _ (when-not (and (= checker-text (slurp checker))
                         (= members-text (slurp members-file)))
            (throw (ex-info "Checker or population changed during validation"
                            {:kind :reading/validation-input-changed})))
        groups (group-by #(get-in % [:record :id]) checked)
        rows (mapv (fn [member]
                     (let [rs (get groups (:id member)) r (first rs)]
                       {:id (:id member) :source (select-keys member [:path :sha256])
                        :retired (:retired member)
                        :status (cond (and (:retired member) (seq rs)) :ambiguous
                                      (:retired member) :retired
                                      (> (count rs) 1) :ambiguous
                                      (:passed? r) :read :else :unread)
                        :record-pin (:record-pin r)
                        :record-pins (mapv :record-pin rs)
                        :checker-results (mapv :checker-result rs)
                        :edge-count (when (and (:passed? r) (not (:retired member))) (count (get-in r [:record :edges])))}))
                   (if target-id (filter #(= target-id (:id %)) members) members))
        readable (set (map :id (filter #(= :read (:status %)) rows)))
        admitted (filter #(contains? readable (get-in % [:record :id])) checked)
        converted (mapcat (fn [{:keys [record]}]
                            (map #(convert-edge repos ids (:id record) %) (:edges record))) admitted)
        unresolved (vec (mapcat (fn [{:keys [record]}]
                                  (map #(assoc % :reading-member (:id record)) (:unresolved record))) admitted))
        counts (frequencies (map :status rows))
        certificate {:schema :giraffe/reading-coverage-v1
                     :scope (or scope :population/missions-excursions)
                     :population-sha (sha256 members-text)
                     :checker-sha (sha256 checker-text)
                     :record-byte-convention :utf8-line-without-lf
                     :status (if target-id :partial
                               (if (= (count members) (:read counts)) :complete :incomplete))
                     :member-count (count members) :counts counts :members rows}]
    {:coverage certificate :checked-records checked
     :edges (vec (keep :edge converted)) :links (vec (keep :link converted))
     :out-of-scope (vec (keep :out-of-scope converted))
     :rejected (vec (keep :rejected converted)) :unresolved unresolved}))
