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

(defn- checked-record [checker batch line-number line]
  (let [file (java.io.File/createTempFile "giraffe-reading-" ".jsonl")
        pin {:sha256 (sha256 line) :batch (str batch) :line line-number}]
    (try
      (spit file (str line "\n"))
      (let [result (shell/sh "python3" "-I" (str checker) (.getPath file))
            record (try (json/parse-string line true) (catch Exception _ nil))]
        {:record record :record-pin pin
         :checker-result (select-keys result [:exit :out :err])
         :passed? (and (zero? (:exit result)) (map? record))})
      (finally (.delete file)))))

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
                       {:from from :to to :kind "needs" :class :closure
                        :file file :line (:line evidence) :quote (:quote evidence)
                        :note (:note edge)})]
        (if-let [pass-edge (:edge converted)]
          {:edge (assoc pass-edge :basis (keyword (:basis edge))
                                  :reading-kind (keyword (:kind edge))
                                  :reading-member owner :evidence-pin evidence)}
          {:rejected (assoc retained :reason (get-in converted [:reject :reject]))})))))

(defn consume
  "Each record invokes the exact checker. Duplicate members are ambiguous even
  when each individual record passes. Certificate hashes bind checker, member
  inventory and the exact JSONL line bytes (excluding LF delimiter)."
  [{:keys [checker members-file batches repos]}]
  (let [checker-text (slurp checker)
        members-text (slurp members-file)
        checker-members (io/file (.getParentFile (io/file checker)) "members.json")
        _ (when-not (= members-text (slurp checker-members))
            (throw (ex-info "Checker uses a different population" {:kind :reading/population-mismatch})))
        members (json/parse-string members-text true)
        ids (set (map (comp keyword :id) members))
        checked (vec (mapcat (fn [batch]
                              (keep-indexed (fn [i line]
                                              (when-not (str/blank? line)
                                                (checked-record checker batch (inc i) line)))
                                            (str/split (slurp batch) #"\n"))) batches))
        _ (when-not (and (= checker-text (slurp checker))
                         (= members-text (slurp members-file)))
            (throw (ex-info "Checker or population changed during validation"
                            {:kind :reading/validation-input-changed})))
        groups (group-by #(get-in % [:record :id]) checked)
        rows (mapv (fn [member]
                     (let [rs (get groups (:id member)) r (first rs)]
                       {:id (:id member) :source (select-keys member [:path :sha256])
                        :status (cond (> (count rs) 1) :ambiguous
                                      (:passed? r) :read :else :unread)
                        :record-pin (:record-pin r)
                        :record-pins (mapv :record-pin rs)
                        :checker-results (mapv :checker-result rs)
                        :edge-count (when (:passed? r) (count (get-in r [:record :edges])))})) members)
        readable (set (map :id (filter #(= :read (:status %)) rows)))
        admitted (filter #(contains? readable (get-in % [:record :id])) checked)
        converted (mapcat (fn [{:keys [record]}]
                            (map #(convert-edge repos ids (:id record) %) (:edges record))) admitted)
        unresolved (vec (mapcat (fn [{:keys [record]}]
                                  (map #(assoc % :reading-member (:id record)) (:unresolved record))) admitted))
        counts (frequencies (map :status rows))
        certificate {:schema :giraffe/reading-coverage-v1
                     :scope :population/missions-excursions
                     :population-sha (sha256 members-text)
                     :checker-sha (sha256 checker-text)
                     :record-byte-convention :utf8-line-without-lf
                     :status (if (= (count members) (:read counts)) :complete :incomplete)
                     :member-count (count members) :counts counts :members rows}]
    {:coverage certificate :checked-records checked
     :edges (vec (keep :edge converted)) :links (vec (keep :link converted))
     :out-of-scope (vec (keep :out-of-scope converted))
     :rejected (vec (keep :rejected converted)) :unresolved unresolved}))
