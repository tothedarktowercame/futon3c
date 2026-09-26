(ns futon3c.apm.library-loop-targets
  "Pure registration gate for changed ConstructionTargets."
  (:require [clojure.string :as str])
  (:import (java.nio.charset StandardCharsets)
           (java.security MessageDigest)))

(defn- sha256 [content]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes (str content) StandardCharsets/UTF_8))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn- canonical [value]
  (cond
    (map? value) (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
                       (map (fn [[k v]] [k (canonical v)])) value)
    (set? value) (vec (sort-by pr-str (map canonical value)))
    (sequential? value) (mapv canonical value)
    :else value))

(defn- snapshot-binding [head-sha changes files targets axiom-audits
                         target-provenance]
  {:head-sha head-sha
   :changes-digest (sha256 (pr-str (canonical changes)))
   :files-digest (sha256 (pr-str (canonical files)))
   :targets-digest (sha256 (pr-str (canonical targets)))
   :axiom-audits-digest (sha256 (pr-str (canonical axiom-audits)))
   :target-provenance-digest
   (sha256 (pr-str (canonical target-provenance)))})

(defn- refuse! [snapshot finding data]
  (throw (ex-info (name finding)
                  (assoc data :finding finding :snapshot snapshot))))

(defn- path->module [path]
  (-> path (str/replace #"\.lean$" "") (str/replace "/" ".")))

(defn- new-kind? [kind]
  (or (= "A" kind) (= "??" kind) (= "R-new" kind)))

(defn- changed-kind? [kind]
  (or (new-kind? kind) (= "M" kind) (str/includes? kind "M")))

(defn- partial-modules [content snapshot]
  (let [rows (->> (str/split-lines (or content ""))
                  (keep #(second (re-matches
                                  #"\|\s*`(ConstructionTargets\.[A-Za-z0-9_.]+)`\s*\|.*"
                                  %))))
        duplicates (->> rows frequencies (filter (fn [[_ n]] (> n 1))) keys seq)]
    (when duplicates
      (refuse! snapshot :ambiguous-partial-records {:modules (set duplicates)}))
    (set rows)))

(defn- ledger-index [targets snapshot]
  (when-not (vector? targets)
    (refuse! snapshot :malformed-targets-ledger {:targets targets}))
  (doseq [row targets]
    (when-not (and (map? row)
                   (string? (:module row))
                   (pos-int? (:created-turn row))
                   (keyword? (:status row))
                   (or (keyword? (:obligation row))
                       (and (string? (:obligation row))
                            (not (str/blank? (:obligation row))))))
      (refuse! snapshot :malformed-target-record {:record row})))
  (let [groups (group-by :module targets)]
    (when-let [[module rows] (first (filter (fn [[_ rows]] (not= 1 (count rows)))
                                           groups))]
      (refuse! snapshot :ambiguous-target-record {:module module :records rows}))
    (into {} (map (fn [[module rows]] [module (first rows)])) groups)))

(defn- valid-delayed-provenance?
  [problem-id module created-turn provenance]
  (and (= 1 (:schema provenance))
       (= problem-id (:problem-id provenance))
       (= module (:module provenance))
       (= created-turn (:created-turn provenance))
       (string? (:turn-receipt-id provenance))
       (re-matches #"[0-9a-f]{64}" (:turn-receipt-id provenance))
       (string? (:turn-head-sha provenance))
       (re-matches #"[0-9a-f]{40}" (:turn-head-sha provenance))
       (string? (:creation-commit-sha provenance))
       (re-matches #"[0-9a-f]{40}" (:creation-commit-sha provenance))
       (true? (:first-observed-at-turn? provenance))))

(def ^:private problem-id-pattern
  #"(?i)\b([a-z][a-z0-9-]*[0-9][a-z0-9-]*)\b")

(defn- rollup-consumer-claims [rollup]
  (->> (str/split-lines (or rollup ""))
       (keep (fn [line]
               (when-let [[_ short-name consumers]
                          (re-matches
                           #"\s*\|\s*`([A-Za-z0-9_.]+)`\s*\|\s*(.*?)\s*\|\s*"
                           line)]
                 (when-not (= "*(none)*" (str/trim consumers))
                   (for [[_ problem-id] (re-seq problem-id-pattern consumers)]
                     {:module (str "ConstructionTargets." short-name)
                      :problem-id problem-id})))))
       (apply concat)
       distinct
       (sort-by (juxt :module :problem-id))
       vec))

(defn- validate-rollup-evidence! [files snapshot]
  (let [rollup (get files "ConstructionTargets.lean" "")]
    (doseq [{:keys [module problem-id]} (rollup-consumer-claims rollup)]
      (let [main-path (str "problems/" problem-id "/lean/Main.lean")
            main (get files main-path)
            referenced? (and (string? main)
                             (str/includes? main module))]
        (when-not referenced?
          (refuse! snapshot :construction-target-consumer-evidence-mismatch
                   {:diagnostic {:module module
                                 :problem-id problem-id
                                 :main-path main-path}}))))
    (when-let [[_ status]
               (re-find #"(?s)## Status[^\n]*\n(.*?)(?=\n## |\z)" rollup)]
      (when (and (re-find #"(?i)\bintend(?:ed)?\b" status)
                 (not (re-find #"(?i)\bverified at committed HEAD\b" status)))
        (refuse! snapshot :construction-target-production-status-non-evidentiary
                 {:diagnostic {:section :construction-targets/status
                               :required-evidence :verified-at-committed-head}})))))

(defn check
  "Checks registration evidence against an exact repository snapshot.

  `changes` are the typed records emitted by the rebuild planner. `files` maps
  relative paths to exact content; `targets` is parsed targets.edn data; and
  `axiom-audits` maps module names to typed evidence bound to `head-sha`."
  [{:keys [problem-id head-sha turn changes files targets axiom-audits
           target-provenance]}]
  (let [snapshot (snapshot-binding head-sha changes files targets axiom-audits
                                   target-provenance)
        changed (->> changes
                     (filter #(and (str/starts-with? (:path %) "ConstructionTargets/")
                                   (str/ends-with? (:path %) ".lean")
                                   (changed-kind? (:kind %))))
                     vec)
        rollup (get files "ConstructionTargets.lean" "")
        partials (partial-modules (get files "ConstructionTargets/PARTIAL.md")
                                  snapshot)
        ledger (ledger-index targets snapshot)]
    (validate-rollup-evidence! files snapshot)
    (doseq [{:keys [kind path]} changed
            :let [module (path->module path)
                  audit (get axiom-audits module)]]
      (when-not (and (map? audit) (true? (:ok? audit))
                     (= head-sha (:head-sha audit))
                     (seq (:declarations audit)))
        (refuse! snapshot :missing-axiom-audit {:module module :audit audit}))
      (when (new-kind? kind)
        (let [doc-path (str/replace path #"\.lean$" ".md")
              imported? (boolean (re-find
                                  (re-pattern
                                   (str "(?m)^\\s*import\\s+"
                                        (java.util.regex.Pattern/quote module)
                                        "(?:\\s|$)"))
                                  rollup))
              partial? (contains? partials module)
              record (get ledger module)]
          (when-not (and (contains? files doc-path)
                         (string? (get files doc-path))
                         (not (str/blank? (get files doc-path))))
            (refuse! snapshot :missing-target-seam-doc
                     {:module module :path doc-path}))
          (when (= imported? partial?)
            (refuse! snapshot :ambiguous-target-disposition
                     {:module module :rollup? imported? :partial? partial?}))
          (when-not record
            (refuse! snapshot :missing-target-ledger-record {:module module}))
          (let [created-turn (:created-turn record)]
            (cond
              (> created-turn turn)
              (refuse! snapshot :target-created-turn-in-future
                       {:module module :current-turn turn :record record})

              (= created-turn turn) nil

              (not (valid-delayed-provenance?
                    problem-id module created-turn (get target-provenance module)))
              (refuse! snapshot :target-created-turn-provenance-invalid
                       {:module module :current-turn turn :record record
                        :provenance (get target-provenance module)}))))))
    {:outcome :green
     :snapshot snapshot
     :checked-modules (set (map (comp path->module :path) changed))}))
