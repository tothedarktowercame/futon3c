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

(defn- snapshot-binding [head-sha changes files targets axiom-audits]
  {:head-sha head-sha
   :changes-digest (sha256 (pr-str (canonical changes)))
   :files-digest (sha256 (pr-str (canonical files)))
   :targets-digest (sha256 (pr-str (canonical targets)))
   :axiom-audits-digest (sha256 (pr-str (canonical axiom-audits)))})

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

(defn check
  "Checks registration evidence against an exact repository snapshot.

  `changes` are the typed records emitted by the rebuild planner. `files` maps
  relative paths to exact content; `targets` is parsed targets.edn data; and
  `axiom-audits` maps module names to typed evidence bound to `head-sha`."
  [{:keys [head-sha turn changes files targets axiom-audits]}]
  (let [snapshot (snapshot-binding head-sha changes files targets axiom-audits)
        changed (->> changes
                     (filter #(and (str/starts-with? (:path %) "ConstructionTargets/")
                                   (str/ends-with? (:path %) ".lean")
                                   (changed-kind? (:kind %))))
                     vec)
        rollup (get files "ConstructionTargets.lean" "")
        partials (partial-modules (get files "ConstructionTargets/PARTIAL.md")
                                  snapshot)
        ledger (ledger-index targets snapshot)]
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
          (when-not (= turn (:created-turn record))
            (refuse! snapshot :target-created-turn-mismatch
                     {:module module :expected turn :record record})))))
    {:outcome :green
     :snapshot snapshot
     :checked-modules (set (map (comp path->module :path) changed))}))
