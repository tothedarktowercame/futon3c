(ns futon3c.apm.library-loop-rebuild
  "Pure ConstructionTargets rebuild-set planning.

  Process execution is intentionally outside this namespace. Git output and
  repository file contents are explicit inputs, making the resulting plan
  deterministic and suitable for inclusion in a gate receipt."
  (:require [clojure.set :as set]
            [clojure.string :as str])
  (:import (java.nio.charset StandardCharsets)
           (java.security MessageDigest)))

(def ^:private ct-path-pattern
  #"^ConstructionTargets/[A-Za-z0-9_./-]+\.lean$")

(def ^:private problem-main-pattern
  #"^problems/[A-Za-z0-9_-]+/lean/Main\.lean$")

(defn- refuse! [finding data]
  (throw (ex-info (name finding) (assoc data :finding finding))))

(defn- sha256 [content]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes (str content) StandardCharsets/UTF_8))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn- ct-path? [path]
  (and (str/starts-with? path "ConstructionTargets/")
       (str/ends-with? path ".lean")))

(defn- validate-ct-path! [path]
  (when-not (re-matches ct-path-pattern path)
    (refuse! :unparseable-construction-target-path {:path path}))
  path)

(defn- parse-name-status-line [line]
  (let [[status & paths] (str/split line #"\t")
        kind (some-> status first str)]
    (cond
      (contains? #{"A" "M" "D"} kind)
      (if (= 1 (count paths))
        [{:kind kind :path (first paths)}]
        (refuse! :unparseable-name-status {:line line}))

      (contains? #{"R" "C"} kind)
      (if (= 2 (count paths))
        [{:kind (str kind "-old") :path (first paths)}
         {:kind (str kind "-new") :path (second paths)}]
        (refuse! :unparseable-name-status {:line line}))

      :else (refuse! :unsupported-name-status {:line line :status status}))))

(defn parse-name-status
  "Parses `git diff --name-status` output. Rename/copy records contribute both
  old and new paths."
  [text]
  (->> (str/split-lines (or text ""))
       (remove str/blank?)
       (mapcat parse-name-status-line)
       vec))

(defn- parse-porcelain-line [line]
  (when (< (count line) 4)
    (refuse! :unparseable-porcelain {:line line}))
  (let [status (subs line 0 2)
        body (subs line 3)]
    (when (or (str/includes? body "\"")
              (str/blank? body))
      (refuse! :unparseable-porcelain {:line line}))
    (if (or (str/includes? status "R") (str/includes? status "C"))
      (let [[old new] (str/split body #" -> " 2)]
        (when-not (and old new)
          (refuse! :unparseable-porcelain {:line line}))
        [{:kind "R-old" :path old} {:kind "R-new" :path new}])
      [{:kind status :path body}])))

(defn parse-porcelain
  "Parses unquoted `git status --porcelain=v1` output. Quoted paths fail
  closed; callers should configure Git's quotePath=false or provide `-z`
  adaptation at the process seam."
  [text]
  (->> (str/split-lines (or text ""))
       (remove str/blank?)
       (mapcat parse-porcelain-line)
       vec))

(defn- path->module [path]
  (-> path (str/replace #"\.lean$" "") (str/replace "/" ".")))

(defn- imports [content]
  (->> (str/split-lines content)
       (mapcat (fn [line]
                 (when-let [[_ imported] (re-matches #"\s*import\s+(.+?)\s*" line)]
                   (str/split imported #"\s+"))))
       set))

(defn- import-index [files]
  (let [lean-files (into {} (filter (fn [[path _]] (str/ends-with? path ".lean")) files))
        modules (into {} (map (fn [[path _]] [(path->module path) path]) lean-files))]
    (doseq [[path content] lean-files
            imported (imports content)
            :when (and (str/starts-with? imported "ConstructionTargets.")
                       (not (contains? modules imported)))]
      (refuse! :unresolved-construction-target-import
               {:path path :import imported}))
    {:modules modules
     :consumers (reduce-kv
                 (fn [index path content]
                   (reduce (fn [m imported]
                             (update m imported (fnil conj #{}) (path->module path)))
                           index
                           (imports content)))
                 {}
                 lean-files)}))

(defn- consumer-closure [consumers seed]
  (loop [seen (set seed) frontier (set seed)]
    (if (empty? frontier)
      seen
      (let [next (set/difference
                  (into #{} (mapcat #(get consumers % #{})) frontier)
                  seen)]
        (recur (set/union seen next) next)))))

(defn- dependency-first-order [consumers nodes]
  (let [node-set (set nodes)
        indegrees
        (reduce-kv
         (fn [result dependency dependents]
           (if (contains? node-set dependency)
             (reduce (fn [m dependent]
                       (if (contains? node-set dependent)
                         (update m dependent inc)
                         m))
                     result dependents)
             result))
         (zipmap node-set (repeat 0))
         consumers)]
    (loop [ordered []
           remaining indegrees
           ready (into (sorted-set)
                       (keep (fn [[node degree]] (when (zero? degree) node)))
                       indegrees)]
      (if-let [node (first ready)]
        (let [[next-degrees next-ready]
              (reduce
               (fn [[degrees available] dependent]
                 (if (contains? degrees dependent)
                   (let [degree (dec (get degrees dependent))]
                     [(assoc degrees dependent degree)
                      (cond-> available (zero? degree) (conj dependent))])
                   [degrees available]))
               [(dissoc remaining node) (disj ready node)]
               (sort (set/intersection node-set (get consumers node #{}))))]
          (recur (conj ordered node) next-degrees next-ready))
        (if (empty? remaining)
          ordered
          (refuse! :cyclic-rebuild-imports {:modules (set (keys remaining))}))))))

(defn plan
  "Builds a deterministic rebuild plan.

  Inputs are exact Git observations and a `{relative-path content}` snapshot.
  `:problem-main` is the problem Main.lean path. The returned commands are
  data; this function never executes them."
  [{:keys [base-sha head-sha name-status porcelain files problem-main]}]
  (when-not (and (string? problem-main)
                 (re-matches problem-main-pattern problem-main))
    (refuse! :malformed-problem-main {:problem-main problem-main}))
  (when-not (and (contains? files problem-main)
                 (string? (get files problem-main)))
    (refuse! :missing-problem-main {:problem-main problem-main}))
  (let [changes (concat (parse-name-status name-status)
                        (parse-porcelain porcelain))
        ct-changes (->> changes
                        (filter (comp ct-path? :path))
                        (map #(update % :path validate-ct-path!))
                        vec)
        deleted (filter #(or (= "D" (:kind %))
                             (= "R-old" (:kind %))) ct-changes)
        genuinely-deleted (filter #(= "D" (:kind %)) deleted)]
    (when-let [change (first genuinely-deleted)]
      (refuse! :deleted-construction-target-requires-review {:change change}))
    (let [{:keys [modules consumers]} (import-index files)
          seed (->> ct-changes (map (comp path->module :path)) set)
          closure (consumer-closure consumers seed)
          missing (->> closure
                       (filter #(and (str/starts-with? % "ConstructionTargets.")
                                     (not (contains? modules %))
                                     (not (some (fn [change]
                                                  (and (= "R-old" (:kind change))
                                                       (= % (path->module (:path change)))))
                                                ct-changes))))
                       seq)]
      (when missing
        (refuse! :unresolved-rebuild-module {:modules (set missing)}))
      (let [main-module (path->module problem-main)
            build-modules (dependency-first-order
                           consumers
                           (->> closure
                                (filter modules)
                                (remove #{main-module})))
            file-digests (into (sorted-map)
                               (map (fn [[path content]] [path (sha256 content)]))
                               (filter (fn [[path _]] (str/ends-with? path ".lean"))
                                       files))
            commands (conj (mapv (fn [module] ["lake" "build" module])
                                 build-modules)
                           ["lake" "env" "lean" problem-main])]
        {:schema 1
         :inputs {:base-sha base-sha
                  :head-sha head-sha
                  :name-status name-status
                  :porcelain porcelain
                  :problem-main problem-main
                  :file-digests file-digests}
         :changes ct-changes
         :seed seed
         :closure closure
         :commands commands}))))
