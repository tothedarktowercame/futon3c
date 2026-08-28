(ns futon3c.apm.workspace-build
  "Build local modules imported by a problem before probing its workspace."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]))

(defn imported-modules
  "Return module names from Lean import commands in SOURCE, in source order."
  [source]
  (->> (str/split-lines (or source ""))
       (map #(first (str/split % #"--" 2)))
       (keep (fn [line]
               (when-let [[_ imports] (re-matches #"\s*import\s+(.+?)\s*" line)]
                 (str/split imports #"\s+"))))
       (apply concat)
       distinct
       vec))

(defn module-path [module]
  (str (str/replace module "." "/") ".lean"))

(defn local-imports
  "Keep imports whose source module exists in WORKSPACE.

  Package modules are absent from the checkout source tree and therefore need
  no local build. A misspelled or absent module is deliberately left for the
  subsequent Lean probe to reject with its native diagnostic."
  [workspace problem-path]
  (let [problem (io/file workspace problem-path)]
    (when (.isFile problem)
      (->> (imported-modules (slurp problem))
           (filter #(-> (io/file workspace (module-path %)) .isFile))
           vec))))

(defn bootstrap!
  "Build each checkout-local direct import of the leased problem.

  RUN-FN receives WORKSPACE and argv, and defaults to `shell/sh`."
  ([lease]
   (bootstrap! lease (fn [workspace argv]
                       (apply shell/sh (concat argv [:dir workspace])))))
  ([lease run-fn]
   (let [workspace (:workspace/path lease)
         problem-path (:problem/path lease)
         modules (local-imports workspace problem-path)]
     (cond
       (not (and (string? workspace) (string? problem-path)))
       {:ok false :error/code :workspace-bootstrap-input-invalid}

       (nil? modules)
       {:ok false :error/code :workspace-bootstrap-problem-missing
        :problem/path problem-path}

       :else
       (loop [[module & remaining] modules, built []]
         (if-not module
           {:ok true :built/modules built}
           (let [result (run-fn workspace ["lake" "build" module])]
             (if (zero? (:exit result))
               (recur remaining (conj built module))
               {:ok false :error/code :workspace-bootstrap-failed
                :module module :finding result}))))))))

(defn probe!
  "Build checkout-local imports, then elaborate the pinned problem file.

  The returned process map is deliberately the Lean process result: callers
  must continue to reject a genuine nonzero elaboration exit. Bootstrap
  failures are returned as nonzero probe results with the typed bootstrap
  failure retained under `:bootstrap`."
  ([lease]
   (probe! lease (fn [workspace argv]
                   (apply shell/sh (concat argv [:dir workspace])))))
  ([lease run-fn]
   (let [bootstrapped (bootstrap! lease run-fn)]
     (if-not (:ok bootstrapped)
       {:exit (or (get-in bootstrapped [:finding :exit]) 1)
        :out ""
        :err (or (get-in bootstrapped [:finding :err])
                 (pr-str bootstrapped))
        :bootstrap bootstrapped}
       (assoc (run-fn (:workspace/path lease)
                      ["lake" "env" "lean" (:problem/path lease)])
              :built/modules (:built/modules bootstrapped))))))
