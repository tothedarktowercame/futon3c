(ns futon3c.apm.authority-port
  "Canonical resolver for every path and revision crossing an APM boundary."
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str])
  (:import [java.nio.file Files LinkOption Path]))

(def path-roots
  {:role-card :control-root
   :contract :control-root
   :manifest :control-root
   :generated-contract :control-root
   :coordinator-state :campaign-root
   :ledger :campaign-root
   :qualification-report :qualification-root
   :problem-file :workspace})

(defn resolve-path
  [authority root-kind path-kind path]
  (let [expected-root (get path-roots path-kind)
        root (get authority root-kind)]
    (cond
      (nil? expected-root)
      {:ok false :error/code :authority-path-kind-unknown
       :path-kind path-kind}

      (not= expected-root root-kind)
      {:ok false :error/code :authority-root-kind-mismatch
       :path-kind path-kind :expected-root-kind expected-root
       :observed-root-kind root-kind}

      (not (string? root))
      {:ok false :error/code :authority-root-missing :root-kind root-kind}

      (not (string? path))
      {:ok false :error/code :authority-path-invalid :path-kind path-kind}

      :else
      (let [candidate (Path/of path (make-array String 0))
            resolved (-> (if (.isAbsolute candidate)
                           candidate
                           (.resolve (Path/of root (make-array String 0)) candidate))
                         .normalize .toAbsolutePath)]
        {:ok true :path-kind path-kind :root-kind root-kind
         :path (str resolved)}))))

(defn require-path
  [authority path-kind path]
  (let [root-kind (get path-roots path-kind)
        resolved (resolve-path authority root-kind path-kind path)]
    (if-not (:ok resolved)
      resolved
      (if (Files/exists (Path/of (:path resolved) (make-array String 0))
                        (make-array LinkOption 0))
        resolved
        (assoc resolved :ok false :error/code :authority-path-missing)))))

(defn resolve-revision
  [authority revision-kind]
  (let [root-kind (case revision-kind
                    :control :control-root
                    :corpus :qualification-root
                    nil)
        root (get authority root-kind)]
    (if-not (and root-kind (string? root))
      {:ok false :error/code :authority-revision-kind-unknown
       :revision-kind revision-kind}
      (let [head (shell/sh "git" "-C" root "rev-parse" "HEAD")
            branch (shell/sh "git" "-C" root "branch" "--show-current")]
        (if (and (zero? (:exit head)) (zero? (:exit branch))
                 (= "master" (str/trim (:out branch))))
          {:ok true :revision-kind revision-kind :root-kind root-kind
           :revision (str/trim (:out head))}
          {:ok false :error/code :authority-revision-not-qualified
           :revision-kind revision-kind :root-kind root-kind
           :branch (str/trim (:out branch))})))))

(defn require-dispatch-paths
  [authority entries]
  (let [results (mapv (fn [[path-kind path]]
                        (require-path authority path-kind path))
                      entries)]
    (or (first (remove :ok results))
        {:ok true :paths (into {} (map (juxt :path-kind :path) results))})))
