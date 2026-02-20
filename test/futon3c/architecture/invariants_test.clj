(ns futon3c.architecture.invariants-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn- repo-root []
  (io/file (System/getProperty "user.dir")))

(defn- clj-files-under
  "Return .clj files under a repo-relative directory, or [] if missing."
  [rel-dir]
  (let [dir (io/file (repo-root) rel-dir)]
    (if (.exists dir)
      (->> (file-seq dir)
           (filter #(.isFile ^java.io.File %))
           (filter #(str/ends-with? (.getName ^java.io.File %) ".clj")))
      [])))

(defn- file-text [^java.io.File f]
  (slurp f))

(defn- find-files-matching
  [files pred]
  (->> files
       (filter (fn [^java.io.File f] (pred (file-text f))))
       (map #(.getPath ^java.io.File %))
       sort
       vec))

(deftest transport-does-not-spawn-processes-or-shell-out
  (testing "Transport adapter code stays routing-only (no ProcessBuilder / shell)."
    (let [transport-files (clj-files-under "src/futon3c/transport")
          offenders (find-files-matching
                     transport-files
                     #(or (re-find #"ProcessBuilder" %)
                          (re-find #"clojure\.java\.shell" %)
                          (re-find #"\bsh/sh\b" %)))]
      (is (empty? offenders)
          (str "Transport code must not spawn or shell out. Offenders: " offenders)))))

(deftest repo-does-not-spawn-claude-or-codex-subprocesses
  (testing "Agent identity is singular: no subprocess impersonation of Claude/Codex."
    (let [source-files (concat (clj-files-under "src") (clj-files-under "dev"))
          ;; Match only concrete spawn command patterns, not incidental comments.
          direct-spawn-re #"(?is)ProcessBuilder\.\s*\[\s*\"(?:claude|codex)\""
          bash-spawn-re #"(?is)ProcessBuilder\.[^\n]*\[\s*\"bash\"\s*,\s*\"-c\"\s*,\s*\"[^\"]*\b(?:claude|codex)\b"
          offenders (find-files-matching
                     source-files
                     #(or (re-find direct-spawn-re %)
                          (re-find bash-spawn-re %)))]
      (is (empty? offenders)
          (str "No source file should mix ProcessBuilder with Claude/Codex tokens. Offenders: " offenders)))))

(deftest deps-do-not-point-at-futon3-repo
  (testing "No direct local dependency on ../futon3."
    (let [deps-file (io/file (repo-root) "deps.edn")
          deps-text (if (.exists deps-file) (slurp deps-file) "")
          has-futon3-local-root? (boolean (re-find #":local/root\s+\"(\.\./)?futon3\"" deps-text))]
      (is (false? has-futon3-local-root?)
          "deps.edn must not reference the futon3 repo as a direct local/root dependency."))))
