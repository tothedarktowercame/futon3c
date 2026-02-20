(ns futon3c.architecture.mission-evidence-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(defn- repo-root []
  (io/file (System/getProperty "user.dir")))

(defn- read-edn
  [rel-path]
  (-> (io/file (repo-root) rel-path) slurp edn/read-string))

(defn- rel-file-exists?
  [rel-path]
  (.exists (io/file (repo-root) rel-path)))

(defonce !changed-files-cache (atom {}))

(defn- changed-files-for-commit
  [sha]
  (if-let [cached (get @!changed-files-cache sha)]
    cached
    (let [{:keys [exit out err]} (sh/sh "git" "show" "--name-only" "--pretty=format:" sha)
          value (when (zero? exit)
                  (->> (str/split-lines out)
                       (remove str/blank?)
                       set))]
      (when-not (zero? exit)
        (throw (ex-info (str "git show failed for commit " sha)
                        {:commit sha :exit exit :err err})))
      (swap! !changed-files-cache assoc sha value)
      value)))

(defn- evidence-modes
  [entries]
  (set (concat
        (when (some #(seq (:commit %)) entries) [:commit])
        (when (some #(seq (:tests %)) entries) [:test])
        (when (some #(seq (:scripts %)) entries) [:script])
        (when (some #(seq (:artifacts %)) entries) [:artifact]))))

(deftest mission-claims-and-evidence-load
  (let [claims-doc (read-edn "docs/mission-claims.edn")
        evidence-doc (read-edn "docs/mission-evidence.edn")]
    (is (= 1 (:contract/version claims-doc)))
    (is (= 1 (:evidence/version evidence-doc)))
    (is (vector? (:claims claims-doc)))
    (is (vector? (:entries evidence-doc)))))

(deftest mission-every-claim-has-required-evidence
  (let [{:keys [claims]} (read-edn "docs/mission-claims.edn")
        {:keys [entries]} (read-edn "docs/mission-evidence.edn")
        entries-by-claim (group-by :claim/id entries)
        claim-ids (mapv :claim/id claims)]
    (is (= (count claim-ids) (count (set claim-ids)))
        "Mission claim IDs must be unique.")
    (doseq [claim claims]
      (let [cid (:claim/id claim)
            c-entries (get entries-by-claim cid)
            required (:verification/required claim)
            present (evidence-modes c-entries)]
        (is (seq c-entries)
            (str "Mission claim has no evidence entries: " cid))
        (is (set/subset? required present)
            (str "Mission claim missing required evidence modes: " cid
                 " required=" required " present=" present))))))

(deftest mission-evidence-references-are-valid
  (let [{:keys [claims]} (read-edn "docs/mission-claims.edn")
        {:keys [entries]} (read-edn "docs/mission-evidence.edn")
        claims-by-id (into {} (map (juxt :claim/id identity) claims))]
    (doseq [entry entries]
      (let [cid (:claim/id entry)
            claim (get claims-by-id cid)]
        (is (some? claim)
            (str "Mission evidence references unknown claim: " cid))
        (is (re-matches #"[0-9a-f]{7,40}" (:commit entry))
            (str "Mission evidence commit must be SHA-like: " (:evidence/id entry)))
        (doseq [p (:implementation/files claim)]
          (is (rel-file-exists? p)
              (str "Missing mission implementation file in claim " cid ": " p)))
        (doseq [p (:tests entry)]
          (is (rel-file-exists? p)
              (str "Missing mission test evidence file in " (:evidence/id entry) ": " p)))
        (doseq [p (:scripts entry)]
          (is (rel-file-exists? p)
              (str "Missing mission script evidence file in " (:evidence/id entry) ": " p)))
        (doseq [p (:artifacts entry)]
          (is (rel-file-exists? p)
              (str "Missing mission artifact evidence file in " (:evidence/id entry) ": " p)))))))

(deftest mission-evidence-commit-touches-claim-surface
  (let [{:keys [claims]} (read-edn "docs/mission-claims.edn")
        {:keys [entries]} (read-edn "docs/mission-evidence.edn")
        claims-by-id (into {} (map (juxt :claim/id identity) claims))]
    (doseq [entry entries]
      (let [claim (get claims-by-id (:claim/id entry))
            changed (changed-files-for-commit (:commit entry))
            witness-files (set (concat (:implementation/files claim)
                                       (:tests entry)
                                       (:scripts entry)
                                       (:artifacts entry)))
            touched? (boolean (seq (set/intersection changed witness-files)))]
        (is touched?
            (str "Mission evidence commit does not touch claim surface: "
                 (:evidence/id entry)
                 " commit=" (:commit entry)))))))
