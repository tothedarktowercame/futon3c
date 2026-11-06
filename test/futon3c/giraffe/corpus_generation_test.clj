(ns futon3c.giraffe.corpus-generation-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.giraffe.corpus-generation :as generation])
  (:import [java.nio.file CopyOption Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute]
           [java.util UUID]))

(defn- temp-dir [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- checker-text []
  (str "#!/usr/bin/env python3\n"
       "import json, pathlib, sys\n"
       "members={x['id']:x for x in json.loads((pathlib.Path(__file__).parent/'members.json').read_text())}\n"
       "bad=False\n"
       "for line in pathlib.Path(sys.argv[1]).read_text().splitlines():\n"
       " r=json.loads(line); m=members.get(r.get('id')); bad = bad or r.get('reject',False) or not m or r.get('source') != {'path':m['path'],'sha256':m['sha256']}\n"
       "sys.exit(1 if bad else 0)\n"))

(defn- record [source & [reject?]]
  (cond-> {:id "M-one" :source source :status_as_read "open"
           :purpose "test" :edges [] :unresolved []}
    reject? (assoc :reject true)))

(defn- install-pointer! [pointer target]
  (let [p (.toPath (io/file pointer))
        temp (.resolve (.getParent p) (str ".next-" (UUID/randomUUID)))]
    (Files/createSymbolicLink temp (.toPath (.getAbsoluteFile (io/file target)))
                              (make-array FileAttribute 0))
    (Files/move temp p (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                               StandardCopyOption/REPLACE_EXISTING]))))

(defn- fixture []
  (let [root (temp-dir "corpus-generation-")
        generations (io/file root "generations")
        base (io/file generations "base")
        source (io/file root "M-one.md")
        pointer (io/file root "corpus")
        source-pin {:path (.getAbsolutePath source) :sha256 nil}]
    (.mkdirs base)
    (spit source "old source\n")
    (let [source-pin (assoc source-pin :sha256 (generation/file-sha source))]
      (spit (io/file base "members.json")
            (json/generate-string [(assoc source-pin :id "M-one")]))
      (spit (io/file base "check_edges.py") (checker-text))
      (spit (io/file base "batch-00.jsonl")
            (str (json/generate-string (record source-pin)) "\n"))
      (Files/createSymbolicLink (.toPath pointer) (.toPath (.getAbsoluteFile base))
                                (make-array FileAttribute 0))
      {:root root :generations generations :base base :source source
       :source-pin source-pin :pointer pointer})))

(defn- publish! [{:keys [pointer generations source source-pin]} replacement & [extra]]
  (generation/publish-generation!
   (merge {:pointer pointer :generations-dir generations
           :claim {:target-id "M-one" :source-path (.getAbsolutePath source)
                   :source-sha (:sha256 source-pin)}
           :replacement replacement}
          extra)))

(deftest replacement-publishes-immutable-generation-and-old-reader-remains
  (let [{:keys [pointer base source-pin] :as f} (fixture)
        old-path (:path (generation/resolve-generation pointer))
        old-bytes (slurp (io/file old-path "batch-00.jsonl"))
        result (publish! f (assoc (record source-pin) :purpose "replacement"))]
    (is (= :published (:status result)))
    (is (not= old-path (:path (generation/resolve-generation pointer))))
    (is (= old-bytes (slurp (io/file base "batch-00.jsonl"))))
    (testing "a reader holding the resolved old path is unaffected by switch"
      (is (= "test" (:purpose (json/parse-string
                                (str (first (line-seq (io/reader
                                                      (io/file old-path "batch-00.jsonl")))))
                                true)))))))

(deftest changed-source-rejects-and-keeps-pointer
  (let [{:keys [pointer source source-pin] :as f} (fixture)
        before (:path (generation/resolve-generation pointer))]
    (spit source "changed after claim\n")
    (let [result (publish! f (record source-pin))]
      (is (= :giraffe-generation/source-changed (:reason result)))
      (is (= before (:path (generation/resolve-generation pointer)))))))

(deftest concurrent-publisher-base-mismatch-keeps-winner
  (let [{:keys [pointer generations base source-pin] :as f} (fixture)
        winner (io/file generations "winner")]
    (#'generation/copy-tree! base winner)
    (let [result (publish! f (assoc (record source-pin) :purpose "loser")
                           {:before-switch-fn #(install-pointer! pointer winner)})]
      (is (= :giraffe-generation/base-mismatch (:reason result)))
      (is (= (.getCanonicalPath winner)
             (:path (generation/resolve-generation pointer)))))))

(deftest checker-failure-rejects-cleans-staging-and-keeps-pointer
  (let [{:keys [pointer generations source-pin] :as f} (fixture)
        before (:path (generation/resolve-generation pointer))
        result (publish! f (record source-pin true))
        staging (io/file generations ".staging")]
    (is (= :giraffe-generation/checker-failed (:reason result)))
    (is (= before (:path (generation/resolve-generation pointer))))
    (is (empty? (or (seq (.listFiles staging)) [])))))

(deftest migration-dry-run-reports-without-writing
  (let [{:keys [root base]} (fixture)
        legacy (io/file root "legacy-corpus")]
    (#'generation/copy-tree! base legacy)
    (let [before (set (map #(.getAbsolutePath %) (file-seq root)))
          report (generation/migrate! {:pointer legacy :dry-run? true})
          after (set (map #(.getAbsolutePath %) (file-seq root)))]
      (is (= :dry-run (:status report)))
      (is (= before after))
      (is (not (.exists (io/file (str (.getAbsolutePath legacy) ".generations"))))))))
