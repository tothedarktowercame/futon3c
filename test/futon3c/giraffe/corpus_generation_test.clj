(ns futon3c.giraffe.corpus-generation-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.giraffe.corpus-generation :as generation]
            [futon3c.giraffe.pass :as gp]
            [futon3c.giraffe.readings :as readings]
            [futon3c.giraffe.run :as run])
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

(deftest resolved-record-pin-and-pre-publish-run-survive-pointer-switch
  (let [{:keys [root pointer source-pin] :as f} (fixture)
        resolved (:path (generation/resolve-generation pointer))
        consumed (readings/consume
                  {:checker (io/file resolved "check_edges.py")
                   :members-file (io/file resolved "members.json")
                   :batches [(io/file resolved "batch-00.jsonl")]
                   :repos []})
        pin (get-in consumed [:coverage :members 0 :record-pin])
        input {:as-of "before-publish" :items [] :edges []}
        pass-output (assoc (gp/pass input) :fixture-record-pin pin)
        written (run/write-run!
                 {:root (io/file root "runs") :run-id "before-publish"
                  :adapter-result {:input input :rejected []
                                   :readings-generation
                                   (generation/resolve-generation pointer)}
                  :pass-output pass-output
                  :git-sha "e90fe14bb3c181964f8d8b239e5bff16225dff37"})]
    (testing "new pins name the resolved immutable generation, not the pointer"
      (is (.startsWith (str (:batch pin)) (str resolved)))
      (is (not (.startsWith (str (:batch pin)) (.getAbsolutePath pointer)))))
    (is (:ok? (run/verify-run (:dir written))))
    (is (= :published
           (:status (publish! f (assoc (record source-pin) :purpose "new")))))
    (testing "verification seals run-local bytes and never follows the old batch pin"
      (is (:ok? (run/verify-run (:dir written)))))))

(deftest changed-source-rejects-and-keeps-pointer
  (let [{:keys [pointer source source-pin] :as f} (fixture)
        before (:path (generation/resolve-generation pointer))]
    (spit source "changed after claim\n")
    (let [result (publish! f (record source-pin))]
      (is (= :giraffe-generation/source-changed (:reason result)))
      (is (= before (:path (generation/resolve-generation pointer)))))))

(deftest never-read-target-adds-a-member-and-validates
  (let [{:keys [pointer generations source source-pin]} (fixture)
        base (:path (generation/resolve-generation pointer))]
    (spit (io/file base "members.json") "[]")
    (spit (io/file base "batch-00.jsonl") "")
    (let [result (generation/publish-generation!
                  {:pointer pointer :generations-dir generations
                   :claim {:target-id "M-one"
                           :source-path (.getAbsolutePath source)
                           :source-sha (:sha256 source-pin)
                           :old-sha nil}
                   :replacement (record source-pin)})
          current (:path (generation/resolve-generation pointer))
          members (json/parse-string (slurp (io/file current "members.json")) true)]
      (is (= :published (:status result)))
      (is (= [{:id "M-one" :path (.getAbsolutePath source)
               :sha256 (:sha256 source-pin)}]
             members)))))

(deftest concurrent-publisher-rebases-revalidates-and-keeps-both-records
  (let [{:keys [root pointer generations source-pin] :as f} (fixture)
        other-source (io/file root "M-two.md")
        calls (atom 0)]
    (spit other-source "other source\n")
    (let [other-pin {:path (.getAbsolutePath other-source)
                     :sha256 (generation/file-sha other-source)}
          publish-other! #(generation/publish-generation!
                           {:pointer pointer :generations-dir generations
                            :claim {:target-id "M-two"
                                    :source-path (:path other-pin)
                                    :source-sha (:sha256 other-pin)
                                    :old-sha nil}
                            :replacement {:id "M-two" :source other-pin
                                          :status_as_read "open" :purpose "winner"
                                          :edges [] :unresolved []}})
          result (publish! f (assoc (record source-pin) :purpose "rebased")
                           {:before-switch-fn #(when (= 1 (swap! calls inc))
                                                (is (= :published (:status (publish-other!)))))})
          current (:path (generation/resolve-generation pointer))
          records (->> ["batch-00.jsonl" "batch-reread.jsonl"]
                       (map #(io/file current %))
                       (filter #(.exists %))
                       (mapcat #(line-seq (io/reader %)))
                       (remove str/blank?)
                       (map #(json/parse-string % true))
                       (into {} (map (juxt :id identity))))]
      (is (= :published (:status result)))
      (is (= 2 (:attempts result)))
      (is (= #{"M-one" "M-two"} (set (keys records))))
      (is (= "rebased" (get-in records ["M-one" :purpose])))
      (is (= "winner" (get-in records ["M-two" :purpose]))))))

(deftest three-consecutive-pointer-moves-return-base-mismatch
  (let [{:keys [pointer generations base source-pin] :as f} (fixture)
        calls (atom 0)
        move! (fn []
                (let [winner (io/file generations (str "winner-" (swap! calls inc)))]
                  (#'generation/copy-tree! base winner)
                  (spit (io/file winner "race-marker") (str @calls))
                  (install-pointer! pointer winner)))
        result (publish! f (assoc (record source-pin) :purpose "loser")
                         {:before-switch-fn move!})]
    (is (= :giraffe-generation/base-mismatch (:reason result)))
    (is (= generation/max-race-attempts (:attempts result)))
    (is (= generation/max-race-attempts @calls))))

(deftest checker-failure-rejects-cleans-staging-and-keeps-pointer
  (let [{:keys [pointer generations source-pin] :as f} (fixture)
        before (:path (generation/resolve-generation pointer))
        result (publish! f (record source-pin true))
        staging (io/file generations ".staging")]
    (is (= :giraffe-generation/checker-failed (:reason result)))
    (is (= ["M-one"] (get-in result [:details :new-failing-record-ids])))
    (is (= before (:path (generation/resolve-generation pointer))))
    (is (empty? (or (seq (.listFiles staging)) [])))))

(deftest correct-replacement-publishes-over-unrelated-base-failure
  (let [{:keys [base pointer source-pin] :as f} (fixture)
        other-source (io/file (:root f) "M-two.md")]
    (spit other-source "other source\n")
    (let [other-pin {:id "M-two" :path (.getAbsolutePath other-source)
                     :sha256 (generation/file-sha other-source)}]
      (spit (io/file base "members.json")
            (json/generate-string [(assoc source-pin :id "M-one") other-pin]))
      (spit (io/file base "batch-00.jsonl")
            (str (json/generate-string (record source-pin)) "\n"
                 (json/generate-string
                  (assoc (record (dissoc other-pin :id) true) :id "M-two")) "\n"))
      (let [result (publish! f (assoc (record source-pin) :purpose "replacement"))]
        (is (= :published (:status result)))
        (is (not= (.getCanonicalPath base)
                  (:path (generation/resolve-generation pointer))))))))

(deftest failing-id-comparison-rejects-a-new-other-member-regression
  ;; apply-replacement! changes only the target's record and member pin, so it
  ;; cannot currently break a different member. Pin the ID-level rule directly
  ;; so a future broader staging transform cannot hide one new break behind one
  ;; repaired record.
  (is (= #{"M-two"}
         (#'generation/new-failing-record-ids #{"M-one"} #{"M-two"}))))

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
