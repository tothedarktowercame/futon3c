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

(deftest stale-reading-is-source-changed-before-the-checker-runs
  ;; a reading of an older source that would ALSO fail the checker must be
  ;; reported as source-changed (superseded), not checker-failed
  (let [{:keys [source source-pin] :as f} (fixture)]
    (spit source "changed after claim\n")
    (is (= :giraffe-generation/source-changed
           (:reason (publish! f (record source-pin true)))))))

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

(defn- multi-fixture
  "A corpus of N members, each with one passing record in batch-00.jsonl.
  When CITE? is true, M-two's first edge carries evidence citing M-one's
  source path."
  ([n] (multi-fixture n false))
  ([n cite?]
   (let [root (temp-dir "corpus-multi-")
        generations (io/file root "generations")
        base (io/file generations "base")
        pointer (io/file root "corpus")
        sources (mapv (fn [n]
                        (let [s (io/file root (str "M-" n ".md"))]
                          (spit s (str "source " n "\n"))
                          {:id (case n 1 "M-one" 2 "M-two" 3 "M-three" 4 "M-four" "M-five")
                           :path (.getAbsolutePath s)
                           :sha256 (generation/file-sha s)}))
      (range 1 (inc n)))
        pin (fn [m] (dissoc m :id))]
    (.mkdirs base)
    (spit (io/file base "members.json") (json/generate-string sources))
    (spit (io/file base "check_edges.py") (checker-text))
    (spit (io/file base "batch-00.jsonl")
          (str/join "\n"
                    (for [m sources
                          :let [edges (if (and cite? (= "M-two" (:id m)))
                                        [{:to "M-one" :kind "depends-on"
                                          :basis "cites"
                                          :evidence {:path (:path (first sources))
                                                     :line 1 :quote "source 1\n"}}]
                                        [])]]
                      (json/generate-string
                       {:id (:id m) :source (pin m) :status_as_read "open"
                        :purpose "test" :edges edges :unresolved []}))))
    (Files/createSymbolicLink (.toPath pointer) (.toPath (.getAbsoluteFile base))
                              (make-array FileAttribute 0))
    {:root root :generations generations :base base :pointer pointer
     :sources sources})))

(defn- counted-check-record [log]
  (fn [_checker _batch _line-number line]
    (let [record (try (json/parse-string line true) (catch Exception _ nil))]
      (swap! log conj (or (:id record) :unidentified-record))
      {:record record :record-pin {:sha256 "pin"}
       :checker-result {:exit 0 :out "" :err ""}
       :passed? (and (map? record) (not (:reject record)))})))

(deftest publish-runs-the-checker-only-on-the-target
  ;; Joe, 2026-11-15: one landing took 5-7 min because validate-staging!
  ;; consumed every record. Only the replacement's line may change, so only
  ;; it (plus explicit citers, next test) is checked.
  (let [{:keys [pointer generations sources]} (multi-fixture 5)
        target (first sources)
        log (atom [])
        publish (fn []
                  (generation/publish-generation!
                   {:pointer pointer :generations-dir generations
                    :claim {:target-id (:id target)
                            :source-path (:path target)
                            :source-sha (:sha256 target)}
                    :replacement {:id (:id target) :source (dissoc target :id)
                                  :status_as_read "open" :purpose "replacement"
                                  :edges [] :unresolved []}}))]
    (with-redefs [readings/check-record (counted-check-record log)]
      (is (= :published (:status (publish))))
      ;; Only the target's staged line and the base's copy of the same id —
      ;; the other four records stay decided.
      (is (= ["M-one" "M-one"] @log)))))

(deftest records-citing-the-targets-source-are-rechecked
  (let [{:keys [pointer generations sources]} (multi-fixture 5 true)
        target (first sources)
        log (atom [])
        publish (fn []
                  (generation/publish-generation!
                   {:pointer pointer :generations-dir generations
                    :claim {:target-id (:id target)
                            :source-path (:path target)
                            :source-sha (:sha256 target)}
                    :replacement {:id (:id target) :source (dissoc target :id)
                                  :status_as_read "open" :purpose "replacement"
                                  :edges [] :unresolved []}}))]
    (with-redefs [readings/check-record (counted-check-record log)]
      (is (= :published (:status (publish))))
    ;; M-two's edge evidence cites M-one's source path; M-three..five do not.
    ;; Each is checked in staging and (same id) in the base.
    (is (= ["M-one" "M-two" "M-one" "M-two"] @log)))))

(deftest replacement-duplicating-another-id-is-still-ambiguous
  ;; Ambiguity must stay detectable over ALL staged lines, by parsed id,
  ;; without the checker deciding it.
  (let [{:keys [pointer generations sources]} (multi-fixture 5)
        target (first sources) second-source (second sources)
        log (atom [])
        before (:path (generation/resolve-generation pointer))
        result (with-redefs [readings/check-record (counted-check-record log)]
                 (generation/publish-generation!
                  {:pointer pointer :generations-dir generations
                   :claim {:target-id (:id target)
                           :source-path (:path target)
                           :source-sha (:sha256 target)}
                   :replacement {:id (:id second-source)
                                 :source (dissoc second-source :id)
                                 :status_as_read "open" :purpose "duplicate"
                                 :edges [] :unresolved []}}))]
    (is (= :giraffe-generation/checker-failed (:reason result)))
    (is (= ["M-two"] (get-in result [:details :ambiguous-members])))
    (is (= before (:path (generation/resolve-generation pointer))))))

(deftest valid-replacement-refuses-an-unrelated-base-duplicate
  ;; apply-replacement! only replaces the target's lines, so an unrelated
  ;; duplicate must already be in the base to survive into staging. Pin the
  ;; global ambiguity guard independently of target validity and new failures:
  ;; both copies of M-two are individually valid and neither cites M-one.
  (let [{:keys [pointer generations base sources]} (multi-fixture 3)
        target (first sources)
        other (second sources)
        log (atom [])]
    (spit (io/file base "batch-01.jsonl")
          (str (json/generate-string
                {:id (:id other) :source (dissoc other :id)
                 :status_as_read "open" :purpose "duplicate fixture reading"
                 :edges [] :unresolved []}) "\n"))
    (let [before (generation/resolve-generation pointer)
          result (with-redefs [readings/check-record (counted-check-record log)]
                   (generation/publish-generation!
                    {:pointer pointer :generations-dir generations
                     :claim {:target-id (:id target) :source-path (:path target)
                             :source-sha (:sha256 target)}
                     :replacement {:id (:id target) :source (dissoc target :id)
                                   :status_as_read "open" :purpose "valid replacement"
                                   :edges [] :unresolved []}}))]
      (is (= ["M-one" "M-one"] @log))
      (is (= :read (get-in result [:details :member :status])))
      (is (= 0 (get-in result [:details :failed-record-count])))
      (is (= [] (get-in result [:details :new-failing-record-ids])))
      (is (= :giraffe-generation/checker-failed (:reason result)))
      (is (= ["M-two"] (get-in result [:details :ambiguous-members])))
      (is (= before (generation/resolve-generation pointer))))))

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

(deftest malformed-line-already-in-the-base-does-not-block-a-publish
  ;; claude-13 review: the staged side checks every unparseable line; the
  ;; base side must too, or a pre-existing malformed line reads as new.
  (let [{:keys [pointer generations base sources]} (multi-fixture 3)
        target (first sources)]
    (spit (io/file base "batch-01.jsonl") "{not json\n")
    (is (= :published
           (:status (generation/publish-generation!
                     {:pointer pointer :generations-dir generations
                      :claim {:target-id (:id target) :source-path (:path target)
                              :source-sha (:sha256 target)}
                      :replacement {:id (:id target) :source (dissoc target :id)
                                    :status_as_read "open" :purpose "replacement"
                                    :edges [] :unresolved []}}))))))

(defn- retire! [f targets & [extra]]
  (generation/retire-readings!
   (merge {:pointer (:pointer f) :generations-dir (:generations f)
           :targets targets :reason :source-missing :now-ms 1234} extra)))

(defn- refusal [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest retirement-preserves-identities-lineage-incoming-edges-and-old-generation
  (let [{:keys [base pointer sources] :as f} (multi-fixture 2 true)
        before (generation/resolve-generation pointer)
        ;; This generation's executable checker additionally resolves incoming
        ;; targets against members, as check_edges.py does. Deleting the member
        ;; rather than retaining it makes this real subprocess check fail.
        checker (io/file base "check_edges.py")]
    (spit checker (str/replace (slurp checker) "sys.exit(1 if bad else 0)"
                              " bad = bad or any(e['to'] not in members for e in r.get('edges', []))\nsys.exit(1 if bad else 0)"))
    (let [base-digest (generation/generation-digest base)]
      (io/delete-file (:path (first sources)))
      (let [result (retire! f ["M-one"])
            current (:path (generation/resolve-generation pointer))
            members (json/parse-string (slurp (io/file current "members.json")) true)
            lines (map #(json/parse-string % true)
                       (str/split-lines (slurp (io/file current "batch-00.jsonl"))))
            lineage (json/parse-string (str/trim (slurp (io/file current "retired.jsonl"))) true)]
        (is (= :published (:status result)))
        (is (not= (:path before) current))
        (is (= (:digest result) (.getName (io/file current))))
        (is (= ["M-two"] (mapv :id lines)))
        (is (= "M-one" (get-in (first lines) [:edges 0 :to])))
        (is (= 2 (count members)))
        (is (= {:reason "source-missing" :at_ms 1234
                :last_sha256 (:sha256 (first sources))} (:retired (first members))))
        (is (= {:id "M-one" :reason "source-missing" :at_ms 1234
                :source (select-keys (first sources) [:path :sha256])
                :base_digest base-digest} lineage))
        (is (= base-digest (generation/generation-digest base)))
        (let [again (retire! f ["M-one"] {:now-ms 9999})]
          (is (= :unchanged (:status again)))
          (is (= (:digest result) (:digest again)))
          (is (= current (:path (generation/resolve-generation pointer)))))))))

(deftest retirement-preconditions-refuse-whole-batch
  (let [{:keys [pointer sources] :as f} (multi-fixture 2)
        before (generation/resolve-generation pointer)]
    (doseq [targets [["M-one"] ["M-absent"]]]
      (let [error (refusal #(retire! f targets))]
        (is (= :giraffe-generation/retirement-precondition (:kind error)))
        (is (= targets (:targets error)))))
    (io/delete-file (:path (first sources)))
    (is (= ["M-two"] (:targets (refusal #(retire! f ["M-one" "M-two"])))))
    (is (= before (generation/resolve-generation pointer)))))

(deftest retirement-rechecks-source-and-pointer-before-switch
  (let [{:keys [pointer sources] :as f} (multi-fixture 1)
        source (:path (first sources))
        before (generation/resolve-generation pointer)]
    (io/delete-file source)
    (is (= :giraffe-generation/retirement-precondition
           (:kind (refusal #(retire! f ["M-one"] {:before-switch-fn (fn [] (spit source "returned"))})))))
    (is (= before (generation/resolve-generation pointer)))
    (io/delete-file source)
    (let [winner (doto (io/file (:generations f) "winner") .mkdirs)]
      (spit (io/file winner "marker") "invented concurrent generation")
      (is (= :giraffe-generation/base-mismatch
             (:kind (refusal #(retire! f ["M-one"]
                                     {:before-switch-fn (fn [] (install-pointer! pointer winner))})))))
      (is (= (.getCanonicalPath winner) (:path (generation/resolve-generation pointer)))))))

(deftest retirement-refuses-duplicates-but-tolerates-base-failures
  (doseq [duplicate? [false true]]
    (let [{:keys [base pointer sources] :as f} (multi-fixture 2)
          batch (io/file base "batch-00.jsonl")
          rows (mapv #(json/parse-string % true) (str/split-lines (slurp batch)))
          other (second rows)]
      (spit batch (str/join "\n" (map json/generate-string
                                     (if duplicate? (conj rows other)
                                         [(first rows) (assoc other :reject true)]))))
      (io/delete-file (:path (first sources)))
      (let [before (generation/resolve-generation pointer)]
        (if duplicate?
          (do (is (= :giraffe-generation/checker-failed
                     (:kind (refusal #(retire! f ["M-one"])))))
              (is (= before (generation/resolve-generation pointer))))
          ;; claude-13 review: a reading that ALREADY fails in the base (the
          ;; live corpus has stale ones) must not block retiring another member.
          (is (= :published (:status (retire! f ["M-one"])))))))))

(deftest replacement-after-retirement-clears-retired
  ;; claude-13 review of R2: a reopened, reread member must not stay retired
  (let [{:keys [pointer generations sources] :as f} (multi-fixture 2)
        one (first sources)]
    (io/delete-file (:path one))
    (is (= :published (:status (retire! f ["M-one"]))))
    (spit (:path one) "source one, restored\n")
    (let [sha (generation/file-sha (io/file (:path one)))]
      (is (= :published
             (:status (generation/publish-generation!
                       {:pointer pointer :generations-dir generations
                        :claim {:target-id "M-one" :source-path (:path one) :source-sha sha
                                :old-sha (:sha256 one)}
                        :replacement {:id "M-one" :source {:path (:path one) :sha256 sha}
                                      :status_as_read "open" :purpose "reread"
                                      :edges [] :unresolved []}}))))
      (let [members (json/parse-string
                     (slurp (io/file (:path (generation/resolve-generation pointer)) "members.json")) true)
            m (first (filter #(= "M-one" (:id %)) members))]
        (is (= sha (:sha256 m)))
        (is (nil? (:retired m)))))))
