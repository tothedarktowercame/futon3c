(ns futon3c.giraffe.run-test
  "M-giraffe stage 1 packet 3 acceptance tests: the run writer.  All
  writes go to a temp root, never the real storage root."
  (:require [clojure.edn :as edn]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.giraffe.pass :as gp]
            [futon3c.giraffe.run :as gr]
            [futon3c.giraffe.tickets :as gt]))

(def tickets-dir "holes/tickets")
(def index-file "holes/tickets/tickets-index.edn")
(def edges-file "test/resources/giraffe/pilot1-edges.json")
(def ids-file "test/resources/giraffe/pilot1-ids.txt")

(def pilot1-ids
  (vec (remove str/blank? (str/split (slurp ids-file) #"\s+"))))

(defn- temp-root [tag]
  (let [root (str (System/getProperty "java.io.tmpdir") "/giraffe-run-" tag "-"
                  (System/currentTimeMillis) "-" (rand-int 100000))]
    (.mkdirs (io/file root))
    root))

(defn- pilot-run
  "Adapter + pass + write, all inside ROOT (a temp dir)."
  ([root] (pilot-run root {}))
  ([root _]
   (let [adapter-result (gt/tickets->input
                         {:tickets-dir tickets-dir :index-file index-file
                          :edges-file edges-file :only pilot1-ids})
         pass-output (gp/pass (:input adapter-result))
         written (gr/write-run! {:root root
                                 :adapter-result adapter-result
                                 :pass-output pass-output
                                 ;; a real current-format sha: the first
                                 ;; commit whose writer seals work-clusters.json
                                 :git-sha "e90fe14bb3c181964f8d8b239e5bff16225dff37"
                                 :index-file index-file
                                 :edges-file edges-file})]
     {:adapter-result adapter-result
      :pass-output pass-output
      :written written})))

(deftest pilot-1-run-on-disk
  (let [root (temp-root "pilot")
        {:keys [adapter-result pass-output written]} (pilot-run root)
        dir (io/file (:dir written))]
    (testing "every file exists"
      (doseq [f ["input.edn" "rejected.edn" "pass.edn" "report.md"
                 "work-clusters.json" "manifest.edn"]]
        (is (.isFile (io/file dir f)) (str f " missing"))))
    (testing "every manifest sha matches the file on disk"
      (is (:ok? (gr/verify-run (str dir))))
      (is (= [] (:mismatches (gr/verify-run (str dir))))))
    (testing "pass.edn round-trips to a value equal to the pass output"
      (is (= pass-output (edn/read-string (slurp (io/file dir "pass.edn"))))))
    (testing "the browser projection carries the same cluster membership"
      (let [projection (json/parse-string
                        (slurp (io/file dir "work-clusters.json")) true)]
        (is (= (count (:work-clusters pass-output))
               (count (:clusters projection))))
        (is (= (:work-cluster-by-item pass-output)
               (:by_item projection)))
        (is (= (get-in pass-output
                       [:work-cluster-interpretation :tickets :analyzed])
               (get-in projection [:interpretation :tickets :analyzed])))
        (is (= (mapv name
                     (get-in pass-output
                             [:work-cluster-interpretation :tickets :unlinked]))
               (get-in projection [:interpretation :tickets :unlinked])))))
    (testing "input.edn round-trips to the adapter's pass input"
      (is (= (:input adapter-result)
             (edn/read-string (slurp (io/file dir "input.edn"))))))
    (testing "manifest pins the source files it read"
      (is (= (gr/sha256 (io/file index-file))
             (get-in (:manifest written) [:sources :tickets-index :sha256])))
      (is (= (gr/sha256 (io/file edges-file))
             (get-in (:manifest written) [:sources :edges :sha256])))
      (is (re-matches #"[0-9a-f]{40}" (str (:git-sha (:manifest written))))))
    (testing "report lists each open id exactly once in each order column"
      (let [report (slurp (io/file dir "report.md"))
            opens (set (map :id (:order pass-output)))]
        (is (= 13 (count opens)))
        (is (= 13 (get-in pass-output [:counts :open])))
        (is (= 13 (count (re-seq #"(?m)^\| T-" report)))
            "13 table rows: one per open item, so 13 ids per column")
        (doseq [id opens
                :let [table (str/join "\n" (re-seq #"(?m)^\| T-.*$" report))]]
          (is (= 2 (count (re-seq (re-pattern (str "\\b" (name id) "\\b")) table)))
              (str (name id)
                   " must occur exactly once per column, twice in the row")))))))

(deftest planted-edit-is-reported-by-verify-run
  (let [root (temp-root "planted")
        {:keys [written]} (pilot-run root)
        dir (str (:dir written))]
    (is (:ok? (gr/verify-run dir)))
    (spit (io/file dir "pass.edn")
          (str "(planted " (rand-int 100000) ")"))
    (let [{:keys [ok? mismatches]} (gr/verify-run dir)]
      (is (not ok?))
      (is (= ["pass.edn"] (map :file mismatches))))))

(deftest two-writes-of-the-same-input-are-deterministic
  (let [root (temp-root "determinism")
        w1 (:written (pilot-run root))
        w2 (:written (pilot-run root))
        id1 (:run-id w1) id2 (:run-id w2)]
    (is (not= id1 id2) "timestamps differ, so run ids differ")
    (is (= (subs id1 (- (count id1) 8)) (subs id2 (- (count id2) 8)))
        "the hash part of the run id is the sha of the same input")
    (is (= (slurp (io/file (:dir w1) "pass.edn"))
           (slurp (io/file (:dir w2) "pass.edn")))
        "pass.edn bytes are identical for the same input")))

(deftest default-root-is-the-canonical-storage-root-never-tmp
  (is (= "/home/joe/code/storage/giraffe/" gr/default-root)))

;; Review (claude-13, 2026-10-27).
(deftest review-a-file-dropped-from-the-manifest-is-reported
  ;; wrong: verify-run checked only the files the manifest listed, so editing
  ;; pass.edn and deleting its manifest row passed.
  (let [root (temp-root "dropped")
        {:keys [written]} (pilot-run root)
        dir (str (:dir written))
        mf (io/file dir "manifest.edn")
        m (edn/read-string (slurp mf))]
    (spit (io/file dir "pass.edn") "(planted)")
    (spit mf (pr-str (update m :files
                             (fn [fs] (vec (remove #(= "pass.edn" (:file %)) fs))))))
    (let [{:keys [ok? mismatches]} (gr/verify-run dir)]
      (is (not ok?))
      (is (= [{:file "pass.edn" :missing-from-manifest true}] mismatches)))))

(deftest review-report-says-what-each-not-ready-item-waits-on
  (let [root (temp-root "waiting")
        {:keys [written]} (pilot-run root)
        report (slurp (io/file (str (:dir written)) "report.md"))]
    (is (str/includes? report "## Waiting on"))
    (is (str/includes? report "### T-cx-new-blocks-emacs (external)"))
    ;; every edge line is one line: no quote breaks out of its bullet
    (is (not-any? #(str/starts-with? % "**")
                (str/split-lines report)))))

;; --------------------------------------------------------------- verify-run
;; format compatibility (zai-4 repair, 2026-10-31): e90fe14bb added
;; work-clusters.json to the sealed file set, which made verify-run
;; reject every historical run whose manifest predates it.  These tests
;; copy REAL run directories from the canonical storage root (read-only
;; source, copy-then-mutate) so both formats are exercised in their
;; actual on-disk shapes.

(def storage-root "/home/joe/code/storage/giraffe/")

(def legacy-run-ids
  ["20261028T154151.285Z-b916cd4e"   ;; git-sha ff0734e1 (pre-e90fe14bb); pinned by live pending f74df9cc
   "20261029T230756.734Z-3fa8fa14"   ;; git-sha e1eea834
   "20261030T193432.152Z-dc280cbf"]) ;; git-sha deeae229

(def current-run-id "20261030T213938.828Z-dc280cbf") ;; git-sha 6b5597d03

(defn- copy-run [run-id tag]
  (let [dest (io/file (temp-root tag) run-id)]
    (when (.isDirectory (io/file storage-root run-id))
      (.mkdirs dest)
      (doseq [f (file-seq (io/file storage-root run-id))
              :when (.isFile f)]
        (io/copy f (io/file dest (.getName f))))
      (str dest))))

(defn- run-dir-exists? [run-id]
  (.isDirectory (io/file storage-root run-id)))

(deftest legacy-runs-verify-their-original-sealed-files
  (doseq [id legacy-run-ids
          :when (run-dir-exists? id)]
    (when-let [copy (copy-run id "legacy-ok")]
      (testing id
        (is (:ok? (gr/verify-run copy)))
        (is (= [] (:mismatches (gr/verify-run copy))))))))

(deftest current-run-verifies-with-work-clusters
  (when-let [copy (and (run-dir-exists? current-run-id)
                       (copy-run current-run-id "current-ok"))]
    (is (:ok? (gr/verify-run copy)))
    (is (= [] (:mismatches (gr/verify-run copy))))))

(deftest legacy-run-with-edited-bytes-fails
  (when-let [copy (and (run-dir-exists? (first legacy-run-ids))
                       (copy-run (first legacy-run-ids) "legacy-edit"))]
    (spit (io/file copy "pass.edn") "(planted)")
    (let [{:keys [ok? mismatches]} (gr/verify-run copy)]
      (is (not ok?))
      (is (= ["pass.edn"] (map :file mismatches)))
      (is (not-any? :missing-from-manifest mismatches)
          "the file is listed; this is a hash mismatch, not a format gap"))))

(deftest legacy-run-with-missing-original-artifact-fails
  (when-let [copy (and (run-dir-exists? (first legacy-run-ids))
                       (copy-run (first legacy-run-ids) "legacy-missing"))]
    (.delete (io/file copy "pass.edn"))
    (let [{:keys [ok? mismatches]} (gr/verify-run copy)]
      (is (not ok?))
      (is (= ["pass.edn"] (map :file mismatches))))))

(deftest current-run-with-missing-work-clusters-fails
  (when-let [copy (and (run-dir-exists? current-run-id)
                       (copy-run current-run-id "current-missing"))]
    (.delete (io/file copy "work-clusters.json"))
    (let [{:keys [ok? mismatches]} (gr/verify-run copy)]
      (is (not ok?))
      (is (= ["work-clusters.json"] (map :file mismatches))))))

(deftest current-run-with-corrupt-work-clusters-fails
  (when-let [copy (and (run-dir-exists? current-run-id)
                       (copy-run current-run-id "current-corrupt"))]
    (spit (io/file copy "work-clusters.json") "{\"clusters\": []}")
    (let [{:keys [ok? mismatches]} (gr/verify-run copy)]
      (is (not ok?))
      (is (= ["work-clusters.json"] (map :file mismatches))))))

(deftest current-manifest-stripped-of-work-clusters-row-is-not-legacy
  ;; a current-format manifest (git-sha descends from e90fe14bb) with the
  ;; work-clusters.json row removed must fail: format is decided by the
  ;; git-sha, not by the artifact's absence
  (when-let [copy (and (run-dir-exists? current-run-id)
                       (copy-run current-run-id "current-stripped"))]
    (let [mf (io/file copy "manifest.edn")
          m (edn/read-string (slurp mf))]
      (.delete (io/file copy "work-clusters.json"))
      (spit mf (pr-str (update m :files
                               (fn [fs]
                                 (vec (remove #(= "work-clusters.json" (:file %))
                                              fs))))))
      (let [{:keys [ok? mismatches]} (gr/verify-run copy)]
        (is (not ok?))
        (is (= [{:file "work-clusters.json" :missing-from-manifest true}]
               mismatches)))))

  ;; an unresolvable git-sha is undecidable and fails closed to the
  ;; current baseline
  (when-let [copy (and (run-dir-exists? (first legacy-run-ids))
                       (copy-run (first legacy-run-ids) "undecidable"))]
    (let [mf (io/file copy "manifest.edn")
          m (edn/read-string (slurp mf))]
      (spit mf (pr-str (assoc m :git-sha
                              (apply str (repeat 40 \a)))))
      (let [{:keys [ok? mismatches]} (gr/verify-run copy)]
        (is (not ok?))
        (is (= [{:file "work-clusters.json"
                 :missing-from-manifest true
                 :format-undecidable true}]
               mismatches))))))
