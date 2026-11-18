(ns futon3c.giraffe.run-test
  "M-giraffe stage 1 packet 3 acceptance tests: the run writer.  All
  writes go to a temp root, never the real storage root."
  (:require [clojure.edn :as edn]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.giraffe.excursions :as excursions]
            [futon3c.giraffe.pass :as gp]
            [futon3c.giraffe.run :as gr]
            [futon3c.circus.operator-focus :as operator-focus]
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
  ([root write-opts]
   (let [adapter-result (gt/tickets->input
                         {:tickets-dir tickets-dir :index-file index-file
                          :edges-file edges-file :only pilot1-ids})
         pass-output (gp/pass (:input adapter-result))
         written (gr/write-run! (merge
                                {:root root
                                 :adapter-result adapter-result
                                 :pass-output pass-output
                                 ;; a real current-format sha: the first
                                 ;; commit whose writer seals work-clusters.json
                                 :git-sha "e90fe14bb3c181964f8d8b239e5bff16225dff37"
                                 :index-file index-file
                                 :edges-file edges-file}
                                write-opts))]
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

(deftest manifest-records-resolved-readings-generation
  (let [root (temp-root "generation-manifest")
        adapter-result (assoc
                        (gt/tickets->input
                         {:tickets-dir tickets-dir :index-file index-file
                          :edges-file edges-file :only pilot1-ids})
                        :readings-generation
                        {:pointer "/compat/corpus" :path "/generations/abc"
                         :digest "abc"})
        pass-output (gp/pass (:input adapter-result))
        written (gr/write-run! {:root root :adapter-result adapter-result
                                :pass-output pass-output :edges-file edges-file})]
    (is (= {:pointer "/compat/corpus" :path "/generations/abc" :digest "abc"}
           (get-in written [:manifest :readings-generation])))))

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

(deftest focus-input-writes-pinned-priority-without-changing-pass-bytes
  (let [root (temp-root "priority")
        baseline (:written (pilot-run root {:run-id "without-focus"}))
        pass-output (:pass-output (pilot-run (temp-root "priority-source")))
        relations (into {} (map (fn [{:keys [id]}] [id :associated])
                                (:order pass-output)))
        focused (:written
                 (pilot-run root {:run-id "with-focus"
                                  :focus-relation-input
                                  {:relations relations
                                   :evidence-pin {:sha256 "received-focus-sha"}}
                                  :priority-cutoff 3}))
        baseline-dir (io/file (:dir baseline))
        focused-dir (io/file (:dir focused))
        priority-file (io/file focused-dir "priority.edn")]
    (is (.isFile priority-file))
    (is (= (gr/sha256 priority-file)
           (get-in (:manifest focused) [:priority-projection :sha256])))
    (is (= :present (get-in (:manifest focused) [:priority-projection :status])))
    (is (not-any? #(contains? % :evidence-pin)
                  (edn/read-string (slurp priority-file))))
    (is (= {:status :absent :reason :focus-relation-input-absent}
           (get-in (:manifest baseline) [:focus-relation-input])))
    (is (not (.exists (io/file baseline-dir "priority.edn"))))
    (is (= (slurp (io/file baseline-dir "pass.edn"))
           (slurp (io/file focused-dir "pass.edn")))
        "the optional projection does not alter pass.edn bytes")))

(defn- make-legacy-priority-pin! [written]
  (let [dir (io/file (:dir written))
        priority-file (io/file dir "priority.edn")
        manifest-file (io/file dir "manifest.edn")
        manifest (edn/read-string (slurp manifest-file))
        pin (get-in manifest [:focus-relation-input :evidence-pin])
        rows (mapv #(assoc % :evidence-pin pin)
                   (edn/read-string (slurp priority-file)))]
    (spit priority-file (pr-str rows))
    (let [old-sha (gr/sha256 priority-file)
          manifest (-> manifest
                       (update :files (fn [files]
                                        (mapv #(if (= "priority.edn" (:file %))
                                                 (assoc % :sha256 old-sha) %) files)))
                       (assoc-in [:priority-projection :sha256] old-sha))]
      (spit manifest-file (pr-str manifest))
      {:dir dir :old-sha old-sha :pin pin})))

(deftest priority-pin-migration-is-verified-idempotent-and-fails-closed
  (let [root (temp-root "priority-migration")
        pass-output (:pass-output (pilot-run (temp-root "priority-migration-source")))
        relations (into {} (map (fn [{:keys [id]}] [id :associated]) (:order pass-output)))
        written (:written (pilot-run root {:run-id "legacy-pin"
                                           :focus-relation-input
                                           {:relations relations
                                            :evidence-pin {:fixture "pin"}}}))
        focus-version (get-in written [:manifest :focus/version])
        {:keys [dir old-sha]} (make-legacy-priority-pin! written)
        before (slurp (io/file dir "priority.edn"))
        dry (gr/migrate-priority-pins! root)]
    (is (= :would-migrate (get-in dry [:runs 0 :status])))
    (is (= before (slurp (io/file dir "priority.edn"))) "dry-run writes nothing")
    (let [migrated (gr/migrate-priority-pins!
                    root {:write? true :at "2026-11-18T00:00:00Z"})]
      (is (pos? (:saved-bytes migrated)))
      (is (:ok? (gr/verify-run dir)))
      (is (= focus-version
             (:focus/version (edn/read-string (slurp (io/file dir "manifest.edn"))))))
      (is (= {:from-sha old-sha :at "2026-11-18T00:00:00Z" :reason :pin-once}
             (:migrated (edn/read-string (slurp (io/file dir "manifest.edn"))))))
      (is (some #(= :priority-pin-migrated (:reason %)) (:notes (gr/verify-run dir))))
      (is (= :unchanged (get-in (gr/migrate-priority-pins! root {:write? true})
                                [:runs 0 :status])))
      (let [manifest-file (io/file dir "manifest.edn")
            manifest (edn/read-string (slurp manifest-file))]
        ;; Planted check: this is the manifest a broken migration would leave.
        (spit manifest-file
              (pr-str (update manifest :files
                              (fn [files]
                                (mapv #(if (= "priority.edn" (:file %))
                                         (assoc % :sha256 old-sha) %) files)))))
        (is (not (:ok? (gr/verify-run dir)))
            "forgetting the migrated priority sha must fail verification")))))

(deftest priority-pin-migration-skips-differing-row-pin
  (let [root (temp-root "priority-mismatch")
        pass-output (:pass-output (pilot-run (temp-root "priority-mismatch-source")))
        relations (into {} (map (fn [{:keys [id]}] [id :associated]) (:order pass-output)))
        written (:written (pilot-run root {:run-id "mismatch"
                                           :focus-relation-input
                                           {:relations relations :evidence-pin {:fixture "pin"}}}))
        {:keys [dir]} (make-legacy-priority-pin! written)
        priority-file (io/file dir "priority.edn")
        rows (edn/read-string (slurp priority-file))]
    (spit priority-file (pr-str (assoc-in rows [0 :evidence-pin] {:different true})))
    (is (= :pin-mismatch (get-in (gr/migrate-priority-pins! root) [:runs 0 :status])))))

(deftest focus-version-digest-is-order-stable-and-verified
  (let [pins-a {:producer :hou
                :evidence-pin [["turn-b" "sha-b"] ["turn-a" "sha-a"]]
                :header-ids ["header-b" "header-a"]}
        pins-b {:header-ids ["header-a" "header-b"]
                :evidence-pin [["turn-a" "sha-a"] ["turn-b" "sha-b"]]
                :producer :hou}
        root (temp-root "focus-version")
        run-a (:written (pilot-run root {:run-id "focus-version-a"
                                         :focus-relation-input
                                         {:relations {} :evidence-pin pins-a}}))
        run-b (:written (pilot-run root {:run-id "focus-version-b"
                                         :focus-relation-input
                                         {:relations {} :evidence-pin pins-b}}))]
    (is (= {:schema [:hou/focus 1]
            :digest (get-in run-a [:manifest :focus/version :digest])}
           (get-in run-a [:manifest :focus/version])))
    (is (= (get-in run-a [:manifest :focus/version])
           (get-in run-b [:manifest :focus/version])))
    (is (:ok? (gr/verify-run (:dir run-a))))))

(deftest focus-version-verifier-reports-tamper-and-legacy-absence
  (let [root (temp-root "focus-version-verifier")
        {:keys [written]}
        (pilot-run root {:run-id "focus-version-verifier"
                         :focus-relation-input
                         {:relations {}
                          :evidence-pin {:evidence-pin [["turn-a" "sha-a"]]}}})
        manifest-file (io/file (:dir written) "manifest.edn")
        original (edn/read-string (slurp manifest-file))]
    (spit manifest-file
          (pr-str (assoc-in original
                            [:focus-relation-input :evidence-pin :evidence-pin 0 1]
                            "tampered")))
    (let [verified (gr/verify-run (:dir written))]
      (is (not (:ok? verified)))
      (is (= :focus-version-mismatch
             (:reason (first (:mismatches verified))))))
    (spit manifest-file (pr-str (dissoc original :focus/version)))
    (let [verified (gr/verify-run (:dir written))]
      (is (:ok? verified))
      (is (= [{:reason :focus-version-absent}] (:notes verified))))))

(deftest retained-focus-mint-boundary-writes-priority
  (let [root (temp-root "hou-focus-mint")
        pass-output (:pass-output (pilot-run (temp-root "hou-focus-source")))
        relations (into {} (map (fn [{:keys [id]}] [(name id) :focus]) (:order pass-output)))
        windows (atom [])]
    (with-redefs [requiring-resolve (fn [_] (throw (ex-info "WM must not be called" {})))
                  operator-focus/operator-focus
                  (fn [window]
                    (swap! windows conj window)
                    {:report {:focus [{:item "fixture"}] :notices []}
                     :focus-input {:relations (assoc relations "M-outside" :focus)
                                   :evidence-pin [["turn-fixture" "digest"]]
                                   :header-ids ["hou-header-fixture"]}})]
      (let [input (gr/retained-focus-input pass-output)
            written (:written (pilot-run root {:run-id "hou-focus" :focus-relation-input input}))
            pin (get-in written [:manifest :focus-relation-input :evidence-pin])]
        (is (= #{:focus} (set (map :relation (read-string (slurp (io/file (:dir written) "priority.edn")))))))
        (is (= (set (map :id (:order pass-output))) (set (keys (:relations input)))))
        (is (= :hou (:producer pin)))
        (is (= [["turn-fixture" "digest"]] (:evidence-pin pin)))
        (is (= ["hou-header-fixture"] (:header-ids pin)))
        (is (= (first @windows) (:window pin)))
        (is (= 6 (get-in pin [:window :window-hours])))))
    (with-redefs [operator-focus/operator-focus (constantly {:report {:focus []} :focus-input {:relations {}}})]
      (let [input (gr/retained-focus-input pass-output)
            written (:written (pilot-run (temp-root "empty-hou") {:focus-relation-input input}))]
        (is (nil? input))
        (is (= :focus-relation-input-absent (get-in written [:manifest :focus-relation-input :reason])))))))

(defn- focus-fixture
  ([members] (focus-fixture members ["r1" "r2"]))
  ([members expected]
   {:report {:focus [{:item "M-a"}] :notices []}
    :focus-input {:relations {"M-a" (if (seq members) :focus :associated)}
                  :evidence-pin (mapv (fn [m] [(str "giraffe-reading/" m) (str "sha-" m)]) members)
                  :header-ids ["hou-header-fixture"]
                  :needs-members members
                  :needs-expected expected}}))

(defn- reading-input [members]
  {:items [{:id :M-a :state-group :open :source-order 0}]
   :edges (mapv (fn [m] {:from :M-a :to :M-a :class :needs :reading-member m}) members)})

(def ^:private one-pass
  {:order [{:id :M-a :rank 1 :status :ready :reason :ready}]
   :readiness {:M-a {:status :ready :reason :ready}}
   :counts {:open 1 :ready 1 :ready-ignoring-externals 1}})

(defn- mint-fixture [root run-id input fixture]
  (with-redefs [operator-focus/operator-focus (constantly fixture)]
    (let [focus (gr/retained-focus-input one-pass {:root root})]
      {:focus focus
       :written (gr/write-run! {:root root :run-id run-id
                                :adapter-result {:input input :rejected []}
                                :pass-output one-pass :focus-relation-input focus
                                :git-sha "e90fe14bb3c181964f8d8b239e5bff16225dff37"})})))

(deftest incomplete-needs-retains-last-complete-focus
  (let [root (temp-root "focus-retention")
        ;; The pass input carries a third member the needs writer skips (an
        ;; edge to a non-inventory item). Completeness follows the needs
        ;; read's own census, so r3 must not make every run incomplete.
        input (reading-input ["r1" "r2" "r3"])]
    (let [{:keys [focus written]} (mint-fixture root "complete-run" input (focus-fixture ["r1" "r2"]))]
      (is (= :complete (get-in focus [:focus/completeness :status])))
      (is (nil? (:focus/retained-from focus)))
      (is (= :complete (get-in written [:manifest :focus/completeness :status]))))
    (testing "a partial non-empty projection is incomplete (planted guard)"
      (let [{:keys [focus written]} (mint-fixture root "partial-run" input (focus-fixture ["r1"]))]
        (is (= "complete-run" (:focus/retained-from focus)))
        (is (= ["r2"] (get-in focus [:focus/retention-reason :missing-ids])))
        (is (= {:M-a :focus} (:relations focus)))
        (is (= "complete-run" (get-in written [:manifest :focus/retained-from])))
        (is (= (slurp (io/file root "complete-run" "priority.edn"))
               (slurp (io/file (:dir written) "priority.edn"))))
        (is (some #(= :focus-input-retained (:reason %))
                  (:notes (gr/verify-run (:dir written)))))))
    (testing "a typed unavailable/zero result retains the newest complete input"
      (let [{:keys [focus]} (mint-fixture root "zero-run" input
                                          (assoc-in (focus-fixture [] []) [:report :notices]
                                                    [{:reason :needs-unavailable}]))]
        (is (= "complete-run" (:focus/retained-from focus))
            "a retained run is not itself a complete source")))))

(deftest incomplete-needs-without-prior-is-marked-not-hidden
  (let [root (temp-root "focus-incomplete")
        {:keys [focus written]} (mint-fixture root "first-run" (reading-input ["r1" "r2"])
                                              (focus-fixture ["r1"]))]
    (is (nil? (:focus/retained-from focus)))
    (is (= {:status :incomplete :reason :needs-incomplete :observed 1 :expected 2
            :missing-count 1 :missing-ids ["r2"]}
           (get-in written [:manifest :focus/completeness])))
    (is (.exists (io/file (:dir written) "priority.edn"))
        "bands still come out, marked incomplete in the manifest")))

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

(defn- write-legacy-run-fixture! [tag git-sha]
  (let [dir (io/file (temp-root tag) "legacy-run")
        files ["input.edn" "rejected.edn" "pass.edn" "report.md"]]
    (.mkdirs dir)
    (doseq [file files]
      (spit (io/file dir file) (str file " fixture\n")))
    (spit (io/file dir "manifest.edn")
          (pr-str {:run-id "legacy-run"
                   :git-sha git-sha
                   :files (mapv (fn [file]
                                  {:file file
                                   :sha256 (gr/sha256 (io/file dir file))})
                                files)}))
    (str dir)))

(deftest verify-run-resolves-format-in-source-checkout-not-process-cwd
  (let [legacy-sha "914f22e150ca793d532a8629b8bcf85553365e81"
        legacy-dir (write-legacy-run-fixture! "wrong-cwd-legacy" legacy-sha)
        nonsense-dir (write-legacy-run-fixture!
                      "wrong-cwd-undecidable" (apply str (repeat 40 \a)))
        empty-cwd (io/file (temp-root "empty-cwd"))
        original-cwd (System/getProperty "user.dir")]
    (try
      (System/setProperty "user.dir" (str empty-cwd))
      (is (= {:ok? true :mismatches []
              :notes [{:reason :focus-version-absent}]}
             (gr/verify-run legacy-dir)))
      (is (= {:ok? false
              :mismatches [{:file "work-clusters.json"
                            :missing-from-manifest true
                            :format-undecidable true}]
              :notes [{:reason :focus-version-absent}]}
             (gr/verify-run nonsense-dir)))
      (finally
        (System/setProperty "user.dir" original-cwd)))))

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

;; ---------------------------------------------------------- freshness

(defn- field-run!
  "Write a missions-style field run (no :tickets-index in :sources) into
  ROOT from the pilot adapter result."
  [root]
  (let [adapter-result (gt/tickets->input
                        {:tickets-dir tickets-dir :index-file index-file
                         :edges-file edges-file :only pilot1-ids})
        pass-output (gp/pass (:input adapter-result))]
    (gr/write-run! {:root root
                    :adapter-result adapter-result
                    :pass-output pass-output
                    :git-sha "e90fe14bb3c181964f8d8b239e5bff16225dff37"
                    ;; deliberately NO :index-file -- a field manifest
                    :edges-file edges-file})))

(deftest check-freshness-matching-run-is-fresh
  (let [root (temp-root "fresh")
        written (field-run! root)
        adapter-result (gt/tickets->input
                        {:tickets-dir tickets-dir :index-file index-file
                         :edges-file edges-file :only pilot1-ids})
        result (gr/check-freshness {:root root :adapter-result adapter-result})]
    (is (:fresh? result))
    (is (= (:run-id written) (:run-id (:newest result))))
    (is (= (gr/sha256 (io/file (:dir written) "input.edn"))
           (:adapter-input-sha256 result)))))

(deftest check-freshness-different-input-is-stale
  (let [root (temp-root "stale")]
    (field-run! root)
    (let [adapter-result (gt/tickets->input
                          {:tickets-dir tickets-dir :index-file index-file
                           :edges-file edges-file :only []})
          result (gr/check-freshness {:root root
                                      :adapter-result adapter-result})]
      (is (not (:fresh? result))))))

(deftest check-freshness-empty-root-is-stale
  (let [root (temp-root "empty")
        adapter-result (gt/tickets->input
                        {:tickets-dir tickets-dir :index-file index-file
                         :edges-file edges-file :only pilot1-ids})
        result (gr/check-freshness {:root root :adapter-result adapter-result})]
    (is (not (:fresh? result)))
    (is (nil? (:newest result)))))

(deftest tickets-run-is-not-a-field-run
  ;; A run whose manifest carries :tickets-index fed the tickets adapter;
  ;; the field reader must not see it, so the newest-field-run scan skips it.
  (let [root (temp-root "tickets-only")
        empty (temp-root "tickets-empty")
        field (field-run! root)
        ;; a tickets run whose INPUT DIFFERS (so its run-id, timestamp
        ;; plus input hash, cannot collide with the field run's) and
        ;; whose manifest carries :tickets-index, written after the
        ;; field run.  Only the manifest filter keeps the scan on the
        ;; field run.
        tickets-adapter (gt/tickets->input
                         {:tickets-dir tickets-dir :index-file index-file
                          :edges-file edges-file :only []})
        tickets (gr/write-run! {:root root
                                :adapter-result tickets-adapter
                                :pass-output (gp/pass (:input tickets-adapter))
                                :git-sha "e90fe14bb3c181964f8d8b239e5bff16225dff37"
                                :index-file index-file
                                :edges-file edges-file})]
    (is (nil? (gr/newest-field-run empty)))
    (is (not= (:run-id field) (:run-id tickets)))
    (is (pos? (compare (:run-id tickets) (:run-id field)))
        "fixture ordering: the tickets run has the newer directory name")
    (is (= (:run-id field) (:run-id (gr/newest-field-run root))))))

(deftest combined-invocation-is-the-single-adapter-construction-path
  (let [reading-root (temp-root "readings")
        _ (spit (io/file reading-root "batch-00.jsonl") "{}\n")
        seen (atom nil)
        base (gt/tickets->input {:tickets-dir tickets-dir
                                 :index-file index-file
                                 :edges-file edges-file :only pilot1-ids})]
    (with-redefs [excursions/missions-and-excursions->input
                  (fn [opts] (reset! seen opts) base)]
      (gr/build-adapter-result
       {:source "missions-excursions" :edges edges-file
        :repos ["/repo"] :readings-root reading-root :only-ids ["M-one"]})
      (is (= ["/repo"] (:repos @seen)))
      (is (:include-tickets @seen))
      (is (= [(str (io/file reading-root "batch-00.jsonl"))]
             (:reading-batches @seen)))
      (is (= :population/missions-excursions-tickets
             (:reading-scope @seen))))))

(deftest current-freshness-replays-the-mint-invocation
  (let [root (temp-root "invocation")
        adapter (gt/tickets->input {:tickets-dir tickets-dir
                                    :index-file index-file
                                    :edges-file edges-file :only pilot1-ids})
        invocation {:source "missions-excursions" :edges edges-file
                    :repos ["/repo"] :readings-root "/readings"
                    :with-retained-focus? true}
        written (gr/write-run! {:root root :adapter-result adapter
                                :pass-output (gp/pass (:input adapter))
                                :git-sha "e90fe14bb3c181964f8d8b239e5bff16225dff37"
                                :edges-file edges-file :invocation invocation})]
    (with-redefs [gr/build-adapter-result (fn [actual]
                                           (is (= invocation actual))
                                           adapter)]
      (let [freshness (gr/current-missions-freshness root)]
        (is (:fresh? freshness))
        (is (= (:run-id written) (:run-id freshness)))))))
