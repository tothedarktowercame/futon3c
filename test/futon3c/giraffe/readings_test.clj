(ns futon3c.giraffe.readings-test
  "Giraffe readings consumer tests.

  HERMETIC WORLD (2026-11-15, kimi-2): these tests used to read the frozen
  pilot under /home/joe/notes/wm-next/giraffe-excursions/ whose 636 members
  pinned sha256s of LIVE documents under /home/joe/code; routine edits to
  those documents tripped the (correct) pin refusals. The world is now
  INVENTED at test time in a temp directory, and the checker is the REAL
  contract — resources/giraffe/check_edges.py, a copy of the pilot's
  check_edges.py (generic mechanical code, no document text) with one added
  override: GIRAFFE_CHECK_CODE_ROOT replaces its hard-wired
  Path.home()/\"code\" when set. Claims that pinned the real corpus (636
  members, per-batch counts) are restated relative to the fixture and say
  so in a comment."
  (:require [clojure.test :refer [deftest is]]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell]
            [clojure.string :as str]
            [futon3c.giraffe.readings :as readings]
            [futon3c.giraffe.missions :as missions]
            [futon3c.giraffe.excursions :as excursions]))

;;; ---------------------------------------------------------- the fixture world

(defn- temp-dir [prefix]
  (str (java.nio.file.Files/createTempDirectory
        prefix (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- write! [root rel text]
  (let [f (io/file root rel)]
    (.mkdirs (.getParentFile f))
    (spit f text)
    f))

(defn- sha256-file [f]
  (readings/sha256 (slurp f)))

;; Invented documents. Names and prose are fixture inventions; no real
;; mission, excursion or ticket text appears here.

(def ^:private aurora-text
  "# M-fixture-aurora — invented fixture mission (readings test world)

**Status:** OPEN — invented fixture mission; no live counterpart exists.

## Acceptance

- [ ] The invented aurora ledger balances inside the fixture world.
")

(def ^:private boreal-text
  "# M-fixture-boreal — invented fixture mission (readings test world)

**Status:** OPEN — invented fixture mission; no live counterpart exists.

## Acceptance

- [ ] The invented boreal ledger is reconciled inside the fixture world.

## Notes

This mission depends on M-fixture-dormant being settled before it can close.
")

(def ^:private cartography-text
  "# E-fixture-cartography — invented fixture excursion

**Status:** OPEN — invented fixture excursion; no live counterpart exists.

## Acceptance

- [ ] The invented cartography sketch is drawn inside the fixture world.

## Notes

This excursion is part of M-fixture-aurora in the invented ledger.
")

(def ^:private soundings-text
  "# E-fixture-soundings — invented fixture excursion

**Status:** OPEN — invented fixture excursion; no live counterpart exists.

## Acceptance

- [ ] The invented soundings are tabulated inside the fixture world.

## Notes

This excursion relates to M-fixture-boreal and advances M-fixture-aurora.
")

(def ^:private dormant-text
  "# M-fixture-dormant — invented fixture mission, parked

**Status:** PARKED — invented fixture mission; an edge target only.
")

(def ^:private closed-ledger-text
  "# M-fixture-closed-ledger — invented fixture mission, already complete

**Status:** COMPLETE — invented fixture mission. It is a population member
with no reading record: the typed unread count the certificate must carry.
")

;; [document-key text marker-line] — marker lines are what the readings quote.
(def ^:private docs
  {:aurora        {:repo "futon0" :rel "holes/missions/M-fixture-aurora.md"
                   :text aurora-text}
   :boreal        {:repo "futon0" :rel "holes/missions/M-fixture-boreal.md"
                   :text boreal-text
                   :edges [{:to "M-fixture-dormant" :kind "depends-on" :basis "declared"
                            :quote "depends on M-fixture-dormant being settled before it can close"}]}
   :cartography   {:repo "futon2" :rel "holes/excursions/E-fixture-cartography.md"
                   :text cartography-text
                   :edges [{:to "M-fixture-aurora" :kind "part-of" :basis "declared"
                            :quote "part of M-fixture-aurora in the invented ledger"}]}
   :soundings     {:repo "futon2" :rel "holes/excursions/E-fixture-soundings.md"
                   :text soundings-text
                   :edges [{:to "M-fixture-boreal" :kind "relates-to" :basis "inferred"
                            :quote "relates to M-fixture-boreal"}
                           {:to "M-fixture-aurora" :kind "advances" :basis "declared"
                            :quote "advances M-fixture-aurora"}]}
   :dormant       {:repo "futon0" :rel "holes/missions/M-fixture-dormant.md"
                   :text dormant-text :member? false :records? false}
   :closed-ledger {:repo "futon2" :rel "holes/missions/M-fixture-closed-ledger.md"
                   :text closed-ledger-text :records? false}})

(defn- line-of [text needle]
  (inc (first (keep-indexed (fn [i l] (when (str/includes? l needle) i))
                            (str/split-lines text)))))

(defn build-world!
  "Build the invented readings world in a fresh temp directory; every pin is
  computed from the fixture bytes at call time, so the world cannot drift.
  Returns {:root :code-root :repos :checker :members-file :batches
           :member-count :expected-read :options :records}."
  []
  (let [root (temp-dir "giraffe-readings-fixture-")
        code (io/file root "code")
        _ (.mkdirs (io/file code "futon0" ".git"))
        _ (.mkdirs (io/file code "futon2" ".git"))
        files (into {} (map (fn [[k {:keys [repo rel text]}]]
                              [k (write! code (str repo "/" rel) text)]))
                        docs)
        ;; The REAL checker contract, copied beside members.json (consume
        ;; requires the checker's own members.json to equal :members-file).
        readings-dir (io/file root "readings")
        checker (write! readings-dir "check_edges.py"
                        (slurp (io/resource "giraffe/check_edges.py")))
        members (vec (for [[k {:keys [member?] :or {member? true}}] docs
                           :when member?
                           :let [^java.io.File f (files k)]]
                       {:id (second (re-matches #"([MET]-[^/]+)\.md" (.getName f)))
                        :path (.getAbsolutePath f) :sha256 (sha256-file f)}))
        _ (write! readings-dir "members.json" (json/generate-string members {:pretty true}))
        records (vec (keep (fn [[k {:keys [records? edges] :or {records? true}}]]
                             (when records?
                               (let [^java.io.File f (files k)
                                     id (second (re-matches #"([MET]-[^/]+)\.md" (.getName f)))]
                                 {:id id
                                  :source {:path (.getAbsolutePath f) :sha256 (sha256-file f)}
                                  :status_as_read "open"
                                  :purpose (str "Invented fixture reading of " id
                                                " for the hermetic readings test world.")
                                  :edges (mapv (fn [{:keys [quote] :as e}]
                                                 (assoc e :evidence
                                                        {:path (.getAbsolutePath f)
                                                         :sha256 (sha256-file f)
                                                         :line (line-of (:text (docs k)) quote)
                                                         :quote quote}))
                                               (or edges []))
                                  :unresolved []})))
                           docs))
        batch (write! readings-dir "batch-00.jsonl"
                      (str (str/join "\n" (map json/generate-string records)) "\n"))
        code-root (.getAbsolutePath code)
        repos [(.getAbsolutePath (io/file code "futon0"))
               (.getAbsolutePath (io/file code "futon2"))]
        options {:checker (.getAbsolutePath checker)
                 :members-file (.getAbsolutePath (io/file readings-dir "members.json"))
                 :batches [(.getAbsolutePath batch)]
                 :repos repos
                 ;; The checker's code-root override, merged over the process
                 ;; env by consume's :checker-env seam (2026-11-15).
                 :checker-env {"GIRAFFE_CHECK_CODE_ROOT" code-root}}]
    {:root root :code-root code-root :repos repos
     :checker (:checker options) :members-file (:members-file options)
     :batches (:batches options) :options options
     :members members :records records
     :member-count (count members) :expected-read (count records)}))

;;; ------------------------------------------------------------------ the tests

(deftest real-pilot-certifies-members-not-absent-edges
  (let [world (build-world!)
        options (:options world)
        result (readings/consume options)
        records (map #(json/parse-string % true)
                     (remove str/blank? (str/split-lines (slurp (first (:batches world))))))
        expected (count records)
        coverage (:coverage result)
        rows (into {} (map (juxt :id identity)) (:members coverage))]
    ;; Member counts are restated relative to the fixture (the pre-hermetic
    ;; test pinned the real pilot's 636 members).
    (is (= (:member-count world) (:member-count coverage)))
    (is (= expected (get-in coverage [:counts :read])))
    (is (= (- (:member-count world) expected) (get-in coverage [:counts :unread])))
    ;; One fixture member (the closed mission) deliberately has no record.
    (is (= 1 (get-in coverage [:counts :unread])))
    (is (= :incomplete (:status coverage)))
    (is (every? :passed? (:checked-records result)))
    (doseq [record records]
      (is (= (count (:edges record)) (:edge-count (rows (:id record)))))
      (is (= :read (:status (rows (:id record))))))
    (is (every? #(#{:declared :inferred} (:basis %)) (:edges result)))
    (is (every? #(#{"part-of" "advances" "relates-to" "duplicates" "superseded-by"} (:kind %)) (:links result)))
    ;; The fixture exercises three partitions: in-scope links (3), an
    ;; out-of-scope prerequisite (boreal -> dormant, not a member), and no
    ;; rejections.
    (is (= 3 (count (:links result))))
    (is (= 1 (count (:out-of-scope result))))
    (is (= (reduce + (map #(count (:edges %)) records))
           (+ (count (:edges result)) (count (:links result))
              (count (:out-of-scope result)) (count (:rejected result)))))
    (let [edges-file (write! (:root world) "edges.json" "{\"edges\": [], \"rejected\": []}")
          adapter (excursions/missions-and-excursions->input
                   {:edges-file (.getAbsolutePath edges-file)
                    :code-root (:code-root world) :repos (:repos world)
                    :reading-checker (:checker options) :members-file (:members-file options)
                    :reading-batches (:batches options)
                    :reading-checker-env (:checker-env options)})
          pass (excursions/combined-pass (:input adapter))]
      ;; :order holds the four OPEN fixture items (aurora, boreal,
      ;; cartography, soundings): the closed member is not an open item and
      ;; the parked edge target never joined the population. Restated from
      ;; the pilot's 636.
      (is (= 4 (count (:order pass))))
      ;; No reading edge is a counting prerequisite (links + one out-of-scope
      ;; depends-on), so no row has dependents or unblocks from the reading.
      ;; The pilot asserted nils because most of its rows were unread, and an
      ;; unread/unresolved row is :undetermined with nil cone/unblocks. Every
      ;; open fixture item is read with no counting prerequisite, so each row
      ;; is KNOWN with exactly 0 (claude-13 review: "never positive" also
      ;; accepted nil, i.e. a known row wrongly falling back to undetermined).
      ;; Both branches, exactly: boreal's declared prerequisite is the parked
      ;; (cancelled) non-member, so boreal is :undetermined with nil terms;
      ;; the other three are KNOWN with exactly 0.
      (let [by-id (into {} (map (juxt :id identity)) (:order pass))
            boreal (by-id :M-fixture-boreal)
            others (vals (dissoc by-id :M-fixture-boreal))]
        (is (= [:undetermined :prerequisite-cancelled nil nil]
               ((juxt :status :reason :cone :unblocks) boreal)))
        (is (= :unknown (get-in boreal [:graph-terms :status])))
        (is (= 3 (count others)))
        (is (every? #(= [:ready :known 0 0]
                        [(:status %) (get-in % [:graph-terms :status]) (:cone %) (:unblocks %)])
                    others)))
      (is (every? #(= (:population-sha coverage) (get-in % [:graph-terms :population-sha])) (:order pass))))))

(deftest checker-results-are-temp-path-free-and-deterministic
  ;; The checker runs on a random temp file; its output used to embed that
  ;; path, making the coverage certificate -- and so input.edn's sha256 and
  ;; the minted run id -- differ on every run over an identical corpus.  Two
  ;; consumes over the same fixture must agree byte-for-byte.
  (let [dir (java.nio.file.Files/createTempDirectory "giraffe-scrub-" (make-array java.nio.file.attribute.FileAttribute 0))
        checker (.getPath (io/file (str dir) "check_edges.py"))
        members (.getPath (io/file (str dir) "members.json"))
        batch (.getPath (io/file (str dir) "batch-00.jsonl"))
        line "{\"id\":\"E-a\",\"edges\":[],\"unresolved\":[]}"]
    (try
      (spit checker (str "#!/usr/bin/env python3\n"
                         "import sys\n"
                         "def main(paths):\n"
                         " sys.stderr.write(paths[0] + ':1 E-a source file changed since the census\\n')\n"
                         " print(paths[0] + ':1 E-a source file changed since the census')\n"
                         " return 1\n"))
      (spit members "[{\"id\":\"E-a\",\"path\":\"x\",\"sha256\":\"00\"}]")
      (spit batch (str line "\n"))
      (let [opts {:checker checker :members-file members :batches [batch]
                  :repos missions/default-repos}
            one (readings/consume opts)
            two (readings/consume opts)
            text (pr-str (:coverage one))
            messages (mapcat #(mapcat (juxt :out :err) (:checker-results %))
                             (get-in one [:coverage :members]))]
        (is (= (:coverage one) (:coverage two)))
        (is (= (:checked-records one) (:checked-records two)))
        (is (= 1 (get-in one [:coverage :counts :unread])))
        (is (pos? (count messages)))
        (is (every? #(str/includes? % "giraffe-reading-line:1 E-a") messages))
        (is (nil? (re-find #"giraffe-reading-\d+" text))))
      (finally
        (run! #(try (io/delete-file %) (catch Exception _))
              [checker members batch])
        (.delete (.toFile dir))))))

(deftest duplicate-pilot-record-is-ambiguous
  ;; The duplicated line is the fixture's first batch record (the
  ;; pre-hermetic test reused the real pilot's first line).
  (let [world (build-world!)
        line (first (remove str/blank? (str/split-lines (slurp (first (:batches world))))))
        file (java.io.File/createTempFile "giraffe-duplicate-" ".jsonl")]
    (try
      (spit file (str line "\n" line "\n"))
      (let [result (readings/consume (assoc (:options world) :batches [(.getPath file)]))]
        (is (= 1 (get-in result [:coverage :counts :ambiguous])))
        (is (empty? (:edges result)))
        (is (empty? (:links result))))
      (finally (io/delete-file file)))))

(deftest prerequisite-direction-and-basis-control
  ;; The fixture's links are non-readiness edges. These are conversion
  ;; controls, not a claim that the quoted relation is a real prerequisite.
  (let [world (build-world!)
        records (:records world)
        ids (set (map (comp keyword :id) (:members world)))
        [record edge] (first (for [r records e (:edges r)
                                  :when (contains? ids (keyword (:to e)))] [r e]))
        convert (ns-resolve 'futon3c.giraffe.readings 'convert-edge)
        forward (:edge (convert (:repos world) ids (:id record)
                                (assoc edge :kind "depends-on" :basis "declared")))
        backward (:edge (convert (:repos world) ids (:id record)
                                 (assoc edge :kind "unblocks" :basis "inferred")))]
    (is (some? edge))
    (is (= [(keyword (:id record)) (keyword (:to edge))]
           [(:from forward) (:to forward)]))
    (is (= [(:to forward) (:from forward)] [(:from backward) (:to backward)]))
    (is (= [:declared :inferred] [(:basis forward) (:basis backward)]))
    (is (= :closure (:class forward) (:class backward)))
    (is (= (:evidence edge) (:evidence-pin forward)))
    ;; The reader's stated class survives conversion (2026-11-10); an unknown
    ;; or absent class stays :closure.
    (is (= :artifact (:class (:edge (convert (:repos world) ids (:id record)
                                             (assoc edge :kind "depends-on" :basis "declared"
                                                    :intended_class "artifact"))))))
    (is (= :alternative (:class (:edge (convert (:repos world) ids (:id record)
                                                (assoc edge :kind "depends-on" :basis "declared"
                                                       :intended_class "alternative"))))))
    (is (= :closure (:class (:edge (convert (:repos world) ids (:id record)
                                            (assoc edge :kind "depends-on" :basis "declared"
                                                   :intended_class "bogus"))))))))

(deftest scope-defaults-to-missions-excursions-and-flows-into-graph-terms
  (let [world (build-world!)
        result (readings/consume (:options world))
        input {:as-of "scope-test"
               :items [{:id :E-a :state-group :unstarted :source-order 0}]
               :edges []
               :graph-coverage (dissoc (:coverage result) :members)
               :reading-evidence {:edges [] :links [] :out-of-scope []
                                  :rejected [] :unresolved []}}
        pass (excursions/combined-pass input)]
    (is (= :population/missions-excursions (:scope (:coverage result))))
    (is (every? #(= :population/missions-excursions (get-in % [:graph-terms :scope])) (:order pass)))))

(deftest explicit-scope-labels-certificate-and-graph-terms
  (let [world (build-world!)
        result (readings/consume (assoc (:options world) :scope :population/missions-excursions-tickets))
        input {:as-of "scope-test"
               :items [{:id :E-a :state-group :unstarted :source-order 0}]
               :edges []
               :graph-coverage (assoc (dissoc (:coverage result) :members)
                                      :status :incomplete)
               :reading-evidence {:edges [] :links [] :out-of-scope []
                                  :rejected [] :unresolved []}}
        pass (excursions/combined-pass input)]
    (is (= :population/missions-excursions-tickets (:scope (:coverage result))))
    (is (every? #(= :population/missions-excursions-tickets (get-in % [:graph-terms :scope])) (:order pass)))))

(deftest retired-coverage-and-reopening
  (let [{:keys [members-file batches options records members]} (build-world!)
        id (:id (first records))
        retirement {:reason "source-missing" :at_ms 1234 :last_sha256 "invented-pin"}
        marked (mapv #(if (= id (:id %)) (assoc % :retired retirement) %) members)
        batch (first batches)]
    (spit members-file (json/generate-string marked))
    ;; Contradictory retirement + record must never admit the record's links.
    (let [result (readings/consume (assoc options :target-id id))]
      (is (= :ambiguous (get-in result [:coverage :members 0 :status])))
      (is (empty? (:edges result)))
      (is (empty? (:links result))))
    (spit batch (str/join "\n" (map json/generate-string (remove #(= id (:id %)) records))))
    (let [result (readings/consume (assoc options :target-id id))
          row (get-in result [:coverage :members 0])
          drift (excursions/reading-drift [{:id (keyword id) :source-pin (:source row)}]
                                         (:coverage result))]
      (is (= :retired (:status row)))
      (is (= retirement (:retired row)))
      (is (empty? (:edges result)))
      (is (= [(keyword id)] (:unread-new drift)))
      (is (empty? (:read drift))))))

(deftest batch-checker-matches-individual-processes
  (let [{:keys [checker options batches root]} (build-world!)
        line (first (str/split-lines (slurp (first batches))))
        record (json/parse-string line true)
        missing (assoc record :edges [{:to "M-fixture-boreal" :kind "depends-on"
                                       :basis "declared" :evidence {:path (str root "/missing.md")
                                                                  :sha256 "absent" :line 1 :quote "invented missing evidence"}}])
        lines [line (json/generate-string (assoc record :status_as_read "invalid"))
               "{bad json" (json/generate-string missing)]
        entries (mapv (fn [i line] {:batch "fixture.jsonl" :line-number (inc i) :line line})
                      (range) lines)
        env (:checker-env options)
        individual (mapv #(#'readings/checked-record checker (:batch %) (:line-number %) (:line %) env) entries)
        calls (atom 0)
        sh clojure.java.shell/sh
        batch (with-redefs [clojure.java.shell/sh (fn [& args] (swap! calls inc) (apply sh args))]
                (readings/check-records checker entries env))]
    (is (= individual batch))
    (is (= [true false false false] (mapv :passed? batch)))
    (is (= 1 @calls))
    (reset! calls 0)
    (with-redefs [clojure.java.shell/sh (fn [& args] (swap! calls inc) (apply sh args))]
      (readings/consume options))
    (is (= 1 @calls))))

(deftest batch-driver-errors-are-never-passes
  (doseq [result [{:exit 2 :out "" :err "fixture crash"}
                  {:exit 0 :out "" :err ""}
                  {:exit 0 :out "not-json" :err ""}]]
    (with-redefs [clojure.java.shell/sh (constantly result)]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Batch checker failed"
                           (readings/check-records "fixture-checker.py"
                                                   [{:batch "fixture" :line-number 1 :line "{}"}]))))))

(deftest actual-driver-crash-is-an-error
  (let [root (temp-dir "giraffe-driver-crash-")
        checker (write! root "check_edges.py" "def main(paths):\n    raise RuntimeError('invented crash')\n")]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Batch checker failed"
                         (readings/check-records checker [{:batch "fixture" :line-number 1 :line "{}"}])))))
