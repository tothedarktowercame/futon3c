(ns futon3c.giraffe.excursions-test
  "Giraffe excursions/census tests.

  HERMETIC WORLD (2026-11-15, kimi-2): these tests used to read the live
  corpus (the 2026-11-01 GR-1 input.edn under /home/joe/code/storage, whose
  source pins point at live documents; the frozen pilot under
  /home/joe/notes/wm-next/giraffe-excursions/; the live registry and a
  tick-run record). Drifted documents tripped the correct pin refusals
  (:provenance/source-pin-mismatch on T-arxana-direct-reads, checker
  \"source file changed since the census\"). The world is now INVENTED at
  test time in a temp directory; pins and evidence line numbers are computed
  from fixture bytes when the test runs. The checker is the REAL contract:
  resources/giraffe/check_edges.py (vendored from the pilot's generic
  checker; GIRAFFE_CHECK_CODE_ROOT overrides its code root, passed through
  consume's :checker-env seam). Claims that pinned the real corpus (636
  field members, named live ids) are restated relative to the fixture and
  say so in a comment."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.edn :as edn] [clojure.set :as set]
            [futon3c.giraffe.readings :as readings]
            [futon3c.giraffe.excursions :as e]
            [futon3c.giraffe.missions :as missions]
            [futon3c.giraffe.pass :as pass]
            [futon3c.giraffe.run :as writer]
            [futon2.aif.outer-score :as score]
            [futon2.aif.outer-observation :as observation]
            [futon2.aif.task-completion-surface :as completion]
            [futon2.aif.wm.construction-inputs :as sources]
            [futon2.aif.mission-registry :as registry]))

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

(defn- line-of [text needle]
  (inc (first (keep-indexed (fn [i l] (when (str/includes? l needle) i))
                            (str/split-lines text)))))

;; Invented documents. Names and prose are fixture inventions; no real
;; mission, excursion or ticket text appears here.

(def ^:private interim-text
  "# M-fixture-interim — invented fixture mission (excursions test world)

**Status:** OPEN — invented fixture mission; no live counterpart exists.

## Acceptance

- [ ] The invented interim ledger is handed over inside the fixture world.

## Notes

This mission depends on M-fixture-keystone, which the fixture closes early.
")

(def ^:private keystone-text
  "# M-fixture-keystone — invented fixture mission, already complete

**Status:** COMPLETE — invented fixture mission; the closed prerequisite.
")

(def ^:private aurora-text
  "# M-fixture-aurora — invented fixture mission (excursions test world)

**Status:** OPEN — invented fixture mission; no live counterpart exists.

## Acceptance

- [ ] The invented aurora ledger balances inside the fixture world.
")

(def ^:private boreal-text
  "# M-fixture-boreal — invented fixture mission (excursions test world)

**Status:** OPEN — invented fixture mission; no live counterpart exists.

## Acceptance

- [ ] The invented boreal ledger is reconciled inside the fixture world.

## Notes

This mission depends on T-fixture-unsurveyed, which the fixture never surveys.
")

(def ^:private cartography-text
  "# E-fixture-cartography — invented fixture excursion

**Status:** OPEN — invented fixture excursion; no live counterpart exists.

## Acceptance

- [ ] The invented cartography sketch is drawn inside the fixture world.

## Notes

This excursion is part of M-fixture-aurora in the invented ledger.
")

(def ^:private murky-text
  "# E-fixture-murky — invented fixture excursion

**Status:** OPEN — invented fixture excursion; no live counterpart exists.

## Acceptance

- [ ] The invented murky survey is sounded inside the fixture world.

## Notes

This excursion depends on T-fixture-unsurveyed, which the fixture never surveys.
")

(def ^:private prosey-text
  "# E-fixture-prosey — invented fixture excursion

**Status:** OPEN — invented fixture excursion; no live counterpart exists.

## Acceptance

- The invented prose criterion is stated without a locator or a checkbox.
")

(def ^:private plain-text
  "# E-fixture-plain — invented fixture excursion

**Status:** OPEN — invented fixture excursion; no live counterpart exists.

## Acceptance

The invented plain excursion states no criteria, locators or checkboxes here.
")

(def ^:private unsurveyed-text
  "# T-fixture-unsurveyed — invented fixture ticket

**Status:** OPEN — invented fixture ticket; an out-of-scope edge target only.

## What would close this

The invented survey is commissioned and its findings are recorded.
")

(def ^:private ledger-gap-text
  "# T-fixture-ledger-gap — invented fixture ticket

**Status:** OPEN — invented fixture ticket; no live counterpart exists.

Parent: M-fixture-aurora

## What would close this

The invented ledger gap is described and an invented remedy is proposed.
")

;; id -> {:repo :rel :text :open? :record? :edges [{:to :kind :basis :quote}]}
;; :record? false = population member with no reading record (closed docs
;; need none). Edges are stated in the member's reading record; quotes are
;; verbatim lines of the member's own document, located at build time.
(def ^:private docs
  {:interim     {:repo "futon0" :rel "holes/missions/M-fixture-interim.md"
                 :text interim-text
                 :edges [{:to "M-fixture-keystone" :kind "depends-on" :basis "declared"
                          :quote "depends on M-fixture-keystone, which the fixture closes early"}]}
   :keystone    {:repo "futon0" :rel "holes/missions/M-fixture-keystone.md"
                 :text keystone-text :open? false :record? false}
   :aurora      {:repo "futon0" :rel "holes/missions/M-fixture-aurora.md"
                 :text aurora-text}
   :boreal      {:repo "futon0" :rel "holes/missions/M-fixture-boreal.md"
                 :text boreal-text
                 :edges [{:to "T-fixture-unsurveyed" :kind "depends-on" :basis "declared"
                          :quote "depends on T-fixture-unsurveyed, which the fixture never surveys"}]}
   :cartography {:repo "futon2" :rel "holes/excursions/E-fixture-cartography.md"
                 :text cartography-text
                 :edges [{:to "M-fixture-aurora" :kind "part-of" :basis "declared"
                          :quote "part of M-fixture-aurora in the invented ledger"}]}
   :murky       {:repo "futon2" :rel "holes/excursions/E-fixture-murky.md"
                 :text murky-text
                 :edges [{:to "T-fixture-unsurveyed" :kind "depends-on" :basis "declared"
                          :quote "depends on T-fixture-unsurveyed, which the fixture never surveys"}]}
   :prosey      {:repo "futon2" :rel "holes/excursions/E-fixture-prosey.md"
                 :text prosey-text}
   :plain       {:repo "futon2" :rel "holes/excursions/E-fixture-plain.md"
                 :text plain-text}
   :unsurveyed  {:repo "futon2" :rel "holes/tickets/T-fixture-unsurveyed.md"
                 :text unsurveyed-text :member? false :record? false :open? false}
   :ledger-gap  {:repo "futon2" :rel "holes/tickets/T-fixture-ledger-gap.md"
                 :text ledger-gap-text :member? false :record? false :open? false}})

(def ^:private doc-id-pattern #"([MET]-[^/]+)\.md")

(defn build-world!
  "Build the invented world in a fresh temp directory; every pin is computed
  from the fixture bytes at call time, so the world cannot drift."
  []
  (let [root (temp-dir "giraffe-excursions-fixture-")
        code (io/file root "code")
        _ (.mkdirs (io/file code "futon0" ".git"))
        _ (.mkdirs (io/file code "futon2" ".git"))
        files (into {} (map (fn [[k {:keys [repo rel text]}]]
                              [k (write! code (str repo "/" rel) text)]))
                        docs)
        doc-id (fn [k] (second (re-matches doc-id-pattern
                                           (.getName ^java.io.File (files k)))))
        readings-dir (io/file root "readings")
        checker (write! readings-dir "check_edges.py"
                        (slurp (io/resource "giraffe/check_edges.py")))
        members (vec (for [[k {:keys [member?] :or {member? true}}] docs
                           :when member?
                           :let [^java.io.File f (files k)]]
                       {:id (doc-id k) :path (.getAbsolutePath f)
                        :sha256 (sha256-file f)}))
        _ (write! readings-dir "members.json" (json/generate-string members {:pretty true}))
        records (vec (keep (fn [[k {:keys [record? edges] :or {record? true member? true}}]]
                             (when (and (get (docs k) :member? true) record?)
                               (let [^java.io.File f (files k)
                                     id (doc-id k)]
                                 {:id id
                                  :source {:path (.getAbsolutePath f) :sha256 (sha256-file f)}
                                  :status_as_read (if (:open? (docs k) true) "open" "done-unmarked")
                                  :purpose (str "Invented fixture reading of " id
                                                " for the hermetic excursions test world.")
                                  :edges (mapv (fn [{:keys [quote] :as edge}]
                                                 (assoc edge :evidence
                                                        {:path (.getAbsolutePath f)
                                                         :sha256 (sha256-file f)
                                                         :line (line-of (:text (docs k)) quote)
                                                         :quote quote}))
                                               (or edges []))
                                  :unresolved []})))
                           docs))
        batch (write! readings-dir "batch-00.jsonl"
                      (str (str/join "\n" (map json/generate-string records)) "\n"))
        edges-file (write! root "edges.json" "{\"edges\": [], \"rejected\": []}")
        code-root (.getAbsolutePath code)
        repos [(.getAbsolutePath (io/file code "futon0"))
               (.getAbsolutePath (io/file code "futon2"))]
        reading-options {:checker (.getAbsolutePath checker)
                         :members-file (.getAbsolutePath (io/file readings-dir "members.json"))
                         :batches [(.getAbsolutePath batch)]
                         :repos repos
                         :checker-env {"GIRAFFE_CHECK_CODE_ROOT" code-root}}]
    {:root root :code-root code-root :repos repos :files files :doc-id doc-id
     :edges-file (.getAbsolutePath edges-file)
     :reading-options reading-options
     :members members :records records
     :open-count (count (filter #(:open? (val %) true) docs))}))

(defn- census-adapter
  "The hermetic census: invented repos, invented readings, real checker."
  ([world] (census-adapter world {}))
  ([world extra]
   (e/missions-and-excursions->input
    (merge {:edges-file (:edges-file world)
            :code-root (:code-root world) :repos (:repos world)
            :reading-checker (get-in world [:reading-options :checker])
            :members-file (get-in world [:reading-options :members-file])
            :reading-batches (get-in world [:reading-options :batches])
            :reading-checker-env (get-in world [:reading-options :checker-env])}
           extra))))

(defn gr1-fixture
  "A fabricated pass input in the shape of the retained 2026-11-01 GR-1
  input.edn, over the invented world: the interim mission's checked reading
  calls its dependency on the (closed) keystone out of scope, and boreal's
  dependency on the unsurveyed ticket is out of scope with no population
  record. Pins ride the fixture files, so provenance can verify them."
  [world]
  (let [doc-id (:doc-id world)
        files (:files world)
        member (fn [k status]
                 {:id (doc-id k) :source {:path (.getAbsolutePath ^java.io.File (files k))
                                          :sha256 (sha256-file (files k))}
                  :status status})
        item (fn [i k state-group]
               {:id (keyword (doc-id k)) :state-group state-group :source-order i
                :source-pin {:path (.getAbsolutePath ^java.io.File (files k))
                             :sha256 (sha256-file (files k))}
                :dependency-reading (member k :read)})
        open-keys [:interim :aurora :boreal :cartography :murky :prosey :plain :ledger-gap]
        items (vec (concat (map-indexed (fn [i k] (item i k :unstarted)) open-keys)
                           [(item (count open-keys) :keystone :completed)]))
        out-of-scope (fn [from-k to-k]
                       {:from (doc-id from-k) :to (doc-id to-k) :kind "depends-on"
                        :basis "declared" :reading-member (doc-id from-k)
                        :note "Invented fixture reading edge."
                        :evidence {:path (.getAbsolutePath ^java.io.File (files from-k))
                                   :sha256 (sha256-file (files from-k))
                                   :line (line-of (:text (docs from-k))
                                                  (:quote (first (:edges (docs from-k)))))
                                   :quote (:quote (first (:edges (docs from-k))))}})]
    {:as-of "2026-11-01T00:00:00Z"
     :edges []
     :graph-coverage {:status :complete :member-count (count items)
                      :scope :population/missions-excursions-tickets
                      :population-sha (readings/sha256 (pr-str (mapv :id items)))
                      :counts {:read (count open-keys)}
                      :checker-sha (readings/sha256 "invented-fixture-checker-pin")}
     :reading-evidence {:edges [] :links [] :rejected [] :unresolved []
                        :out-of-scope [(out-of-scope :interim :keystone)
                                       (out-of-scope :boreal :unsurveyed)]}
     :items items}))

(defn row-by-id [pass-output id]
  (first (filter #(= id (:id %)) (:order pass-output))))

(defn- keystone-prerequisite [row]
  (first (filter #(= "M-fixture-keystone" (:to %))
                 (get-in row [:graph-terms :prerequisites]))))

;;; ------------------------------------------------------------------ the tests

(deftest requisition-records-never-enter-the-giraffe-population
  (let [requisition {:id "E-kimi-task-103"
                     :requisition-record? true
                     :source {:path "holes/excursions/E-kimi-task-103.md"}}
        ordinary {:id "E-ordinary"
                  :requisition-record? false
                  :source {:path "holes/excursions/E-ordinary.md"}}
        declarations (fn [ids _]
                       (mapv (fn [id] {:target id :path (str id ".md")}) ids))]
    (with-redefs [registry/load-excursions
                  (fn [_] {:excursions [requisition ordinary]})
                  registry/live-excursion? (constantly true)
                  missions/missions->input
                  (fn [_] {:input {:items []} :excluded []})
                  sources/target-source-declarations declarations
                  completion/describe (fn [_] {:completion-class :fixture})]
      (let [ids (set (map :id (get-in (e/missions-and-excursions->input
                                       {:code-root "/fixture"
                                        :edges-file "/unused"})
                                      [:input :items])))]
        (is (= #{:E-ordinary} ids))
        (is (not (contains? ids :E-kimi-task-103)))))))

(deftest reading-stage-reranks-the-pinned-pass
  (let [input (gr1-fixture (build-world!))
        base (pass/pass input)
        result (e/combined-pass input)
        rows (:order result)
        ready (filter #(= :ready (:status %)) rows)
        undetermined (filter #(= :undetermined (:status %)) rows)
        post-ready-order (mapv :id ready)
        post-ready? (set post-ready-order)
        base-ready-order (mapv :id (filter #(post-ready? (:id %)) (:order base)))
        ;; The demoted row is the fixture's boreal (was M-apm-capability-
        ;; ratchet in the retained pass): ready in the pinned pass, called
        ;; undetermined once its out-of-scope reading is honoured.
        demoted (row-by-id result :M-fixture-boreal)]
    (is (every? #(= :after-reading (:rank-basis %)) rows))
    (is (< (apply max (map :rank ready))
           (apply min (map :rank undetermined))))
    (is (= :undetermined (:status demoted)))
    (is (> (:rank demoted) (count ready)))
    (is (= base-ready-order post-ready-order))))

(deftest report-giraffe-column-agrees-with-post-reading-order
  (let [input (gr1-fixture (build-world!))
        result (e/combined-pass input)
        root (str (java.nio.file.Files/createTempDirectory
                   "giraffe-gr-2-report-"
                   (make-array java.nio.file.attribute.FileAttribute 0)))
        written (writer/write-run! {:root root
                                    :adapter-result {:input input :rejected []}
                                    :pass-output result})
        table-lines (filter #(str/starts-with? % "|")
                            (str/split-lines
                             (slurp (io/file (:dir written) "report.md"))))
        data-lines (take (count (:order result)) (drop 2 table-lines))
        giraffe-ids (mapv (fn [line]
                            (-> (str/split line #"\|")
                                (nth 6)
                                str/trim
                                keyword))
                          data-lines)]
    (is (= (mapv :id (:order result)) giraffe-ids))))

(deftest completed-out-of-scope-prerequisite-is-met-from-retained-population
  ;; The retained 2026-11-01 bad case, restated on the fixture: the checked
  ;; reading calls interim's dependency out of scope while the population
  ;; retains keystone's CLOSED mission record.
  (let [input (gr1-fixture (build-world!))
        target (get (into {} (map (juxt :id identity)) (:items input))
                    :M-fixture-keystone)
        row (row-by-id (e/combined-pass input) :M-fixture-interim)
        prerequisite (keystone-prerequisite row)]
    (is (= :completed (:state-group target)))
    (is (= :met (:resolution prerequisite)))
    (is (= (:source-pin target) (:source-pin prerequisite)))
    (is (= :ready (:status row)))
    (is (not= :needs/dependency-reading (:reason row)))))

(deftest cancelled-out-of-scope-prerequisite-is-typed
  (let [input (gr1-fixture (build-world!))
        input' (update input :items
                       (fn [items]
                         (mapv #(if (= :M-fixture-keystone (:id %))
                                  (assoc % :state-group :cancelled)
                                  %)
                               items)))
        row (row-by-id (e/combined-pass input') :M-fixture-interim)
        prerequisite (keystone-prerequisite row)]
    (is (= 1 (count (get-in row [:graph-terms :prerequisites]))))
    (is (= :cancelled (:resolution prerequisite)))
    (is (= :prerequisite-cancelled (:absence prerequisite)))
    (is (= :undetermined (:status row)))
    (is (= :prerequisite-cancelled (:reason row)))))

(deftest absent-out-of-scope-prerequisite-remains-unresolved
  (let [input (gr1-fixture (build-world!))
        input' (update input :items
                       #(filterv (fn [item]
                                   (not= :M-fixture-keystone (:id item)))
                                 %))
        row (row-by-id (e/combined-pass input') :M-fixture-interim)
        prerequisite (keystone-prerequisite row)]
    (is (= :absent (:resolution prerequisite)))
    (is (= :out-of-scope-prerequisite (:absence prerequisite)))
    (is (= :undetermined (:status row)))
    (is (= :needs/dependency-reading (:reason row)))))

(deftest open-retained-out-of-scope-prerequisite-is-typed-not-refused
  ;; Bad case for the resolution: the checked reading calls the dependency out
  ;; of scope while the population retains the target as open. Reader and
  ;; population disagree; the pass must not refuse (that would stop every
  ;; click), the member stays undetermined under a typed reason.
  (let [input (gr1-fixture (build-world!))
        input' (update input :items
                       (fn [items]
                         (mapv #(if (= :M-fixture-keystone (:id %))
                                  (assoc % :state-group :started)
                                  %)
                               items)))
        row (row-by-id (e/combined-pass input') :M-fixture-interim)
        prerequisite (keystone-prerequisite row)]
    (is (= :inconsistent (:resolution prerequisite)))
    (is (= :inconsistent-out-of-scope-prerequisite (:absence prerequisite)))
    (is (= :started (:target-state-group prerequisite)))
    (is (= :undetermined (:status row)))
    (is (= :needs/dependency-reading (:reason row)))))

(deftest frozen-census-retains-real-completion-classes-and-unknown-graph
  (let [world (build-world!)
        adapter (census-adapter world)
        result (e/combined-pass (:input adapter))
        rows (into {} (map (juxt :id identity)) (:order result))
        ids (set (map (comp name :id) (:order result)))
        ;; The independent census the open carrier is compared against,
        ;; restated from the tick-run record to the fixture registry.
        open (set (concat
                   (map :id (filter registry/live-mission?
                                    (:missions (registry/load-missions-from-files
                                                (:code-root world)))))
                   (map :id (filter registry/live-excursion?
                                    (:excursions (registry/load-excursions
                                                  (:code-root world)))))
                   (map :id (filter registry/live-ticket?
                                    (:tickets (registry/load-tickets
                                               (:code-root world)))))))
        missing (set/difference open ids)
        output (writer/write-run! {:root (str (java.nio.file.Files/createTempDirectory
                                              "q1-E-test-"
                                              (make-array java.nio.file.attribute.FileAttribute 0)))
                                   :adapter-result adapter :pass-output result})]
    (let [excursions (registry/load-excursions (:code-root world))
          observed (observation/giraffe-field
                    (:dir output) {:default-reach 1/2}
                    {:loaded-missions (registry/load-missions-from-files (:code-root world))
                     :loaded-excursions excursions :loaded-tickets {:tickets []}
                     :excursion-targets (mapv :id (:excursions excursions))})]
      ;; Restated from the live 636: field + excluded partitions exactly the
      ;; fixture's open population (7: three missions + four excursions;
      ;; tickets are not enumerated without :include-tickets).
      (is (= (:open-count world)
             (+ (count (:field observed)) (count (:outer-field/excluded observed)))))
      (is (= :unknown (get-in observed [:facts "E-fixture-murky" :graph-terms :status])))
      (is (= :selected-task-revision-baseline
             (get-in observed [:facts "E-fixture-murky" :completion :producer]))))
    (is (= (:open-count world) (count ids)))
    ;; The fixture's one live ticket is the whole missing set (tickets are
    ;; out of this census without the opt); the pre-hermetic test counted
    ;; 189 live tickets here.
    (is (= #{"T-fixture-unsurveyed" "T-fixture-ledger-gap"} missing))
    (is (every? #(.startsWith ^String % "T-") missing))
    (is (empty? (set/difference ids open)))
    (doseq [[id class] [[:E-fixture-murky :checkbox-locators-with-retained-prose]
                        [:E-fixture-prosey :completion-prose-needs-locators]
                        [:E-fixture-plain :completion-definition-not-found]]]
      (is (= class (get-in rows [id :completion :completion-class]))))
    (is (= :task-completion-author (get-in rows [:E-fixture-plain :completion :producer])))
    ;; No fixture edge is a counting prerequisite, so no row has dependents
    ;; or unblocks. The pre-hermetic test asserted literal nils; readiness
    ;; arithmetic now reports 0 for a ready row with no dependents (nil only
    ;; when a row is undetermined), so the claim is kept as never-positive.
    (is (every? #(not (pos? (or (:cone %) 0))) (vals rows)))
    (is (every? #(not (pos? (or (:unblocks %) 0))) (vals rows)))
    (is (= :undetermined (get-in rows [:E-fixture-murky :status])))
    (is (= :undetermined (get-in rows [:M-fixture-boreal :status])))
    (is (:ok? (writer/verify-run (:dir output))))
    (doseq [step [:by-cone :by-unblocks]]
      (is (= :outer-score/graph-term-unavailable
             (try (score/apply-selection-step {"E-fixture-murky" (rows :E-fixture-murky)}
                                              [{:target "E-fixture-murky"}] step)
                  nil (catch clojure.lang.ExceptionInfo ex (:kind (ex-data ex)))))))))

;; ---------------------------------------------- partial reading drift
;; claude-1, 2026-10-31: the population check used to demand exact equality,
;; so ONE new item blocked the whole field on every click.  Drift is partial:
;; :unread-new and :reading-source-changed items stay out of the graph with a
;; typed reason, are reported in the manifest and coverage, and every other
;; reading still counts.

(def ^:private drift-docs
  {:drift-alpha   {:repo "futon0" :rel "holes/missions/M-fixture-drift-alpha.md"}
   :drift-beta    {:repo "futon0" :rel "holes/missions/M-fixture-drift-beta.md"}
   :drift-gamma   {:repo "futon0" :rel "holes/missions/M-fixture-drift-gamma.md"}
   :drift-delta   {:repo "futon2" :rel "holes/excursions/E-fixture-drift-delta.md"}
   :drift-epsilon {:repo "futon2" :rel "holes/excursions/E-fixture-drift-epsilon.md"}})

(defn- drift-doc-text [rel]
  (let [id (second (re-matches doc-id-pattern rel))]
    (str "# " id " — invented fixture drift member\n\n"
         "**Status:** OPEN — invented fixture document; no live counterpart exists.\n\n"
         "## Acceptance\n\n"
         "- [ ] The invented " id " ledger balances inside the fixture world.\n")))

(defn drift-fixture
  "An invented corpus over five members that are open items whose records
  the REAL checker passes.  DROP removes one member from members.json; its
  record line stays in the batch (unadmitted), so it has no current reading.
  Restated from the frozen pilot corpus, whose live pins drifted."
  [& {:keys [drop]}]
  (let [root (temp-dir "giraffe-drift-fixture-")
        code (io/file root "code")
        _ (.mkdirs (io/file code "futon0" ".git"))
        _ (.mkdirs (io/file code "futon2" ".git"))
        files (into {} (map (fn [[k {:keys [repo rel]}]]
                              [k (write! code (str repo "/" rel) (drift-doc-text rel))]))
                        drift-docs)
        doc-id (fn [k] (second (re-matches doc-id-pattern
                                           (.getName ^java.io.File (files k)))))
        chosen (vec (keys drift-docs))
        chosen-ids (mapv doc-id chosen)
        members (vec (for [k chosen :when (not= k drop)]
                       {:id (doc-id k) :path (.getAbsolutePath ^java.io.File (files k))
                        :sha256 (sha256-file (files k))}))
        records (mapv (fn [k]
                        (let [f (files k)]
                          {:id (doc-id k)
                           :source {:path (.getAbsolutePath ^java.io.File f)
                                    :sha256 (sha256-file f)}
                           :status_as_read "open"
                           :purpose (str "Invented fixture drift reading of " (doc-id k) ".")
                           :edges [] :unresolved []}))
                      chosen)
        dir (io/file root "readings")
        _ (write! dir "check_edges.py" (slurp (io/resource "giraffe/check_edges.py")))
        _ (write! dir "members.json" (json/generate-string members {:pretty true}))
        _ (write! dir "batch-fixture.jsonl"
                  (str (str/join "\n" (map json/generate-string records)) "\n"))
        edges-file (write! root "edges.json" "{\"edges\": [], \"rejected\": []}")
        code-root (.getAbsolutePath code)]
    {:dir (.getAbsolutePath dir) :root root :code-root code-root
     :repos [(.getAbsolutePath (io/file code "futon0"))
             (.getAbsolutePath (io/file code "futon2"))]
     :edges-file (.getAbsolutePath edges-file)
     :chosen chosen-ids :dropped (when drop (doc-id drop))}))

(defn drift-adapter [fixture]
  (let [dir (:dir fixture)
        adapter (e/missions-and-excursions->input
                 {:edges-file (:edges-file fixture)
                  :code-root (:code-root fixture) :repos (:repos fixture)
                  :reading-checker (str dir "/check_edges.py")
                  :members-file (str dir "/members.json")
                  :reading-batches [(str dir "/batch-fixture.jsonl")]
                  :reading-checker-env {"GIRAFFE_CHECK_CODE_ROOT" (:code-root fixture)}
                  :reading-scope :population/missions-excursions-tickets})
        pass (e/combined-pass (:input adapter))]
    {:adapter adapter :pass pass}))

(deftest missing-member-is-unread-new-and-everything-else-still-counts
  (let [fx (drift-fixture :drop (nth (keys drift-docs) 0))
        {:keys [adapter pass]} (drift-adapter fx)
        order-ids (set (map :id (:order pass)))
        coverage (get-in adapter [:input :graph-coverage])
        drift (:drift coverage)]
    (is (= 4 (count (:order pass))) "only the four read members remain in the graph")
    (is (not (contains? order-ids (keyword (:dropped fx)))))
    (is (contains? (set (:unread-new drift)) (keyword (:dropped fx))))
    (is (= :partial (:status coverage)))
    (is (= 4 (get-in coverage [:drift-counts :read])))
    (is (= (- (:open-item-count coverage) 4)
           (+ (get-in coverage [:drift-counts :unread-new])
              (get-in coverage [:drift-counts :reading-source-changed]))))
    (is (= 0 (get-in coverage [:drift-counts :members-not-open])))
    (is (some #(and (= (:id %) (keyword (:dropped fx))) (= :unread-new (:reason %)))
              (:excluded adapter)))
    (let [root (str (java.nio.file.Files/createTempDirectory
                     "giraffe-drift-run-" (make-array java.nio.file.attribute.FileAttribute 0)))
          written (writer/write-run! {:root root :adapter-result adapter :pass-output pass})
          manifest (edn/read-string (slurp (str (:dir written) "/manifest.edn")))]
      (is (:ok? (writer/verify-run (:dir written))))
      (is (= :partial (get-in manifest [:reading-drift :status])))
      (is (= 4 (get-in manifest [:reading-drift :drift-counts :read])))
      (is (contains? (set (map name (get-in manifest [:reading-drift :drift :unread-new])))
                     (:dropped fx)))
      ;; from what the outer field reader consumes: the field carries ONLY
      ;; the read members -- the drifted one is not silently admitted -- and
      ;; its typed exclusion is in the run itself (excluded.edn row,
      ;; manifest :reading-drift, coverage :drift id lists, asserted above).
      ;; (Surfacing non-ticket drift as futon2 :outer-field/excluded rows is
      ;; a named futon2-side follow-up: futon2's absent-excluded path today
      ;; covers open tickets only.)
      (let [excursions (registry/load-excursions (:code-root fx))
            observed (observation/giraffe-field
                      (:dir written) {:default-reach 1/2}
                      {:loaded-missions (registry/load-missions-from-files (:code-root fx))
                       :loaded-excursions excursions :loaded-tickets {:tickets []}
                       :excursion-targets (mapv :id (:excursions excursions))})
            field-targets (set (map :target (:field observed)))]
        (is (= 4 (count field-targets)))
        (is (not (contains? field-targets (:dropped fx))))
        (is (contains? field-targets (nth (:chosen fx) 4)))))))

(deftest read-member-with-wrong-sha-is-reading-source-changed-and-does-not-count
  ;; The classification seam, tested directly: a member that IS read but
  ;; whose sha no longer matches the pin must not count as read. (Already
  ;; hermetic: pure maps, no files.)
  (let [pin {:path "/repo/E-x.md" :sha256 (apply str (repeat 64 \a))}
        item (fn [id sha] {:id (keyword id) :state-group :unstarted
                           :source-pin {:path "/repo/E-x.md" :sha256 sha}})
        open-items [(item "E-good" (apply str (repeat 64 \a)))
                    (item "E-bad" (apply str (repeat 64 \b)))]
        coverage {:members [{:id "E-good" :source pin :status :read}
                            {:id "E-bad" :source pin :status :read}
                            {:id "E-gone" :source pin :status :read}]}
        drift (e/reading-drift open-items coverage)]
    (is (= [:E-good] (:read drift)))
    (is (contains? (set (:reading-source-changed drift)) :E-bad))
    (is (not (contains? (set (:read drift)) :E-bad)))
    (is (= ["E-gone"] (:members-not-open drift)))))
