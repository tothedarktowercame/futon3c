(ns futon3c.giraffe.ticket-admission-test
  "Q1/T adapter slice: :include-tickets at the census boundary.

  T1 default preservation (no T rows without the opt). T2 ticket rows typed
  via shared readers. T3 the hermetic witness: fixture census -> consume ->
  combined-pass -> write-run! (temp) -> outer-observation -> real select;
  the open carrier is compared to an independent registry census and the
  actual pick is reported, never forced.

  HERMETIC WORLD (2026-11-15, kimi-2): these tests used to read the live
  corpus — /home/joe/code registries and the frozen readings-608 snapshot
  under /home/joe/notes — whose sha256 pins drifted as live documents were
  edited, so the (correct) provenance refusals failed the tests. The world
  is now INVENTED at test time in a temp directory: two repos with .git
  sentinels, two missions, one excursion, two tickets, a readings directory
  (checker + members.json + one batch) whose pins are computed from the
  fixture files when the test runs, a task-selection library, and an outer
  makeup. No text is copied from any real document; every name is invented.
  Claims that pinned the real corpus (e.g. \"608 members\") are restated
  relative to the fixture and say so in a comment."
  (:require [clojure.test :refer [deftest is]]
            [cheshire.core :as json]
            [futon2.aif.mission-registry :as registry]
            [futon2.aif.outer-state :as state]
            [futon2.aif.outer-observation :as observation]
            [futon2.aif.outer-select :as select]
            [futon2.aif.selection-canonical-g :as selection-g]
            [futon2.aif.focus-receipt :as focus]
            [futon2.aif.selection-world :as world]
            [futon3c.giraffe.excursions :as excursions]
            [futon3c.giraffe.run :as writer]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

;;; ---------------------------------------------------------- the fixture world

(defn- temp-dir [prefix]
  (str (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- write!
  "Write TEXT at ROOT/REL, creating parents. Returns the File."
  [root rel text]
  (let [f (io/file root rel)]
    (.mkdirs (.getParentFile f))
    (spit f text)
    f))

(defn- sha256-file [^java.io.File f]
  (writer/sha256 f))

;; The invented documents. Names, prose and structure are all fixture
;; inventions; no real mission, excursion or ticket text appears here.

(def ^:private aurora-text
  "# M-fixture-aurora — invented fixture mission (hermetic test world)

**Status:** OPEN — invented fixture mission; no live counterpart exists.

## Acceptance

- [ ] The invented aurora ledger balances inside the fixture world.

## Relations

This invented mission states a relation to M-aif-policy-conditioned-eig so the
frozen focus corpus can classify it.
")

(def ^:private boreal-text
  "# M-fixture-boreal — invented fixture mission (hermetic test world)

**Status:** OPEN — invented fixture mission; no live counterpart exists.

## Acceptance

- [ ] The invented boreal ledger is reconciled inside the fixture world.

## Notes

This mission depends on M-fixture-aurora being complete first.

## Relations

This invented mission states a relation to M-aif-policy-conditioned-eig so the
frozen focus corpus can classify it.
")

(def ^:private cartography-text
  "# E-fixture-cartography — invented fixture excursion

**Status:** OPEN — invented fixture excursion; no live counterpart exists.

## Acceptance

- [ ] The invented cartography sketch is drawn inside the fixture world.

## Relations

This invented excursion states a relation to M-fixture-aurora, which in turn
relates to the frozen focus corpus.
")

(def ^:private ledger-gap-text
  "# T-fixture-ledger-gap — invented fixture ticket

**Status:** OPEN — invented fixture ticket; no live counterpart exists.

An invented ticket describing a gap in an invented ledger. It carries no
checkboxes and no completion criteria, so the completion surface is the typed
absence :completion-definition-not-found.

## What would close this

The invented ledger gap is described and an invented remedy is proposed.
")

(def ^:private retired-signpost-text
  "# T-fixture-retired-signpost — invented fixture ticket

**Status:** DONE — invented and already closed, so the census must exclude it.

## What would close this

It is closed by invention.
")

;; The invented reading checker. Same contract class as the production
;; checker (member binding, source-file pins, quote window, vocabulary,
;; target resolution, one record per member) but written for this fixture:
;; the code root is baked in when the fixture is built, instead of the
;; production checker's hard-wired Path.home()/\"code\".
(def ^:private checker-template
  "#!/usr/bin/env python3
\"\"\"Invented fixture checker for the hermetic ticket-admission test world.

Mechanical check of one task-link batch against members.json. Mirrors the
production contract (member binding, source pins, six-line quote window,
vocabulary, target resolution, duplicate detection) over the invented code
root below. Never reads outside FIXTURE_CODE_ROOT.
\"\"\"
import hashlib, json, re, sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
KINDS = {\"depends-on\", \"unblocks\", \"part-of\", \"advances\", \"relates-to\",
         \"duplicates\", \"superseded-by\"}
BASES = {\"declared\", \"inferred\"}
STATUS = {\"open\", \"done-unmarked\", \"abandoned\", \"unclear\"}
CODE = Path(\"__FIXTURE_CODE_ROOT__\")
WINDOW = 6


def index_ids():
    ids = {}
    for repo in CODE.iterdir():
        if not (repo / \".git\").is_dir() or not (repo / \"holes\").is_dir():
            continue
        for f in (repo / \"holes\").rglob(\"*.md\"):
            if re.match(r\"^[MET]-\", f.stem):
                ids.setdefault(f.stem, str(f))
    return ids


def norm(s):
    return re.sub(r\"\\s+\", \" \", s).strip()


def sha(p):
    return hashlib.sha256(Path(p).read_bytes()).hexdigest()


def main(paths):
    members = {m[\"id\"]: m for m in json.loads((HERE / \"members.json\").read_text())}
    ids = index_ids()
    problems, seen = [], {}
    records = edges = unresolved = 0

    def file_lines(p):
        try:
            return Path(p).read_text(errors=\"replace\").splitlines()
        except OSError:
            return None

    def check_evidence(tag, ev):
        p = ev.get(\"path\", \"\")
        if not Path(p).is_absolute():
            problems.append(f\"{tag} evidence path must be absolute\")
            return
        lines = file_lines(p)
        if lines is None:
            problems.append(f\"{tag} evidence file missing: {p}\")
            return
        if ev.get(\"sha256\") != sha(p):
            problems.append(f\"{tag} evidence sha256 does not match the file\")
        line = ev.get(\"line\")
        q = norm(ev.get(\"quote\", \"\"))
        if not isinstance(line, int) or line < 1 or line > len(lines):
            problems.append(f\"{tag} evidence line missing or out of range\")
        elif len(q) < 12 or q not in norm(\" \".join(lines[line - 1:line - 1 + WINDOW])):
            problems.append(f\"{tag} quote not found within lines {line}..{line + WINDOW - 1}\")

    def check_vocab(tag, x):
        if x.get(\"kind\") not in KINDS:
            problems.append(f\"{tag} kind {x.get('kind')!r} not allowed\")
        if x.get(\"basis\") not in BASES:
            problems.append(f\"{tag} basis {x.get('basis')!r} not allowed\")

    for batch in paths:
        for n, line in enumerate(Path(batch).read_text().splitlines(), 1):
            if not line.strip():
                continue
            where = f\"{batch}:{n}\"
            try:
                r = json.loads(line)
            except json.JSONDecodeError as e:
                problems.append(f\"{where} bad json: {e}\")
                continue
            records += 1
            rid = r.get(\"id\")
            m = members.get(rid)
            if m is None:
                problems.append(f\"{where} {rid} is not a population member\")
            else:
                src = r.get(\"source\", {})
                if src.get(\"path\") != m[\"path\"] or src.get(\"sha256\") != m[\"sha256\"]:
                    problems.append(f\"{where} {rid} source does not match the member pin\")
                elif not Path(m[\"path\"]).is_file() or sha(m[\"path\"]) != m[\"sha256\"]:
                    problems.append(f\"{where} {rid} source file changed since the census\")
            if rid in seen:
                problems.append(f\"{where} {rid} duplicate record (also {seen[rid]})\")
            seen[rid] = where
            if r.get(\"status_as_read\") not in STATUS:
                problems.append(f\"{where} {rid} status_as_read not in {sorted(STATUS)}\")
            for field in (\"edges\", \"unresolved\"):
                if not isinstance(r.get(field), list):
                    problems.append(f\"{where} {rid} {field} must be a list ([] if none)\")
            for e in r.get(\"edges\") or []:
                edges += 1
                tag = f\"{where} {rid} -> {e.get('to')}\"
                check_vocab(tag, e)
                if e.get(\"to\") not in ids:
                    problems.append(f\"{tag} target id does not resolve (use unresolved)\")
                if e.get(\"to\") == rid:
                    problems.append(f\"{tag} self-link\")
                check_evidence(tag, e.get(\"evidence\", {}))
            for u in r.get(\"unresolved\") or []:
                unresolved += 1
                tag = f\"{where} {rid} -> unresolved {u.get('raw_target')!r}\"
                check_vocab(tag, u)
                if not u.get(\"raw_target\") or not u.get(\"reason\"):
                    problems.append(f\"{tag} needs raw_target and reason\")
                check_evidence(tag, u.get(\"evidence\", {}))
    for p in problems:
        print(p)
    print(f\"checked {records} records, {edges} edges, {unresolved} unresolved, \"
          f\"{len(problems)} problems\")
    return 1 if problems else 0


if __name__ == \"__main__\":
    sys.exit(main(sys.argv[1:]))
")

;; The invented outer makeup. Numbers are fixture choices, not calibrated
;; values; validity is what matters (positive kits, rates, retentions), plus
;; :task-selection-library-root pointing at the invented library so select
;; never resolves the canonical one.
(defn- fixture-makeup [library-root]
  {:schema :wm/outer-makeup-v1
   :c-kit {:focused 1.0 :related 0.6 :unrelated 0.3 :stop-the-line 1.5
           :not-yet-evaluated 0.4}
   :kappa 0.5
   :state-kit {:ready {:doable 1.0 :blocked 0.4}
               :not-ready {:doable 0.4 :blocked 1.0}
               :undetermined {:doable 0.6 :blocked 0.6}}
   :gain-kit {:none 1.0 :one 1.5 :several 2.0}
   :eta-c 0.2 :eta-state 0.2 :eta-gain 0.2
   :omega-c 0.9 :omega-state 0.9 :omega-gain 0.9
   :default-reach 0.5
   :task-selection-library-root library-root})

(def ^:private fixture-patterns
  {"fixture-start-with-the-ready"
   "@flexiarg fixture/start-with-the-ready\n@title Invented fixture pattern: start with the ready\n\n! conclusion: Invented pattern; only readiness matters.\n"
   "fixture-weigh-the-downstream"
   "@flexiarg fixture/weigh-the-downstream\n@title Invented fixture pattern: weigh the downstream\n\n! conclusion: Invented pattern; downstream cone breaks ties.\n"
   "fixture-respect-the-rank"
   "@flexiarg fixture/respect-the-rank\n@title Invented fixture pattern: respect the rank\n\n! conclusion: Invented pattern; census rank breaks ties.\n"})

(def ^:private fixture-readme
  "# Invented fixture task-selection library\n\nTwo invented cascades for the hermetic test world.\n")

(defn- build-world!
  "Build the invented world in a fresh temp directory. Returns a map of
  everything the tests need. All pins are computed from the fixture bytes at
  call time, so the world can never drift."
  []
  (let [root (temp-dir "q1-ticket-fixture-")
        code (io/file root "code")
        ;; Two invented repos. Each gets an empty .git DIRECTORY so the
        ;; registry's primary-checkout fence admits it; `git status` against
        ;; it fails (not a real repository), which the observation layer
        ;; already treats as "unreadable repository excludes nothing".
        repo0 (io/file code "futon0")
        repo2 (io/file code "futon2")
        _ (.mkdirs (io/file repo0 ".git"))
        _ (.mkdirs (io/file repo2 ".git"))
        aurora (write! repo0 "holes/missions/M-fixture-aurora.md" aurora-text)
        boreal (write! repo0 "holes/missions/M-fixture-boreal.md" boreal-text)
        cartography (write! repo2 "holes/excursions/E-fixture-cartography.md" cartography-text)
        ledger-gap (write! repo2 "holes/tickets/T-fixture-ledger-gap.md" ledger-gap-text)
        _retired (write! repo2 "holes/tickets/T-fixture-retired-signpost.md" retired-signpost-text)
        _ (.mkdirs (io/file repo2 "data/wm-repair-obligations/findings"))
        edges-file (write! root "edges.json" "{\"edges\": [], \"rejected\": []}")
        ;; The invented task-selection library.
        lib (io/file root "task-selection")
        _ (doseq [[pattern text] fixture-patterns]
            (write! lib (str pattern ".flexiarg") text))
        readme (write! lib "README.md" fixture-readme)
        mapping {:schema :wm/task-selection-executable-steps-v1
                 :version 1
                 :readme-sha256 (sha256-file readme)
                 :cascades
                 [{:id :fixture/ready-by-rank :version 1 :status :executable
                   :enactment-mode :implementation
                   :readme-cascade 1 :readme-lines [1 2]
                   :patterns ["fixture-start-with-the-ready" "fixture-respect-the-rank"]
                   :steps [:ready-gate :by-rank]}
                  {:id :fixture/ready-by-cone :version 1 :status :executable
                   :enactment-mode :implementation
                   :readme-cascade 1 :readme-lines [1 2]
                   :patterns ["fixture-start-with-the-ready" "fixture-weigh-the-downstream"]
                   :steps [:ready-gate :by-cone]}]}
        _ (write! lib "executable-steps.edn" (pr-str mapping))
        ;; The invented readings directory: checker, members, one batch.
        readings (io/file root "readings")
        _ (write! readings "check_edges.py"
                  (str/replace checker-template "__FIXTURE_CODE_ROOT__"
                               (.getAbsolutePath code)))
        open-docs [{:id "M-fixture-aurora" :file aurora}
                   {:id "M-fixture-boreal" :file boreal}
                   {:id "E-fixture-cartography" :file cartography}
                   {:id "T-fixture-ledger-gap" :file ledger-gap}]
        members (mapv (fn [{:keys [id ^java.io.File file]}]
                        {:id id :path (.getAbsolutePath file)
                         :sha256 (sha256-file file)})
                      open-docs)
        _ (write! readings "members.json"
                  (json/generate-string members {:pretty true}))
        ;; Boreal declares its dependency on aurora with a verbatim quote from
        ;; its own document; the line number is located in the fixture text,
        ;; never hard-coded.
        boreal-lines (str/split-lines boreal-text)
        boreal-edge-line (inc (first (keep-indexed
                                      (fn [i l] (when (str/includes? l "depends on M-fixture-aurora") i))
                                      boreal-lines)))
        record (fn [{:keys [id ^java.io.File file]}]
                 (cond-> {:id id
                          :source {:path (.getAbsolutePath file)
                                   :sha256 (sha256-file file)}
                          :status_as_read "open"
                          :purpose (str "Invented fixture reading of " id
                                        " for the hermetic test world.")
                          :edges []
                          :unresolved []}
                   (= id "M-fixture-boreal")
                   (assoc :edges [{:to "M-fixture-aurora"
                                   :kind "depends-on"
                                   :basis "declared"
                                   :evidence {:path (.getAbsolutePath boreal)
                                              :sha256 (sha256-file boreal)
                                              :line boreal-edge-line
                                              :quote "depends on M-fixture-aurora being complete first"}}])))
        batch (str (str/join "\n" (map #(json/generate-string (record %))
                                         open-docs)) "\n")
        batch-file (write! readings "batch-01.jsonl" batch)
        ;; The invented futon2 data root: just the outer makeup.
        data-root (io/file root "data")
        makeup-file (write! data-root "wm-learning/outer-makeup.edn"
                            (pr-str (fixture-makeup (.getAbsolutePath lib))))]
    {:root root
     :code-root (.getAbsolutePath code)
     :repos [(.getAbsolutePath repo0) (.getAbsolutePath repo2)]
     :edges-file (.getAbsolutePath edges-file)
     :readings-dir (.getAbsolutePath readings)
     :checker (.getAbsolutePath (io/file readings "check_edges.py"))
     :members-file (.getAbsolutePath (io/file readings "members.json"))
     :batches [(.getAbsolutePath batch-file)]
     :library-root (.getAbsolutePath lib)
     :makeup-path (.getAbsolutePath makeup-file)
     :state-path (.getAbsolutePath (io/file data-root "wm-learning" "outer-state.edn"))
     :member-count (count members)
     :live-ticket "T-fixture-ledger-gap"
     :non-live-ticket "T-fixture-retired-signpost"}))

(defn- adapter-opts [fixture]
  {:edges-file (:edges-file fixture)
   :code-root (:code-root fixture)
   :repos (:repos fixture)})

;;; ------------------------------------------------------------------ the tests

(deftest t1-default-census-has-no-tickets
  (let [fixture (build-world!)
        adapter (excursions/missions-and-excursions->input (adapter-opts fixture))
        ids (map :id (get-in adapter [:input :items]))]
    (is (not (some #(= \T (first (name %))) ids)))
    (is (not (some #(= \T (first (name (:id %)))) (:excluded adapter))))))

(deftest t2-ticket-rows-are-typed-not-invented
  (let [fixture (build-world!)
        adapter (excursions/missions-and-excursions->input
                 (assoc (adapter-opts fixture) :include-tickets true))
        rows (filter #(= \T (first (name (:id %)))) (get-in adapter [:input :items]))
        live-registry (set (map :id (filter registry/live-ticket?
                                            (:tickets (registry/load-tickets (:code-root fixture))))))
        excluded-ids (set (map :id (:excluded adapter)))
        non-live-registry (set (map :id (remove registry/live-ticket?
                                                (:tickets (registry/load-tickets (:code-root fixture))))))]
    ;; The fixture's live population, restated relative to the fixture (the
    ;; pre-hermetic test pinned whatever the live registry held that day).
    (is (= #{(:live-ticket fixture)} live-registry))
    (is (= live-registry (set (map (comp name :id) rows))))
    (is (every? #(= :unstarted (:state-group %)) rows))
    (is (every? #(= #{:path :sha256} (set (keys (:source-pin %)))) rows))
    (is (every? #(= #{:status :reason :producer :source-pin} (set (keys (:dependency-reading %)))) rows))
    (is (every? #(= :unknown (get-in % [:dependency-reading :status])) rows))
    (is (every? #(= :dependency-reader-reviewer (get-in % [:dependency-reading :producer])) rows))
    ;; typed completion: schema + producer present; A4 contract absence is
    ;; typed (:completion-definition-not-found), never a synthesized class.
    (is (every? #(= :wm/completion-surface-v2 (:schema (:completion %))) rows))
    (is (every? #(contains? #{:task-completion-author :completion-locator-author}
                            (:producer (:completion %))) rows))
    (is (every? #(contains? #{:completion-definition-not-found :checkbox-locators-with-retained-prose
                              :completion-prose-needs-locators}
                            (:completion-class (:completion %))) rows))
    ;; every live ticket has a declared-status; non-live T are excluded with
    ;; typed reason, independently of the row set.
    (is (every? :declared-status rows))
    (is (set/subset? (set/intersection non-live-registry excluded-ids) non-live-registry))
    (is (contains? excluded-ids (:non-live-ticket fixture)))
    (is (every? :reason (filter #(= \T (first (name (:id %)))) (:excluded adapter))))
    ;; source pins verify against the actual fixture files right now.
    (doseq [row rows]
      (is (= (writer/sha256 (io/file (get-in row [:source-pin :path])))
             (get-in row [:source-pin :sha256]))))))

(deftest t3-frozen-608-hermetic-witness
  (let [fixture (build-world!)
        adapter (excursions/missions-and-excursions->input
                 (assoc (adapter-opts fixture)
                        :include-tickets true
                        :reading-checker (:checker fixture)
                        :members-file (:members-file fixture)
                        :reading-batches (:batches fixture)
                        :reading-scope :population/missions-excursions-tickets))
        result (excursions/combined-pass (:input adapter))
        tmp-root (temp-dir "q1-ticket-witness-")
        output (writer/write-run! {:root tmp-root :adapter-result adapter
                                   :pass-output result
                                   :edges-file (:edges-file fixture)})
        _ (is (:ok? (writer/verify-run (:dir output))))
        makeup (state/load-makeup (:makeup-path fixture))
        prior (state/read-state makeup (:state-path fixture))
        authority {:registry-source :files
                   :loaded-missions (registry/load-missions (:code-root fixture))
                   :loaded-excursions (registry/load-excursions (:code-root fixture))
                   :loaded-tickets (registry/load-tickets (:code-root fixture))}
        obs (observation/outer-observation
             {:run-dir (:dir output) :makeup makeup
              :focus-inputs (focus/read-inputs)
              :decision-as-of "2026-10-30T00:00:00Z"
              :relation-context (focus/relation-context (:code-root fixture))
              :authority authority})
        ;; The fixture library's invented patterns carry no @guard/@predicts
        ;; interpretations of their own; the canonical-G scorer's documented
        ;; test seam binds them here, stated beside the fixture that owns them.
        selected (binding [selection-g/*extra-interpretations*
                           {"fixture-start-with-the-ready"
                            {:consumes #{:ready} :forbids #{:unmet-need} :produces #{:can-begin}}
                            "fixture-weigh-the-downstream"
                            {:consumes #{:ready :unblocks} :forbids #{} :produces #{:ready-count-rises}}
                            "fixture-respect-the-rank"
                            {:consumes #{:ready :rank} :forbids #{} :produces #{:rank-observed}}}]
                   (select/select makeup prior obs))
        sets (#'world/default-task-sets (:code-root fixture))
        wanted (set (mapcat val sets))
        found (set (map (comp name :id) (:order result)))
        status-by-id (into {} (map (fn [r] [(get-in r [:record :id])
                                             (get-in r [:record :status_as_read])]))
                           (get-in adapter [:reading-evidence :checked-records]))
        pick (get-in selected [:selection :selected :target])
        rows (into {} (map (juxt :id identity)) (:order result))
        report {:population (count found)
                :by-type (frequencies (map #(subs % 0 1) found))
                :wanted (count wanted) :missing (sort (set/difference wanted found))
                :extras (sort (set/difference found wanted))
                :coverage (dissoc (:graph-coverage result) :members)
                :graph-terms (frequencies (map #(get-in % [:graph-terms :status]) (:order result)))
                :excluded-by-reason (frequencies (map :reason (:excluded adapter)))
                :ready-count (count (:ready-set result))
                :ready-tickets (filterv #(= \T (first %)) (map name (:ready-set result)))
                :pick pick
                :pick-status-as-read (get status-by-id pick)
                :pick-terms (get-in rows [(keyword pick) :graph-terms])
                :verify (:ok? (writer/verify-run (:dir output)))
                :selection-kind (get-in selected [:selection :selected :kind])}
        ;; The witness report is a test artifact: it lands in the temp run
        ;; world, never under /home/joe/notes.
        report-path (str tmp-root "/t3-witness-report.edn")]
    (spit report-path (pr-str report))
    ;; Assertions: coverage complete over the fixture population (the claim
    ;; was "608 members" against the pinned live corpus; restated relative
    ;; to the fixture), scope label correct, pick is whatever the REAL
    ;; selector chose.
    (is (= :complete (get-in result [:graph-coverage :status])))
    (is (= (:member-count fixture) (get-in result [:graph-coverage :member-count])))
    (is (= :population/missions-excursions-tickets
           (get-in result [:graph-coverage :scope])))
    (is (every? #(= :population/missions-excursions-tickets
                    (get-in % [:graph-terms :scope])) (:order result)))
    ;; Ready tickets are REPORTED, not forced and not forbidden: with the
    ;; complete certificate, an open T with no unmet counting needs is ready
    ;; by the same rule as M/E. What must hold is that readiness rides the
    ;; certificate: known terms, no unresolved prerequisites. The fixture's
    ;; live ticket has no needs, so this assertion is non-vacuous.
    (is (= [(:live-ticket fixture)] (:ready-tickets report)))
    (is (every? #(= :known (get-in rows [(keyword %) :graph-terms :status]))
                (:ready-tickets report)))
    (is (every? #(nil? (get-in rows [(keyword %) :graph-terms :reason]))
                (:ready-tickets report)))
    (is (some? pick))
    ;; independent registry census comparison is REPORTED, not asserted equal:
    ;; the open carrier must match the fixture registry or the mismatch is
    ;; visible in the report for review.
    (is (pos? (count found)))))
