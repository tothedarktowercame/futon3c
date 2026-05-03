(ns futon3c.peripheral.portfolio-inference-backend
  "Backend tool implementations for the portfolio-inference peripheral.

   Tools:
   - :pi-mission-features      — full mission inventory annotated with
                                 criterion-shape scoring + family mapping
   - :pi-precision-table       — per-family aggregation with precision-proxy
   - :pi-promotion-candidates  — structural-recurring criteria touching
                                 candidate families (direct-promotion targets)
   - :pi-aif-step              — pure aif-step over supplied state/observation
   - :pi-step                  — live aif-step using mission-control review
   - :pi-heartbeat             — weekly bid/clear heartbeat
   - :pi-adjacent              — adjacent-possible boundary

   Family-mapping + criterion-shape logic is a faithful port of
   futon5a/scripts/run_mission_feature_loop.clj — that babashka script
   is the regression baseline for VERIFY."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.peripheral.mission-control-backend :as mcb]
            [futon3c.portfolio.adjacent :as adjacent]
            [futon3c.portfolio.core :as portfolio]
            [futon3c.portfolio.observe :as observe]))

;; =============================================================================
;; Family registry (read from futon4 invariant model)
;; =============================================================================

(def default-invariant-model-path
  "Default location for the rendered invariant family model. Override
   by passing :invariant-model-path through opts."
  (str (System/getProperty "user.home") "/code/futon4/futon-stack-invariant-model.edn"))

(defn load-invariant-model
  ([] (load-invariant-model default-invariant-model-path))
  ([path]
   (try
     (edn/read-string (slurp path))
     (catch Exception e
       {:error :model-load-failed :message (.getMessage e) :path path}))))

(defn family-phrases
  "Return the curated multi-token phrase list for FAMILY-ID. Phrases are
   matched with case-insensitive word-boundary regex against mission body
   text. Tight by design — a phrase that is too generic produces runaway
   matches, as v1 of run_mission_feature_loop.clj demonstrated."
  [family-id]
  (case family-id
    :F-startup-contracts            ["startup contract" "fail loudly" "boot policy"
                                     "startup policy" "fail-loudly"]
    :F-authorization-and-identity   ["penholder" "authorization" "authentication"
                                     "identity uniqueness" "write authority"
                                     "allowlist" "x-penholder"]
    :F-layered-error-hierarchy      ["layered error" "error hierarchy" "layer order"
                                     "l4 to l0" "strict layer"]
    :F-phase-ordering               ["phase ordering" "phase advance" "phase transition"
                                     "out of order" "phase sequence"]
    :F-gate-pipeline-phase-ordering ["gate pipeline" "first failing gate"
                                     "g5 to g0" "gate traversal"]
    :F-status-discipline            ["status discipline" "obligation status"
                                     "status validate" "state label"]
    :F-existence                    ["must exist" "missing entity" "referent"
                                     "endpoint exists" "blocker must exist"]
    :F-dependency-satisfaction      ["dependency satisfaction" "unresolved dependency"
                                     "completed prerequisite" "dependency chain"]
    :F-failure-locality             ["failure locality" "first failing" "stop the line"
                                     "fail at source" "bounded search" "diagnosis with"]
    :F-human-visible-inspectability ["operator visibility" "evidence-over-folklore"
                                     "human-visible" "readable surface"
                                     "inspectable surface" "evidence over folklore"
                                     "operator inspect"]
    :F-graph-symmetry               ["graph symmetry" "inverse relation"
                                     "bidirectional" "portfolio symmetry"]
    :F-atomic-inspectable-units     ["atomic unit" "single-live-copy" "home-repo"
                                     "checkout-before-work" "checkin-on-exit"
                                     "inspectable boundary" "atomic inspectable"]
    :F-artifact-custody             ["artifact custody" "scratch marking"
                                     "generated vs source" "narrow ignore"
                                     "artifact locality" "no silent library overwrite"]
    :F-repo-role-clarity            ["repo role" "main-branch coherence"
                                     "legacy surface" "root legibility"
                                     "declared role"]
    :F-required-outputs             ["required output" "phase output"
                                     "missing output" "required artifact"
                                     "missing-phase-outputs" "phase-required-outputs"]
    :F-peripheral-custody           ["peripheral custody" "domain-id required"
                                     "home-repo in context" "hop preserves"
                                     "exit produces fruit"]
    :F-budgeted-action-selection    ["mana budget" "budget state" "depletion"
                                     "replenishment" "priority within budget"
                                     "budget-state-visible" "no costly action without budget"]
    :F-archaeology-control          ["stash debt" "archaeology control"
                                     "stash archaeology" "obsolescence"
                                     "stash-debt-bounded"]
    :F-cross-store-agreement        ["cross-store agreement" "session continuity"
                                     "ledger backing" "announcement agent"
                                     "mirrored actor" "canonical-ledger"]
    []))

(defn make-family-pattern
  [phrases]
  (when (seq phrases)
    (let [escape (fn [s] (java.util.regex.Pattern/quote s))
          alt (str/join "|" (map escape phrases))]
      (re-pattern (str "(?i)\\b(?:" alt ")\\b")))))

(defn build-family-table
  "Decorate the invariant model's :families with curated keyword
   patterns. Returns a vector of family records ready for matching."
  [model]
  (mapv (fn [f]
          (let [phrases (family-phrases (:id f))]
            (assoc f
                   :__phrases phrases
                   :__pattern (make-family-pattern phrases))))
        (:families model)))

(defn family-by-id-from
  [family-table]
  (into {} (map (juxt :id identity)) family-table))

;; =============================================================================
;; Mission file enumeration (extends mission-control-backend default roots)
;; =============================================================================

(def default-extended-roots
  "Mission Control's default-repo-roots only covers futon3c/3b/3a/5.
   Path B needs the full eight-repo sweep for the precision-proxy
   aggregation to be meaningful. This map is the explicit override."
  (let [home (System/getProperty "user.home")
        d (fn [n] (str home "/code/" n))]
    {:futon0  (d "futon0")
     :futon1a (d "futon1a")
     :futon3  (d "futon3")
     :futon3a (d "futon3a")
     :futon3b (d "futon3b")
     :futon3c (d "futon3c")
     :futon4  (d "futon4")
     :futon5  (d "futon5")
     :futon5a (d "futon5a")
     :futon6  (d "futon6")}))

(defn list-mission-files
  [repo-root]
  (let [dir (io/file repo-root "holes" "missions")]
    (when (.isDirectory dir)
      (->> (file-seq dir)
           (filter (fn [^java.io.File f]
                     (and (.isFile f)
                          (let [n (.getName f)]
                            (and (str/starts-with? n "M-")
                                 (str/ends-with? n ".md"))))))
           (sort-by #(.getPath ^java.io.File %))))))

(defn all-mission-files
  [repos]
  (mapcat (fn [[repo root]]
            (map (fn [f] {:repo repo :file f}) (list-mission-files root)))
          repos))

;; =============================================================================
;; Closure-criteria block extraction
;; =============================================================================

(def criteria-section-titles
  ["Completion criteria"
   "Closure criteria"
   "Closure"
   "Exit criteria"
   "Exit criterion"
   "Success criteria"
   "Acceptance criteria"
   "Done means"
   "Definition of done"])

(defn slice-section
  "Return the markdown body under a heading whose title matches TITLE
   (case-insensitive). Heading depth ## or ###. Stops at next same-depth
   or shallower heading."
  [text title]
  (let [pat (re-pattern (str "(?mi)^(#{2,3})\\s+"
                             (java.util.regex.Pattern/quote title) "\\s*$"))
        m (re-matcher pat text)]
    (when (.find m)
      (let [start (.end m)
            depth (count (.group m 1))
            stop-pat (re-pattern (str "(?m)^#{1," depth "}\\s+\\S"))
            after (subs text start)
            stop-m (re-matcher stop-pat after)
            stop (when (.find stop-m) (.start stop-m))]
        (str/trim (subs after 0 (or stop (count after))))))))

(defn extract-criteria-blocks
  [text]
  (into {}
        (keep (fn [title]
                (when-let [body (slice-section text title)]
                  [title body]))
              criteria-section-titles)))

(defn split-criteria-items
  "Split a markdown body into individual criterion strings. Recognises
   bulleted (-, *) and numbered (1.) list items, and treats blank lines
   as separators."
  [body]
  (let [lines (str/split-lines body)
        items (atom [])
        cur (atom nil)
        flush! #(when @cur
                  (let [s (str/trim @cur)]
                    (when (seq s) (swap! items conj s)))
                  (reset! cur nil))]
    (doseq [line lines]
      (cond
        (re-matches #"^\s*$" line)
        (flush!)
        (re-find #"^\s*(?:[-*]|\d+\.)\s+" line)
        (do (flush!)
            (reset! cur (str/replace line #"^\s*(?:[-*]|\d+\.)\s+" "")))
        (and @cur (re-find #"^\s+" line))
        (swap! cur str " " (str/trim line))
        :else
        (flush!)))
    (flush!)
    @items))

;; =============================================================================
;; Criterion shape scoring
;; =============================================================================

(def structural-recurring-patterns
  [#"(?i)\bevery\s+\w+(?:\s+\w+)?\s+(?:must|is|has|carries|holds|will|should|requires)\b"
   #"(?i)\beach\s+\w+\s+(?:must|is|has|carries|holds|requires)\b"
   #"(?i)\bno\s+(?:orphan|dead|rhetoric|silent|unattached|unbacked|stale)\b"
   #"(?i)\binvariant\s+(?:bites|holds|is\s+enforced|must|surfaces)\b"
   #"(?i)\b(?:preserved|preserves|enforces|enforced)\b"
   #"(?i)\b(?:parity|totality)\b"
   #"(?i)\bmust\s+(?:hold|not\s+\w+)\b"
   #"(?i)\bnever\s+(?:silently|drift|lose|skip)\b"
   #"(?i)\b(?:always-on|operationally\s+enforced|hard\s+gate)\b"])

(def process-recurring-patterns
  [#"(?i)\b(?:psr|pur|par)\b"
   #"(?i)\bcheckpoint(?:s|ed)?\b"
   #"(?i)\b(?:per-cycle|per-session|per-run|weekly\s+\w+)\b"
   #"(?i)\bevidence\s+(?:emission|emitted|deposited|recorded|append)"
   #"(?i)\b(?:audit|audited|audits)\b"
   #"(?i)\bdiscipline\b"
   #"(?i)\b(?:reflection|reflect|review)\s+(?:loop|cycle|step)\b"])

(def one-shot-patterns
  [#"(?i)\b(?:exists|created|generated|published|delivered|shipped|landed|committed|checked\s+in)\b"
   #"(?i)\b(?:first|initial)\s+(?:artefact|artifact|version|pass|render)\b"
   #"(?i)\b(?:first|the)\s+(?:demo|printout|file)\s+(?:exists|is\s+generated|emits|produces)\b"])

(defn- any-pattern? [patterns text]
  (some #(re-find % text) patterns))

(defn score-criterion-shape
  [criterion]
  (let [struct? (any-pattern? structural-recurring-patterns criterion)
        proc?   (any-pattern? process-recurring-patterns criterion)
        one?    (any-pattern? one-shot-patterns criterion)]
    (cond
      (and struct? (not proc?)) :structural-recurring
      proc?                     :process-recurring
      one?                      :one-shot
      :else                     :ambiguous)))

;; =============================================================================
;; Mission → families mapping
;; =============================================================================

(defn map-mission-to-families
  [mission-text family-table]
  (->> family-table
       (keep (fn [f]
               (when-let [pat (:__pattern f)]
                 (let [n (count (re-seq pat mission-text))]
                   (when (pos? n) [(:id f) n])))))
       (sort-by (fn [[_ s]] (- s)))
       vec))

;; =============================================================================
;; Header parsing — delegates status classification to mission-control-backend
;; =============================================================================

(defn- extract-header
  "Local copy of the bold/plain header extractor — mc-backend's version is
   private. Same semantics as mcb/parse-mission-md uses internally."
  [text key-name]
  (let [quoted (java.util.regex.Pattern/quote key-name)
        bold-pat   (re-pattern (str "(?mi)^\\*\\*" quoted ":\\*\\*\\s*(.+)$"))
        bold-pat2  (re-pattern (str "(?mi)^\\*\\*" quoted "\\*\\*:\\s*(.+)$"))
        plain-pat  (re-pattern (str "(?mi)^(?:#{1,3}\\s+)?" quoted ":\\s*(.+)$"))]
    (or (some-> (re-find bold-pat text)  second str/trim)
        (some-> (re-find bold-pat2 text) second str/trim)
        (some-> (re-find plain-pat text) second str/trim))))

(defn parse-mission-feature
  [{:keys [repo file]} family-table]
  (let [path (.getPath ^java.io.File file)
        text (slurp file)
        header (->> (str/split-lines text) (take 80) (str/join "\n"))
        title (some-> (re-find #"(?m)^#\s+(?:Mission:\s*)?(.+)$" header) second str/trim)
        raw-status (extract-header header "Status")
        ;; Reuse Mission Control's status classification by parsing the
        ;; mission with its public parser, then keeping just the status.
        partial-mc (mcb/parse-mission-md path repo)
        status (or (:mission/status partial-mc) :unknown)
        date (extract-header header "Date")
        criteria-blocks (extract-criteria-blocks text)
        criteria-items (mapcat (fn [[_ body]] (split-criteria-items body))
                               criteria-blocks)
        scored-items (mapv (fn [c] {:text c :shape (score-criterion-shape c)})
                           criteria-items)
        families (map-mission-to-families text family-table)
        mission-id (-> (.getName ^java.io.File file)
                       (str/replace #"^M-" "")
                       (str/replace #"\.md$" ""))]
    {:mission/id mission-id
     :mission/repo (name repo)
     :mission/path path
     :mission/title title
     :mission/status status
     :mission/raw-status raw-status
     :mission/date date
     :mission/criteria-section-count (count criteria-blocks)
     :mission/criteria scored-items
     :mission/criteria-shape-counts (frequencies (map :shape scored-items))
     :mission/families families}))

(defn build-mission-features
  ([] (build-mission-features {}))
  ([{:keys [repos invariant-model-path]
     :or {repos default-extended-roots}}]
   (let [model (load-invariant-model (or invariant-model-path
                                          default-invariant-model-path))
         family-table (build-family-table model)
         files (all-mission-files repos)]
     {:missions (mapv #(parse-mission-feature % family-table) files)
      :family-table family-table
      :family-by-id (family-by-id-from family-table)
      :model model})))

;; =============================================================================
;; Per-family precision-proxy aggregation
;; =============================================================================

(defn- precision-row
  [family entries]
  (let [missions-here (mapv second entries)
        scores (mapv #(nth % 2) entries)
        complete (filter #(= :complete (:mission/status %)) missions-here)
        in-prog (filter #(= :in-progress (:mission/status %)) missions-here)
        blocked (filter #(= :blocked (:mission/status %)) missions-here)
        criteria-by-shape (->> missions-here
                               (mapcat :mission/criteria)
                               (group-by :shape))
        struct-rec (count (criteria-by-shape :structural-recurring))
        proc-rec   (count (criteria-by-shape :process-recurring))
        one-shot   (count (criteria-by-shape :one-shot))
        ambig      (count (criteria-by-shape :ambiguous))
        expectation-drift (count (mapcat :mission/criteria complete))
        loss-severity (+ (* 2 (count blocked)) (count in-prog) struct-rec)
        witness-quality (+ (* 3 struct-rec) (* 2 (count complete)) (count in-prog))
        actionability-gradient (- struct-rec (+ proc-rec ambig))
        pp-raw (+ (double expectation-drift)
                  (* 2.0 loss-severity)
                  (* 1.5 witness-quality)
                  (* 0.5 (max 0 actionability-gradient)))]
    {:family/id (:id family)
     :family/name (:name family)
     :family/layer (:layer family)
     :family/status (:status family)
     :family/firm-exemplar (:firm-exemplar family)
     :missions/total (count missions-here)
     :missions/complete (count complete)
     :missions/in-progress (count in-prog)
     :missions/blocked (count blocked)
     :missions/total-score (reduce + scores)
     :criteria/structural-recurring struct-rec
     :criteria/process-recurring proc-rec
     :criteria/one-shot one-shot
     :criteria/ambiguous ambig
     :precision/expectation-drift expectation-drift
     :precision/loss-severity loss-severity
     :precision/witness-quality witness-quality
     :precision/actionability-gradient actionability-gradient
     :precision/raw-score pp-raw
     :missions/ids (mapv :mission/id missions-here)}))

(defn aggregate-per-family
  [missions family-table]
  (let [by-family (group-by first
                            (mapcat (fn [m]
                                      (map (fn [[fid score]] [fid m score])
                                           (:mission/families m)))
                                    missions))]
    (mapv (fn [family]
            (precision-row family (get by-family (:id family) [])))
          family-table)))

(defn normalize-precision
  [agg]
  (let [max-raw (reduce max 1.0 (map :precision/raw-score agg))]
    (mapv (fn [row]
            (assoc row :precision/proxy
                   (long (Math/round (* 10.0 (/ (:precision/raw-score row)
                                                max-raw))))))
          agg)))

(defn build-precision-table
  ([] (build-precision-table {}))
  ([opts]
   (let [{:keys [missions family-table]} (build-mission-features opts)]
     (-> missions (aggregate-per-family family-table) normalize-precision))))

;; =============================================================================
;; Promotion candidate extraction
;; =============================================================================

(defn build-promotion-candidates
  ([] (build-promotion-candidates {}))
  ([opts]
   (let [{:keys [missions family-by-id]} (build-mission-features opts)]
     (->> missions
          (filter #(#{:complete :in-progress} (:mission/status %)))
          (mapcat (fn [m]
                    (let [candidate-fams
                          (->> (:mission/families m)
                               (map (fn [[fid score]]
                                      [fid (family-by-id fid) score]))
                               (filter (fn [[_ fam _]]
                                         (= :candidate (:status fam)))))]
                      (mapcat
                       (fn [[fid fam score]]
                         (->> (:mission/criteria m)
                              (filter #(= :structural-recurring (:shape %)))
                              (map (fn [c]
                                     {:mission/id (:mission/id m)
                                      :mission/repo (:mission/repo m)
                                      :mission/status (:mission/status m)
                                      :family/id fid
                                      :family/name (:name fam)
                                      :family/layer (:layer fam)
                                      :family/match-score score
                                      :criterion (:text c)}))))
                       candidate-fams))))
          vec))))

;; =============================================================================
;; AIF wrappers — re-export portfolio.core entry points
;; =============================================================================

(defn aif-step-pure
  "Wrap futon3c.portfolio.core/aif-step. Pure single-step over supplied
   state/observation/adjacent-missions/opts. Does not read the live
   portfolio review; useful for VERIFY harnesses and offline replays."
  [{:keys [state observation adjacent-missions opts]
    :or {opts {}}}]
  (portfolio/aif-step state observation adjacent-missions opts))

(defn aif-step-live
  "Wrap futon3c.portfolio.core/portfolio-step!. Live AIF step that builds
   a mission-control review internally and mutates the !state atom."
  ([] (aif-step-live {}))
  ([{:keys [evidence-store opts]
     :or {opts {}}}]
   (portfolio/portfolio-step! evidence-store opts)))

(defn heartbeat
  "Wrap futon3c.portfolio.core/portfolio-heartbeat!. Weekly bid/clear cycle."
  [{:keys [evidence-store heartbeat-data opts]
    :or {opts {}}}]
  (portfolio/portfolio-heartbeat! evidence-store heartbeat-data opts))

(defn adjacent-set
  "Wrap futon3c.portfolio.adjacent/compute-adjacent-set."
  ([] (adjacent-set {}))
  ([{:keys [missions mana opts]
     :or {opts {}}}]
   (let [review (or (when (and missions mana) {:portfolio/missions missions
                                                :portfolio/mana mana})
                    (mcb/build-portfolio-review))]
     (adjacent/compute-adjacent-set
      (:portfolio/missions review)
      (:portfolio/mana review)
      opts))))
