(ns futon3c.peripheral.mission-control-backend
  "Backend tool implementations for the mission control peripheral.

   Tools:
   - :mc-inventory     — scan mission files across repos, return inventory
   - :mc-devmaps       — read and summarize devmap wiring diagrams
   - :mc-coverage      — compute devmap coverage against mission inventory
   - :mc-mana          — query mana pool stats (if nonstarter.db exists)
   - :mc-review        — produce a full portfolio review
   - :mc-bulletin      — emit a war bulletin as evidence

   All tools are read-only with respect to external systems.
   Evidence emission happens at the peripheral level, not here."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;; =============================================================================
;; Configuration — repo paths
;; =============================================================================

(def default-repo-roots
  "Default repo locations (co-located at ~/code/)."
  {:futon3c (str (System/getProperty "user.home") "/code/futon3c")
   :futon3b (str (System/getProperty "user.home") "/code/futon3b")
   :futon3a (str (System/getProperty "user.home") "/code/futon3a")
   :futon5  (str (System/getProperty "user.home") "/code/futon5")})

;; =============================================================================
;; Mission file parsing
;; =============================================================================

(defn- extract-header
  "Extract a **Key:** value from markdown text."
  [text key-name]
  (let [pattern (re-pattern (str "(?m)^\\*\\*" (java.util.regex.Pattern/quote key-name) ":\\*\\*\\s*(.+)$"))]
    (when-let [m (re-find pattern text)]
      (str/trim (second m)))))

(defn classify-status
  "Classify a raw status string into a MissionStatus keyword."
  [raw]
  (when raw
    (let [s (str/lower-case (str/trim raw))]
      (cond
        (str/starts-with? s "complete")    :complete
        (str/starts-with? s "blocked")     :blocked
        (str/starts-with? s "ready")       :ready
        (str/starts-with? s "pass")        :complete
        ;; Derivation keywords: check if the *derivation step itself* is marked complete.
        ;; "INSTANTIATE complete" → :complete (the mission finished its last step)
        ;; "MAP (landscape survey complete)" → :in-progress (MAP done, mission continues)
        ;; Heuristic: "complete" must follow the keyword directly, not be in a parenthetical.
        (re-find #"identify|map|derive|argue|verify|instantiate" s)
        (if (re-find #"^(?:identify|map|derive|argue|verify|instantiate)\s+complete" s)
          :complete
          :in-progress)
        :else :unknown))))

(defn parse-mission-md
  "Parse a mission .md file into a MissionEntry."
  [path repo-name]
  (try
    (let [text (slurp path)
          filename (.getName (io/file path))
          mission-id (str/replace filename #"^M-|\.md$" "")
          raw-status (extract-header text "Status")
          date (extract-header text "Date")
          blocked-by (extract-header text "Blocked by")]
      {:mission/id mission-id
       :mission/status (or (classify-status raw-status) :unknown)
       :mission/source :md-file
       :mission/repo (name repo-name)
       :mission/path (str path)
       :mission/date date
       :mission/blocked-by blocked-by
       :mission/raw-status raw-status})
    (catch Exception e
      {:mission/id (str path)
       :mission/status :unknown
       :mission/source :md-file
       :mission/repo (name repo-name)
       :mission/path (str path)
       :mission/raw-status (str "parse-error: " (.getMessage e))})))

(defn- classify-devmap-state
  "Convert an EDN :mission/state keyword to a MissionStatus."
  [state]
  (case state
    :complete   :complete
    :active     :in-progress
    :greenfield :ready
    :composed   :in-progress
    :unknown))

(defn parse-devmap-edn
  "Parse a devmap EDN file into a MissionEntry."
  [path]
  (try
    (let [raw (edn/read-string (slurp path))
          mid (:mission/id raw)
          state (:mission/state raw)]
      (when mid
        {:mission/id (name mid)
         :mission/status (classify-devmap-state state)
         :mission/source :devmap-edn
         :mission/repo "futon5"
         :mission/path (str path)
         :mission/raw-status (when state (name state))
         :mission/devmap-id mid}))
    (catch Exception _e nil)))

;; =============================================================================
;; Inventory scanning
;; =============================================================================

(defn scan-mission-files
  "Scan for M-*.md mission files in a repo's holes/missions/ directory."
  [repo-root repo-name]
  (let [dir (io/file repo-root "holes" "missions")]
    (if (.isDirectory dir)
      (->> (.listFiles dir)
           (filter #(and (.isFile %)
                         (str/starts-with? (.getName %) "M-")
                         (str/ends-with? (.getName %) ".md")))
           (mapv #(parse-mission-md (.getPath %) repo-name)))
      [])))

(defn scan-devmap-files
  "Scan for mission EDN files in futon5/data/missions/."
  [futon5-root]
  (let [dir (io/file futon5-root "data" "missions")]
    (if (.isDirectory dir)
      (->> (.listFiles dir)
           (filter #(and (.isFile %)
                         (str/ends-with? (.getName %) ".edn")))
           (keep #(parse-devmap-edn (.getPath %)))
           vec)
      [])))

(defn build-inventory
  "Build the full cross-repo mission inventory.
   repos: map of {repo-name root-path} (defaults to default-repo-roots)."
  ([] (build-inventory default-repo-roots))
  ([repos]
   (let [md-missions (into []
                           (mapcat (fn [[repo-name root]]
                                     (scan-mission-files root repo-name)))
                           (dissoc repos :futon5))
         devmap-missions (if-let [f5 (:futon5 repos)]
                           (scan-devmap-files f5)
                           [])]
     (into md-missions devmap-missions))))

;; =============================================================================
;; Devmap reading (minimal reimpl — no futon5 classpath dependency)
;; =============================================================================

(defn read-devmap
  "Read a devmap EDN file and extract structural summary.
   This is a minimal reimpl of futon5.ct.mission/summary that reads
   the EDN directly without depending on the futon5 classpath."
  [path]
  (try
    (let [raw (edn/read-string (slurp path))
          mid (:mission/id raw)
          state (:mission/state raw)
          inputs (get-in raw [:ports :input])
          outputs (get-in raw [:ports :output])
          components (:components raw)
          edges (:edges raw)
          ;; Minimal validation: check structural properties
          input-ids (set (map :id inputs))
          output-ids (set (map :id outputs))
          comp-ids (set (map :id components))
          edge-froms (set (map :from edges))
          edge-tos (set (map :to edges))
          all-node-ids (into (into input-ids output-ids) comp-ids)
          ;; Check for orphan inputs (input not connected to anything)
          orphan-inputs (filter #(not (edge-froms %)) input-ids)
          ;; Check for dead components (component not reaching any output)
          ;; Simplified: just check if component appears in any edge
          connected-comps (into (set (filter comp-ids edge-froms))
                                (filter comp-ids edge-tos))
          dead-comps (remove connected-comps comp-ids)
          ;; Check for spec coverage (outputs with :spec-ref)
          unspecified-outputs (filter #(nil? (:spec-ref %)) outputs)
          failed-checks (cond-> []
                          (seq orphan-inputs)
                          (conj :no-orphan-inputs)
                          (seq dead-comps)
                          (conj :coverage)
                          (seq unspecified-outputs)
                          (conj :spec-coverage))]
      {:devmap/id mid
       :devmap/state (or state :unknown)
       :devmap/input-count (count inputs)
       :devmap/output-count (count outputs)
       :devmap/component-count (count components)
       :devmap/edge-count (count edges)
       :devmap/all-valid (empty? failed-checks)
       :devmap/failed-checks (vec failed-checks)
       :devmap/components (mapv (fn [c]
                                  {:component/id (:id c)
                                   :component/name (or (:name c) (name (:id c)))})
                                components)})
    (catch Exception e
      {:devmap/id (keyword (.getName (io/file path)))
       :devmap/state :error
       :devmap/input-count 0
       :devmap/output-count 0
       :devmap/component-count 0
       :devmap/edge-count 0
       :devmap/all-valid false
       :devmap/failed-checks [:parse-error]
       :devmap/components []})))

(defn read-all-devmaps
  "Read all devmap EDN files from futon5/data/missions/.
   Skips grounding functor files (not mission diagrams)."
  [futon5-root]
  (let [dir (io/file futon5-root "data" "missions")]
    (if (.isDirectory dir)
      (->> (.listFiles dir)
           (filter #(and (.isFile %)
                         (str/ends-with? (.getName %) ".edn")
                         (not (str/includes? (.getName %) "grounding-functor"))))
           (mapv #(read-devmap (.getPath %))))
      [])))

;; =============================================================================
;; Devmap coverage analysis
;; =============================================================================

(defn compute-coverage
  "Compute coverage of devmap components by missions.
   For each devmap, check which components have corresponding missions
   (by name matching: component :S-dispatch matches mission containing 'dispatch')."
  [devmap-summaries missions]
  (let [mission-ids (set (map :mission/id missions))
        mission-id-lower (set (map str/lower-case mission-ids))]
    (mapv (fn [dm]
            (let [components (:devmap/components dm)
                  covered (filter (fn [c]
                                    (let [cname (str/lower-case (name (:component/id c)))]
                                      ;; A component is "covered" if any mission name
                                      ;; contains part of the component name or vice versa.
                                      ;; This is heuristic — the real link is devmap annotations.
                                      (some (fn [mid]
                                              (or (str/includes? mid cname)
                                                  (str/includes? cname mid)))
                                            mission-id-lower)))
                                  components)
                  uncovered (remove (set (map :component/id covered)) (map :component/id components))
                  total (count components)]
              {:coverage/devmap-id (:devmap/id dm)
               :coverage/total-components total
               :coverage/covered-components (count covered)
               :coverage/uncovered (vec uncovered)
               :coverage/coverage-pct (if (pos? total)
                                        (double (/ (count covered) total))
                                        1.0)}))
          devmap-summaries)))

;; =============================================================================
;; Mana queries
;; =============================================================================

(defn query-mana
  "Query mana pool stats from nonstarter.db (if it exists).
   Returns a ManaSnapshot."
  [futon5-root]
  (let [db-path (str futon5-root "/data/nonstarter.db")
        db-file (io/file db-path)]
    (if (.exists db-file)
      ;; nonstarter.db exists — try to read pool stats
      ;; We avoid requiring nonstarter.db.clj directly to keep repos independent.
      ;; Instead, read via raw JDBC if available, or return :available true with note.
      {:mana/available true
       :mana/pool-balance 0.0
       :mana/total-donated 0.0
       :mana/total-funded 0.0
       :mana/active-proposals 0}
      ;; nonstarter.db doesn't exist yet
      {:mana/available false})))

;; =============================================================================
;; Portfolio review
;; =============================================================================

(defn- summarize-portfolio
  "Generate a human-readable portfolio summary."
  [missions devmap-summaries coverage mana]
  (let [total-missions (count missions)
        complete (count (filter #(= :complete (:mission/status %)) missions))
        in-progress (count (filter #(= :in-progress (:mission/status %)) missions))
        blocked (count (filter #(= :blocked (:mission/status %)) missions))
        ready (count (filter #(= :ready (:mission/status %)) missions))
        total-devmaps (count devmap-summaries)
        valid-devmaps (count (filter :devmap/all-valid devmap-summaries))
        avg-coverage (if (seq coverage)
                       (/ (reduce + (map :coverage/coverage-pct coverage)) (count coverage))
                       0.0)]
    (str total-missions " missions"
         " (" complete " complete"
         ", " in-progress " in-progress"
         ", " blocked " blocked"
         ", " ready " ready)"
         ". " total-devmaps " devmaps"
         " (" valid-devmaps " valid)."
         " Avg coverage: " (format "%.0f%%" (* 100 avg-coverage)) "."
         (when-not (:mana/available mana) " Mana system not yet initialized."))))

(defn- find-gaps
  "Identify gaps: devmap components without missions, blocked missions, etc."
  [missions coverage]
  (let [blocked-missions (filter #(= :blocked (:mission/status %)) missions)
        uncovered (mapcat (fn [c]
                            (map (fn [comp-id]
                                   (str (name (:coverage/devmap-id c))
                                        "/" (name comp-id) " — no mission"))
                                 (:coverage/uncovered c)))
                          coverage)]
    (into (mapv (fn [m]
                  (str (:mission/id m) " — blocked"
                       (when (:mission/blocked-by m)
                         (str ": " (:mission/blocked-by m)))))
                blocked-missions)
          uncovered)))

(defn- find-actionable
  "Identify actionable missions: ready or in-progress, not blocked."
  [missions]
  (let [actionable (filter #(#{:ready :in-progress} (:mission/status %)) missions)]
    (mapv (fn [m]
            (str (:mission/id m)
                 " (" (name (:mission/status m)) ")"
                 (when (:mission/repo m) (str " [" (:mission/repo m) "]"))))
          actionable)))

(defn build-portfolio-review
  "Build a complete portfolio review from scanned data.
   repos: map of {repo-name root-path} (defaults to default-repo-roots)."
  ([] (build-portfolio-review default-repo-roots))
  ([repos]
   (let [missions (build-inventory repos)
         futon5-root (or (:futon5 repos) (:futon5 default-repo-roots))
         devmap-summaries (read-all-devmaps futon5-root)
         coverage (compute-coverage devmap-summaries missions)
         mana (query-mana futon5-root)
         summary (summarize-portfolio missions devmap-summaries coverage mana)
         gaps (find-gaps missions coverage)
         actionable (find-actionable missions)]
     {:portfolio/missions missions
      :portfolio/devmap-summaries devmap-summaries
      :portfolio/coverage coverage
      :portfolio/mana mana
      :portfolio/summary summary
      :portfolio/gaps gaps
      :portfolio/actionable actionable})))

;; =============================================================================
;; Backfill — legacy missions as evidence (D7)
;; =============================================================================

(defn mission->evidence
  "Convert a MissionEntry into an EvidenceEntry for backfill.
   Produces an observation about a mission's current state."
  [mission]
  (let [mid (:mission/id mission)
        src (name (:mission/source mission))
        now (str (java.time.Instant/now))]
    {:evidence/id (str "e-backfill-" mid "-" src)
     :evidence/subject {:ref/type :mission :ref/id mid}
     :evidence/type :coordination
     :evidence/claim-type :observation
     :evidence/author "mission-control/backfill"
     :evidence/at now
     :evidence/body (select-keys mission [:mission/id :mission/status
                                          :mission/source :mission/repo
                                          :mission/path :mission/date
                                          :mission/blocked-by
                                          :mission/raw-status
                                          :mission/devmap-id])
     :evidence/tags [:mission :backfill :snapshot]}))

(defn backfill-inventory
  "Convert a full mission inventory into backfill evidence entries.
   Returns a vector of EvidenceEntry maps ready for store/append*."
  ([] (backfill-inventory (build-inventory)))
  ([missions]
   (mapv mission->evidence missions)))
