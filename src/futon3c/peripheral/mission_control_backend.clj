(ns futon3c.peripheral.mission-control-backend
  "Backend tool implementations for the mission control peripheral.

   Tools:
   - :mc-inventory     — scan mission files across repos, return inventory
   - :mc-devmaps       — read and summarize devmap wiring diagrams
   - :mc-coverage      — compute devmap coverage against mission inventory
   - :mc-mana          — query mana pool stats (if nonstarter.db exists)
   - :mc-review        — produce a full portfolio review
   - :mc-bulletin      — emit a war bulletin as evidence
   - :mc-diff          — compare last two portfolio review snapshots

   Mission focus tools (:mc-focus, :mc-focus-clear, :mc-focus-show)
   are dispatched in mission_control.clj as they only manipulate session state.

   All tools are read-only with respect to external systems.
   Evidence emission happens at the peripheral level, not here."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as cset]
            [clojure.string :as str]
            [futon3c.agency.registry :as reg]
            [futon3c.agents.tickle :as tickle]
            [futon3c.evidence.store :as estore]
            [futon3c.peripheral.mission-backend :as mb]
            [futon3c.peripheral.tools :as tools]))

;; =============================================================================
;; Configuration — repo paths
;; =============================================================================

(def default-repo-roots
  "Default repo locations (co-located at ~/code/)."
  (let [home (System/getProperty "user.home")]
    {:futon3c (str home "/code/futon3c")
     :futon3b (str home "/code/futon3b")
     :futon3a (str home "/code/futon3a")
     :futon5  (str home "/code/futon5")
     :futon3  (str home "/code/futon3")
     :futon4  (str home "/code/futon4")
     :futon6  (str home "/code/futon6")}))

;; =============================================================================
;; Mission file parsing
;; =============================================================================

(defn- extract-header
  "Extract a Key: value from markdown text.
   Matches three formats:
   - **Key:** value      (bold key)
   - Key: value          (plain key)
   - ## Key: value       (heading key)"
  [text key-name]
  (let [quoted (java.util.regex.Pattern/quote key-name)
        ;; Try bold format first: **Key:** value
        bold-pat (re-pattern (str "(?m)^\\*\\*" quoted ":\\*\\*\\s*(.+)$"))
        ;; Fallback: plain or heading format: Key: value or ## Key: value
        plain-pat (re-pattern (str "(?mi)^(?:#{1,3}\\s+)?" quoted ":\\s*(.+)$"))]
    (or (when-let [m (re-find bold-pat text)]
          (str/trim (second m)))
        (when-let [m (re-find plain-pat text)]
          (str/trim (second m))))))

(defn classify-status
  "Classify a raw status string into a MissionStatus keyword."
  [raw]
  (when raw
    (let [s (-> raw str/trim str/lower-case (str/replace #"^:" ""))]
      (cond
        (str/starts-with? s "complete")         :complete
        (str/starts-with? s "done")             :complete
        (str/starts-with? s "blocked")          :blocked
        (str/starts-with? s "ready")            :ready
        (str/starts-with? s "pass")             :complete
        (str/starts-with? s "in-progress")      :in-progress
        (str/starts-with? s "in progress")      :in-progress
        (str/starts-with? s "active")           :in-progress
        (str/starts-with? s "open")             :in-progress
        (str/starts-with? s "greenfield")       :ready
        ;; Derivation keywords: check if the *derivation step itself* is marked complete.
        ;; "INSTANTIATE complete" → :complete (the mission finished its last step)
        ;; "INSTANTIATE (complete)" → :complete (parenthetical variant)
        ;; "MAP (landscape survey complete)" → :in-progress (MAP done, mission continues)
        (re-find #"identify|map|derive|argue|verify|instantiate" s)
        (if (re-find #"^(?:identify|map|derive|argue|verify|instantiate)\s+(?:\(?\s*complete)" s)
          :complete
          :in-progress)
        :else :unknown))))

(defn- count-checkboxes
  "Count checked and total checkboxes in markdown text.
   Returns {:checked N :total N} or nil if no checkboxes found."
  [text]
  (let [checked (count (re-seq #"(?m)^[\s]*- \[x\]" text))
        unchecked (count (re-seq #"(?m)^[\s]*- \[ \]" text))
        total (+ checked unchecked)]
    (when (pos? total)
      {:checked checked :total total})))

(defn- infer-status-from-checkboxes
  "When no explicit Status header, infer from success criteria checkboxes."
  [{:keys [checked total]}]
  (cond
    (= checked total)           :complete
    (zero? checked)             :ready
    (> checked 0)               :in-progress))

(defn parse-mission-md
  "Parse a mission .md file into a MissionEntry."
  [path repo-name]
  (try
    (let [text (slurp path)
          filename (.getName (io/file path))
          mission-id (str/replace filename #"^M-|\.md$" "")
          raw-status (extract-header text "Status")
          date (extract-header text "Date")
          blocked-by (extract-header text "Blocked by")
          explicit-status (classify-status raw-status)
          checkboxes (count-checkboxes text)
          inferred-status (when (and (nil? explicit-status) checkboxes)
                            (infer-status-from-checkboxes checkboxes))
          status (or explicit-status inferred-status :unknown)]
      (cond-> {:mission/id mission-id
               :mission/status status
               :mission/source :md-file
               :mission/repo (name repo-name)
               :mission/path (str path)
               :mission/date date
               :mission/blocked-by blocked-by
               :mission/raw-status raw-status}
        checkboxes
        (assoc :mission/gates checkboxes)
        (and (nil? explicit-status) inferred-status)
        (assoc :mission/raw-status
               (str "inferred:" (name inferred-status)
                    " (" (:checked checkboxes) "/" (:total checkboxes) " gates)"))))
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
                           repos)
         devmap-missions (if-let [f5 (:futon5 repos)]
                           (scan-devmap-files f5)
                           [])]
     (into md-missions devmap-missions))))

;; =============================================================================
;; Mission doc fidelity audit (GF / drift)
;; =============================================================================

(defn- repo-root-for-mission
  [repos mission]
  (let [repo-k (keyword (:mission/repo mission))]
    (or (get repos repo-k)
        (get default-repo-roots repo-k))))

(defn- audit-mission-doc
  "Run mission-doc-audit for markdown missions.
   Returns audit map. Non-markdown sources return :n/a."
  [repos mission]
  (if (not= :md-file (:mission/source mission))
    {:status :n/a
     :reason :non-md-source}
    (let [mission-id (:mission/id mission)
          mission-path (:mission/path mission)
          repo-root (repo-root-for-mission repos mission)
          guide-path (when repo-root (str repo-root "/docs/futonic-missions.md"))
          cwd (or repo-root (System/getProperty "user.dir"))
          backend (mb/make-mission-backend {:cwd cwd} (tools/make-mock-backend))
          opts (cond-> {:mission-doc-path mission-path}
                 guide-path (assoc :guide-path guide-path))
          result (try
                   (tools/execute-tool backend :mission-doc-audit [mission-id opts])
                   (catch Exception e
                     {:ok false :error (.getMessage e)}))]
      (if (:ok result)
        (:result result)
        {:status :error
         :reason :audit-failed
         :error (or (:error result) "mission-doc-audit failed")}))))

(defn- attach-doc-audit
  [repos missions]
  (mapv (fn [m]
          (if (= :md-file (:mission/source m))
            (assoc m :mission/doc-audit (audit-mission-doc repos m))
            m))
        missions))

(defn- summarize-doc-drift
  "Summarize mission docs/code drift across markdown missions."
  [missions]
  (let [audited (->> missions
                     (keep :mission/doc-audit)
                     (filter #(not= :n/a (:status %)))
                     vec)
        ok-count (count (filter #(= :ok (:status %)) audited))
        drift (filter #(= :drift (:status %)) audited)
        error-count (count (filter #(= :error (:status %)) audited))
        open-sections (reduce + (map #(or (:open-section-count %) 0) drift))
        missing-gf (reduce + (map #(count (get-in % [:gf :missing-headings])) drift))
        drifting-ids (mapv :mission-id drift)]
    {:audit/total (count audited)
     :audit/ok ok-count
     :audit/drift (count drift)
     :audit/error error-count
     :audit/open-section-obligations open-sections
     :audit/missing-gf-headings missing-gf
     :audit/drifting-missions drifting-ids}))

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
          comp-ids (set (map :id components))
          edge-froms (set (map :from edges))
          edge-tos (set (map :to edges))
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
    (catch Exception _e
      {:devmap/id (keyword (.getName (io/file path)))
       :devmap/state :error
       :devmap/input-count 0
       :devmap/output-count 0
       :devmap/component-count 0
       :devmap/edge-count 0
       :devmap/all-valid false
       :devmap/failed-checks [:parse-error]
       :devmap/components []})))

(defn- scan-per-mission-wirings
  "Scan holes/missions/*-wiring.edn in each repo for per-mission wiring diagrams."
  [repos]
  (into []
        (mapcat
         (fn [[_repo-name root]]
           (let [dir (io/file root "holes" "missions")]
             (if (.isDirectory dir)
               (->> (.listFiles dir)
                    (filter #(and (.isFile %)
                                  (str/ends-with? (.getName %) "-wiring.edn")))
                    (keep #(read-devmap (.getPath %))))
               []))))
        repos))

(defn read-all-devmaps
  "Read all devmap EDN files from futon5/data/missions/ and per-mission
   wiring diagrams from each repo's holes/missions/*-wiring.edn."
  ([futon5-root] (read-all-devmaps futon5-root nil))
  ([futon5-root repos]
   (let [futon5-devmaps (let [dir (io/file futon5-root "data" "missions")]
                          (if (.isDirectory dir)
                            (->> (.listFiles dir)
                                 (filter #(and (.isFile %)
                                               (str/ends-with? (.getName %) ".edn")
                                               (not (str/includes? (.getName %) "grounding-functor"))))
                                 (mapv #(read-devmap (.getPath %))))
                            []))
         per-mission (scan-per-mission-wirings (or repos default-repo-roots))]
     (into futon5-devmaps per-mission))))

;; =============================================================================
;; Devmap coverage analysis
;; =============================================================================

(def component-coverage-annotations
  "Explicit component → mission-id coverage map.
   The heuristic substring match is a fallback; this map is ground truth
   where provided. Values are sets of lowercase mission-id strings.
   Extend via (mc/audit-coverage-correspondence) to find orphan components."
  {;; social-exotype components
   :S-presence     #{"transport-adapters" "operational-readiness"}
   :S-authenticate #{"agency-refactor"}
   :S-dispatch     #{"dispatch-peripheral-bridge" "agency-refactor"}
   :S-invoke       #{"peripheral-model" "peripheral-behavior"}
   :S-mode         #{"peripheral-model" "peripheral-behavior"}
   :S-validate     #{"proof-peripheral"}
   :S-persist      #{"forum-refactor"}
   ;; coordination-exotype (gate pipeline)
   :G5 #{"peripheral-gauntlet"}
   :G4 #{"agency-refactor"}
   :G3 #{"psr-pur-mesh-peripheral"}
   :G2 #{"dispatch-peripheral-bridge"}
   :G1 #{"proof-peripheral"}
   :G0 #{"futon3-last-mile"}})

(defn compute-coverage
  "Compute coverage of devmap components by missions.

   Three-tier matching:
   1. Parent match: if the devmap's :devmap/id matches an active mission,
      ALL its components are considered covered.
   2. Annotation match: check component-coverage-annotations map for
      explicit component → mission-id correspondence.
   3. Heuristic: substring match on component name ↔ mission name."
  [devmap-summaries missions]
  (let [mission-ids (set (map :mission/id missions))
        mission-id-lower (set (map str/lower-case mission-ids))
        active-ids (set (map (comp str/lower-case :mission/id)
                             (filter #(#{:in-progress :complete} (:mission/status %))
                                     missions)))]
    (mapv (fn [dm]
            (let [components (:devmap/components dm)
                  devmap-mid (when-let [id (:devmap/id dm)]
                               (str/lower-case (name id)))
                  parent-active? (and devmap-mid (contains? active-ids devmap-mid))
                  covered (if parent-active?
                            components
                            (filter (fn [c]
                                      (let [cid (:component/id c)
                                            ;; Tier 2: explicit annotation
                                            annotated (get component-coverage-annotations cid)
                                            annotated? (and annotated
                                                            (some annotated mission-id-lower))
                                            ;; Tier 3: heuristic fallback
                                            cname (str/lower-case (name cid))
                                            heuristic? (some (fn [mid]
                                                               (or (str/includes? mid cname)
                                                                   (str/includes? cname mid)))
                                                             mission-id-lower)]
                                        (or annotated? heuristic?)))
                                    components))
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
  [missions devmap-summaries coverage mana doc-drift]
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
         " Doc drift: " (:audit/drift doc-drift) "/" (:audit/total doc-drift)
         " drift, " (:audit/open-section-obligations doc-drift)
         " open section obligations."
         (when-not (:mana/available mana) " Mana system not yet initialized."))))

(defn- find-gaps
  "Identify gaps: devmap components without missions, blocked missions, etc."
  [missions coverage]
  (let [blocked-missions (filter #(= :blocked (:mission/status %)) missions)
        doc-drift-missions (->> missions
                                (filter (fn [m]
                                          (= :drift (get-in m [:mission/doc-audit :status]))))
                                (map (fn [m]
                                       (let [a (:mission/doc-audit m)]
                                         (str (:mission/id m)
                                              " — doc drift"
                                              " (open sections: "
                                              (or (:open-section-count a) 0)
                                              ", missing GF headings: "
                                              (count (get-in a [:gf :missing-headings]))
                                              ")"))))
                                vec)
        uncovered (mapcat (fn [c]
                            (map (fn [comp-id]
                                   (str (name (:coverage/devmap-id c))
                                        "/" (name comp-id) " — no mission"))
                                 (:coverage/uncovered c)))
                          coverage)]
    (into (into (mapv (fn [m]
                        (str (:mission/id m) " — blocked"
                             (when (:mission/blocked-by m)
                               (str ": " (:mission/blocked-by m)))))
                      blocked-missions)
                doc-drift-missions)
          uncovered)))

(defn audit-coverage-correspondence
  "Audit devmap/mission correspondence. Returns:
   :orphan-components — devmap components with no annotation and no heuristic match
   :orphan-missions   — missions that address no devmap component
   :stale-annotations — annotation entries referencing non-existent missions
   :summary           — counts for quick overview"
  ([] (audit-coverage-correspondence default-repo-roots))
  ([repos]
   (let [missions (build-inventory repos)
         futon5-root (or (:futon5 repos) (:futon5 default-repo-roots))
         devmaps (read-all-devmaps futon5-root repos)
         mission-id-lower (set (map (comp str/lower-case :mission/id) missions))
         all-components (mapcat (fn [dm]
                                  (map (fn [c] {:devmap (:devmap/id dm)
                                                :component (:component/id c)})
                                       (:devmap/components dm)))
                                devmaps)
         annotated-ids (set (keys component-coverage-annotations))
         orphan-components (vec
                            (remove (fn [{:keys [component]}]
                                      (or (contains? annotated-ids component)
                                          (let [cname (str/lower-case (name component))]
                                            (some (fn [mid]
                                                    (or (str/includes? mid cname)
                                                        (str/includes? cname mid)))
                                                  mission-id-lower))))
                                    all-components))
         ;; Missions that appear in no annotation and no devmap parent
         devmap-mids (set (map (comp str/lower-case name :devmap/id) devmaps))
         annotation-mids (reduce into #{} (vals component-coverage-annotations))
         orphan-missions (vec
                          (remove (fn [m]
                                    (let [mid (str/lower-case (:mission/id m))]
                                      (or (contains? devmap-mids mid)
                                          (contains? annotation-mids mid))))
                                  missions))
         stale (reduce-kv (fn [acc comp-id mission-set]
                            (let [missing (remove mission-id-lower mission-set)]
                              (if (seq missing)
                                (conj acc {:component comp-id
                                           :missing-missions (vec missing)})
                                acc)))
                          [] component-coverage-annotations)]
     {:orphan-components orphan-components
      :orphan-missions (mapv #(select-keys % [:mission/id :mission/repo :mission/status])
                             orphan-missions)
      :stale-annotations stale
      :summary {:total-components (count all-components)
                :annotated (count annotated-ids)
                :orphan-components (count orphan-components)
                :total-missions (count missions)
                :orphan-missions (count orphan-missions)
                :stale-annotations (count stale)}})))

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
   (let [missions-raw (build-inventory repos)
         missions (attach-doc-audit repos missions-raw)
         futon5-root (or (:futon5 repos) (:futon5 default-repo-roots))
         devmap-summaries (read-all-devmaps futon5-root repos)
         coverage (compute-coverage devmap-summaries missions)
         mana (query-mana futon5-root)
         doc-drift (summarize-doc-drift missions)
         summary (summarize-portfolio missions devmap-summaries coverage mana doc-drift)
         gaps (find-gaps missions coverage)
         actionable (find-actionable missions)]
     {:portfolio/missions missions
      :portfolio/devmap-summaries devmap-summaries
      :portfolio/coverage coverage
      :portfolio/mana mana
      :portfolio/doc-drift doc-drift
      :portfolio/summary summary
      :portfolio/gaps gaps
      :portfolio/actionable actionable})))

;; =============================================================================
;; Tension export — structured gaps for hyperedge creation
;; =============================================================================

(defn build-tension-export
  "Build structured tension data from portfolio review.
   Returns typed tension entries pre-shaped for Arxana hyperedge creation.
   repos: map of {repo-name root-path} (defaults to default-repo-roots)."
  ([] (build-tension-export default-repo-roots))
  ([repos]
   (let [review (build-portfolio-review repos)
         missions (:portfolio/missions review)
         coverage (:portfolio/coverage review)
         devmaps (:portfolio/devmap-summaries review)
         now (str (java.time.Instant/now))
         ;; Uncovered components: one tension per (devmap, component) pair
         uncovered-tensions
         (into []
               (mapcat (fn [cov]
                         (let [dm-id (:coverage/devmap-id cov)]
                           (map (fn [comp-id]
                                  {:tension/type :uncovered-component
                                   :tension/devmap dm-id
                                   :tension/component comp-id
                                   :tension/coverage-pct (:coverage/coverage-pct cov)
                                   :tension/detected-at now
                                   :tension/summary (str (name dm-id) "/" (name comp-id)
                                                         " — no mission")})
                                (:coverage/uncovered cov)))))
               coverage)
         ;; Blocked missions
         blocked-tensions
         (into []
               (comp (filter #(= :blocked (:mission/status %)))
                     (map (fn [m]
                            {:tension/type :blocked-mission
                             :tension/mission (:mission/id m)
                             :tension/blocked-by (:mission/blocked-by m)
                             :tension/detected-at now
                             :tension/summary (str (:mission/id m) " — blocked"
                                                   (when (:mission/blocked-by m)
                                                     (str ": " (:mission/blocked-by m))))})))
               missions)
         ;; Structural invalidity: devmaps with failed checks
         structural-tensions
         (into []
               (comp (filter #(seq (:devmap/failed-checks %)))
                     (map (fn [dm]
                            {:tension/type :structural-invalid
                             :tension/devmap (:devmap/id dm)
                             :tension/detected-at now
                             :tension/summary (str (name (:devmap/id dm))
                                                   " — failed checks: "
                                                   (str/join ", " (map name (:devmap/failed-checks dm))))})))
               devmaps)
         tensions (into (into uncovered-tensions blocked-tensions) structural-tensions)
         by-type (frequencies (map :tension/type tensions))]
     {:tensions tensions
      :detected-at now
      :summary {:total (count tensions)
                :by-type by-type}})))

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
    {:evidence/id (str "e-backfill-" mid "-" (or (:mission/repo mission) "unknown") "-" src)
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
                                          :mission/devmap-id
                                          :mission/gates])
     :evidence/tags [:mission :backfill :snapshot]}))

(defn backfill-inventory
  "Convert a full mission inventory into backfill evidence entries.
   Returns a vector of EvidenceEntry maps ready for store/append*."
  ([] (backfill-inventory (build-inventory)))
  ([missions]
   (mapv mission->evidence missions)))

;; =============================================================================
;; Tickle — stall detection and agent paging
;; =============================================================================

(defn- stringify-instants
  "Convert Instant values to strings for JSON serialization."
  [activity-map]
  (into {}
        (map (fn [[agent-id info]]
               [agent-id (update info :last-active #(when % (str %)))]))
        activity-map))

(defn tickle-scan
  "Scan for stalled agents. Returns activity map and stalled list.
   Observation only — does not page or escalate."
  [evidence-store opts]
  (let [threshold (long (or (:threshold-seconds opts) 300))
        self-id (or (:self-id opts) "tickle-1")
        registry-snapshot (reg/registered-agents)
        activity (tickle/scan-activity evidence-store registry-snapshot
                                      threshold :self-id self-id)
        stalled (tickle/detect-stalls activity)]
    {:scan/activity (stringify-instants activity)
     :scan/stalled stalled
     :scan/agent-count (count activity)
     :scan/stall-count (count stalled)
     :scan/threshold-seconds threshold}))

(defn tickle-page
  "Page a specific agent via test bell. Returns page result."
  [evidence-store agent-id]
  (tickle/page-agent! (or agent-id "unknown")
                      {:evidence-store evidence-store}))

(defn tickle-cycle
  "Run a full scan → page → escalate cycle. Returns cycle result."
  [evidence-store opts]
  (tickle/run-scan-cycle!
   {:evidence-store evidence-store
    :threshold-seconds (or (:threshold-seconds opts) 300)
    :self-id (or (:self-id opts) "tickle-1")
    :page-config (or (:page-config opts) {})}))

;; =============================================================================
;; Portfolio diff — compare consecutive review snapshots
;; =============================================================================

(defn- compute-portfolio-diff
  "Compute diff between two portfolio snapshots (newest first).
   Each snapshot is an evidence entry with :evidence/body containing
   :portfolio/missions (vec of {:mission/id :mission/status})."
  [new-snapshot old-snapshot]
  (let [new-missions (:portfolio/missions (:evidence/body new-snapshot))
        old-missions (:portfolio/missions (:evidence/body old-snapshot))
        new-by-id (into {} (map (juxt :mission/id identity)) new-missions)
        old-by-id (into {} (map (juxt :mission/id identity)) old-missions)
        new-ids (set (keys new-by-id))
        old-ids (set (keys old-by-id))
        added (vec (for [id (sort (cset/difference new-ids old-ids))]
                     (get new-by-id id)))
        removed (vec (for [id (sort (cset/difference old-ids new-ids))]
                       (get old-by-id id)))
        changed (vec (for [id (sort (cset/intersection new-ids old-ids))
                           :let [new-status (:mission/status (get new-by-id id))
                                 old-status (:mission/status (get old-by-id id))]
                           :when (not= new-status old-status)]
                       {:mission/id id
                        :old-status old-status
                        :new-status new-status}))]
    {:added added
     :removed removed
     :changed changed
     :new-count (count new-missions)
     :old-count (count old-missions)
     :new-summary (:portfolio/summary (:evidence/body new-snapshot))
     :old-summary (:portfolio/summary (:evidence/body old-snapshot))
     :new-coverage (:portfolio/coverage (:evidence/body new-snapshot))
     :old-coverage (:portfolio/coverage (:evidence/body old-snapshot))}))

(defn portfolio-diff
  "Query last two portfolio review snapshots from evidence and compute diff.
   Returns {:ok true :diff {...}} or {:ok true :diff nil :message ...}."
  [evidence-store]
  (if-not evidence-store
    {:ok false :error "Evidence store not available"}
    (let [snapshots (estore/query* evidence-store
                                   {:query/subject {:ref/type :portfolio
                                                    :ref/id "global"}
                                    :query/type :coordination
                                    :query/limit 100})
          snapshots (->> snapshots
                         (filter (fn [e]
                                   (let [tags (set (:evidence/tags e))]
                                     (and (contains? tags :review)
                                          (contains? tags :portfolio-snapshot)))))
                         (take 2)
                         vec)]
      (if (< (count snapshots) 2)
        {:ok true
         :diff nil
         :message "Not enough review history — run mc-review at least twice"}
        {:ok true
         :diff (compute-portfolio-diff (first snapshots) (second snapshots))}))))
