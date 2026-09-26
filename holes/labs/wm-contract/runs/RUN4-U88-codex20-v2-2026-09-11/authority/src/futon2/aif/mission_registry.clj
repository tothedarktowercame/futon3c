(ns futon2.aif.mission-registry
  "Mission-doc substrate adapter for the WM AIF apparatus.

   This namespace is the first concrete adapter for `:open-mission`.
   It scans live top-level mission docs at `*/holes/missions/M-*.md`,
   extracts a lightweight status/title view, filters to missions that are
   not closed/draft/sandbox, and exposes them as addressable targets for the WM's
   action layer.

   Honest scope: file-backed and heuristic. The adapter reads mission docs'
   `Status:` line rather than a typed substrate. This is still an honest
   improvement over the prior state where `:open-mission` was permanently
   gated and only surfaced as `:learn-action-class`.

   The interface (`load-missions` / `open-missions` /
   `can-propose? :open-mission` / the enumerator proposer) is the swap
   target for a richer substrate if mission metadata becomes canonical
   elsewhere."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.action-proposer :as ap]
            [futon2.aif.forward-model :as fm])
  (:import (java.io File)))

(def ^:private default-code-root
  (str (System/getProperty "user.home") "/code"))

(def ^:private mission-path-pattern
  #".*/holes/missions/(M-[^/]+)\.md$")

(def ^:private status-line-pattern
  #"(?i)^\s*(?:[-*]\s*)?(?:#+\s*)?(?:\*\*)?Status:?(?:\*\*)?\s*:?\s*(.+)$")

(def ^:private unchecked-task-pattern
  #"^\s*[-*]\s+\[\s\]\s+\S.*$")

(def ^:private explicit-work-marker-pattern
  #"(?i)(?:^|[\s`(*_:-])(?:TODO|FIXME|XXX|TBD)(?:$|[\s`),.:;_-])|(?:\b(?:open\s+)?(?:hole|sorry)\s*:)|\bsorry/[-A-Za-z0-9_.]+")

(def ^:private pending-lifecycle-pattern
  #"(?i)\b(?:HEAD|IDENTIFY|MAP|DERIVE|ARGUE|VERIFY|INSTANTIATE)\b[^.\n;|]*\b(?:pending|next|open|blocked|active|in[ -]progress|remaining|not yet|needed)\b|\b(?:pending|next|open|blocked|active|in[ -]progress|remaining|not yet|needed)\b[^.\n;|]*\b(?:HEAD|IDENTIFY|MAP|DERIVE|ARGUE|VERIFY|INSTANTIATE)\b")

(def ^:private open-section-heading-pattern
  #"(?i)^\s*#{2,6}\s+(?:open questions?|open tasks?|remaining work|remaining tasks?|pending work|next steps)\b.*$")

(def ^:private list-item-pattern
  #"^\s*(?:[-*+]\s+|\d+[.)]\s+)\S.*$")

(defn- mission-id-from-path
  [path]
  (second (re-matches mission-path-pattern path)))

(defn- sandbox-path?
  [path]
  (str/includes? path "/.state/"))

(def ^:private non-primary-path-patterns
  "Patterns that identify non-primary checkout paths — git worktrees,
   directory copies, and other sources of duplicate mission-doc hits.

   Each entry is [pattern source-name]. The scan-root fence (non-primary-path?)
   excludes these BEFORE dedupe, so the path-length sort heuristic in dedupe-by-id
   is a defense-in-depth fallback, not the sole guard against re-pollution.

   The patterns are deliberately structural (not per-directory-name):
     - /.worktrees/    — the standard git worktree location
     - -health-main    — futon5's health-check worktree
     - -index-check    — futon3c's index-check directory copy
     - futon3b/        — futon3b is a separate repo that duplicates some futon3 docs

   Adding a new drift source = adding a pattern here with its source name.
   The census script (scripts/mission_scan_census.bb) reports which patterns fire
   so future drift has a one-line explanation."
  [[#"/\.worktrees/" "git-worktree"]
   [#"-health-main/" "health-main-worktree"]
   [#"-index-check/" "index-check-copy"]
   [#"futon3b/holes/missions/" "futon3b-cross-repo"]])

(defn- non-primary-path?
  "True when PATH is a non-primary checkout (worktree, directory copy, or
   cross-repo duplicate). These are excluded at scan time so the candidate
   pool reflects only the primary checkout of each repo."
  [path]
  (some (fn [[pattern _]] (re-find pattern path)) non-primary-path-patterns))

(defn- derived-mission-id?
  [mission-id]
  (str/includes? (or mission-id "") "."))

(defn- mission-title-from-lines
  [mission-id lines]
  (or (some (fn [line]
              (when-let [[_ title] (re-matches #"^#\s+(.+)$" line)]
                title))
            lines)
      mission-id))

(defn- classify-status
  "Classify a mission from its Status line. Terminal / draft / inactive states are
   recognised ONLY from the LEADING state token (the mission's overall state, after
   stripping markdown emphasis). Mission statuses describe per-phase progress, so a
   mid-line keyword — \"HEAD complete; IDENTIFY drafted\", \"… 4 done\", \"MAP completed\",
   \"PARTIAL (Phases 2-4 deferred)\", \"revised-draft landed\" — must NOT terminally
   classify a still-live mission; the leading token wins. Bias is toward :unknown
   (kept live) when the lead is unrecognised — a work queue should not silently hide
   work."
  [status-line]
  (let [upper (str/upper-case (or status-line ""))
        lead  (-> upper (str/replace #"^[\s>*#`_~-]+" "") str/trim)
        head  (or (re-find #"[A-Z][A-Z-]*" lead) "")
        prefix? (fn [coll] (some #(str/starts-with? head %) coll))]
    (cond
      (str/includes? upper "SPECIFIED, NOT YET IMPLEMENTED")            :draft
      (= "DRAFT" head)                                                  :draft
      ;; Finding-2 (E-live-loop-3): prefix match catches compound forms
      ;; like SUPERSEDED-AS-MISSION that the old exact-match missed.
      (prefix? #{"ARCHIVED" "PARKED" "SUPERSEDED" "ABANDONED" "DEFERRED"}) :inactive
      (prefix? #{"COMPLETE" "COMPLETED" "CLOSED" "DONE" "DISCHARGED"
                 "ANSWERED" "DISSOLVED"})                                :complete
      (= "ACTIVE" head)                                                 :active
      (= "OPEN" head)                                                   :open
      (= "PARTIAL" head)                                                :partial
      (= "IDENTIFY" head)                                               :identify
      (= "HEAD" head)                                                   :open
      ;; a leading lifecycle-phase token = work in flight = live
      (#{"INSTANTIATE" "MAP" "DERIVE" "ARGUE" "VERIFY"} head)           :active
      :else                                                            :unknown)))

(defn- heading-level
  [line]
  (some-> (re-find #"^\s*(#{1,6})\s+" line) second count))

(defn- open-section-item-count
  [lines]
  (loop [remaining lines
         active-level nil
         n 0]
    (if-let [line (first remaining)]
      (let [level (heading-level line)
            next-active-level (cond
                                (re-find open-section-heading-pattern line) level
                                (and active-level level (<= level active-level)) nil
                                :else active-level)
            count-line? (and next-active-level
                             (not (re-find open-section-heading-pattern line))
                             (re-find list-item-pattern line))]
        (recur (rest remaining)
               next-active-level
               (cond-> n count-line? inc)))
      n)))

(defn- open-hole-count
  "Conservative per-mission remaining-work count from the mission doc itself.
   Terminal/draft/inactive mission states force zero; live documents count
   explicit incomplete work signals only."
  [status-class lines]
  (if (contains? #{:complete :inactive :draft} status-class)
    0
    (+ (count (filter #(re-find unchecked-task-pattern %) lines))
       (count (filter #(re-find explicit-work-marker-pattern %) lines))
       (count (filter #(re-find pending-lifecycle-pattern %) lines))
       (open-section-item-count lines))))

(defn- mission-doc->entry
  [path]
  (let [lines (str/split-lines (slurp path))
        mission-id (mission-id-from-path path)
        status-line (some (fn [line]
                            (when-let [[_ status] (re-matches status-line-pattern line)]
                              status))
                          (take 20 lines))
        status-class (classify-status status-line)]
    {:id mission-id
     :path path
     :title (mission-title-from-lines mission-id lines)
     :status-line status-line
     :status-class status-class
     :open-hole-count (open-hole-count status-class lines)}))

(defn- dedupe-by-id
  "Keep the first entry per mission id (sort order = shortest path = primary
   checkout). Worktrees and directory copies under ~/code/ produce duplicate
   mission-doc hits with identical ids; this removes them post-scan."
  [entries]
  (let [seen (atom #{})]
    (remove (fn [e]
              (if (contains? @seen (:id e))
                true
                (do (swap! seen conj (:id e))
                    false)))
            entries)))

(defn load-missions
  "Scan the code root for top-level mission docs and return
   `{:missions [...]}` with lightweight metadata for each one.

   Only files matching `*/holes/missions/M-*.md` at the immediate mission-doc
   level are included; nested handoff/journal/support docs are excluded."
  ([] (load-missions default-code-root))
  ([code-root]
   (let [root (io/file code-root)
         ;; The contract admits only <code-root>/<repo>/holes/missions/M-*.md.
         ;; Enumerate exactly those directories. Walking all of ~/code also
         ;; traversed build trees, worktrees and node_modules before filtering
         ;; them out, turning a single construction lookup into minutes of IO.
         mission-files (->> (or (.listFiles root) (make-array File 0))
                            (filter #(.isDirectory ^File %))
                            (map #(io/file % "holes" "missions"))
                            (filter #(.isDirectory ^File %))
                            (mapcat #(or (.listFiles ^File %)
                                         (make-array File 0))))
         missions (->> mission-files
                       (filter #(.isFile %))
                       (map #(.getAbsolutePath %))
                       (filter #(re-matches mission-path-pattern %))
                       (remove sandbox-path?)
                       ;; Scan-root fence (W-candidate-drift-fence): exclude
                       ;; non-primary checkouts BEFORE dedupe, so worktrees and
                       ;; directory copies never enter the candidate pool. The
                       ;; path-length sort + dedupe-by-id below is now
                       ;; defense-in-depth, not the sole guard.
                       (remove non-primary-path?)
                       ;; Sort by path length first (shorter = primary checkout,
                       ;; not a worktree/copy), then alphabetically. This ensures
                       ;; dedupe-by-id keeps the primary checkout.
                       (sort-by (juxt count identity))
                       (map mission-doc->entry)
                       (remove #(derived-mission-id? (:id %)))
                       (dedupe-by-id)
                       vec)]
     {:missions missions})))

(def ^:private missions-cache (atom nil))

(def missions-cache-ttl-ms
  "TTL for the load-missions snapshot. A full scan walks ~/code and parses every
   mission doc (~10s), so per-action callers (the WM guardrail selector calls
   `mission-status` once per ranked :open-mission — dozens per selection) MUST
   share one snapshot or a selection takes minutes. The TTL is short so
   cross-cycle freshness (e.g. after the pilot advances a hole) still refreshes."
  15000)

(defn load-missions-cached
  "Memoized `load-missions` (TTL = `missions-cache-ttl-ms`). Within one WM
   selection the per-action calls hit the cache after the first scan; across
   cycles the TTL refreshes. Use this on hot per-action paths, not `load-missions`."
  ([] (load-missions-cached nil))
  ([code-root]
   (let [now (System/currentTimeMillis)
         c   @missions-cache]
     (if (and c (= code-root (:code-root c)) (< (- now (:at c)) missions-cache-ttl-ms))
       (:doc c)
       ;; nil code-root => zero-arg load-missions (preserves any rebinding of the
       ;; zero-arity var, e.g. test redefs pointing at a tmpdir).
       (let [doc (if code-root (load-missions code-root) (load-missions))]
         (reset! missions-cache {:at now :code-root code-root :doc doc})
         doc)))))

(defn live-mission?
  "True when a loaded mission entry is eligible for WM `:open-mission`
   enumeration/ranking."
  [mission]
  (not (contains? #{:complete :inactive :draft}
                  (:status-class mission))))

(defn open-missions
  "Filter loaded missions to those that are live. Zero-arg variant
   scans the default code root; one-arg variant accepts a pre-loaded doc."
  ([] (open-missions (load-missions)))
  ([loaded]
   (vec (filter live-mission? (:missions loaded)))))

(defn mission-target-id
  "Normalize a mission action target to the file-backed registry id when possible.
   Substrate-2 mission endpoints such as `futon4-d/mission/foo` normalize to
   `M-foo`; ordinary registry ids such as `M-foo` are returned unchanged."
  [target]
  (let [target (cond
                 (keyword? target) (name target)
                 (some? target) (str target)
                 :else nil)]
    (cond
      (nil? target) nil
      (str/starts-with? target "M-") target
      :else (when-let [[_ local-id] (re-find #"/mission/([^/]+)$" target)]
              (str "M-" local-id)))))

(defn live-mission-target?
  "True if TARGET resolves to one of MISSIONS' live registry ids."
  [missions target]
  (let [live-ids (set (map :id (filter live-mission? missions)))]
    (contains? live-ids (mission-target-id target))))

(defn mission-status
  "Return WM guardrail status for TARGET from the mission registry.
   The result is deliberately small and stable for requiring-resolve callers:
   `{:open? <bool> :open-hole-count <n>}`. Unknown targets are closed with
   zero holes."
  [target]
  (let [target-id (mission-target-id target)
        mission (some #(when (= target-id (:id %)) %)
                      (:missions (load-missions-cached)))]
    {:open? (boolean (and mission (live-mission? mission)))
     :open-hole-count (long (or (:open-hole-count mission) 0))}))

(defmethod fm/can-propose? :open-mission
  [state _action-type]
  (boolean (seq (:missions state))))

(defmethod fm/can-execute? :open-mission
  [state action]
  (live-mission-target? (:missions state []) (:target action)))

(defmethod fm/can-propose? :advance-mission
  [state _action-type]
  (boolean (seq (:missions state))))

(defmethod fm/can-execute? :advance-mission
  [state action]
  (live-mission-target? (:missions state []) (:target action)))

(def mission-enumerator-proposer
  "Proposer that emits one mission candidate per addressable mission in the
   state map. State must carry `:missions` (populated by `open-missions`).

   A live mission doc IS an already-open mission (the doc exists because the
   mission was opened), so the candidate type is `:advance-mission` — engage
   the mission's open holes — not `:open-mission`. Proposing `:open-mission`
   for already-open missions made the whole top of the WM differential
   un-earnable (teleport): the forward model predicted :spawned for missions
   that already existed (pilot cycle #1 finding, 2026-06-10). `:open-mission`
   remains a recognised type for a future substrate that can distinguish
   genuinely-unopened missions (e.g. proposals/drafts greenlit by the
   operator)."
  (reify ap/ActionProposer
    (propose [_ state]
      (for [m (:missions state)]
        {:type :advance-mission
         :target (:id m)
         :weight 1.0
         :mission-path (:path m)
         ;; carried so the forward model can be TARGET-SENSITIVE without a
         ;; registry dependency cycle (predict reads the action, not the
         ;; registry) — same pattern as :intrinsic-value on address-sorry.
         :open-hole-count (:open-hole-count m)
         :rationale (str "mission substrate: " (:title m)
                         " [" (name (:status-class m))
                         "; advance open holes]")}))
    (proposer-id [_] :mission-enumerator)))
