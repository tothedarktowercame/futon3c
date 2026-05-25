(ns futon3c.aif.stack-generator
  "Live projection of the AIF+ stack self-model from the current registry state.

   Loads cached structural priors from futon5a/holes/stories/*.aif.edn,
   then overlays live mission status from `mcb/build-inventory'.

   The cached EDN supplies the *structure* (which nodes form the spine,
   what conflicts coalesce, edge topology, frame definitions, cross-leaf
   relations).  The registry supplies the *data* (current node statuses).

   This is the perception half of the AIF loop at stack scale; the
   policy half is the recommendation in `:reading :next-move'.  See
   futon5a/holes/excursions/E-aif-stack.md for the architectural context.

   This namespace is a complement to (not a duplicate of) the existing
   futon3c.aif/* namespaces, which run AIF over the Mission Peripheral.
   Where mission_head.clj is the AIF head for a single peripheral, this
  generator is the AIF perception over the stack-as-a-whole."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.peripheral.mission-control-backend :as mcb]
            [futon3c.evidence.store :as estore]))

;; =============================================================================
;; Paths
;; =============================================================================

(def stack-edn-path
  "/home/joe/code/futon5a/holes/stories/THE-STACK.aif.edn")

(def stories-dir
  "/home/joe/code/futon5a/holes/stories")

(def stack-priority-queue-json-path
  "/home/joe/code/futon5a/data/stack-stereolithography-priority-queue.json")

(def s6-agenda-id
  "wm.close-s6.v1")

(def s6-successor-agenda-id
  "wm.close-s6.v2")

(def stack-observation-path
  "futon5a/holes/stories/THE-STACK.aif.edn")

(def s6-successor-specifically
  "clear the completed War Machine item and set the next item for work via the Candidate Queue invariant")

(def s6-successor-invariant
  {:title "completed items in the War Machine get cleared and the next item is set for work"
   :surface "Arxana Browser -> Invariants -> Candidate Queue"})

(def s6-successor-missing-fields
  [:action-surface :step-witness :effect-witness :successor-witness])

(def s6-successor-note
  "v2 is documented as a Candidate Queue follow-on, but it is not yet preregistered as an executable closure agenda.")

;; =============================================================================
;; Cached structural prior loading
;; =============================================================================

(defn- read-edn-file
  "Read an EDN file with a permissive #inst reader (instants kept as strings).
   Returns nil on parse error or missing file."
  [path]
  (when (.exists (io/file path))
    (try
      (with-open [r (java.io.PushbackReader. (io/reader path))]
        (edn/read {:readers {'inst (fn [s] s)}} r))
      (catch Exception _ nil))))

(defn load-cached-stack
  "Load THE-STACK.aif.edn — the structural prior."
  []
  (read-edn-file stack-edn-path))

(defn- aif-edn-leaf-files
  "List immediate .aif.edn files in stories-dir, excluding THE-STACK."
  []
  (let [d (io/file stories-dir)]
    (when (.isDirectory d)
      (->> (.listFiles d)
           (filter #(re-matches #".*\.aif\.edn" (.getName %)))
           (remove #(= "THE-STACK.aif.edn" (.getName %)))))))

(defn load-cached-leaves
  "Load all 16 leaf .aif.edn files into a map keyed by basename."
  []
  (->> (aif-edn-leaf-files)
       (keep (fn [f]
               (when-let [leaf (read-edn-file (.getPath f))]
                 [(str/replace (.getName f) ".aif.edn" "") leaf])))
       (into {})))

(defn- read-json-file
  "Read a JSON file into keywordized Clojure data. Returns nil on parse error
   or missing file."
  [path]
  (when (.exists (io/file path))
    (try
      (json/parse-string (slurp path) true)
      (catch Exception _ nil))))

(def ^:private candidate-family->leaf-family
  {"atomic-inspectable-units" "custody"
   "artifact-custody" "custody"
   "repo-role-clarity" "custody"
   "archaeology-control" "custody"
   "peripheral-custody" "custody"
   "human-visible-inspectability" "inspectability"
   "interaction-evidence-continuity" "inspectability"
   "failure-locality" "control"
   "budgeted-action-selection" "control"
   "cross-store-agreement" "control"})

(defn- kw->id-text [x]
  (cond
    (keyword? x) (name x)
    (symbol? x)  (name x)
    (string? x)  x
    :else        (str x)))

(defn load-priority-queue-summary
  "Load the generated candidate queue and derive overlays for UI surfaces.

   `:by-family` preserves the ranked per-family view.
   `:by-leaf-family` projects into the compressed AIF candidate tier
   (`custody` / `inspectability` / `control`).
   Families with no clean projection are reported in `:unmapped-families`
   rather than silently forced into an AIF bucket."
  []
  (when-let [payload (read-json-file stack-priority-queue-json-path)]
    (let [runs (vec (:runs payload))
          ranked-runs (map-indexed (fn [idx run]
                                     (assoc run :priority/rank (inc idx)))
                                   runs)
          by-family (->> ranked-runs
                         (group-by (comp kw->id-text :family/id))
                         (map (fn [[family-id grouped]]
                                [family-id
                                 {:top-rank (:priority/rank (first grouped))
                                  :top-score (:priority/score (first grouped))
                                  :item-count (count grouped)
                                  :invariants (->> grouped
                                                   (map (comp kw->id-text :invariant/id))
                                                   distinct
                                                   vec)}]))
                         (into (sorted-map)))
          by-leaf-family (->> ranked-runs
                              (keep (fn [run]
                                      (when-let [leaf-family
                                                 (get candidate-family->leaf-family
                                                      (kw->id-text (:family/id run)))]
                                        [leaf-family run])))
                              (group-by first)
                              (map (fn [[leaf-family entries]]
                                     (let [grouped (mapv second entries)
                                           top (first grouped)]
                                       [leaf-family
                                        {:top-rank (:priority/rank top)
                                         :top-score (:priority/score top)
                                         :item-count (count grouped)
                                         :families (->> grouped
                                                        (map (comp kw->id-text :family/id))
                                                        distinct
                                                        vec)
                                         :invariants (->> grouped
                                                          (map (comp kw->id-text :invariant/id))
                                                          distinct
                                                          vec)}])))
                              (into (sorted-map)))
          unmapped-families (->> ranked-runs
                                 (keep (fn [run]
                                         (let [family-id (kw->id-text (:family/id run))]
                                           (when-not (get candidate-family->leaf-family family-id)
                                             {:family-id family-id
                                              :invariant-id (kw->id-text (:invariant/id run))
                                              :rank (:priority/rank run)
                                              :score (:priority/score run)}))))
                                 vec)]
      {:generated-at (:generated-at payload)
       :run-count (count ranked-runs)
       :by-family by-family
       :by-leaf-family by-leaf-family
       :unmapped-families unmapped-families})))

;; =============================================================================
;; Origin / ref resolution
;; =============================================================================

(defn parse-origin
  "Parse a spine-node :origin string like 'leaf-argument#n0' or
   'leaf-6-4-4#n5,n6,n7' — returns {:leaf 'leaf-argument' :node-ids [:n0]}.
   Returns nil if origin is not in the leaf#node form (e.g. it's a sorry id)."
  [origin]
  (when (and (string? origin) (str/includes? origin "#"))
    (let [[leaf-name rest] (str/split origin #"#" 2)
          ids (->> (str/split rest #",|\s+")
                   (map str/trim)
                   (remove str/blank?)
                   (map keyword)
                   vec)]
      {:leaf leaf-name :node-ids ids})))

(defn- lookup-leaf-node
  "Find a node in a leaf by :id."
  [leaf node-id]
  (first (filter #(= node-id (:id %)) (:nodes leaf))))

(defn ref-for-spine-origin
  "Resolve a spine node's :origin to the underlying leaf node's :ref field.
   Multiple origins (e.g. 'leaf-X#nA,nB,nC') yield the first node's ref."
  [cached-leaves origin]
  (when-let [{:keys [leaf node-ids]} (parse-origin origin)]
    (when-let [leaf-data (get cached-leaves leaf)]
      (some-> (lookup-leaf-node leaf-data (first node-ids)) :ref))))

;; =============================================================================
;; Live mission-status projection
;; =============================================================================

(defn parse-mission-ref
  "Parse a leaf :ref like 'self-representing-stack@futon4' →
   {:mission-name 'self-representing-stack' :repo 'futon4'}.
   Returns nil for non-mission refs (e.g., sorry|... refs)."
  [ref]
  (when (and (string? ref) (str/includes? ref "@"))
    (let [[mission-name repo] (str/split ref #"@" 2)]
      {:mission-name mission-name :repo repo})))

(defn- mission-name-of
  [m]
  (or (:mission/id m) (:mission/name m) (get m "mission/id") (get m "mission/name")))

(defn- mission-repo-of
  [m]
  (or (:mission/repo m)
      (get m "mission/repo")
      (some-> (or (:mission/path m) (get m "mission/path"))
              (str/split #"/")
              second)))

(defn- mission-status-of
  [m]
  (or (:mission/status m) (get m "mission/status") (:status m) (get m "status")))

(defn live-status-by-ref
  "Look up live mission status by leaf :ref form. Returns keyword or nil."
  [missions ref]
  (when-let [{:keys [mission-name repo]} (parse-mission-ref ref)]
    (some (fn [m]
            (when (and (= mission-name (mission-name-of m))
                       (or (nil? repo) (= repo (mission-repo-of m))))
              (mission-status-of m)))
          missions)))

(defn normalize-status
  "Map a live status (mission or sorry) to the AIF+ status vocabulary."
  [s]
  (case (some-> s name keyword)
    (:complete :settled)   :settled
    :operational           :operational
    :in-progress           :in-progress
    :active                :active
    (:ready :spec-only)    :spec-only
    :prototype             :prototype
    :nascent               :nascent
    :deferred              :deferred
    :testing               :in-progress
    :unknown               nil
    nil))

;; =============================================================================
;; Recommendation-agenda witness projection
;; =============================================================================

(defn- portfolio-step-evidence
  [evidence-store]
  (when evidence-store
    (->> (estore/query* evidence-store
                        {:query/subject {:ref/type :portfolio
                                         :ref/id "inference"}
                         :query/type :coordination
                         :query/tags [:portfolio :step]
                         :query/limit 50})
         (filter (fn [entry]
                   (let [run (get-in entry [:evidence/body :run])
                         obs (:observation-source run)
                         obs-path (cond
                                    (map? obs)    (:path obs)
                                    (string? obs) obs
                                    :else         nil)]
                     (and (= s6-agenda-id (:agenda-id run))
                          (or (= stack-observation-path obs-path)
                              ;; PI scheduler currently emits the source as a
                              ;; symbolic short name "the-stack"; accept that
                              ;; as well, since semantically it names the
                              ;; same source as stack-observation-path.
                              (= "the-stack" obs-path))))))
         first)))

(defn- next-move-agenda
  [evidence-store]
  (if-let [entry (portfolio-step-evidence evidence-store)]
    (let [run (select-keys (get-in entry [:evidence/body :run])
                           [:run-id :agenda-id :step-before :step-after])
          step-after (:step-after run)
          effect-witness (when (and (number? step-after) (pos? step-after))
                           {:kind :stack-self-step-count
                            :node-id :S6
                            :counter-id :pi-self-step-count
                            :before (:step-before run)
                            :after step-after})]
      {:id s6-agenda-id
       :status (if effect-witness :rolled-forward :stepped)
       :witness {:kind :portfolio-step-evidence
                 :evidence-id (:evidence/id entry)
                 :at (:evidence/at entry)
                 :run run
                 :action (get-in entry [:evidence/body :action])
                 :observation-source (get-in entry [:evidence/body :observation-source])}
       :effect-witness effect-witness
       :successor (when effect-witness
                    {:id s6-successor-agenda-id
                     :status :underspecified
                     :close :S6
                     :specifically s6-successor-specifically
                     :candidate-invariant s6-successor-invariant
                     :recommendation-grade? false
                     :documented? true
                     :missing-fields s6-successor-missing-fields
                     :note s6-successor-note})})
    {:id s6-agenda-id
     :status :unattempted}))

(defn- live-s6-step-count
  [evidence-store]
  (some-> (portfolio-step-evidence evidence-store)
          (get-in [:evidence/body :run :step-after])))

(defn- attach-live-s6-counter
  [stack evidence-store]
  (if-let [step-count (live-s6-step-count evidence-store)]
    (update stack :stack-nodes
            (fn [nodes]
              (mapv (fn [node]
                      (if (= :S6 (:id node))
                        (assoc node
                               :gap (str "PI loop step-count = " step-count "; HGO spec-only")
                               :live-self-step-count step-count
                               :live-gap? true)
                        node))
                    nodes)))
    stack))

(defn- attach-next-move-agenda
  [stack evidence-store]
  (if (= "S6" (some-> (get-in stack [:reading :next-move :close]) name))
    (assoc-in stack [:reading :next-move :agenda] (next-move-agenda evidence-store))
    stack))

;; =============================================================================
;; Live next-move projection from judgement.ranked-actions
;; =============================================================================
;;
;; E-wm-live-recommendation: the cached :reading :next-move in THE-STACK.aif.edn
;; is a static prior (mtime ~33d at time of authorship).  The War Machine's
;; scheduler produces a fresh :judgement :ranked-actions every ~300s.  Project
;; the top of the live ranked-actions into a next-move-shape that the cljs
;; next-move-tile can render alongside the cached prose, so the pilot's loop
;; reads live data rather than a poster on the wall.

(def ^:private live-rec-default-days 14)
(def ^:private live-rec-alt-count 4)

(defn- wm-snapshot-for [days]
  (try
    (when-let [f (requiring-resolve 'futon3c.wm.scheduler/snapshot-for-days)]
      (f days))
    (catch Throwable _ nil)))

(defn- action-target-str [action]
  (let [t (or (:target action) (get action "target"))]
    (when (some? t)
      (if (keyword? t) (name t) (str t)))))

(defn- action-type-str [action]
  (let [t (or (:type action) (get action "type"))]
    (when (some? t)
      (if (keyword? t) (name t) (str t)))))

(defn- action->specifically
  "Project a ranked-action :action map into a short human-legible
   'do this thing' string for the next-move-tile's :specifically slot."
  [action]
  (when action
    (let [tp (action-type-str action)
          tg (action-target-str action)]
      (cond
        (and tp tg) (str tp " " tg)
        tp          tp
        :else       (pr-str action)))))

(defn- ranked-entry-action [entry]
  (or (:action entry) (get entry "action")))

(defn- ranked-entry-g-total [entry]
  (or (:G-total entry) (get entry "G-total")))

(defn- ranked-entry-rank [entry]
  (or (:rank entry) (get entry "rank")))

(defn- alternatives-from-ranked [ranked-actions]
  (->> (rest (take (inc live-rec-alt-count) ranked-actions))
       (map-indexed
        (fn [i entry]
          (let [a (ranked-entry-action entry)]
            [(keyword (str "rank-" (+ 2 i)))
             (str (action-type-str a)
                  " " (action-target-str a)
                  " (G=" (some-> (ranked-entry-g-total entry)
                                 (#(format "%.3f" (double %)))) ")")])))
       (into {})))

(defn- snapshot-judgement [snapshot]
  ;; The cached payload is stored in JSON-key-stringified form
  ;; (see futon3c.wm.scheduler/render-payload-json → stringify-wm-response),
  ;; so the inner map uses string keys.  Tolerate both forms.
  (when-let [payload (:payload snapshot)]
    (or (:judgement payload) (get payload "judgement"))))

(defn- snapshot-as-of [snapshot]
  (some-> snapshot :as-of str))

(defn- snapshot-age-seconds [snapshot]
  (when-let [as-of (:as-of snapshot)]
    (try
      (.toSeconds (java.time.Duration/between
                   ^java.time.Instant as-of
                   (java.time.Instant/now)))
      (catch Throwable _ nil))))

(defn derive-next-move-live
  "Project the top of judgement.ranked-actions into the next-move-tile shape.

   Returns a map with :action, :rank, :G-total, :specifically, :rationale,
   :alternatives-considered, :priorities, :mode, :source, :as-of,
   :scheduler-period-seconds, :age-seconds, :stale?, :note — or nil if no
   live snapshot is available.

   Stale? is true when the snapshot is older than 2× the scheduler period
   (default period 300s → stale at >600s).  The cljs tile uses :stale? to
   render an 'aging' badge."
  ([] (derive-next-move-live (wm-snapshot-for live-rec-default-days)))
  ([snapshot]
   (when-let [judgement (snapshot-judgement snapshot)]
     (let [ranked (or (:ranked-actions judgement)
                      (get judgement "ranked-actions")
                      (get judgement "ranked_actions"))
           top    (first ranked)
           top-action (ranked-entry-action top)
           priorities (or (:priorities judgement)
                          (get judgement "priorities"))
           mode (or (:mode judgement) (get judgement "mode"))
           as-of (snapshot-as-of snapshot)
           age-s (snapshot-age-seconds snapshot)
           ;; Period defaults to 300; we don't import the scheduler ns here
           ;; to keep stack-generator's surface minimal.
           period-s 300
           stale? (when (number? age-s) (> age-s (* 2 period-s)))]
       (when top-action
         {:action top-action
          :rank (or (ranked-entry-rank top) 1)
          :G-total (ranked-entry-g-total top)
          :specifically (action->specifically top-action)
          :rationale (or (:rationale top-action)
                         (get top-action "rationale")
                         "Top of judgement.ranked-actions for this WM tick")
          :alternatives-considered (alternatives-from-ranked ranked)
          :priorities (vec (take 5 (or priorities [])))
          :mode mode
          :source :wm-judgement-ranked-actions
          :as-of as-of
          :scheduler-period-seconds period-s
          :age-seconds age-s
          :stale? (boolean stale?)
          :note (str "Recomputed every WM scheduler tick (default "
                     period-s "s). See E-wm-live-recommendation.md.")})))))

(defn- cached-prose-mtime []
  (try
    (let [f (io/file stack-edn-path)]
      (when (.exists f)
        (str (java.time.Instant/ofEpochMilli (.lastModified f)))))
    (catch Throwable _ nil)))

(defn- cached-prose-age-days []
  (try
    (let [f (io/file stack-edn-path)]
      (when (.exists f)
        (let [ms (- (System/currentTimeMillis) (.lastModified f))]
          (long (/ ms 86400000.0)))))
    (catch Throwable _ nil)))

(defn- attach-next-move-live
  "Add :next-move-live (from live WM snapshot) AND :freshness-warning on
   the cached :next-move prose, so operators can see when the static prior
   is N days old."
  [stack]
  (let [snapshot (wm-snapshot-for live-rec-default-days)
        live-rec (derive-next-move-live snapshot)
        warning {:source :the-stack-aif-edn-cached-prior
                 :path "futon5a/holes/stories/THE-STACK.aif.edn"
                 :mtime (cached-prose-mtime)
                 :age-days (cached-prose-age-days)
                 :note (str "Static cached prior; not recomputed per tick. "
                            "See :reading :next-move-live for live recommendation, "
                            "or futon3c/holes/missions/E-wm-live-recommendation.md.")}]
    (cond-> stack
      live-rec
      (assoc-in [:reading :next-move-live] live-rec)

      (get-in stack [:reading :next-move])
      (assoc-in [:reading :next-move :freshness-warning] warning))))

;; =============================================================================
;; Main projection
;; =============================================================================

(defn- leaf-cached-status-for-origin
  "Resolve the spine origin to a leaf+node and return that node's cached
  :status field.  This is a finer-grained cache layer than THE-STACK
  itself (leaves get re-zipped more often than the meta-rebuild), so it
  is a useful fallback for spine nodes whose origin is a sorry ref
  rather than a mission ref."
  [cached-leaves origin]
  (when-let [{:keys [leaf node-ids]} (parse-origin origin)]
    (when-let [leaf-data (get cached-leaves leaf)]
      (some-> (lookup-leaf-node leaf-data (first node-ids)) :status))))

(defn project-spine-node
  "Overlay status onto a cached spine node.

   Two overlay layers, in order of preference:
     1. Live mission status (when origin → leaf-node has mission@repo ref).
        Marker: :live-status? true.
     2. Leaf-cached status (when origin → leaf-node exists, regardless of
        ref shape). Marker: :live-status? :leaf-derived.

   If neither yields anything, the spine's THE-STACK-cached :status stays."
  [cached-leaves missions node]
  (let [origin (:origin node)
        ref    (ref-for-spine-origin cached-leaves origin)
        mission-live (some-> (live-status-by-ref missions ref) normalize-status)
        leaf-status  (some-> (leaf-cached-status-for-origin cached-leaves origin)
                             normalize-status)]
    (cond
      mission-live           (assoc node :status mission-live :live-status? true)
      leaf-status            (assoc node :status leaf-status  :live-status? :leaf-derived)
      :else                  node)))

(defn generate-live
  "Generate the live AIF+ stack representation by overlaying current
   mission registry state on the cached structural prior.

   Returns a map with the same shape as THE-STACK.aif.edn, plus:
     :live?              true
     :generated-at       <ISO-8601 string>
     :live-data-source   'mission-control-inventory'
     :live-mission-count <int>
     :stack-nodes[i] :live-status? true|false  (per-node overlay marker)
     :leaf-invariants    <leaf-invariants.aif.edn raw map>
                         (carried so the Web War Machine's Invariants view
                          can render the 9 operational + 10 candidate
                          family claims without needing its own endpoint)

   Returns nil if the cached stack EDN cannot be loaded.  If the mission
   inventory is unavailable, returns the cached stack with :live? false
   and no overlays applied."
  ([] (generate-live nil))
  ([opts]
   (when-let [stack (load-cached-stack)]
     (let [cached-leaves (load-cached-leaves)
           priority-queue (load-priority-queue-summary)
           evidence-store (:evidence-store opts)
           missions (try (mcb/build-inventory) (catch Exception _ nil))
           live-applied? (boolean (seq missions))]
       (-> stack
           (update :stack-nodes
                   #(mapv (partial project-spine-node cached-leaves (or missions []))
                          %))
           (attach-live-s6-counter evidence-store)
           (attach-next-move-agenda evidence-store)
           (attach-next-move-live)
           (assoc :live? live-applied?
                  :generated-at (str (java.time.Instant/now))
                  :live-data-source (if live-applied?
                                      "mission-control-inventory"
                                      "cached-only")
                  :live-mission-count (count (or missions []))
                  :leaf-invariants (get cached-leaves "leaf-invariants")
                  :candidate-queue priority-queue))))))
