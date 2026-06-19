(ns futon3c.logic.snapshot
  "State-snapshot-witness check-fn under family `evidence-per-turn`.

   First sibling: `state-snapshot-witness/inventory` — projects the
   structural-law inventory to a flat snapshot record and emits one
   `:event :inventory-snapshot` evidence entry per JVM boot (or on
   operator demand).

   The snapshot complements per-event evidence (`:family-fired`,
   `:family-demoted`) which records deltas. The snapshot records
   cycle-boundary state. Together: full reconstructible history.

   Library reference:
     - futon3/library/invariant-coherence/state-snapshot-witness.flexiarg

   Mission: M-state-snapshot-witness (futon3c/holes/missions/)."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.logic.inventory :as inventory]
            [futon3c.logic.probe :as probe])
  (:import [java.time Instant]))

(def I-state-snapshot-witness
  "Canonical statement of the state-snapshot-witness shape. Grep-verifiable."
  (str "I-state-snapshot-witness: for each registered "
       "state-snapshot-witness/<container> sibling, the durable evidence "
       "store carries at least one :event :state-snapshot (or namespaced "
       "subtype) entry per JVM boot, containing the full projection of "
       "the container's state at boot time. Snapshots complement per-event "
       "deltas (:family-fired, :family-demoted) — together they "
       "reconstruct full history without aggregation gymnastics."))

(def default-inventory-path
  "Default structural-law inventory path."
  "/home/joe/code/futon3c/docs/structural-law-inventory.sexp")

(def default-code-root
  "Default code root scanned for `futon*` git repos."
  "/home/joe/code")

(defonce !last-hud-render
  (atom nil))

(defn- iso-now-str []
  (str (Instant/now)))

(defn- project-family
  "Project an inventory family map to a flat snapshot record."
  [family]
  (let [{:keys [id status scope kind question summary]} family]
    (cond-> {:family-id id}
      status (assoc :status status)
      scope (assoc :scope scope)
      kind (assoc :kind kind)
      question (assoc :question question)
      summary (assoc :summary summary))))

(defn project-inventory
  "Read the inventory at PATH and return a snapshot-shaped projection.

   Returns
     {:source <path>
      :read-at <iso>
      :families <count>
      :families-by-status {:operational <count> :candidate <count> ...}
      :scope-stack-families <count>
      :family-records <vec of project-family results>}

   Returns nil if the inventory cannot be read. Tolerant; never throws."
  ([] (project-inventory default-inventory-path))
  ([path]
   (try
     (let [text (slurp path)
           parsed (inventory/parse-sexp-string text)
           families (inventory/extract-all-families parsed)
           records (mapv project-family families)
           by-status (frequencies (keep :status records))
           scope-stack (count (filter #(= :stack (:scope %)) records))]
       {:source path
        :read-at (str (Instant/now))
        :families (count records)
        :families-by-status by-status
        :scope-stack-families scope-stack
        :family-records records})
     (catch Throwable _ nil))))

(defn- deferred-check-fn?
  "True iff CHECK-FN returns an inactive result with `:deferred? true`."
  [check-fn]
  (try
    (let [r (check-fn nil)]
      (and (= :inactive (:outcome r))
           (true? (get-in r [:detail :deferred?]))))
    (catch Throwable _ false)))

(defn project-registry
  "Project the live `family-check-fns` registry to a snapshot-shaped map."
  ([] (project-registry @probe/family-check-fns))
  ([registry]
   (let [records (->> registry
                      (sort-by (comp str key))
                      (mapv (fn [[family-id check-fn]]
                              {:family-id family-id
                               :deferred? (deferred-check-fn? check-fn)
                               :registered-at (-> check-fn meta :registered-at)})))]
     {:captured-at (iso-now-str)
      :registered-families (count records)
      :deferred-families (count (filter :deferred? records))
      :family-records records})))

(defn- git-output
  "Return trimmed stdout for `git -C REPO-PATH ...ARGS`, or nil on failure."
  [repo-path & args]
  (try
    (let [{:keys [exit out]} (apply shell/sh "git" "-C" repo-path args)]
      (when (zero? exit)
        (some-> out str/trim not-empty)))
    (catch Throwable _ nil)))

(defn- git-lines
  "Return stdout lines for `git -C REPO-PATH ...ARGS`, or [] on failure."
  [repo-path & args]
  (let [out (apply git-output repo-path args)]
    (if out
      (->> (str/split-lines out)
           (remove str/blank?)
           vec)
      [])))

(defn- repo-root?
  [dir]
  (and (.isDirectory dir)
       (.exists (io/file dir ".git"))))

(defn discover-repo-roots
  "Return sorted absolute paths of `futon*` git repos under ROOT."
  ([] (discover-repo-roots default-code-root))
  ([root]
   (let [base (io/file root)]
     (if-not (.isDirectory base)
       []
       (->> (or (.listFiles base) [])
            (filter repo-root?)
            (filter #(str/starts-with? (.getName %) "futon"))
            (mapv #(.getAbsolutePath %))
            sort)))))

(defn- project-repo-ref
  [repo-path]
  (when-let [head-sha (git-output repo-path "rev-parse" "HEAD")]
    (let [branch (or (git-output repo-path "rev-parse" "--abbrev-ref" "HEAD")
                     "DETACHED")
          dirty? (boolean (seq (git-lines repo-path "status" "--porcelain")))
          stash-count (count (git-lines repo-path "stash" "list" "--format=%gd"))
          ahead-behind (git-output repo-path "rev-list" "--left-right" "--count" "HEAD...@{upstream}")
          [ahead behind] (when ahead-behind
                           (let [[a b] (str/split ahead-behind #"\s+" 2)]
                             [(some-> a Long/parseLong)
                              (some-> b Long/parseLong)]))]
      {:repo repo-path
       :branch branch
       :HEAD-sha head-sha
       :ahead-of-origin ahead
       :behind-origin behind
       :dirty? dirty?
       :stash-count stash-count})))

(defn project-repo-refs
  "Project git state for each repo into a snapshot-shaped map."
  ([] (project-repo-refs (discover-repo-roots)))
  ([repo-paths]
   (let [records (->> repo-paths
                      (keep project-repo-ref)
                      (sort-by :repo)
                      vec)]
     {:captured-at (iso-now-str)
      :repos (count records)
      :dirty-repos (count (filter :dirty? records))
      :repo-records records})))

(defn current-hud-render
  "Return the last recorded HUD render snapshot, or nil."
  []
  @!last-hud-render)

(defn clear-hud-render!
  "Clear the HUD render snapshot test/dev state."
  []
  (reset! !last-hud-render nil))

(defn record-hud-render!
  "Record the last HUD render snapshot.

   Expected keys:
     :widget-id
     :rendered-at (optional; defaults to now)
     :counts
     :motion-flag"
  [{:keys [widget-id rendered-at counts motion-flag] :as snapshot}]
  (let [normalized (cond-> {:widget-id widget-id
                            :rendered-at (or rendered-at (iso-now-str))
                            :counts (or counts {})
                            :motion-flag motion-flag}
                     (:source snapshot) (assoc :source (:source snapshot)))]
    (reset! !last-hud-render normalized)
    normalized))

(defn project-hud-render
  "Project the last HUD render to a snapshot-shaped map.

   When no render has been reported yet, returns an inactive placeholder
   rather than failing."
  []
  (if-let [snapshot (current-hud-render)]
    (assoc snapshot :outcome :ok :captured-at (iso-now-str))
    {:outcome :inactive
     :captured-at (iso-now-str)
     :reason "no HUD render snapshot has been reported yet"}))

(defn- emit-snapshot!
  [evidence-store {:keys [subject-id author event container state]}]
  (boundary/append!
   evidence-store
   {:subject {:ref/type :pattern
              :ref/id subject-id}
    :type :coordination
    :claim-type :observation
    :author author
    :body {:event event
           :container container
           :state state
           :emitted-at (iso-now-str)
           :invariant I-state-snapshot-witness}
    :tags [:invariant-queue :state-snapshot container]}))

(defn snapshot-inventory!
  "Emit one `:event :inventory-snapshot` evidence entry through the
   boundary. Reads the inventory at PATH (default `default-inventory-path`).

   Returns the boundary's delivery-receipt-shaped result; on inventory-
   read failure, returns `{:ok false :error/code :inventory-unreadable}`."
  ([evidence-store] (snapshot-inventory! evidence-store default-inventory-path))
  ([evidence-store path]
   (if-let [snap (project-inventory path)]
     (emit-snapshot! evidence-store
                     {:subject-id "state-snapshot/inventory"
                      :author "snapshot/snapshot-inventory!"
                      :event :inventory-snapshot
                      :container :inventory
                      :state snap})
     {:ok false
      :error/code :inventory-unreadable
      :error/message (str "Could not read inventory at " path)})))

(defn snapshot-registry!
  "Emit one `:event :registry-snapshot` evidence entry through the boundary."
  ([evidence-store] (snapshot-registry! evidence-store @probe/family-check-fns))
  ([evidence-store registry]
   (emit-snapshot! evidence-store
                   {:subject-id "state-snapshot/registry"
                    :author "snapshot/snapshot-registry!"
                    :event :registry-snapshot
                    :container :registry
                    :state (project-registry registry)})))

(defn snapshot-repo-refs!
  "Emit one `:event :repo-refs-snapshot` evidence entry through the boundary."
  ([evidence-store] (snapshot-repo-refs! evidence-store (discover-repo-roots)))
  ([evidence-store repo-paths]
   (emit-snapshot! evidence-store
                   {:subject-id "state-snapshot/repo-refs"
                    :author "snapshot/snapshot-repo-refs!"
                    :event :repo-refs-snapshot
                    :container :repo-refs
                    :state (project-repo-refs repo-paths)})))

(defn snapshot-hud-render!
  "Emit one `:event :hud-render-snapshot` evidence entry through the boundary."
  [evidence-store]
  (emit-snapshot! evidence-store
                  {:subject-id "state-snapshot/hud-render"
                   :author "snapshot/snapshot-hud-render!"
                   :event :hud-render-snapshot
                   :container :hud-render
                   :state (project-hud-render)}))

(defn snapshot-inventory-on-load!
  "Boot-callable wrapper. Mirrors the archaeology / locus on-load
   pattern. Prints a one-line OK / structured banner on failure.
   Boot continues either way."
  ([evidence-store] (snapshot-inventory-on-load! evidence-store {}))
  ([evidence-store {:keys [path print?] :or {path default-inventory-path
                                              print? true}}]
   (let [r (snapshot-inventory! evidence-store path)]
     (when print?
       (cond
         (:ok r)
         (let [snap (project-inventory path)]
           (println (str "[snapshot] inventory snapshot emitted: "
                         (:families snap) " families, "
                         (:scope-stack-families snap) " scope=:stack")))

         :else
         (do
           (println "================================================================")
           (println "[snapshot] inventory-snapshot emit FAILED")
           (println (str "          " (:error/message r)))
           (println "          Boot continues; the failure is recorded as evidence.")
           (println "================================================================"))))
     r)))

(defn snapshot-registry-on-load!
  "Boot-callable wrapper for `snapshot-registry!`."
  ([evidence-store] (snapshot-registry-on-load! evidence-store {}))
  ([evidence-store {:keys [registry print?] :or {registry @probe/family-check-fns
                                                 print? true}}]
   (let [r (snapshot-registry! evidence-store registry)]
     (when (and print? (:ok r))
       (let [snap (project-registry registry)]
         (println (str "[snapshot] registry snapshot emitted: "
                       (:registered-families snap) " families, "
                       (:deferred-families snap) " deferred"))))
     r)))

(defn snapshot-repo-refs-on-load!
  "Boot-callable wrapper for `snapshot-repo-refs!`."
  ([evidence-store] (snapshot-repo-refs-on-load! evidence-store {}))
  ([evidence-store {:keys [repo-paths print?]
                    :or {repo-paths (discover-repo-roots)
                         print? true}}]
   (let [r (snapshot-repo-refs! evidence-store repo-paths)]
     (when (and print? (:ok r))
       (let [snap (project-repo-refs repo-paths)]
         (println (str "[snapshot] repo-refs snapshot emitted: "
                       (:repos snap) " repos, "
                       (:dirty-repos snap) " dirty"))))
     r)))

(defn snapshot-hud-render-on-load!
  "Boot-callable wrapper for `snapshot-hud-render!`."
  ([evidence-store] (snapshot-hud-render-on-load! evidence-store {}))
  ([evidence-store {:keys [print?] :or {print? true}}]
   (let [r (snapshot-hud-render! evidence-store)]
     (when (and print? (:ok r))
       (let [snap (project-hud-render)]
         (println (str "[snapshot] hud-render snapshot emitted: "
                       (name (:outcome snap))
                       (when-let [widget-id (:widget-id snap)]
                         (str " (" (name widget-id) ")"))))))
     r)))
