(ns futon3c.logic.tracer
  "Pipeline-tracer items: outstanding non-invariant work added as evidence
   entries so the operational-families view can surface them next to
   `:family-fired` events.

   The invariant-queue apparatus (M-invariant-queue-unstuck +
   M-invariant-queue-extend) shipped a probe that walks the inventory and
   emits `:family-fired :ok | :violation | :inactive` per family per fire.
   That covers things that ARE invariants. Outstanding work items that are
   NOT invariants per se (UI extensions, convention decisions, sub-mission
   scoping, etc.) need a parallel surface so the apparatus can surface them
   too.

   Joe's reframe (2026-04-29): the apparatus's first real customer is the
   work that built it. If the pipeline moves these tracers from open →
   closed over the next ≈ week, the apparatus has passed its own test. If
   they stay inert, we learn what's still clogged. The substrate-2 +
   War-Machine deferred-stubs (registered in `probe-taps`) handle that
   self-test for the invariant-shaped tracks; this namespace handles it for
   the rest.

   Shape: each tracer is one `:event :pipeline-tracer-item` evidence entry
   with `:track-id`, `:target-date`, `:expected-outcome`, `:opened-at`,
   tagged `[:invariant-queue :pipeline-tracer :open <track-id>]` so
   query-by-tag finds the open set.

   Mission: M-invariant-queue-extend (futon3c/holes/missions/), Track 4-as-
   tracer reframe."
  (:require [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.store :as store])
  (:import [java.time Instant]))

(def I-pipeline-tracer
  "Canonical statement of the pipeline-tracer convention. Grep-verifiable."
  (str "I-pipeline-tracer: every outstanding non-invariant work item that "
       "is being watched as a pipeline-flow signal must be recorded as a "
       ":event :pipeline-tracer-item evidence entry, tagged "
       ":pipeline-tracer + :open + <track-id>, with :target-date and "
       ":expected-outcome populated. Closure = a sibling :event "
       ":pipeline-tracer-closed entry referencing the same :track-id."))

(def pipeline-prototype-path
  "Historical prototype data that used to seed `default-tracers` at boot."
  "holes/excursions/pipeline-prototype.edn")

(def default-tracers
  "Boot-time pipeline tracer defaults.

   This is intentionally empty. The old static six-track pipeline dataset
   is unhooked from runtime and preserved in `pipeline-prototype-path`.
   Live projections should call `emit-pipeline-tracers!` with explicit
   generated tracer maps instead of relying on boot to seed historical data.

   Per-tracer keys:
     :track-id          — keyword identifier (also used as a tag)
     :title             — short human-readable label
     :mission           — mission id this tracer belongs to
     :target-date       — ISO date by which the apparatus expects movement
     :expected-outcome  — what 'closed' looks like
     :owner             — who is responsible (or nil for unassigned)"
  [])

(defn emit-tracer!
  "Emit one `:event :pipeline-tracer-item` evidence entry through the
   boundary. Returns the boundary's delivery-receipt-shaped result."
  [evidence-store {:keys [track-id title mission target-date
                          expected-outcome owner]}]
  (boundary/append!
   evidence-store
   {:subject {:ref/type :mission
              :ref/id (when mission (name mission))}
    :type :coordination
    :claim-type :tension
    :author "tracer/emit-pipeline-tracers!"
    :body {:event :pipeline-tracer-item
           :track-id track-id
           :title title
           :mission mission
           :target-date target-date
           :expected-outcome expected-outcome
           :owner owner
           :opened-at (str (Instant/now))
           :invariant I-pipeline-tracer}
    :tags (cond-> [:invariant-queue :pipeline-tracer :open]
            (keyword? track-id) (conj track-id)
            (keyword? mission) (conj mission))}))

(defn emit-pipeline-tracers!
  "Emit one `:pipeline-tracer-item` entry per tracer in TRACERS (default
   `default-tracers`). Operator-runnable; idempotent in the sense that
   re-running creates additional dated entries (each fire is its own
   timestamped record). To close a tracer, emit a sibling
   `:pipeline-tracer-closed` entry via `emit-tracer-closed!`.

   Returns a vector of receipts (one per emit)."
  ([evidence-store]
   (emit-pipeline-tracers! evidence-store default-tracers))
  ([evidence-store tracers]
   (mapv (partial emit-tracer! evidence-store) tracers)))

(defn emit-tracer-closed!
  "Emit a `:pipeline-tracer-closed` entry referencing TRACK-ID. Used when
   a tracer's expected-outcome has been satisfied. Returns the receipt."
  [evidence-store {:keys [track-id resolution closed-by] :as _close}]
  (boundary/append!
   evidence-store
   {:subject {:ref/type :pattern
              :ref/id (str "pipeline-tracer/" (when track-id (name track-id)))}
    :type :coordination
    :claim-type :conclusion
    :author (or closed-by "tracer/emit-tracer-closed!")
    :body {:event :pipeline-tracer-closed
           :track-id track-id
           :resolution resolution
           :closed-at (str (Instant/now))
           :invariant I-pipeline-tracer}
    :tags (cond-> [:invariant-queue :pipeline-tracer :closed]
            (keyword? track-id) (conj track-id))}))

(defn- existing-tracer-track-ids
  "Return the set of track-ids for which an `:pipeline-tracer-item`
   evidence entry exists (any tag `:open` or `:closed`). Tolerant of
   query failure: returns empty set."
  [evidence-store]
  (try
    (let [entries (concat
                   (store/query*
                    evidence-store
                    {:query/type :coordination
                     :query/tags [:pipeline-tracer :open]})
                   (store/query*
                    evidence-store
                    {:query/type :coordination
                     :query/tags [:pipeline-tracer :closed]}))]
      (->> entries
           (keep #(get-in % [:evidence/body :track-id]))
           set))
    (catch Throwable _ #{})))

(defn ensure-default-tracers!
  "Idempotent: emits any explicit `default-tracers` that don't already have a
   `:pipeline-tracer-item` (or matching `:pipeline-tracer-closed`) entry
   in the durable store. Returns
   `{:already-present <count> :emitted <count> :failed [tracers]}`.

   Reachable-from-boot discipline (M-reachable-from-boot 2026-05-01):
   this remains a boot-safe projection point, but the static prototype
   dataset has been unhooked from runtime. With `default-tracers` empty,
   boot emits nothing. Pipeline experiments should feed explicit tracers
   into `emit-pipeline-tracers!` from a live cascade/scan projection."
  [evidence-store]
  (if (empty? default-tracers)
    {:already-present 0 :emitted 0 :attempted 0 :failed []}
    (let [default-ids (set (map :track-id default-tracers))
          present (existing-tracer-track-ids evidence-store)
          present-defaults (filter default-ids present)
          to-emit (remove #(contains? present (:track-id %)) default-tracers)
          emitted (mapv #(do (emit-tracer! evidence-store %) %) to-emit)
          ok-emitted (filter
                      (fn [t]
                        (let [tid (:track-id t)]
                          (try
                            (some #(= tid (get-in % [:evidence/body :track-id]))
                                  (store/query*
                                   evidence-store
                                   {:query/type :coordination
                                    :query/tags [:pipeline-tracer :open tid]}))
                            (catch Throwable _ false))))
                      emitted)]
      {:already-present (count present-defaults)
       :emitted (count ok-emitted)
       :attempted (count emitted)
       :failed (vec (remove (set ok-emitted) emitted))})))
