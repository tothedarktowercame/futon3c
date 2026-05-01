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

(def default-tracers
  "Outstanding non-invariant tracks of M-invariant-queue-extend, registered
   as pipeline-tracer items by default. Edit this list as new tracks open
   or close.

   Per-tracer keys:
     :track-id          — keyword identifier (also used as a tag)
     :title             — short human-readable label
     :mission           — mission id this tracer belongs to
     :target-date       — ISO date by which the apparatus expects movement
     :expected-outcome  — what 'closed' looks like
     :owner             — who is responsible (or nil for unassigned)"
  [{:track-id :track-4-2-snapshot-as-evidence
    :title "Snapshot-as-evidence convention (decide + implement)"
    :mission :M-invariant-queue-extend
    :target-date "2026-05-06"
    :expected-outcome
    "Either: extend `:family-fired` body with full inventory snapshot maps,
     OR ship a separate `:event :inventory-snapshot` shape. Decision +
     implementation lands as a checkpoint."
    :owner nil}

   {:track-id :track-4-3-arxana-view-columns
    :title "Arxana operational-families view columns"
    :mission :M-invariant-queue-extend
    :target-date "2026-05-06"
    :expected-outcome
    "futon4/dev/arxana-browser-{core,lab}.el render `:last-fire-at`,
     `:last-violation-at`, `:inactive-since` columns from the probe's
     `:family-fired` evidence."
    :owner nil}

   {:track-id :track-3-write-class-scoping
    :title "Write-class generalization sub-mission stubs"
    :mission :M-invariant-queue-extend
    :target-date "2026-05-06"
    :expected-outcome
    "Two new mission stubs opened (probably bell-receipts +
     gate-traversals), each with IDENTIFY and a sketch of the boundary +
     ratchet + canary triple."
    :owner nil}

   {:track-id :track-1-substrate-2-lift
    :title "Substrate-2 phase-1 lift (≥3 of 6 families)"
    :mission :M-invariant-queue-extend
    :target-date "2026-05-06"
    :expected-outcome
    "≥3 of 6 substrate-2 phase-1 deferred-stubs converted from `:inactive`
     to a real probe check-fn returning `:ok` or `:violation`. Source:
     futon3/holes/labs/M-live-geometric-stack/tests/phase_1_invariants_test.clj.
     Note: also tracered via the predecessor's `:substrate-2/*` deferred-
     stubs in `probe-taps`; this entry is the bookkeeping surface."
    :owner nil}

   {:track-id :track-2-war-machine-aif-lift
    :title "War-Machine AIF lift (≥2 of 6 OR documented deferral)"
    :mission :M-invariant-queue-extend
    :target-date "2026-05-06"
    :expected-outcome
    "Either ≥2 of 6 War-Machine AIF deferred-stubs converted to real
     check-fns, OR a recorded decision to wait for M-war-machine with
     criteria for revisit. Note: also tracered via `:war-machine/*`
     deferred-stubs."
    :owner nil}

   {:track-id :track-5-vsatarcs
    :title "VSATARCS narrative coherence (parked → re-park or register)"
    :mission :M-invariant-queue-extend
    :target-date "2026-05-06"
    :expected-outcome
    "Either re-park with current rationale, OR upgrade to a deferred-stub
     registration if VSATARCS work has progressed enough to project a
     stable contract."
    :owner nil}])

(defn emit-tracer!
  "Emit one `:event :pipeline-tracer-item` evidence entry through the
   boundary. Returns the boundary's delivery-receipt-shaped result."
  [evidence-store {:keys [track-id title mission target-date
                          expected-outcome owner] :as tracer}]
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
  "Idempotent: emits any of `default-tracers` that don't already have a
   `:pipeline-tracer-item` (or matching `:pipeline-tracer-closed`) entry
   in the durable store. Returns
   `{:already-present <count> :emitted <count> :failed [tracers]}`.

   Reachable-from-boot discipline (M-reachable-from-boot 2026-05-01):
   tracer state is reconstructible-from-disk via this function, called
   from `bootstrap.clj` at boot. Restart-equivalent. The
   `default-tracers` data lives in this namespace (on-disk source); the
   evidence emission is the projection of that data into the durable
   store. Subsequent boots find the entries already-present and emit
   nothing."
  [evidence-store]
  (let [present (existing-tracer-track-ids evidence-store)
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
    {:already-present (count present)
     :emitted (count ok-emitted)
     :attempted (count emitted)
     :failed (vec (remove (set ok-emitted) emitted))}))
