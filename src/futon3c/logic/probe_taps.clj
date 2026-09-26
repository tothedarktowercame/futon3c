(ns futon3c.logic.probe-taps
  "Per-source tap factories that bridge existing core.logic invariant
   layers to `futon3c.logic.probe`'s family-check-fn registry.

   The probe (futon3c.logic.probe) walks the inventory and invokes a
   registered check-fn per family. This namespace builds those check-fns
   for the three operational core.logic invariant layers documented in
   the inventory's `:operational-families` section:

     - agency      → futon3c.agency.logic
     - tickle      → futon3c.agents.tickle-logic
     - portfolio   → futon3c.portfolio.logic

   Each tap is a *factory* — it takes the layer's state source (an atom
   or a zero-arg function returning the state) and returns a probe
   check-fn `(fn [evidence-store] {:outcome :ok | :violation :detail …})`.

   This separation lets the probe register a check without owning the
   state, and lets the operator wire concrete state at activation time.
   It is the projection-not-enumeration framing made literal: each tap
   is a thin wrapper that READS from where the layer's invariants live.

   Usage:

     (require '[futon3c.logic.probe :as probe]
              '[futon3c.logic.probe-taps :as taps]
              '[futon3c.agency.registry :as reg])

     ;; Register agency taps
     (probe/register-family-check!
       :agency-route-consistency
       (taps/make-agency-tap reg/!registry
                             :route-inconsistencies))

     ;; Run a sweep on demand
     (probe/probe-now! @futon3c.evidence.store/!store)

   Mission: M-invariant-queue-unstuck (futon3c/holes/missions/),
   INSTANTIATE-4 (multi-source projection wave taps)."
  (:require [futon3c.agency.logic :as agency-logic]
            [futon3c.agents.tickle-logic :as tickle-logic]
            [futon3c.evidence.boundary :as evidence]
            [futon3c.portfolio.logic :as portfolio-logic]))

;; ---------------------------------------------------------------------------
;; Helper: state-source-fn
;; ---------------------------------------------------------------------------

(defn- resolve-state
  "Resolve a state-source argument to its current value.
   Accepts an atom (deref'd), a zero-arg fn (called), or a literal map."
  [state-source]
  (cond
    (instance? clojure.lang.IAtom state-source) @state-source
    (fn? state-source) (state-source)
    :else state-source))

(defn- count-violations
  "Sum the count of violations across a map of category → seq, optionally
   excluding informational keys."
  ([violations-map] (count-violations violations-map #{}))
  ([violations-map informational-keys]
   (reduce-kv
    (fn [acc k v]
      (if (informational-keys k)
        acc
        (+ acc (count v))))
    0
    violations-map)))

;; ---------------------------------------------------------------------------
;; Agency tap — wraps futon3c.agency.logic/check-registry
;; ---------------------------------------------------------------------------

(defn- agency-state->logic-state
  "Normalize agency state source values.

   Historical callers pass the raw registry map or registry atom. Newer callers
   can pass the full agency logic snapshot:
     {:registry ... :routing ... :connections ... :server-accept ...}"
  [state]
  (if (and (map? state) (contains? state :registry))
    state
    {:registry (or state {})}))

(defn- agency-violations
  [state]
  (-> state
      agency-state->logic-state
      agency-logic/build-db
      agency-logic/query-violations))

(defn registry-status->routing
  "Project `futon3c.agency.registry/registry-status` into agency.logic routing facts."
  [registry-status]
  (into {}
        (map (fn [[aid info]]
               [aid (select-keys info [:invoke-route :invoke-ready? :invoke-diagnostic])]))
        (:agents registry-status)))

(defn pouch-snapshot->live-writers
  "Project warm-pouch snapshot rows into AG-1 live-writer facts.

   Only alive pouches with a session id are writers. The inhabitant-id is stable
   enough for diagnostics: pid when available, otherwise `pouch:<agent-id>`."
  [pouch-snapshot]
  (->> pouch-snapshot
       (keep (fn [[aid {:keys [alive? session-id pid]}]]
               (when (and alive? session-id)
                 {:agent-id aid
                  :session-id session-id
                  :inhabitant-id (or pid (str "pouch:" aid))})))
       vec))

(defn- optional-supplier
  [x]
  (cond
    (nil? x) nil
    (fn? x) (x)
    :else x))

(defn- default-local-point []
  (or (some-> (System/getProperty "FUTON3C_SITE") not-empty keyword)
      (some-> (System/getenv "FUTON3C_SITE") not-empty keyword)
      :local))

(defn make-live-agency-state-source
  "Return a zero-arg source for the full agency.logic snapshot.

   Defaults read live Agency registry/status/pouches via requiring-resolve.
   Probe facts that need active external checks are supplied optionally:
     :connections, :remote-health, :session-capacity, :server-accept
   Each option may be a literal collection or a zero-arg function."
  ([] (make-live-agency-state-source {}))
  ([{:keys [registry registry-status pouch-snapshot local-point
            connections remote-health session-capacity server-accept]
     :or {local-point ::default}}]
   (fn []
     (let [registry-atom (or registry
                             @(requiring-resolve 'futon3c.agency.registry/!registry))
           registry-status-fn (or registry-status
                                  (requiring-resolve 'futon3c.agency.registry/registry-status))
           pouch-snapshot-fn (or pouch-snapshot
                                 (requiring-resolve 'futon3c.agency.agent-pouch/snapshot))
           status (when registry-status-fn (registry-status-fn))
           pouches (when pouch-snapshot-fn (pouch-snapshot-fn))]
       (cond-> {:registry @registry-atom
                :routing (registry-status->routing status)
                :local-point (if (= ::default local-point)
                               (default-local-point)
                               local-point)
                :live-writers (pouch-snapshot->live-writers pouches)}
         connections (assoc :connections (optional-supplier connections))
         remote-health (assoc :remote-health (optional-supplier remote-health))
         session-capacity (assoc :session-capacity (optional-supplier session-capacity))
         server-accept (assoc :server-accept (optional-supplier server-accept)))))))

(defn emit-agency-backtrace!
  "Append a non-fatal Agency invariant backtrace evidence entry.

   This deliberately writes evidence instead of invoking an Agency-managed
   agent. The operator can feed the emitted payload to an out-of-band agent
   without risking recursive Agency failure."
  [evidence-store {:keys [violations total-violations checked-categories]}]
  (evidence/append!
   evidence-store
   {:subject {:ref/type :agent :ref/id "backtrace"}
    :type :coordination
    :claim-type :observation
    :author "agency-backtrace"
    :body {:event :agency-invariant-backtrace
           :checked-categories checked-categories
           :total-violations total-violations
           :violations violations}
    :tags [:agency :agency-invariants :backtrace :violation]}))

(defn make-agency-tap
  "Build a probe check-fn for the agency invariant layer.

   Parameters:
     STATE-SOURCE — either the registry atom (e.g. `reg/!registry`) or a
                    zero-arg function returning the registry snapshot map.
     CATEGORY     — optional. If supplied, the check focuses on a single
                    violation category (e.g. :route-inconsistencies);
                    register one tap per category as separate families.
                    If omitted, the tap is on the aggregate.

   Returned check-fn:
     {:outcome :ok :detail {:checked-categories <set> :total-violations 0}}
     {:outcome :violation :detail {:violations <category->vec>
                                   :total-violations N}}"
  ([state-source]
   (make-agency-tap state-source nil))
  ([state-source category]
   (make-agency-tap state-source category {}))
  ([state-source category {:keys [backtrace?]
                           :or {backtrace? false}}]
   (let [informational? #{:proxy-agents :agents-invoking}]
     (fn [_evidence-store]
       (try
         (let [state (resolve-state state-source)
               all-violations (agency-violations state)
               focus (if category
                       (select-keys all-violations [category])
                       all-violations)
               total (count-violations focus
                                       (if category #{} informational?))]
           (if (zero? total)
             {:outcome :ok
              :detail {:checked-categories (set (keys focus))
                       :total-violations 0}}
             (let [detail {:violations focus
                           :checked-categories (set (keys focus))
                           :total-violations total}
                   receipt (when backtrace?
                             (emit-agency-backtrace! _evidence-store detail))]
               {:outcome :violation
                :detail (cond-> detail
                          backtrace? (assoc :backtrace-emitted? (true? (:ok receipt))
                                            :backtrace-evidence/id (:evidence/id receipt)))})))
         (catch Throwable t
           {:outcome :violation
            :detail {:exception (str (.getName (class t)) ": " (.getMessage t))}}))))))

;; ---------------------------------------------------------------------------
;; Tickle tap — wraps futon3c.agents.tickle-logic/query-violations
;; ---------------------------------------------------------------------------

(defn make-tickle-tap
  "Build a probe check-fn for the tickle coordination invariant layer.

   Parameters:
     STATE-SOURCE — atom or zero-arg fn returning the tickle state map
                    expected by `tickle-logic/build-db`.
     CATEGORY     — optional, focuses the tap on one violation category.

   Returned check-fn shape matches make-agency-tap's."
  ([state-source]
   (make-tickle-tap state-source nil))
  ([state-source category]
   (fn [_evidence-store]
     (try
       (let [state (resolve-state state-source)
             db (tickle-logic/build-db state)
             all-violations (tickle-logic/query-violations db)
             focus (if category
                     (select-keys all-violations [category])
                     all-violations)
             total (count-violations focus)]
         (if (zero? total)
           {:outcome :ok
            :detail {:checked-categories (set (keys focus))
                     :total-violations 0}}
           {:outcome :violation
            :detail {:violations focus
                     :total-violations total}}))
       (catch Throwable t
         {:outcome :violation
          :detail {:exception (str (.getName (class t)) ": " (.getMessage t))}})))))

;; ---------------------------------------------------------------------------
;; Portfolio tap — wraps a subset of portfolio.logic queries
;; ---------------------------------------------------------------------------

(defn make-portfolio-tap
  "Build a probe check-fn for the portfolio invariant layer.

   The portfolio layer doesn't have a single `query-violations` entry
   point; instead it offers individual queries for blocked missions,
   uncovered components, derived tensions, and unported invariants.
   This tap aggregates a configurable subset.

   Parameters:
     STATE-SOURCE — atom or zero-arg fn returning the portfolio state
                    map expected by `portfolio-logic/build-db`.
     QUERIES      — set of keyword identifiers naming which queries to
                    run. Defaults to all four.

   Recognized query keywords:
     :blocked-missions   — `query-blocked-missions` (informational, not a
                            violation by itself, but useful as a probe
                            signal — non-empty means progress is gated).
     :uncovered          — `query-uncovered-components` (true violations).
     :derived-tensions   — `query-derived-tensions` (informational).
     :unported           — `query-unported-invariants` (true violations
                            for porting completeness)."
  ([state-source]
   (make-portfolio-tap state-source #{:uncovered :unported}))
  ([state-source queries]
   (fn [_evidence-store]
     (try
       (let [state (resolve-state state-source)
             db (portfolio-logic/build-db state)
             coverage-db (when (or (queries :uncovered) (queries :derived-tensions))
                           (portfolio-logic/build-coverage-db db state))
             results (cond-> {}
                       (queries :blocked-missions)
                       (assoc :blocked-missions
                              (portfolio-logic/query-blocked-missions db))

                       (and coverage-db (queries :uncovered))
                       (assoc :uncovered
                              (portfolio-logic/query-uncovered-components coverage-db))

                       (and coverage-db (queries :derived-tensions))
                       (assoc :derived-tensions
                              (portfolio-logic/query-derived-tensions coverage-db))

                       (queries :unported)
                       (assoc :unported
                              (portfolio-logic/query-unported-invariants db)))
             total (count-violations results
                                     #{:blocked-missions :derived-tensions})]
         (if (zero? total)
           {:outcome :ok
            :detail {:checked-categories (set (keys results))
                     :total-violations 0
                     :informational-counts
                     (-> {}
                         (cond-> (results :blocked-missions)
                           (assoc :blocked-missions
                                  (count (results :blocked-missions))))
                         (cond-> (results :derived-tensions)
                           (assoc :derived-tensions
                                  (count (results :derived-tensions)))))}}
           {:outcome :violation
            :detail {:violations results
                     :total-violations total}}))
       (catch Throwable t
         {:outcome :violation
          :detail {:exception (str (.getName (class t)) ": " (.getMessage t))}})))))

;; ---------------------------------------------------------------------------
;; Deferred-stub tap — for documented sources whose check logic isn't yet wired.
;; Surfaces as :inactive with informative detail (NOT silent).
;; ---------------------------------------------------------------------------

(defn make-deferred-tap
  "Build a probe check-fn that always returns :outcome :inactive with
   informative detail. Used for invariants that are *documented* in
   inventory or in source documents (substrate-2 phase-1 tests, War
   Machine AIF diagram-invariants, VSATARCS narrative coherence) but
   whose live-check logic is not yet wired.

   The deferred tap is structurally different from the probe's default
   :inactive outcome (which fires when no check is registered at all).
   A deferred-tap registration is an explicit acknowledgement: 'we know
   this invariant exists; the apparatus surfaces it as inactive with the
   reason'. Distinguishing between unknown-inactive and acknowledged-inactive
   is what makes the queue's projection honest about its gaps.

   META is a map carrying any of:
     :source         — file path or URL of the documented invariant
     :follow-on      — mission id that should land the live check
     :contact        — name or role responsible for wiring the check
     :note           — free-text explanation"
  [meta]
  (fn [_evidence-store]
    {:outcome :inactive
     :detail (merge {:reason "documented; live check not yet wired"
                     :deferred? true}
                    (or meta {}))}))

;; ---------------------------------------------------------------------------
;; Source: substrate-2 phase-1 invariants (M-live-geometric-stack)
;;
;; These are bb scripts at futon3/holes/labs/M-live-geometric-stack/tests/
;; that hit a live futon1a HTTP endpoint and assert specific properties.
;; Wrapping them as live-probe taps requires either invoking the bb scripts
;; or porting their assertions to in-process Clojure. Both are out of scope
;; for INSTANTIATE-4-rest; deferred-stub for now so the families surface in
;; the probe sweep with a clear "deferred" marker.
;; ---------------------------------------------------------------------------

(def substrate-2-phase-1-invariants
  "The six phase-1 invariants enumerated in
   futon3/holes/labs/M-live-geometric-stack/tests/phase_1_invariants_test.clj"
  [:substrate-2/L1-stable-id
   :substrate-2/L1-idempotency
   :substrate-2/L2-endpoint-resolution
   :substrate-2/L2-vocab-target-resolution
   :substrate-2/L2-counter-ratchet
   :substrate-2/regression-vs-bb-v0_5])

;; ---------------------------------------------------------------------------
;; Source: War Machine AIF diagram-invariants
;;
;; These are listed in futon5a/holes/holistic-argument-aif2.edn under
;; :formalism :diagram-invariants. Each is a named invariant of the
;; AIF wiring; check logic exists conceptually in the M-war-machine
;; mission but is not yet runtime-callable. Deferred-stub for now.
;; ---------------------------------------------------------------------------

(def war-machine-aif-invariants
  "The six AIF diagram-invariants from
   futon5a/holes/holistic-argument-aif2.edn :formalism :diagram-invariants"
  [:war-machine/boundary-integrity
   :war-machine/observation-action-asymmetry
   :war-machine/timescale-separation
   :war-machine/preference-exogeneity
   :war-machine/model-adequacy
   :war-machine/compositional-closure])

;; ---------------------------------------------------------------------------
;; Convenience: register-all
;; ---------------------------------------------------------------------------

(defn register-default-taps!
  "Register a default set of taps with `futon3c.logic.probe`. Intended for
   activation-time wiring; tests should call individual `make-*-tap`
   factories with controlled state instead.

   STATES is a map of layer-id → state-source:
     {:agency    <registry-atom>
      :tickle    <tickle-state-fn-or-map>
      :portfolio <portfolio-state-fn-or-map>}

   Layers absent from STATES are not registered; the probe will surface
   their families as `:inactive` until tapped."
  [{:keys [agency tickle portfolio] :as states}]
  (let [probe (requiring-resolve 'futon3c.logic.probe/register-family-check!)
        registered (atom #{})]
    (when agency
      (@probe :agency-invariants (make-agency-tap agency))
      (swap! registered conj :agency-invariants))
    (when tickle
      (@probe :tickle-invariants (make-tickle-tap tickle))
      (swap! registered conj :tickle-invariants))
    (when portfolio
      (@probe :portfolio-invariants (make-portfolio-tap portfolio))
      (swap! registered conj :portfolio-invariants))
    {:registered @registered :sources-with-state (set (keys states))}))

(defn register-deferred-taps!
  "Register deferred-stub taps for documented invariant sources whose
   live-check logic is not yet wired (substrate-2 phase-1 invariants and
   War Machine AIF diagram-invariants). The probe sweep surfaces these
   as :outcome :inactive with informative deferred-detail rather than
   silently omitting them.

   Returns the set of family-ids registered."
  []
  (let [probe (requiring-resolve 'futon3c.logic.probe/register-family-check!)
        registered (atom #{})]
    (doseq [fid substrate-2-phase-1-invariants]
      (@probe fid
              (make-deferred-tap
               {:source "futon3/holes/labs/M-live-geometric-stack/tests/phase_1_invariants_test.clj"
                :follow-on "M-invariant-queue-unstuck (INSTANTIATE-5: substrate-2 lift)"
                :note "bb-script test against live futon1a; lift requires either invoking the bb script or porting assertions to in-process Clojure"}))
      (swap! registered conj fid))
    (doseq [fid war-machine-aif-invariants]
      (@probe fid
              (make-deferred-tap
               {:source "futon5a/holes/holistic-argument-aif2.edn :formalism :diagram-invariants"
                :follow-on "M-war-machine"
                :note "AIF diagram-invariant; check logic exists conceptually in M-war-machine mission, not yet runtime-callable"}))
      (swap! registered conj fid))
    {:registered @registered
     :substrate-2-count (count substrate-2-phase-1-invariants)
     :war-machine-count (count war-machine-aif-invariants)}))
