(ns futon3c.arxana.correspond
  "D4 correspondence model (INSTANTIATE increment 5, per D7).

  Manifestation registry with deterministic authority/family/role identity,
  immutable directional revision-pinned correspondence assertions, distinct
  attributable correspondence-assessment records, filtered queries, and
  caller-ordered absence/ambiguity helpers.

  Persistence is behind the CorrespondenceRepository protocol; this namespace
  ships an in-memory implementation. Service/store wiring is a later
  integration increment."
  (:refer-clojure :exclude [methods reversible?])
  (:require [futon3c.arxana.digest :as digest]))

;;; ---------------------------------------------------------------------------
;;; Standing and vocabularies

(def verification-rank
  "Standing order for verification floors. Ambiguity is a candidate state,
  not a strength; it ranks below partial because no single endpoint is
  established."
  {:stale 0 :unverified 1 :ambiguous 2 :partial 3 :verified 4})

(def verifications (set (keys verification-rank)))

(def directions #{:source-to-target :target-to-source})

(def cardinalities #{:one-to-one :one-to-many :many-to-one :many-to-many})

(def locator-kinds
  #{:codepoint-range :codepoint-point :quote-context :dom-id :dom-selector
    :source-map-coordinate})

(def methods
  #{:exact-range :quote :source-map :dom-id :transformation})

(def default-reversible-methods
  "Methods whose evidence supports a reverse query projection by default.
  :transformation is directional unless the assertion explicitly declares
  reversibility in :method-options."
  #{:exact-range :quote :source-map :dom-id})

(def manifestation-roles #{:authored :generated})

(def assessment-verdicts #{:accepted :rejected :absent})

;;; ---------------------------------------------------------------------------
;;; Repository protocol

(declare mint-manifestation-id canonical-tuple definition-fields
         assertion-id assessment-id)

(defprotocol CorrespondenceRepository
  "Persistence for the correspondence model. All records immutable."
  (put-manifestation! [repo def]
    "Register a manifestation. Returns the definition with :manifestation/id.
    Re-registering a byte-equivalent definition is idempotent; a conflicting
    definition for the same canonical tuple throws ex-info
    :arxana/manifestation-conflict.")
  (get-manifestation [repo id])
  (find-manifestation [repo authority family role-key])
  (put-alias! [repo from-id to-id reason]
    "Record an explicit rename alias. Never recycles an ID onto a different
    role; both IDs must be registered.")
  (aliases [repo id])
  (put-assertion! [repo assertion]
    "Store an immutable correspondence assertion. A second assertion whose
    semantic identity digest already exists throws ex-info
    :arxana/correspondence-duplicate.")
  (get-assertion [repo id])
  (all-assertions [repo])
  (put-assessment! [repo assessment])
  (assessments-for [repo assertion-id])
  (all-assessments [repo])
  (put-usefulness! [repo record]
    "Store a namespaced, opaque consumer usefulness record. Core arxana keys
    are rejected; the record can never mutate correspondence state.")
  (usefulness-for [repo subject-id]))

(defrecord InMemoryRepository [state]
  CorrespondenceRepository
  (put-manifestation! [_ def]
    (let [id (mint-manifestation-id def)
          tuple (canonical-tuple def)
          existing (get-in @state [:manifestations id])]
      (if existing
        (if (= (select-keys existing definition-fields)
               (select-keys (assoc def :manifestation/id id) definition-fields))
          existing
          (throw (ex-info "conflicting manifestation definition for canonical tuple"
                          {:type :arxana/manifestation-conflict
                           :tuple tuple :existing-id (:manifestation/id existing)})))
        (let [def' (assoc def :manifestation/id id :type :arxana/manifestation)]
          (swap! state assoc-in [:manifestations id] def')
          (swap! state assoc-in [:manifestation-tuples tuple] id)
          def'))))
  (get-manifestation [_ id] (get-in @state [:manifestations id]))
  (find-manifestation [_ authority family role-key]
    (when-let [id (get-in @state [:manifestation-tuples [authority family role-key]])]
      (get-in @state [:manifestations id])))
  (put-alias! [_ from-id to-id reason]
    (when-not (get-in @state [:manifestations from-id])
      (throw (ex-info "alias source is not a registered manifestation"
                      {:type :arxana/manifestation-unknown :id from-id})))
    (when-not (get-in @state [:manifestations to-id])
      (throw (ex-info "alias target is not a registered manifestation"
                      {:type :arxana/manifestation-unknown :id to-id})))
    (let [alias {:type :arxana/manifestation-alias
                 :alias/from from-id :alias/to to-id :alias/reason reason}]
      (swap! state update-in [:aliases from-id] (fnil conj []) alias)
      alias))
  (aliases [_ id] (get-in @state [:aliases id] []))
  (put-assertion! [_ assertion]
    (let [id (assertion-id assertion)]
      (when (get-in @state [:assertions id])
        (throw (ex-info "correspondence assertion with this semantic identity already exists"
                        {:type :arxana/correspondence-duplicate :id id})))
      (let [a (assoc assertion :id id :type :arxana/correspondence-assertion)]
        (swap! state assoc-in [:assertions id] a)
        a)))
  (get-assertion [_ id] (get-in @state [:assertions id]))
  (all-assertions [_] (vals (:assertions @state)))
  (put-assessment! [_ assessment]
    (let [id (assessment-id assessment)
          a (assoc assessment :assessment/id id :type :arxana/correspondence-assessment)]
      (swap! state update-in [:assessments] (fnil conj []) a)
      a))
  (assessments-for [_ assertion-id]
    (filterv #(= assertion-id (get-in % [:subject :assertion/id]))
             (:assessments @state)))
  (all-assessments [_] (vec (:assessments @state)))
  (put-usefulness! [_ record]
    (let [bad (filter #(= "arxana" (namespace %)) (keys record))]
      (when (seq bad)
        (throw (ex-info "usefulness records may not carry core arxana keys"
                        {:type :arxana/usefulness-core-keys :keys (vec bad)})))
      (when-not (every? #(namespace %) (keys record))
        (throw (ex-info "usefulness record keys must be namespaced"
                        {:type :arxana/usefulness-unnamespaced})))
      (swap! state update-in [:usefulness] (fnil conj []) record)
      record))
  (usefulness-for [_ subject-id]
    (filterv #(some #{subject-id} (vals %)) (:usefulness @state))))

(defn in-memory-repository []
  (->InMemoryRepository (atom {})))

;;; ---------------------------------------------------------------------------
;;; Manifestation identity

(defn canonical-tuple
  "The (authority, family, role-key) identity tuple of a manifestation def."
  [def]
  [(:manifestation/authority def)
   (:artifact/family-id def)
   (:manifestation/role-key def)])

(defn mint-manifestation-id
  "Deterministic ID from the canonical tuple (D4/O-D4c)."
  [def]
  (let [[authority family role-key] (canonical-tuple def)]
    (when-not (and (string? authority) (string? family) (string? role-key))
      (throw (ex-info "manifestation definition requires string authority, family-id and role-key"
                      {:type :arxana/manifestation-invalid :def def})))
    (str "arxana:manifestation:sha256:"
         (digest/canonical-digest {:authority authority
                                   :family family
                                   :role-key role-key}))))

(def definition-fields
  "Fields that participate in byte-equivalence for re-registration."
  [:manifestation/id :manifestation/authority :artifact/family-id
   :manifestation/role-key :manifestation/label :manifestation/media-role
   :manifestation/role :manifestation/subject :manifestation/origin])

(defn validate-manifestation-def!
  [def]
  (when-not (manifestation-roles (:manifestation/role def))
    (throw (ex-info "manifestation role must be :authored or :generated"
                    {:type :arxana/manifestation-invalid :def def})))
  (when (= :generated (:manifestation/role def))
    (let [origin (:manifestation/origin def)]
      (when-not (and (map? origin)
                     (string? (:origin/tool origin))
                     (string? (:origin/version origin))
                     (sequential? (:origin/source-revisions origin)))
        (throw (ex-info "generated manifestations must record tool/version/source revisions"
                        {:type :arxana/manifestation-origin-missing :def def})))))
  def)

(defn register-manifestation!
  [repo def]
  (put-manifestation! repo (validate-manifestation-def! def)))

;;; ---------------------------------------------------------------------------
;;; Assertions

(defn- validate-endpoint! [repo ep which]
  (when-not (map? ep)
    (throw (ex-info "assertion endpoint must be a map"
                    {:type :arxana/assertion-invalid :endpoint which})))
  (let [{mid :manifestation/id rev :artifact/revision-id loc :locator} ep]
    (when-not (and (string? mid) (string? rev))
      (throw (ex-info "endpoint requires manifestation/id and artifact/revision-id"
                      {:type :arxana/assertion-invalid :endpoint which})))
    (when-not (get-manifestation repo mid)
      (throw (ex-info "endpoint names an unregistered manifestation"
                      {:type :arxana/manifestation-unknown :id mid})))
    (when-not (locator-kinds (:kind loc))
      (throw (ex-info "unknown locator kind"
                      {:type :arxana/locator-invalid :locator loc})))
    (when (= :codepoint-point (:kind loc))
      (when (some loc [:end])
        (throw (ex-info "point locators must not carry an end (no fabricated ranges)"
                        {:type :arxana/locator-invalid :locator loc}))))
    (when (= :codepoint-range (:kind loc))
      (when-not (and (int? (:start loc)) (int? (:end loc))
                     (<= 0 (:start loc) (:end loc)))
        (throw (ex-info "codepoint-range locators require integer :start/:end with 0 <= start <= end (half-open, unicode codepoints)"
                        {:type :arxana/locator-invalid :locator loc}))))))

(defn validate-assertion!
  [repo assertion]
  (validate-endpoint! repo (:source assertion) :source)
  (validate-endpoint! repo (:target assertion) :target)
  (when-not (directions (:direction assertion))
    (throw (ex-info "unknown direction" {:type :arxana/assertion-invalid
                                         :direction (:direction assertion)})))
  (when-not (cardinalities (:cardinality assertion))
    (throw (ex-info "unknown cardinality" {:type :arxana/assertion-invalid
                                           :cardinality (:cardinality assertion)})))
  (when-not (and (sequential? (:method assertion))
                 (seq (:method assertion))
                 (every? methods (:method assertion)))
    (throw (ex-info "method must be a non-empty sequence of known methods"
                    {:type :arxana/assertion-invalid :method (:method assertion)})))
  (when-not (verifications (:verification assertion))
    (throw (ex-info "unknown verification standing"
                    {:type :arxana/assertion-invalid
                     :verification (:verification assertion)})))
  assertion)

(defn assertion-id
  "Semantic identity digest: endpoints (with revisions), direction, method,
  and semantic evidence. Timestamps/transport metadata are excluded (D4)."
  [assertion]
  (str "arxana:correspondence:sha256:"
       (digest/canonical-digest
        {:source (:source assertion)
         :target (:target assertion)
         :direction (:direction assertion)
         :cardinality (:cardinality assertion)
         :method (vec (:method assertion))
         :evidence (vec (:evidence assertion))})))

(defn assert-correspondence!
  [repo assertion]
  (put-assertion! repo (validate-assertion! repo assertion)))

(defn reversible?
  "True when every method of the assertion supports a reverse projection,
  either via the reversible-methods set or an explicit declaration."
  ([assertion] (reversible? default-reversible-methods assertion))
  ([reversible-methods assertion]
   (or (get-in assertion [:method-options :reversible])
       (every? reversible-methods (:method assertion)))))

(defn reverse-assertion
  "Query projection swapping direction. Only lawful when reversible?; throws
  otherwise (D4: reversing is a projection, not a new assertion)."
  ([assertion] (reverse-assertion default-reversible-methods assertion))
  ([reversible-methods assertion]
   (when-not (reversible? reversible-methods assertion)
     (throw (ex-info "assertion method does not declare reversibility"
                     {:type :arxana/not-reversible :id (:id assertion)
                      :method (:method assertion)})))
   (-> assertion
       (assoc :source (:target assertion)
              :target (:source assertion)
              :direction (if (= :source-to-target (:direction assertion))
                           :target-to-source :source-to-target)
              :projection/of (:id assertion)))))

;;; ---------------------------------------------------------------------------
;;; Assessments (distinct from D2 resolution acceptance and D6 admission)

(defn assessment-id
  [assessment]
  (str "arxana:correspondence-assessment:sha256:"
       (digest/canonical-digest
        {:subject (:subject assessment)
         :verdict (:verdict assessment)
         :actor/id (:actor/id assessment)
         :policy/id (:policy/id assessment)
         :policy/revision (:policy/revision assessment)
         :reason (:reason assessment)
         :evidence/ids (vec (:evidence/ids assessment))
         :assessed-at (:assessed-at assessment)})))

(defn validate-assessment!
  [assessment]
  (when-not (assessment-verdicts (:verdict assessment))
    (throw (ex-info "unknown assessment verdict"
                    {:type :arxana/assessment-invalid
                     :verdict (:verdict assessment)})))
  (if (= :absent (:verdict assessment))
    (let [s (:subject assessment)]
      (when-not (and (:expected/role s) (:source/entity s)
                     (:artifact/revision-id s) (:search/bounded-by s))
        (throw (ex-info "explicit-absence assessments must name expected role, source entity/revision and a bounded search envelope"
                        {:type :arxana/assessment-invalid :subject s}))))
    (when-not (get-in assessment [:subject :assertion/id])
      (throw (ex-info "assessment subject must name an assertion/id (or be an explicit-absence record)"
                      {:type :arxana/assessment-invalid :subject (:subject assessment)}))))
  (when-not (and (:actor/id assessment) (:policy/id assessment))
    (throw (ex-info "assessments are attributable: actor and policy required"
                    {:type :arxana/assessment-invalid})))
  assessment)

(defn assess!
  "Record an immutable, attributable correspondence assessment. Conflicting
  assessments coexist; nothing here mutates the assessed assertion."
  [repo assessment]
  (put-assessment! repo (validate-assessment! assessment)))

;;; ---------------------------------------------------------------------------
;;; Queries

(defn- endpoint-matches? [ep mid rev]
  (and (or (nil? mid) (= mid (:manifestation/id ep)))
       (or (nil? rev) (= rev (:artifact/revision-id ep)))))

(defn- assertion-matches?
  [repo a {:keys [manifestation/id artifact/revision-id direction methods
                  verification verification-floor role assessment-verdict
                  current-only?]
           :or {current-only? false}}]
  (let [fwd (and (= :source-to-target (:direction a))
                 (endpoint-matches? (:source a) id revision-id))
        bwd (and (= :target-to-source (:direction a))
                 (endpoint-matches? (:target a) id revision-id))
        as-source (and (endpoint-matches? (:source a) id revision-id)
                       (or (nil? direction) (= direction (:direction a))))
        as-target (and (endpoint-matches? (:target a) id revision-id)
                       (or (nil? direction)
                           (= direction (if (= :source-to-target (:direction a))
                                          :target-to-source :source-to-target))))]
    (and (or (and (nil? id) (nil? revision-id))
             (and (nil? direction) (or fwd bwd as-source as-target))
             (and (= direction :source-to-target) as-source)
             (and (= direction :target-to-source) as-target)
             (and (nil? direction) (or as-source as-target)))
         (or (nil? methods) (every? methods (:method a)))
         (or (nil? verification) (= verification (:verification a)))
         (or (nil? verification-floor)
             (>= (verification-rank (:verification a))
                 (verification-rank verification-floor)))
         (or (nil? role)
             (= role (:manifestation/role
                      (get-manifestation repo (:manifestation/id (:target a)))))
             (= role (:manifestation/role
                      (get-manifestation repo (:manifestation/id (:source a))))))
         (or (not current-only?) (not= :stale (:verification a)))
         (or (nil? assessment-verdict)
             (some #(= assessment-verdict (:verdict %))
                   (assessments-for repo (:id a)))))))

(defn query
  "correspondence/query per D4. Filters: :manifestation/id,
  :artifact/revision-id, :direction, :methods (set; all must be present),
  :verification, :verification-floor, :role (endpoint manifestation role),
  :assessment-verdict, :current-only? (excludes :stale). Returns ALL matching
  assertions; never selects among ambiguous candidates."
  [repo filters]
  (filterv #(assertion-matches? repo % filters) (all-assertions repo)))

(defn candidates
  "All assertions standing between two manifestations (either direction),
  optionally restricted to current (non-stale) standing. Ambiguity is
  preserved: callers receive every candidate."
  [repo source-mid target-mid & {:keys [current-only?] :or {current-only? true}}]
  (filterv (fn [a]
             (and (or (and (= source-mid (:manifestation/id (:source a)))
                           (= target-mid (:manifestation/id (:target a))))
                      (and (= target-mid (:manifestation/id (:source a)))
                           (= source-mid (:manifestation/id (:target a)))))
                  (or (not current-only?) (not= :stale (:verification a)))))
           (all-assertions repo)))

;;; ---------------------------------------------------------------------------
;;; Manifestations of a subject; caller-ordered absence/ambiguity

(defn all-manifestations
  "Every registered manifestation whose :manifestation/subject equals
  subject-id (e.g. a proof/move). Multiple manifestations of one subject are
  returned without identity merger; authored and generated never share an ID."
  [repo subject-id]
  (filterv #(= subject-id (:manifestation/subject %))
           (vals (:manifestations @(:state repo)))))

(defn- require-order! [order]
  (when-not (sequential? order)
    (throw (ex-info "absence/ambiguity helpers require a caller-supplied order; Arxana does not invent domain sequencing"
                    {:type :arxana/order-required}))))

(defn next-absent
  "First subject in the caller-supplied order that has no current
  correspondence assertion touching any of its manifestations with the given
  target :role (default any role). Absence of a stored assertion is reported
  as a fact about the store, never as evidence of absence; use an explicit
  :absent assessment for that judgment."
  [repo order & {:keys [role current-only?] :or {current-only? true}}]
  (require-order! order)
  (first
   (filter (fn [subject]
             (let [ms (all-manifestations repo subject)
                   ids (set (map :manifestation/id ms))]
               (not-any?
                (fn [a]
                  (and (or (ids (:manifestation/id (:source a)))
                           (ids (:manifestation/id (:target a))))
                       (or (not current-only?) (not= :stale (:verification a)))
                       (or (nil? role)
                           (= role (:manifestation/role
                                    (get-manifestation repo (:manifestation/id (:target a))))))))
                (all-assertions repo))))
           order)))

(defn next-ambiguous
  "First subject in the caller-supplied order with at least one current
  :ambiguous assertion touching its manifestations."
  [repo order & {:keys [current-only?] :or {current-only? true}}]
  (require-order! order)
  (first
   (filter (fn [subject]
             (let [ids (set (map :manifestation/id (all-manifestations repo subject)))]
               (some (fn [a]
                       (and (= :ambiguous (:verification a))
                            (or (ids (:manifestation/id (:source a)))
                                (ids (:manifestation/id (:target a))))
                            (or (not current-only?) (not= :stale (:verification a)))))
                     (all-assertions repo))))
           order)))
