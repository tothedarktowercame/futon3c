(ns futon3c.evidence.boundary
  "Single routing authority for evidence appends — the boundary discipline.

   All evidence emits in futon3c go through this namespace's append! function.
   The boundary:

     (a) Coerces commonly-misshaped fields (string→keyword for :tags,
         :subject/:ref/type, :type, :claim-type, :pattern-id) before
         shape-validation. Unknown coercion shapes throw — loud failure
         is preferred over silent best-effort.

     (b) Validates against shapes/EvidenceEntry and surfaces violations
         as structured results. The store's existing ensure-entry catches
         remaining shape mismatches; the boundary turns those into
         I-single-boundary VIOLATIONs visible in stderr and queryable as
         evidence entries themselves.

     (c) Verifies durable persistence via invariant/verify-persisted —
         the I-evidence-per-turn check from c6f2c32. A successful boundary
         append! returns only after the store has confirmed the entry
         is readable back through the same backend.

     (d) Returns a delivery-receipt-shaped result:
           {:ok true  :entry <full-EvidenceEntry> :evidence/id <id>}
           {:ok false :error/code <kw> :error/message <str>
                      :invariant/violation <map>}

   Patterns this boundary applies (see PSR
   `holes/labs/M-invariant-queue-unstuck/psr/2026-04-29__derive__single-routing-authority.md`):
     - agency/single-routing-authority — one boundary owns evidence writes.
     - agency/loud-failure — every failure surfaces; no silent catch.
     - agency/delivery-receipt — return value is a structured receipt.
     - storage/durability-first — verify-persisted before declaring success.

   Invariant statements this boundary participates in:
     - I-single-boundary: every `store/append*` call originates here.
       Verifiable via static grep:
         `grep -rn 'estore/append\\*' src dev | grep -v 'evidence/boundary'`
       returns zero hits.
     - I-evidence-per-turn (existing, c6f2c32): each substantive turn's
       evidence is durably persisted, verified via read-back through
       the same backend.

   Mission: M-invariant-queue-unstuck (futon3c/holes/missions/)."
  (:require [futon3c.evidence.backend :as backend]
            [futon3c.evidence.invariant :as invariant]
            [futon3c.evidence.store :as store]
            [futon3c.marks :as marks]
            [futon3c.social.shapes :as shapes]
            [clojure.string :as str]))

(def I-single-boundary
  "Canonical statement of the single-boundary invariant. Grep-verifiable."
  (str "I-single-boundary: every futon3c evidence append originates from "
       "futon3c.evidence.boundary/append!, which is the only path that "
       "calls futon3c.evidence.store/append*. New direct callers of "
       "store/append* are I-single-boundary violations."))

(defn- social-error-taxonomy
  [error-code]
  (case error-code
    :duplicate-id {:kind :duplicate-id
                   :label "duplicate id"
                   :invariant I-single-boundary
                   :quiet? true
                   :idempotent? true}
    :store-timeout {:kind :timeout
                    :label "persistence timeout"
                    :invariant invariant/I-evidence-per-turn}
    :store-unreachable {:kind :unreachable
                        :label "persistence transport unreachable"
                        :invariant invariant/I-evidence-per-turn}
    :store-serialization {:kind :serialization
                          :label "persistence serialization rejected"
                          :invariant invariant/I-evidence-per-turn}
    :store-rejected {:kind :store-rejected
                     :label "persistence rejected"
                     :invariant invariant/I-evidence-per-turn}
    {:kind :shape
     :label "shape rejected"
     :invariant invariant/I-evidence-per-turn}))

;; ---------------------------------------------------------------------------
;; Coercion — translate commonly-misshaped fields to their canonical types
;; before shape validation. Loud failure for unknown shapes.
;; ---------------------------------------------------------------------------

(defn- coerce-keyword
  "Convert string or keyword to a keyword. Throws on blank, nil, or other types.
   Used for fields whose shape is :keyword or [:enum ...]."
  [field-name x]
  (cond
    (keyword? x) x
    (and (string? x) (not (str/blank? x))) (keyword (str/replace x #"^:" ""))
    :else (throw (ex-info (str "Cannot coerce " (name field-name) " to keyword")
                          {:field field-name
                           :value x
                           :value-type (some-> x class .getName)
                           :invariant I-single-boundary}))))

(defn- coerce-tag
  "Coerce one tag to a keyword."
  [t]
  (coerce-keyword :evidence/tags t))

(defn- coerce-tags
  "Coerce a tags collection (or nil) to [:vector :keyword]."
  [ts]
  (mapv coerce-tag (or ts [])))

(defn- coerce-subject
  "Coerce {:ref/type <kw-or-str> :ref/id <str>} so :ref/type is a keyword."
  [subject]
  (cond
    (nil? subject) subject
    (and (map? subject) (contains? subject :ref/type))
    (update subject :ref/type (partial coerce-keyword :evidence/subject))
    :else subject))

(defn- coerce-entry-fields
  "Apply coercions to a partially-namespaced entry. Idempotent: already-correct
   fields pass through. Unknown shapes throw via coerce-keyword."
  [entry]
  (cond-> entry
    (contains? entry :evidence/tags)
    (update :evidence/tags coerce-tags)

    (contains? entry :evidence/subject)
    (update :evidence/subject coerce-subject)

    (contains? entry :evidence/type)
    (update :evidence/type (partial coerce-keyword :evidence/type))

    (contains? entry :evidence/claim-type)
    (update :evidence/claim-type (partial coerce-keyword :evidence/claim-type))

    (contains? entry :evidence/pattern-id)
    (update :evidence/pattern-id (partial coerce-keyword :evidence/pattern-id))))

(defn- coerce-args-fields
  "Apply coercions to an unqualified args map (the unqualified-keys input
   form that store/append* accepts and re-namespaces internally)."
  [m]
  (cond-> m
    (contains? m :tags)
    (update :tags coerce-tags)

    (contains? m :subject)
    (update :subject coerce-subject)

    (contains? m :type)
    (update :type (partial coerce-keyword :type))

    (contains? m :claim-type)
    (update :claim-type (partial coerce-keyword :claim-type))

    (contains? m :pattern-id)
    (update :pattern-id (partial coerce-keyword :pattern-id))))

(defn- un-namespace-evidence-keys
  "Convert :evidence/X keys to bare :X. Leaves non-namespaced keys alone.
   Used internally to normalize partially-namespaced input to the args-map
   form that store/append* understands and auto-completes (id + at)."
  [m]
  (reduce-kv
   (fn [acc k v]
     (if (and (keyword? k) (= "evidence" (namespace k)))
       (assoc acc (keyword (name k)) v)
       (assoc acc k v)))
   {}
   m))

(defn- coerce-input
  "Coerce input (full namespaced EvidenceEntry or unqualified args map).

   Three cases:
     (a) Fully-namespaced input that is already a valid EvidenceEntry
         (has all required fields including :evidence/id and :evidence/at) →
         coerce in place and pass through; preserves the caller's
         :evidence/id (critical for in-reply-to chains and any other case
         where the caller pre-generates the id).
     (b) Partially-namespaced input (has :evidence/X keys but is missing
         :evidence/id or :evidence/at) → coerce, then un-namespace so
         store/append* auto-completes the missing fields via gen-id +
         now-str.
     (c) Unqualified args-map input → coerce in place; store/append*
         already handles re-namespacing and auto-completion."
  [m]
  (let [evidence-keyed? (some #(and (keyword? %) (= "evidence" (namespace %)))
                              (keys m))]
    (cond
      ;; (a) Pre-coerce, then if already a valid EvidenceEntry, pass through.
      ;; (b) If still namespaced after coercion but invalid (missing id/at),
      ;;     un-namespace so store/append*'s args-map branch can auto-complete.
      evidence-keyed?
      (let [coerced (coerce-entry-fields m)]
        (if (shapes/valid? shapes/EvidenceEntry coerced)
          coerced
          (un-namespace-evidence-keys coerced)))

      ;; (c) Already unqualified.
      :else
      (coerce-args-fields m))))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn- resolve-backend
  "Resolve a maybe-store argument to a concrete EvidenceBackend, including
   the same fallback semantics that store/append* uses internally. Returns
   nil when no fallback is possible (i.e., default store is also unavailable).

   Mirrors `futon3c.evidence.store/resolve-backend` (which is private)
   so the boundary can resolve once and pass the same backend to both
   append* and verify-persisted — preventing a class of false-positive
   I-evidence-per-turn violations where the store is nil and the two
   calls disagree about which backend they're talking to."
  [maybe-store]
  (cond
    (nil? maybe-store)
    (when-let [default (some-> 'futon3c.evidence.store/!store
                               find-var var-get)]
      (backend/->AtomBackend default))

    (satisfies? backend/EvidenceBackend maybe-store)
    maybe-store

    (instance? clojure.lang.IAtom maybe-store)
    (backend/->AtomBackend maybe-store)

    :else nil))

(defn- evidence-field
  [entry namespaced-key unqualified-key]
  (or (get entry namespaced-key)
      (get entry unqualified-key)))

(defn- event-kind
  [entry]
  (let [body (evidence-field entry :evidence/body :body)]
    (when (map? body)
      (or (get body :event)
          (get body "event")))))

(defn- diagnostic-suffix
  "Render producer identity and wire diagnostics without logging evidence body."
  [entry result]
  (let [context (:error/context result)
        fields [["trace-id" (:trace-id context)]
                ["evidence-id" (or (:evidence/id entry)
                                   (:evidence-id context))]
                ["author" (evidence-field entry :evidence/author :author)]
                ["session" (evidence-field entry
                                           :evidence/session-id
                                           :session-id)]
                ["event" (event-kind entry)]
                ["status" (:status context)]
                ["invalid-edn" (:invalid-edn context)]]]
    (str/join " "
              (keep (fn [[label value]]
                      (when (some? value)
                        (str label "=" (pr-str value))))
                    fields))))

(defn append!
  "Append an evidence entry through the single boundary.

   Accepts the same input forms as store/append*: a full EvidenceEntry
   (namespaced keys) or an unqualified args map. Coerces commonly-misshaped
   fields before validation, calls store/append*, then verifies the entry
   is readable back via invariant/verify-persisted (I-evidence-per-turn).

   Returns a delivery-receipt-shaped result:

     Success:
       {:ok true
        :entry <EvidenceEntry>
        :evidence/id <string>}

     Coercion failure (input shape unknown):
       {:ok false
        :error/code :uncoerced-input
        :error/message <string>
        :invariant/violation {:invariant I-single-boundary :reason <string>}}

     Shape failure (passed coercion but failed shape validation):
       {:ok false
        :error/code :invalid-entry
        :error/message <string>
        :error/at <iso-string>
        :invariant/violation {:invariant invariant/I-evidence-per-turn
                              :kind :shape :reason <string>}}

     Persistence failure (appended but not readable back):
       {:ok false
        :error/code :not-persisted
        :error/message <string>
        :evidence/id <string-of-attempted-id>
        :invariant/violation <verify-persisted-result>}

     Exception during append:
       {:ok false
        :error/code :exception
        :error/message <string>
        :invariant/violation {:kind :exception :invariant ... :reason ...}}

   In all failure modes a structured VIOLATION line is printed to stderr
   for operator visibility (matching the convention from
   `dev/futon3c/dev/invoke.clj` and `src/futon3c/transport/http.clj`)."
  [evidence-store entry-or-args]
  (try
    (let [coerced (marks/maybe-decorate-turn (coerce-input entry-or-args))
          ;; Resolve the backend ONCE so append* and verify-persisted
          ;; agree on which backend they're addressing. Without this, a
          ;; nil evidence-store causes append* to silently fall back to
          ;; !store while verify-persisted reports "Store is not
          ;; resolvable" — a false-positive I-evidence-per-turn violation.
          resolved (resolve-backend evidence-store)
          result (store/append* (or resolved evidence-store) coerced)]
      (cond
        ;; Shape failure surfaces as a SocialError map (not {:ok false ...}).
        ;; Detect via the SocialError shape; convert to boundary's receipt shape.
        (shapes/valid? shapes/SocialError result)
        (let [msg (:error/message result)
              taxonomy (social-error-taxonomy (:error/code result))
              diagnostic (diagnostic-suffix coerced result)]
          (when-not (:quiet? taxonomy)
            (binding [*out* *err*]
              (println (str "[boundary] "
                            (if (= I-single-boundary (:invariant taxonomy))
                              "I-single-boundary"
                              "I-evidence-per-turn")
                            " VIOLATION: "
                            (:label taxonomy) " — " msg
                            (when (seq diagnostic)
                              (str " " diagnostic))))))
          {:ok false
           :error/code (:error/code result)
           :error/message msg
           :error/at (:error/at result)
           :trace-id (get-in result [:error/context :trace-id])
           :evidence/id (or (:evidence/id coerced)
                            (get-in result [:error/context :evidence-id]))
           :invariant/violation
           {:invariant (:invariant taxonomy)
            :kind (:kind taxonomy)
            :reason msg
            :idempotent? (boolean (:idempotent? taxonomy))
            :rejected-entry coerced}})

        ;; Backend returned a typed-result with :ok semantics.
        (and (map? result) (contains? result :ok) (not (:ok result)))
        (let [msg (or (:error/message result) "append failed")]
          (binding [*out* *err*]
            (println (str "[boundary] I-single-boundary VIOLATION: "
                          "append returned not-ok — " msg)))
          {:ok false
           :error/code (or (:error/code result) :append-failed)
           :error/message msg
           :invariant/violation
           {:invariant invariant/I-evidence-per-turn
            :kind :append-not-ok
            :reason msg}})

        :else
        ;; The store/append* call returned the entry-bearing map. Extract
        ;; the id and verify durable persistence via the I-evidence-per-turn
        ;; verify-persisted check.
        (let [entry (or (:entry result)
                        (when (and (map? result) (contains? result :evidence/id)) result))
              eid (or (:evidence/id entry)
                      (get-in result [:entry :evidence/id]))
              verify (when eid
                       (invariant/verify-persisted (or resolved evidence-store)
                                                   eid))]
          (cond
            (nil? eid)
            (do
              (binding [*out* *err*]
                (println (str "[boundary] I-single-boundary VIOLATION: "
                              "store/append* returned no evidence/id")))
              {:ok false
               :error/code :no-evidence-id
               :error/message "store/append* returned no :evidence/id"
               :invariant/violation {:invariant I-single-boundary
                                     :kind :missing-id
                                     :raw-result result}})

            (not (:ok verify))
            (do
              (binding [*out* *err*]
                (println (str "[boundary] I-single-boundary VIOLATION: "
                              "entry " eid " appended but not readable back from "
                              (name (:kind verify)) " — " (:reason verify))))
              {:ok false
               :error/code :not-persisted
               :error/message (str "Entry " eid " not readable back: "
                                   (:reason verify))
               :evidence/id eid
               :invariant/violation verify})

            :else
            {:ok true
             :entry entry
             :evidence/id eid
             :trace-id (:trace-id result)}))))
    (catch clojure.lang.ExceptionInfo e
      (let [msg (.getMessage e)
            data (ex-data e)]
        (binding [*out* *err*]
          (println (str "[boundary] I-single-boundary VIOLATION: "
                        "coercion or append threw "
                        (.getName (class e)) " — " msg)))
        {:ok false
         :error/code :exception
         :error/message msg
         :invariant/violation
         (merge {:invariant I-single-boundary
                 :kind :exception
                 :reason msg}
                (when (map? data) data))}))
    (catch Exception e
      (binding [*out* *err*]
        (println (str "[boundary] I-single-boundary VIOLATION: "
                      "append threw " (.getName (class e)) " — " (.getMessage e))))
      {:ok false
       :error/code :exception
       :error/message (.getMessage e)
       :invariant/violation
       {:invariant I-single-boundary
        :kind :exception
        :reason (.getMessage e)}})))

(defn append-default!
  "Convenience wrapper that targets the default store. Equivalent to
   (append! futon3c.evidence.store/!store entry-or-args)."
  [entry-or-args]
  (append! @(requiring-resolve 'futon3c.evidence.store/!store) entry-or-args))
