(ns futon3c.evidence.store
  "Evidence store — append/query for the evidence landscape.

   Supports pluggable backends via the EvidenceBackend protocol (backend.clj).
   Default backend: in-memory atom (AtomBackend). Production: XTDB (XtdbBackend).

   R8 (authoritative transcript): the store is the authority for EvidenceEntry.
   R9 (structured events): entries are typed maps (EvidenceEntry), not free text.
   R4 (loud failure): operations return typed results; no silent failures."
  (:require [futon3c.evidence.backend :as backend]
            [futon3c.social.shapes :as shapes])
  (:import [java.time Instant]
           [java.util UUID]))

;; Store model: {:entries {id EvidenceEntry} :order [id ...]} in an atom.
;; This atom is the default backend; callers can pass an EvidenceBackend instead.
(defonce ^{:doc "Default evidence store atom."} !store
  (atom {:entries {} :order []}))

(defn reset-store!
  "Reset the default store (testing only)."
  []
  (reset! !store {:entries {} :order []}))

(defn- now-str []
  (str (Instant/now)))

(defn- social-error
  [code message & {:as context}]
  (cond-> {:error/component :E-store
           :error/code code
           :error/message message
           :error/at (now-str)}
    (seq context) (assoc :error/context context)))

(defn- resolve-backend
  "Resolve a store argument to an EvidenceBackend.
   - If it satisfies EvidenceBackend, return it directly.
   - If it's an atom, wrap in AtomBackend.
   - Otherwise, wrap the default !store atom."
  [maybe-store]
  (cond
    (satisfies? backend/EvidenceBackend maybe-store) maybe-store
    (instance? clojure.lang.IAtom maybe-store) (backend/->AtomBackend maybe-store)
    :else (backend/->AtomBackend !store)))

(defn- gen-id []
  (str "e-" (UUID/randomUUID)))

(defn- ensure-entry
  [entry]
  (if (shapes/valid? shapes/EvidenceEntry entry)
    entry
    (social-error :invalid-entry
                  "EvidenceEntry did not conform to shape"
                  :entry entry
                  :validation (or (:error (shapes/validate shapes/EvidenceEntry entry)) {}))))

;; =============================================================================
;; Public API — parameterised by store (atom or EvidenceBackend)
;; =============================================================================

(defn get-entry
  "Get an entry by id from the default store.
   Returns EvidenceEntry or nil."
  [evidence-id]
  (backend/-get (resolve-backend !store) evidence-id))

(defn get-entry*
  "Get an entry by id from a specific store.
   Returns EvidenceEntry or nil."
  [store evidence-id]
  (backend/-get (resolve-backend store) evidence-id))

(defn append*
  "Append to a specific store. Same semantics as append!.
   Accepts either a full EvidenceEntry or an unqualified args map."
  [store m]
  (let [b (resolve-backend store)
        validated (if (shapes/valid? shapes/EvidenceEntry m)
                    m
                    (let [{:keys [evidence-id subject type claim-type author body pattern-id session-id
                                  in-reply-to fork-of conjecture? ephemeral? tags]} m
                          entry {:evidence/id (or evidence-id (gen-id))
                                 :evidence/subject subject
                                 :evidence/type type
                                 :evidence/claim-type claim-type
                                 :evidence/author author
                                 :evidence/at (now-str)
                                 :evidence/body body
                                 :evidence/tags (vec (or tags []))}
                          entry (cond-> entry
                                  pattern-id (assoc :evidence/pattern-id pattern-id)
                                  session-id (assoc :evidence/session-id session-id)
                                  in-reply-to (assoc :evidence/in-reply-to in-reply-to)
                                  fork-of (assoc :evidence/fork-of fork-of)
                                  (some? conjecture?) (assoc :evidence/conjecture? (boolean conjecture?))
                                  (some? ephemeral?) (assoc :evidence/ephemeral? (boolean ephemeral?)))]
                      (ensure-entry entry)))]
    (if (shapes/valid? shapes/SocialError validated)
      validated
      (backend/-append b validated))))

(defn append!
  "Append an evidence entry to the default store.
   Accepts either:
   1) a full EvidenceEntry map (namespaced keys), or
   2) an append args map (unqualified keys per mission handoff)."
  [m]
  (append* !store m))

(defn query*
  "Query a specific store.
   Returns [EvidenceEntry], excluding ephemeral entries by default."
  [store evidence-query]
  (if-not (shapes/valid? shapes/EvidenceQuery evidence-query)
    []
    (backend/-query (resolve-backend store) evidence-query)))

(defn query
  "Query the default store. Returns [EvidenceEntry] (excludes ephemeral by default)."
  [evidence-query]
  (query* !store evidence-query))

(defn get-reply-chain*
  "Return the ordered ancestor chain for evidence-id, including the entry itself.
   Missing evidence-id returns []."
  [store evidence-id]
  (let [b (resolve-backend store)]
    (loop [eid evidence-id acc [] seen #{}]
      (if (or (nil? eid) (contains? seen eid))
        (vec (reverse acc))
        (if-let [e (backend/-get b eid)]
          (recur (:evidence/in-reply-to e) (conj acc e) (conj seen eid))
          (vec (reverse acc)))))))

(defn get-reply-chain
  "Return the ordered ancestor chain for evidence-id, including the entry itself.
   Missing evidence-id returns []."
  [evidence-id]
  (get-reply-chain* !store evidence-id))

(defn get-forks*
  "Return all entries forked from evidence-id."
  [store evidence-id]
  (backend/-forks-of (resolve-backend store) evidence-id))

(defn get-forks
  "Return all entries forked from evidence-id."
  [evidence-id]
  (get-forks* !store evidence-id))

(defn recent-activity
  "Front page: recent non-ephemeral activity, newest first."
  [{:keys [limit since]}]
  (let [q (cond-> {}
            since (assoc :query/since since)
            (some? limit) (assoc :query/limit limit)
            true (assoc :query/include-ephemeral? false))]
    (query q)))

(defn compact-ephemeral!*
  "Remove ephemeral entries older than threshold from a specific store.
   older-than must be ISO string or Instant."
  [store {:keys [older-than]}]
  (let [cutoff (try
                 (backend/parse-instant older-than)
                 (catch Exception _ ::bad))]
    (if (= ::bad cutoff)
      (social-error :invalid-input "older-than must be an ISO timestamp string or Instant" :older-than older-than)
      (let [b (resolve-backend store)
            all-entries (backend/-all b)
            to-drop (->> all-entries
                         (keep (fn [e]
                                 (when (and (true? (:evidence/ephemeral? e))
                                            (.isBefore (backend/entry-at e) cutoff))
                                   (:evidence/id e))))
                         set)]
        (if (empty? to-drop)
          {:compacted 0}
          (backend/-delete! b to-drop))))))

(defn compact-ephemeral!
  "Remove ephemeral entries older than threshold from the default store.
   older-than must be ISO string or Instant."
  [{:keys [older-than]}]
  (compact-ephemeral!* !store {:older-than older-than}))
