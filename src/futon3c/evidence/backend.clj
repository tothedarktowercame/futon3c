(ns futon3c.evidence.backend
  "Backend protocol for evidence persistence.

   Abstracts over in-memory (atom) and durable (XTDB) storage.
   The protocol captures the 7 operations that store.clj needs:
   append, get, exists?, query, forks-of, delete, and all-entries.

   AtomBackend wraps a Clojure atom with {:entries {} :order []},
   providing the same CAS-based semantics as the original store.clj."
  (:require [futon3c.social.shapes :as shapes])
  (:import [java.time Instant]
           [java.time.format DateTimeParseException]))

;; =============================================================================
;; Protocol
;; =============================================================================

(defprotocol EvidenceBackend
  "Pluggable persistence layer for the evidence store."
  (-append [b entry]
    "Store a validated EvidenceEntry. Returns {:ok true :entry <entry>} or SocialError.
     Must enforce: duplicate-id, reply-not-found, fork-not-found.")
  (-get [b id]
    "Return EvidenceEntry by id, or nil if not found.")
  (-exists? [b id]
    "Return true if evidence-id exists in the store.")
  (-query [b params]
    "Return [EvidenceEntry...] matching the query params. Newest-first.
     Excludes ephemeral by default.")
  (-forks-of [b id]
    "Return [EvidenceEntry...] where :evidence/fork-of = id, sorted by time.")
  (-delete! [b ids]
    "Remove entries by id set. Returns {:compacted n}.")
  (-all [b]
    "Return all entries as a seq."))

;; =============================================================================
;; Helpers (shared by backends)
;; =============================================================================

(defn- social-error
  [code message & {:as context}]
  (cond-> {:error/component :E-store
           :error/code code
           :error/message message
           :error/at (str (Instant/now))}
    (seq context) (assoc :error/context context)))

(defn parse-instant
  "Parse a timestamp to java.time.Instant."
  [t]
  (cond
    (instance? Instant t) t
    (string? t) (Instant/parse t)
    :else (throw (ex-info "bad-timestamp" {:t t}))))

(defn entry-at
  "Extract the Instant from an evidence entry, with fallback to epoch."
  [entry]
  (try
    (parse-instant (:evidence/at entry))
    (catch Exception _
      (Instant/ofEpochMilli 0))))

(defn filter-and-sort-entries
  "Apply standard query filtering and sorting to a seq of entries.
   Shared logic used by both AtomBackend and XtdbBackend."
  [entries {:query/keys [subject type claim-type author since limit include-ephemeral? tags]}]
  (let [include-ephemeral? (true? include-ephemeral?)
        since-inst (when since
                     (try (parse-instant since)
                          (catch DateTimeParseException _ nil)
                          (catch Exception _ nil)))
        entries (if include-ephemeral?
                  entries
                  (remove #(true? (:evidence/ephemeral? %)) entries))
        entries (if subject (filter #(= subject (:evidence/subject %)) entries) entries)
        entries (if type (filter #(= type (:evidence/type %)) entries) entries)
        entries (if claim-type (filter #(= claim-type (:evidence/claim-type %)) entries) entries)
        entries (if author (filter #(= author (:evidence/author %)) entries) entries)
        entries (if (seq tags)
                  (filter #(let [et (set (:evidence/tags %))]
                             (every? et tags))
                          entries)
                  entries)
        entries (if since-inst
                  (filter #(not (.isBefore (entry-at %) since-inst)) entries)
                  entries)
        entries (sort-by entry-at #(compare %2 %1) entries)
        entries (if (and (int? limit) (pos? limit)) (take limit entries) entries)]
    (vec entries)))

;; =============================================================================
;; AtomBackend
;; =============================================================================

(defrecord AtomBackend [!store]
  EvidenceBackend

  (-append [_ validated]
    (let [eid (:evidence/id validated)]
      (loop []
        (let [st @!store]
          (cond
            (contains? (:entries st) eid)
            (social-error :duplicate-id "Evidence id already exists" :evidence-id eid)

            (and (:evidence/in-reply-to validated)
                 (not (contains? (:entries st) (:evidence/in-reply-to validated))))
            (social-error :reply-not-found
                          "in-reply-to references missing entry"
                          :in-reply-to (:evidence/in-reply-to validated)
                          :evidence-id eid)

            (and (:evidence/fork-of validated)
                 (not (contains? (:entries st) (:evidence/fork-of validated))))
            (social-error :fork-not-found
                          "fork-of references missing entry"
                          :fork-of (:evidence/fork-of validated)
                          :evidence-id eid)

            :else
            (let [next-st (-> st
                              (assoc-in [:entries eid] validated)
                              (update :order conj eid))]
              (if (compare-and-set! !store st next-st)
                {:ok true :entry validated}
                (recur))))))))

  (-get [_ evidence-id]
    (get-in @!store [:entries evidence-id]))

  (-exists? [_ evidence-id]
    (contains? (:entries @!store) evidence-id))

  (-query [_ params]
    (let [entries (vals (:entries @!store))]
      (filter-and-sort-entries entries params)))

  (-forks-of [_ evidence-id]
    (let [entries (vals (:entries @!store))
          forks (filter #(= evidence-id (:evidence/fork-of %)) entries)]
      (->> forks (sort-by entry-at) vec)))

  (-delete! [_ ids]
    (let [id-set (set ids)]
      (swap! !store (fn [{:keys [entries order] :as st}]
                      (-> st
                          (assoc :entries (apply dissoc entries id-set))
                          (assoc :order (vec (remove id-set order))))))
      {:compacted (count ids)}))

  (-all [_]
    (vals (:entries @!store))))
