(ns futon3c.evidence.store
  "In-memory evidence store (append/query) for the evidence landscape.

   R8 (authoritative transcript): the store is the authority for EvidenceEntry.
   R9 (structured events): entries are typed maps (EvidenceEntry), not free text.
   R4 (loud failure): operations return typed results; no silent failures."
  (:require [clojure.string :as str]
            [futon3c.social.shapes :as shapes])
  (:import [java.time Instant]
           [java.time.format DateTimeParseException]
           [java.util UUID]))

;; Store model: {:entries {id EvidenceEntry} :order [id ...]} in an atom.
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

(defn- store-atom
  [maybe-store]
  (if (instance? clojure.lang.IAtom maybe-store) maybe-store !store))

(defn- parse-instant
  [t]
  (cond
    (instance? Instant t) t
    (string? t) (Instant/parse t)
    :else (throw (ex-info "bad-timestamp" {:t t}))))

(defn- entry-at
  [entry]
  (try
    (parse-instant (:evidence/at entry))
    (catch Exception _
      ;; If a bad timestamp slipped in, treat as epoch so ordering is stable.
      (Instant/ofEpochMilli 0))))

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

(defn- exists-id?
  [st evidence-id]
  (contains? (:entries st) evidence-id))

(defn get-entry
  "Get an entry by id from the default store.
   Returns EvidenceEntry or nil."
  [evidence-id]
  (get-in @!store [:entries evidence-id]))

(defn get-entry*
  "Get an entry by id from a specific store atom.
   Returns EvidenceEntry or nil."
  [store evidence-id]
  (get-in @(store-atom store) [:entries evidence-id]))

(defn- append-entry!
  [s validated]
  (let [eid (:evidence/id validated)]
    (loop []
      (let [st @s]
        (cond
          (exists-id? st eid)
          (social-error :duplicate-id "Evidence id already exists" :evidence-id eid)

          (and (:evidence/in-reply-to validated)
               (not (exists-id? st (:evidence/in-reply-to validated))))
          (social-error :reply-not-found
                        "in-reply-to references missing entry"
                        :in-reply-to (:evidence/in-reply-to validated)
                        :evidence-id eid)

          (and (:evidence/fork-of validated)
               (not (exists-id? st (:evidence/fork-of validated))))
          (social-error :fork-not-found
                        "fork-of references missing entry"
                        :fork-of (:evidence/fork-of validated)
                        :evidence-id eid)

          :else
          (let [next-st (-> st
                            (assoc-in [:entries eid] validated)
                            (update :order conj eid))]
            (if (compare-and-set! s st next-st)
              {:ok true :entry validated}
              (recur))))))))

(defn append*
  "Append to a specific store atom. Same semantics as append!."
  [store m]
  (let [s (store-atom store)
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
      (append-entry! s validated))))

(defn append!
  "Append an evidence entry to the default store.
   Accepts either:
   1) a full EvidenceEntry map (namespaced keys), or
   2) an append args map (unqualified keys per mission handoff)."
  [m]
  (append* !store m))

(defn query*
  "Query a specific store atom.
   Returns [EvidenceEntry], excluding ephemeral entries by default."
  [store evidence-query]
  (if-not (shapes/valid? shapes/EvidenceQuery evidence-query)
    []
    (let [{:query/keys [subject type claim-type since limit include-ephemeral?]} evidence-query
          include-ephemeral? (true? include-ephemeral?)
          since-inst (when since
                       (try (parse-instant since)
                            (catch DateTimeParseException _ nil)
                            (catch Exception _ nil)))
          entries (vals (:entries @(store-atom store)))
          entries (remove #(and (not include-ephemeral?) (true? (:evidence/ephemeral? %))) entries)
          entries (if subject (filter #(= subject (:evidence/subject %)) entries) entries)
          entries (if type (filter #(= type (:evidence/type %)) entries) entries)
          entries (if claim-type (filter #(= claim-type (:evidence/claim-type %)) entries) entries)
          entries (if since-inst (filter #(not (.isBefore (entry-at %) since-inst)) entries) entries)
          entries (sort-by entry-at #(compare %2 %1) entries) ; newest first
          entries (if (and (int? limit) (pos? limit)) (take limit entries) entries)]
      (vec entries))))

(defn query
  "Query the default store. Returns [EvidenceEntry] (excludes ephemeral by default)."
  [evidence-query]
  (query* !store evidence-query))

(defn get-reply-chain*
  "Return the ordered ancestor chain for evidence-id, including the entry itself.
   Missing evidence-id returns []."
  [store evidence-id]
  (let [st @(store-atom store)]
    (loop [eid evidence-id acc [] seen #{}]
      (if (or (nil? eid) (contains? seen eid))
        (vec (reverse acc))
        (if-let [e (get-in st [:entries eid])]
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
  (let [entries (vals (:entries @(store-atom store)))
        forks (filter #(= evidence-id (:evidence/fork-of %)) entries)]
    (->> forks (sort-by entry-at) vec)))

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
  "Remove ephemeral entries older than threshold from a specific store atom.
   older-than must be ISO string or Instant."
  [store {:keys [older-than]}]
  (let [cutoff (try
                 (parse-instant older-than)
                 (catch Exception _ ::bad))]
    (if (= ::bad cutoff)
      (social-error :invalid-input "older-than must be an ISO timestamp string or Instant" :older-than older-than)
      (let [s (store-atom store)
            {:keys [entries order]} @s
            to-drop (->> entries
                         (keep (fn [[eid e]]
                                 (when (and (true? (:evidence/ephemeral? e))
                                            (.isBefore (entry-at e) cutoff))
                                   eid)))
                         set)
            n (count to-drop)]
        (swap! s (fn [{:keys [entries order] :as st}]
                   (-> st
                       (assoc :entries (apply dissoc entries to-drop))
                       (assoc :order (vec (remove to-drop order))))))
        {:compacted n}))))

(defn compact-ephemeral!
  "Remove ephemeral entries older than threshold from the default store.
   older-than must be ISO string or Instant."
  [{:keys [older-than]}]
  (compact-ephemeral!* !store {:older-than older-than}))
