(ns futon3c.evidence.xtdb-backend
  "XTDB-backed evidence persistence via futon1a.

   Wraps an XTDB node (from futon1a's XtdbStore) to provide durable
   storage for the evidence landscape. Evidence entries are stored as
   XTDB documents with :xt/id = :evidence/id.

   Constructor: (make-xtdb-backend xtdb-store-or-node)
   Accepts either a futon1a XtdbStore record or a raw XTDB node."
  (:require [xtdb.api :as xtdb]
            [futon3c.evidence.backend :as backend])
  (:import [java.time Instant]
           [java.util Date]))

(defn- social-error
  [code message & {:as context}]
  (cond-> {:error/component :E-store
           :error/code code
           :error/message message
           :error/at (str (java.time.Instant/now))}
    (seq context) (assoc :error/context context)))

(defn- strip-xt-id
  "Remove :xt/id from an XTDB document to return a clean EvidenceEntry."
  [doc]
  (when doc
    (dissoc doc :xt/id)))

(defn- db
  "Get a consistent db snapshot from the node."
  [node]
  (xtdb/db node))

(defn- entity
  "Read an entity by id, stripping :xt/id."
  [node id]
  (strip-xt-id (xtdb/entity (db node) id)))

(defn- entity-exists?
  "Check if an entity exists by id."
  [node id]
  (some? (xtdb/entity (db node) id)))

(defn- parse-valid-time
  "Parse :evidence/at to a java.util.Date for XTDB valid-time.
   Returns nil if unparseable (XTDB will use transaction time as fallback)."
  [doc]
  (when-let [at (:evidence/at doc)]
    (try
      (Date/from (Instant/parse at))
      (catch Exception _ nil))))

(defn- put-and-sync!
  "Submit a put transaction and wait for it to be indexed.
   Sets XTDB valid-time to :evidence/at so entries appear at their true
   chronological position, not when they were physically written (important
   for replicated entries that arrive after a delay)."
  [node doc]
  (let [valid-time (parse-valid-time doc)
        tx-op (if valid-time
                [:xtdb.api/put doc valid-time]
                [:xtdb.api/put doc])
        tx (xtdb/submit-tx node [tx-op])]
    (xtdb/await-tx node tx)))

(defn- delete-and-sync!
  "Submit delete transactions for a set of ids and wait for indexing."
  [node ids]
  (let [tx-ops (mapv (fn [id] [:xtdb.api/delete id]) ids)
        tx (xtdb/submit-tx node tx-ops)]
    (xtdb/await-tx node tx)))

(defn- query-all-entries
  "Datalog query returning all evidence entries."
  [node]
  (->> (xtdb/q (db node)
               '{:find [(pull e [*])]
                 :where [[e :evidence/id _]]})
       (map first)
       (map strip-xt-id)))

(defrecord XtdbBackend [node]
  backend/EvidenceBackend

  (-append [_ validated]
    (let [eid (:evidence/id validated)]
      (cond
        (entity-exists? node eid)
        (social-error :duplicate-id "Evidence id already exists" :evidence-id eid)

        (and (:evidence/in-reply-to validated)
             (not (entity-exists? node (:evidence/in-reply-to validated))))
        (social-error :reply-not-found
                      "in-reply-to references missing entry"
                      :in-reply-to (:evidence/in-reply-to validated)
                      :evidence-id eid)

        (and (:evidence/fork-of validated)
             (not (entity-exists? node (:evidence/fork-of validated))))
        (social-error :fork-not-found
                      "fork-of references missing entry"
                      :fork-of (:evidence/fork-of validated)
                      :evidence-id eid)

        :else
        (do
          (put-and-sync! node (assoc validated :xt/id eid))
          {:ok true :entry validated}))))

  (-get [_ id]
    (entity node id))

  (-exists? [_ id]
    (entity-exists? node id))

  (-query [_ params]
    (let [entries (query-all-entries node)]
      (backend/filter-and-sort-entries entries params)))

  (-forks-of [_ evidence-id]
    (let [results (->> (xtdb/q (db node)
                               '{:find [(pull e [*])]
                                 :where [[e :evidence/fork-of eid]]
                                 :in [eid]}
                               evidence-id)
                       (map first)
                       (map strip-xt-id))]
      (->> results (sort-by backend/entry-at) vec)))

  (-delete! [_ ids]
    (let [id-set (set ids)]
      (when (seq id-set)
        (delete-and-sync! node id-set))
      {:compacted (count id-set)}))

  (-all [_]
    (query-all-entries node)))

(defn make-xtdb-backend
  "Create an XtdbBackend from either a futon1a XtdbStore or a raw XTDB node.
   XtdbStore has a :node field; raw nodes are used directly."
  [store-or-node]
  (let [node (if-let [n (:node store-or-node)]
               n
               store-or-node)]
    (->XtdbBackend node)))
