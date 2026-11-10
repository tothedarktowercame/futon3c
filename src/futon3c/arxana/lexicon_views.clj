(ns futon3c.arxana.lexicon-views
  "Public read views for reviewing lexicon relations in context (象 stepper).

  The lexicon loader (futon3c.arxana.lexicon-loader) writes one immutable
  [:lexicon/entity 1] clusion per entity, content-addressed by slug, plus
  [:workplan/referent 1] edges for each proposed relation.  These views put
  that graph in front of the operator without a word rule (the
  entity-backlinks view only reaches entities the rules catalog names).

  Enumerating clusions by type is not part of the futon1b read surface
  (role-reverse, attestations and assessments all need a known id), so
  `lexicon-slugs` seeds slugs from the loader's own sources and every slug
  is confirmed against the store by its deterministic clusion id before it
  enters the index: name, aka and every relation come from stored records,
  never from the seed files.  A slug the store does not hold is skipped."
  (:require [futon3c.arxana.lexicon-loader :as loader]
            [futon3c.arxana.records :as records]))

(def index-limit 400)

(defn- ok! [r] (or (:ok r) (throw (ex-info "public read failed" {:response r}))))
(defn- entity! [read! id] (:record (ok! (read! :entity/get {:entity/id id}))))
(defn- query! [read! body] (ok! (read! :graph/query (assoc body :query/limit 200))))
(defn- reverse-ids [read! type role id]
  (:result/edge-ids (query! read! {:query/type :role-reverse :edge/type type
                                   :endpoint/role role :endpoint/id id :time/order :asc})))
(defn- attestations [read! id]
  (mapv #(entity! read! %) (:result/attestation-ids
                            (query! read! {:query/type :attestations :subject/id id :time/order :asc}))))
(defn- assessments [read! id]
  (:result/assessments (query! read! {:query/type :assessments :subject/id id :time/order :asc})))

(defn slug->entity-id
  "The deterministic clusion id for a lexicon entity slug: the loader wrote
  exactly this record, so the store holds this id when the slug was loaded."
  [slug]
  (:id (records/make-clusion {:type-revision [:lexicon/entity 1] :endpoints []
                              :asserted-content {:entity/slug slug}})))

(defn lexicon-slugs
  "Seed slugs from the loader's sources (lexicon merge + Agency survey).
  Read failures yield no seeds: the view then reports what the store can be
  asked about directly, never invents entities."
  []
  (try (vec (distinct (concat (map :entity (loader/read-lexicon))
                              (map :entity (loader/read-survey-entities)))))
       (catch Exception _ [])))

(defn- entity-by-slug
  "The stored [:lexicon/entity 1] clusion for SLUG, or nil when absent."
  [read! slug]
  (when-let [e (try (entity! read! (slug->entity-id slug)) (catch Exception _ nil))]
    (when (and (= [:lexicon/entity 1] (:type-revision e))
               (= slug (get-in e [:asserted-content :entity/slug])))
      e)))

(defn- newest [xs] (first xs))
(defn- oldest [xs] (last xs))

(defn- named-attestation [atts]
  (first (filter #(get-in % [:content :name]) atts)))

(defn entity-index-view
  "Public view: one row per stored lexicon entity."
  ([read!] (entity-index-view read! (lexicon-slugs)))
  ([read! slugs]
   (let [rows (vec (for [slug (take index-limit (distinct slugs))
                         :let [e (entity-by-slug read! slug)]
                         :when e
                         :let [atts (attestations read! (:id e))
                               att (named-attestation atts)
                               c (:content att)]]
                     (cond-> {:entity/slug slug}
                       (:name c) (assoc :entity/name (:name c))
                       (:aka c) (assoc :entity/aka (:aka c)))))]
     {:result/status :complete :result/total (count rows) :entities rows})))

(def index-cache-ttl-ms 60000)
(defonce ^:private index-cache (atom {:at 0 :read! nil :value nil}))

(defn index-view-cached
  "entity-index-view with a 60 s cache.  The read! closure changes per
  request, but reads are store-truthful and idempotent, so the first
  closure may serve the whole window."
  [read!]
  (let [now (System/currentTimeMillis)]
    (if (and (:value @index-cache) (< (- now (:at @index-cache)) index-cache-ttl-ms))
      (:value @index-cache)
      (let [v (entity-index-view read!)]
        (reset! index-cache {:at now :value v})
        v))))

(defn- endpoint-id [record role]
  (get-in (first (filter #(= role (:role %)) (:endpoints record))) [:target :clusion/id]))

(defn- referent-row [read! slug direction record]
  (let [as (attestations read! (:id record))
        aa (assessments read! (:id record))
        ;; The index returns attestations and assessments NEWEST first and
        ;; ignores :time/order (futon1b_arxana_index.clj); the newest
        ;; assessment is `first`, the oldest attestation (the proposal) is
        ;; `last` — same logic as the entity-backlinks view (00f11cf05).
        latest (:verdict (newest aa))
        other-id (endpoint-id record (case direction :outgoing :target :incoming :entity))
        other (when other-id (try (entity! read! other-id) (catch Exception _ nil)))
        other-slug (get-in other [:asserted-content :entity/slug])
        proposal (oldest as)]
    {:record/id (:id record) :entity/slug slug
     :direction direction
     :referent/role (get-in record [:asserted-content :referent/role])
     :other/slug (or other-slug (str other-id))
     :proposed-by (:producer proposal)
     :relation/raw (get-in proposal [:content :relation/raw])
     :review/status (case latest :accepted :accepted :rejected :rejected :proposed)
     :review/actor (get-in (newest aa) [:actor])}))

(defn entity-relations-view
  "Public view: SLUG's referents in both directions, ruled on by the newest
  assessment.  A relation already ruled on is reported with its verdict, so
  a caller listing only proposed rows never re-offers a ruled one."
  [read! slug]
  (if-not slug
    {:result/status :invalid :error/detail {:entity/slug slug}}
    (let [e (entity-by-slug read! slug)]
      (if-not e
        {:result/status :missing :error/detail {:entity/slug slug}}
        (let [outgoing (mapv #(referent-row read! slug :outgoing (entity! read! %))
                             (reverse-ids read! [:workplan/referent 1] :entity (:id e)))
              incoming (mapv #(referent-row read! slug :incoming (entity! read! %))
                             (reverse-ids read! [:workplan/referent 1] :target (:id e)))]
          {:result/status :complete
           :entity/slug slug
           :entity e
           :relations (vec (concat outgoing incoming))
           :record/ids (vec (concat [(:id e)] (map :record/id (concat outgoing incoming))))})))))
