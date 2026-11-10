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
(def ^:private read-parallelism 3)

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

;; The index costs about a thousand store reads (45 s on :7094, 2026-11-10),
;; so it is cached for ten minutes, timed from when the build finished, and
;; built once at a time: a stale value is served while one rebuild runs.
;; Lexicon entities and their naming attestations come only from the loader,
;; so a stale index can miss only entities loaded in the last ten minutes.
(def index-cache-ttl-ms 600000)
(defonce ^:private index-cache (atom {:at 0 :value nil :building nil}))

(defn- start-index-build!
  "The running build, starting one when none runs."
  [read!]
  (locking index-cache
    (or (:building @index-cache)
        (let [f (future
                  ;; The lock orders this future's update after the
                  ;; assignment of :building below.
                  (let [v (try (entity-index-view read!) (catch Throwable _ nil))]
                    (locking index-cache
                      (swap! index-cache merge {:building nil}
                             (when v {:value v :at (System/currentTimeMillis)})))
                    v))]
          (swap! index-cache assoc :building f)
          f))))

(defn index-view-cached
  "entity-index-view, cached (see index-cache-ttl-ms).  The read! closure
  changes per request, but reads are store-truthful and idempotent, so the
  closure of the request that starts a build may serve it."
  [read!]
  (let [{:keys [at value]} @index-cache
        fresh? (< (- (System/currentTimeMillis) at) index-cache-ttl-ms)]
    (cond
      (and value fresh?) value
      value (do (start-index-build! read!) value)
      :else (let [b (start-index-build! read!)]
              (or (and b @b) (entity-index-view read!))))))

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
        ;; Each row costs several store reads; rows are independent, so they
        ;; are read a few at a time (agency: 36 rows, 8.9 s one at a time).
        ;; futon1b admits 4 expensive reads at once and answers 503 beyond
        ;; that, so at most read-parallelism run together.
        (let [rows (fn [direction role]
                     (->> (reverse-ids read! [:workplan/referent 1] role (:id e))
                          (partition-all read-parallelism)
                          (mapcat (fn [ids] (doall (pmap #(referent-row read! slug direction
                                                                        (entity! read! %))
                                                         ids))))
                          vec))
              outgoing (rows :outgoing :entity)
              incoming (rows :incoming :target)]
          {:result/status :complete
           :entity/slug slug
           :entity e
           :relations (vec (concat outgoing incoming))
           :record/ids (vec (concat [(:id e)] (map :record/id (concat outgoing incoming))))})))))

;; ---------------------------------------------------------------------------
;; The review table: every lexicon entity and every referent, in memory.
;;
;; The 象 stepper paints a REVIEW section on every frame.  Reading it from the
;; store costs about five reads per relation (a 16-entity frame: 14 s on
;; :7094, 2026-11-10).  The whole lexicon is small (260 entities, 589
;; relations), so the service keeps one table of it:
;;
;; - built in the background, once at a time; a stale table is served while a
;;   rebuild runs;
;; - each referent is read once (it is outgoing for one entity and incoming
;;   for another);
;; - a ruling through :workplan/assess-referent in this JVM re-reads that one
;;   row's assessments from the store (`note-ruling!`), so the store's own
;;   newest-first order still decides and the table never guesses;
;; - a ruling written past this service is picked up by the next rebuild, at
;;   most `table-ttl-ms` after the last one finished, plus the build time;
;; - `:table/version` changes whenever the content does, so a client that
;;   holds the table asks "anything newer than V?" and gets a few bytes back.

(def table-ttl-ms 600000)
(def empty-table-ttl-ms 15000)
(def ^:private first-build-parallelism 3)
(def ^:private rebuild-parallelism 1)
(def ^:private boot-id (str (System/currentTimeMillis)))

(defonce ^:private table
  (atom {:n 0 :at 0 :value nil :building nil :dirty #{}}))
(def ^:private ruling-lock (Object.))

(defn reset-table!
  "Forget the table.  For tests, which start a fresh store per test."
  []
  (reset! table {:n 0 :at 0 :value nil :building nil :dirty #{}}))

(defn- retrying
  "READ! that tries again when the store is busy.  futon1b answers 503 beyond
  four expensive reads at once, and the store client turns that into a thrown
  AssertionError (envelope/error rejects :unavailable), so both a throw and a
  non-missing error response are retried before the build gives up."
  [read!]
  (fn [op body]
    (loop [n 0]
      (let [r (try (read! op body)
                   (catch Throwable t (if (< n 5) ::again (throw t))))
            category (get-in r [:error :error/category])]
        (if (and (< n 5)
                 (or (= ::again r)
                     (and category (not (#{:missing :invalid :unsupported :forbidden} category)))))
          (do (Thread/sleep (long (* 200 (inc n)))) (recur (inc n)))
          r)))))

(defn- chunked-pmap [par f xs]
  (->> xs (partition-all par) (mapcat #(doall (pmap f %))) vec))

(defn- ruling-of
  "Status and actor from a subject's assessments, NEWEST first as the store
  returns them (see referent-row)."
  [aa]
  (let [latest (newest aa)]
    {:review/status (case (:verdict latest) :accepted :accepted :rejected :rejected :proposed)
     :review/actor (:actor latest)}))

(defn- table-row [read! id->slug record]
  (let [as (attestations read! (:id record))
        proposal (oldest as)
        end (fn [role]
              (let [id (endpoint-id record role)]
                (or (id->slug id)
                    (when id (get-in (try (entity! read! id) (catch Exception _ nil))
                                     [:asserted-content :entity/slug]))
                    (str id))))]
    (merge {:record/id (:id record)
            :entity/slug (end :entity)
            :target/slug (end :target)
            :referent/role (get-in record [:asserted-content :referent/role])
            :proposed-by (:producer proposal)
            :relation/raw (get-in proposal [:content :relation/raw])}
           (ruling-of (assessments read! (:id record))))))

(defn build-table
  "Read every lexicon entity named by SLUGS and every [:workplan/referent 1]
  touching one, PAR store reads at a time.  Content comes only from the
  store; a slug the store does not hold is skipped."
  ([read!] (build-table read! (lexicon-slugs) first-build-parallelism))
  ([read! slugs par]
   (let [entities (->> (chunked-pmap
                        par
                        (fn [slug]
                          (when-let [e (entity-by-slug read! slug)]
                            (let [c (:content (named-attestation (attestations read! (:id e))))]
                              (cond-> {:entity/slug slug :entity/record e}
                                (:name c) (assoc :entity/name (:name c))
                                (:aka c) (assoc :entity/aka (:aka c))))))
                        (take index-limit (distinct slugs)))
                       (remove nil?) vec)
         id->slug (into {} (map (juxt (comp :id :entity/record) :entity/slug)) entities)
         edge-ids (->> (chunked-pmap
                        par
                        (fn [{e :entity/record}]
                          (concat (reverse-ids read! [:workplan/referent 1] :entity (:id e))
                                  (reverse-ids read! [:workplan/referent 1] :target (:id e))))
                        entities)
                       (mapcat identity) distinct vec)
         rows (chunked-pmap par #(table-row read! id->slug (entity! read! %)) edge-ids)]
     {:entities entities
      :order (mapv :record/id rows)
      :rows (into {} (map (juxt :record/id identity)) rows)})))

(defn- refresh-row!
  "Re-read one row's assessments from the store and put the result in the
  table.  Serialised, so two rulings on one row land in store order."
  [read! id]
  (locking ruling-lock
    (when (get-in @table [:value :rows id])
      (let [ruling (ruling-of (assessments read! id))]
        (swap! table (fn [t]
                       (if (and (get-in t [:value :rows id])
                                (not= ruling (select-keys (get-in t [:value :rows id])
                                                          [:review/status :review/actor])))
                         (-> t (update-in [:value :rows id] merge ruling) (update :n inc))
                         t)))))))

(defn note-ruling!
  "Tell the table that referent ID has a new assessment.  A row read by a
  build that is still running may predate the ruling, so the id is also kept
  for that build to re-read when it finishes."
  [read! id]
  (locking table
    (when (:building @table) (swap! table update :dirty conj id)))
  (refresh-row! read! id)
  nil)

(defn start-table-build!
  "The running build, starting one when none runs."
  [read!]
  (locking table
    (or (:building @table)
        (let [par (if (:value @table) rebuild-parallelism first-build-parallelism)
              read! (retrying read!)
              f (future
                  (let [v (try (build-table read! (lexicon-slugs) par) (catch Throwable _ nil))
                        dirty (locking table
                                (let [d (:dirty @table)]
                                  (swap! table (fn [t]
                                                 (cond-> (assoc t :building nil :dirty #{})
                                                   v (assoc :value v :at (System/currentTimeMillis))
                                                   (and v (not= v (:value t))) (update :n inc))))
                                  d))]
                    (when v (doseq [id dirty] (refresh-row! read! id)))
                    (boolean v)))]
          (swap! table assoc :building f)
          f))))

(defn- current-table
  "The table when one has been built, starting a rebuild when it is older
  than table-ttl-ms.  Never waits."
  [read!]
  (let [{:keys [at value] :as t} @table]
    ;; A table with no entities was built before the lexicon was loaded;
    ;; it is cheap to rebuild and wrong to keep for ten minutes.
    (when (or (nil? value)
              (>= (- (System/currentTimeMillis) at)
                  (if (seq (:entities value)) table-ttl-ms empty-table-ttl-ms)))
      (start-table-build! read!))
    (when value t)))

(defn- version [t] (str boot-id "-" (:n t)))

(defn review-table-view
  "Public view: the whole table, or :unchanged when the caller already holds
  KNOWN-VERSION, or :building before the first build has finished."
  [read! known-version]
  (if-let [{:keys [value at] :as t} (current-table read!)]
    (if (= known-version (version t))
      {:result/status :unchanged :table/version (version t)}
      {:result/status :complete
       :table/version (version t)
       :table/built-at at
       :result/total (count (:order value))
       :entities (mapv #(dissoc % :entity/record) (:entities value))
       :relations (mapv (:rows value) (:order value))})
    {:result/status :building}))

(defn table-index-view
  "entity-index-view's answer from the table, or nil before the first build."
  []
  (when-let [value (:value @table)]
    (let [rows (mapv #(dissoc % :entity/record) (:entities value))]
      {:result/status :complete :result/total (count rows) :entities rows})))

(defn table-relations-view
  "entity-relations-view's answer from the table, or nil when the table is
  not built or does not hold SLUG (the caller then asks the store)."
  [slug]
  (when-let [value (:value @table)]
    (when-let [e (some #(when (= slug (:entity/slug %)) (:entity/record %)) (:entities value))]
      (let [rows (map (:rows value) (:order value))
            row (fn [direction other r]
                  {:record/id (:record/id r) :entity/slug slug :direction direction
                   :referent/role (:referent/role r) :other/slug (other r)
                   :proposed-by (:proposed-by r) :relation/raw (:relation/raw r)
                   :review/status (:review/status r) :review/actor (:review/actor r)})
            outgoing (mapv #(row :outgoing :target/slug %) (filter #(= slug (:entity/slug %)) rows))
            incoming (mapv #(row :incoming :entity/slug %) (filter #(= slug (:target/slug %)) rows))]
        {:result/status :complete
         :entity/slug slug
         :entity e
         :relations (vec (concat outgoing incoming))
         :record/ids (vec (concat [(:id e)] (map :record/id (concat outgoing incoming))))}))))
