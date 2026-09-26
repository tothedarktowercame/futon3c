(ns futon3c.apm.memory-caption-store
  "Append-only grounded applicability observations and reviewed captions.

  Caption evidence is searchable, but it is never recall authority. A search
  hit must resolve through the current reviewed caption index and then through
  the original memory projection."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.futon1b-backend :as f1b]
            [futon3c.evidence.store :as estore]
            [futon3c.substrate.client :as substrate])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file Files StandardCopyOption]
           [java.time Instant]
           [java.util UUID]))

(def caption-schema :apm-memory-caption-v1)
(def observation-schema :apm-memory-applicability-v1)
(def schema caption-schema)
(def max-caption-bytes 1024)
(def epistemic-statuses #{:supported :suggested :unknown})
(def condition-statuses #{:observed :established :absent :unchecked})
(def observation-outcomes #{:used :considered :not-used :unresolved})
(def review-verdicts #{:approve :reject :retract :cannot-judge})
(def ^:dynamic *store-root* "data/apm-memory-captions")

(defn- nonblank? [x]
  (and (string? x) (not (str/blank? x))))

(defn- utf8-size [x]
  (alength (.getBytes ^String x StandardCharsets/UTF_8)))

(defn- record-path [kind id]
  (io/file *store-root* (name kind) (str id ".edn")))

(defn- current-path [memory-id]
  (io/file *store-root* "current" (str (machine/ledger-digest [memory-id]) ".edn")))

(defn- atomic-write! [file value]
  (io/make-parents file)
  (let [tmp (io/file (.getParentFile file)
                     (str "." (.getName file) "." (UUID/randomUUID) ".tmp"))]
    (spit tmp (str (pr-str value) "\n"))
    (Files/move (.toPath tmp) (.toPath file)
                (into-array StandardCopyOption
                            [StandardCopyOption/ATOMIC_MOVE
                             StandardCopyOption/REPLACE_EXISTING]))))

(defn- read-edn [file]
  (when (.isFile file)
    (try (edn/read-string (slurp file)) (catch Throwable _ nil))))

(defn- exact-record? [record]
  (= (:record/id record)
     (machine/ledger-digest [(dissoc record :record/id)])))

(defn- persist-record! [kind record]
  (let [record (assoc record :record/id
                      (machine/ledger-digest [(dissoc record :record/id)]))
        file (record-path kind (:evidence/id record))
        prior (read-edn file)]
    (cond
      (and prior (not= prior record))
      {:ok false :error/code :memory-caption-record-conflict}
      prior {:ok true :status :already-recorded :record prior}
      :else (do (atomic-write! file record)
                (let [observed (read-edn file)]
                  (if (and (= record observed) (exact-record? observed))
                    {:ok true :status :recorded :record observed}
                    {:ok false :error/code :memory-caption-record-not-readable}))))))

(defn- conditions-valid? [conditions]
  (and (vector? conditions) (seq conditions)
       (every? #(and (nonblank? (:condition %))
                     (contains? condition-statuses (:status %))
                     (or (nil? (:evidence-id %))
                         (nonblank? (:evidence-id %))))
               conditions)))

(defn- suggestions-valid? [contexts]
  (and (vector? contexts)
       (every? #(and (nonblank? (:context %))
                     (contains? #{:suggested :unknown} (:status %))
                     (nonblank? (:basis %)))
               contexts)))

(defn- ids-valid? [xs]
  (and (vector? xs) (every? nonblank? xs)))

(defn- wire-keyword [x]
  (if (string? x) (keyword x) x))

(defn- normalize-conditions [conditions]
  (mapv #(update % :status wire-keyword) conditions))

(defn- normalize-suggestions [contexts]
  (mapv #(update % :status wire-keyword) contexts))

(defn normalize-observation [observation]
  (let [context (:context observation)
        exposure (:exposure observation)
        task (:task-observation observation)]
    (-> observation
      (assoc :applicability/schema
             (wire-keyword (or (:applicability/schema observation)
                               (:schema observation))))
      (cond-> (:memory-id observation)
        (assoc :memory/id (:memory-id observation)))
      (cond-> (:memory-content-digest observation)
        (assoc :memory/revision (:memory-content-digest observation)))
      (update :epistemic-status wire-keyword)
      (update :conditions normalize-conditions)
      (update :suggested-contexts #(normalize-suggestions (or % [])))
      (update :task-observation
              #(when %
                 (-> %
                     (assoc :outcome
                            (wire-keyword (or (:outcome %) (:disposition %)))))))
      (cond-> context
        (assoc :context (update context :kind wire-keyword)))
      (cond-> (and context (nil? (:problem-id observation)))
        (assoc :problem-id (:problem-id context)))
      (cond-> (and context (nil? (:task-id observation)))
        (assoc :task-id (or (:task-ref context) (name (:kind context)))))
      (cond-> (and context (nil? (:attempt-id observation)))
        (assoc :attempt-id (or (:attempt-job-id context)
                               (first (:source-attempt-ids context)))))
      (cond-> (and exposure (nil? (:search-receipt-id observation)))
        (assoc :search-receipt-id (:receipt-id exposure)))
      (cond-> (and (= :historical-source (wire-keyword (:kind context)))
                   (nil? (:search-receipt-id observation)))
        (assoc :search-receipt-id "historical-source"))
      (cond-> (and task (nil? (:evidence-ids observation)))
        (assoc :evidence-ids (vec (:evidence-refs task)))))))

(defn normalize-caption [caption]
  (-> caption
      (assoc :caption/schema
             (wire-keyword (or (:caption/schema caption) (:schema caption))))
      (cond-> (:memory-id caption) (assoc :memory/id (:memory-id caption)))
      (cond-> (:memory-content-digest caption)
        (assoc :memory/revision (:memory-content-digest caption)))
      (cond-> (:revision caption) (assoc :caption/version (:revision caption)))
      (cond-> (:useful-when caption) (assoc :text (:useful-when caption)))
      (update :epistemic-status wire-keyword)
      (update :conditions normalize-conditions)
      (update :suggested-contexts #(normalize-suggestions (or % [])))
      (update :compression #(when % (update % :trigger wire-keyword)))))

(defn observation-findings [observation]
  (let [task (:task-observation observation)]
    (cond-> []
      (not= observation-schema (:applicability/schema observation))
      (conj :applicability-schema-invalid)
      (not (every? nonblank? ((juxt :memory/id :memory/revision :problem-id
                                    :task-id :search-receipt-id :attempt-id
                                    :useful-when :scope-limit) observation)))
      (conj :applicability-provenance-incomplete)
      (not (contains? epistemic-statuses (:epistemic-status observation)))
      (conj :applicability-status-invalid)
      (not (conditions-valid? (:conditions observation)))
      (conj :applicability-conditions-invalid)
      (not (and (map? task)
                (contains? observation-outcomes (:outcome task))
                (nonblank? (:reason task))))
      (conj :applicability-task-observation-invalid)
      (not (suggestions-valid? (or (:suggested-contexts observation) [])))
      (conj :applicability-suggestions-invalid)
      (not (ids-valid? (:evidence-ids observation)))
      (conj :applicability-evidence-invalid)
      (not (ids-valid? (or (:supersedes observation) [])))
      (conj :applicability-supersession-invalid))))

(defn caption-findings [caption]
  (let [compression (:compression caption)]
    (cond-> []
      (not= caption-schema (:caption/schema caption))
      (conj :caption-schema-invalid)
      (not (every? nonblank? ((juxt :memory/id :memory/revision :text
                                    :scope-limit) caption)))
      (conj :caption-provenance-incomplete)
      (not (pos-int? (:caption/version caption))) (conj :caption-version-invalid)
      (and (string? (:text caption))
           (> (utf8-size
               (pr-str
                (select-keys caption [:text :epistemic-status :conditions
                                      :suggested-contexts :scope-limit])))
              max-caption-bytes))
      (conj :caption-size-limit-exceeded)
      (not (contains? epistemic-statuses (:epistemic-status caption)))
      (conj :caption-status-invalid)
      (not (conditions-valid? (:conditions caption)))
      (conj :caption-conditions-invalid)
      (not (suggestions-valid? (or (:suggested-contexts caption) [])))
      (conj :caption-suggestions-invalid)
      (not (and (ids-valid? (:observation-ids caption))
                (seq (:observation-ids caption))))
      (conj :caption-observations-invalid)
      (not (and (ids-valid? (:basis-evidence-ids caption))
                (seq (:basis-evidence-ids caption))))
      (conj :caption-basis-invalid)
      (not (or (nil? (:previous-caption-id caption))
               (nonblank? (:previous-caption-id caption))))
      (conj :caption-previous-invalid)
      (not (and (map? compression)
                (contains? #{:review :evidence-threshold} (:trigger compression))
                (or (= :review (:trigger compression))
                    (pos-int? (:threshold compression)))))
      (conj :caption-compression-invalid))))

(defn- evidence-entry [id author session-id subject type tags body]
  {:evidence/id id :evidence/subject subject :evidence/type type
   :evidence/claim-type :observation :evidence/author author
   :evidence/session-id session-id :evidence/at (str (Instant/now))
   :evidence/tags tags :evidence/body body})

(defn- append-and-read! [entry {:keys [append-entry fetch-entry]}]
  (let [id (:evidence/id entry) prior (fetch-entry id)
        appended (when-not prior (append-entry entry))
        observed (or (fetch-entry id) (:entry appended))]
    (cond
      (and prior (not= (dissoc prior :evidence/at) (dissoc entry :evidence/at)))
      {:ok false :error/code :memory-caption-evidence-conflict}
      (and (nil? prior) (not (:ok appended)))
      {:ok false :error/code :memory-caption-evidence-write-failed}
      (not= (dissoc observed :evidence/at) (dissoc entry :evidence/at))
      {:ok false :error/code :memory-caption-evidence-not-readable}
      :else {:ok true :entry observed})))

(defn- default-ports []
  (let [backend (f1b/make-futon1b-backend (substrate/configured-url))]
    {:append-entry #(boundary/append! backend %)
     :fetch-entry #(estore/get-entry* backend %)
     :memory-admissible?
     (fn [memory-id]
       (some (fn [edge]
               (and (= :memory/assert (:hx/type edge))
                    (= memory-id (get-in edge [:hx/props :roles :entry]))
                    (= :mathematics (get-in edge [:hx/props :domain]))
                    (= :reviewed (get-in edge [:hx/props :attachment-status]))
                    (= :current (or (get-in edge [:hx/props :state]) :current))))
             (substrate/hyperedges-by-end memory-id)))}))

(declare propose-compression)

(defn admit-observation!
  ([authority observation] (admit-observation! authority observation (default-ports)))
  ([authority observation ports]
   (let [observation (normalize-observation observation)
         ports (merge (default-ports) ports)
         author (:agent-id authority)
         receipt ((:fetch-search-receipt ports (constantly nil))
                  (:search-receipt-id observation))
         historical? (= :historical-source (get-in observation [:context :kind]))
         memory ((:fetch-memory ports (:fetch-entry ports (constantly nil)))
                 (:memory/id observation))
         findings (cond-> (observation-findings observation)
                    (not (contains? #{:student :scribe :zai-scribe}
                                    (:role authority)))
                    (conj :applicability-authority-invalid)
                    (and (not historical?)
                         (not= (:job-id authority) (:job-id receipt)))
                    (conj :applicability-search-receipt-invalid)
                    (and (not historical?)
                         (not (contains? (set (:result-ids receipt))
                                         (:memory/id observation))))
                    (conj :applicability-memory-not-exposed)
                    (and historical?
                         (not (and (vector? (:basis observation))
                                   (seq (:basis observation))
                                   (every? #((:verify-source ports
                                                              (constantly false)) %)
                                           (:basis observation)))))
                    (conj :applicability-historical-source-unverified)
                    (nil? memory) (conj :applicability-memory-unknown)
                    (and memory
                         (not ((:memory-admissible? ports) (:memory/id observation))))
                    (conj :applicability-memory-not-admissible)
                    (and memory
                         (not= (:memory/revision observation)
                               (machine/ledger-digest [(:evidence/body memory)])))
                    (conj :applicability-memory-revision-mismatch))]
     (if (seq findings)
       {:ok false :error/code :applicability-observation-invalid :findings findings}
       (let [body (assoc observation :event :memory-applicability-observation)
             id (str "e-apm-applicability-"
                     (subs (machine/ledger-digest [author body]) 0 32))
             entry (evidence-entry id author (:job-id authority)
                                   {:ref/type :memory :ref/id (:memory/id body)}
                                   :reflection
                                   [:memory :memory/applicability-observation] body)
             written (append-and-read! entry ports)]
         (if-not (:ok written) written
                 (persist-record! :observations
                                  {:record/type :applicability-observation
                                   :evidence/id id :author author :body body})))))))

(defn admit-caption!
  ([authority caption] (admit-caption! authority caption (default-ports)))
  ([authority caption ports]
   (let [caption (normalize-caption caption)
         ports (merge (default-ports) ports)
         author (:agent-id authority)
         memory ((:fetch-memory ports (:fetch-entry ports (constantly nil)))
                 (:memory/id caption))
         observations (mapv #(read-edn (record-path :observations %))
                            (:observation-ids caption))
         previous (when-let [id (:previous-caption-id caption)]
                    (read-edn (record-path :captions id)))
         current (read-edn (current-path (:memory/id caption)))
         compression (propose-compression caption observations)
         findings (cond-> (vec (:findings compression))
                    (not (contains? #{:scribe :zai-scribe} (:role authority)))
                    (conj :caption-authority-invalid)
                    (some nil? observations) (conj :caption-observation-missing)
                    (nil? memory) (conj :caption-memory-unknown)
                    (and memory
                         (not ((:memory-admissible? ports) (:memory/id caption))))
                    (conj :caption-memory-not-admissible)
                    (and memory
                         (not= (:memory/revision caption)
                               (machine/ledger-digest [(:evidence/body memory)])))
                    (conj :caption-memory-revision-mismatch)
                    (some #(not= (:memory/id caption)
                                 (get-in % [:body :memory/id])) observations)
                    (conj :caption-observation-memory-mismatch)
                    (and (= 1 (:caption/version caption))
                         (:previous-caption-id caption))
                    (conj :caption-first-version-has-previous)
                    (and (> (or (:caption/version caption) 0) 1)
                         (or (nil? previous)
                             (not= (dec (:caption/version caption))
                                   (get-in previous [:body :caption/version]))))
                    (conj :caption-version-chain-invalid)
                    (and current
                         (not= (:previous-caption-id caption)
                               (:caption/id current)))
                    (conj :caption-does-not-supersede-current))]
     (if (seq findings)
       {:ok false :error/code :memory-caption-invalid :findings findings}
       (let [body (assoc caption :event :memory-caption)
             id (str "e-apm-caption-"
                     (subs (machine/ledger-digest [author body]) 0 32))
             body (assoc body :caption/id id)
             entry (evidence-entry id author (:job-id authority)
                                   {:ref/type :memory :ref/id (:memory/id body)}
                                   :reflection [:memory :memory/caption :caption/proposed]
                                   body)
             written (append-and-read! entry ports)]
         (if-not (:ok written) written
                 (persist-record! :captions
                                  {:record/type :caption-revision
                                   :evidence/id id :author author :body body})))))))

(defn review-caption!
  ([authority review] (review-caption! authority review (default-ports)))
  ([authority review ports]
   (let [review (update review :verdict wire-keyword)
         ports (merge (default-ports) ports)
         caption (when-let [id (:caption/id review)]
                   (read-edn (record-path :captions id)))
         author (:agent-id authority)
         verdict (:verdict review)
         observation-authors
         (when caption
           (->> (get-in caption [:body :observation-ids])
                (keep #(some-> (record-path :observations %) read-edn :author))
                set))
         current (when caption
                   (read-edn (current-path (get-in caption [:body :memory/id]))))
         findings (cond-> []
                    (not= :promotion-proctor (:role authority))
                    (conj :caption-review-authority-invalid)
                    (nil? caption) (conj :caption-review-caption-missing)
                    (and caption (not (exact-record? caption)))
                    (conj :caption-review-caption-invalid)
                    (= author (:author caption)) (conj :caption-reviewer-is-author)
                    (contains? observation-authors author)
                    (conj :caption-reviewer-authored-observation)
                    (not (contains? review-verdicts verdict))
                    (conj :caption-review-verdict-invalid)
                    (not (nonblank? (:reason review)))
                    (conj :caption-review-reason-missing)
                    (and (= :approve verdict) current
                         (not= (:caption/id review) (:caption/id current))
                         (<= (get-in caption [:body :caption/version] 0)
                             (:caption/version current)))
                    (conj :caption-review-version-not-current))]
     (if (seq findings)
       {:ok false :error/code :memory-caption-review-invalid :findings findings}
       (let [body {:caption-review/event :review
                   :caption-review/schema caption-schema
                   :caption/id (:caption/id review)
                   :caption/version (get-in caption [:body :caption/version])
                   :memory/id (get-in caption [:body :memory/id])
                   :verdict verdict :reason (:reason review)}
             id (str "e-apm-caption-review-"
                     (subs (machine/ledger-digest [author body]) 0 32))
             entry (evidence-entry id author (:job-id authority)
                                   {:ref/type :memory-caption
                                    :ref/id (:caption/id review)}
                                   :reflection [:memory :memory/caption-review] body)
             written (append-and-read! entry ports)
             record-result (when (:ok written)
                             (persist-record! :reviews
                                              {:record/type :caption-review
                                               :evidence/id id :author author
                                               :body body}))]
         (if-not (:ok record-result) (or record-result written)
           (do
             (when (= :approve verdict)
               (atomic-write! (current-path (:memory/id body))
                              {:memory/id (:memory/id body)
                               :caption/id (:caption/id body)
                               :caption/version (:caption/version body)
                               :review/id id}))
             (when (contains? #{:reject :retract} verdict)
               (let [current (read-edn (current-path (:memory/id body)))]
                 (when (= (:caption/id body) (:caption/id current))
                   (.delete (current-path (:memory/id body))))))
             {:ok true :caption/id (:caption/id body)
             :caption/version (:caption/version body) :review/id id})))))))

(defn propose-compression
  "Validate a bounded caption projection over selected immutable observations.
  The caller authors the wording; this function never truncates or upgrades
  epistemic status."
  [caption observations]
  (let [by-id (into {} (map (juxt :evidence/id identity)) observations)
        selected (mapv by-id (:observation-ids caption))
        statuses (set (map #(get-in % [:body :epistemic-status]) selected))
        suggested? (some #(seq (get-in % [:body :suggested-contexts])) selected)
        findings (cond-> (caption-findings caption)
                   (some nil? selected) (conj :caption-observation-missing)
                   (and (= :evidence-threshold
                           (get-in caption [:compression :trigger]))
                        (< (count selected)
                           (get-in caption [:compression :threshold] 0)))
                   (conj :caption-evidence-threshold-unmet)
                   (and (contains? statuses :unknown)
                        (not= :unknown (:epistemic-status caption)))
                   (conj :caption-unknown-status-lost)
                   (and (contains? statuses :suggested)
                        (= :supported (:epistemic-status caption)))
                   (conj :caption-suggestion-upgraded)
                   (and suggested? (empty? (:suggested-contexts caption)))
                   (conj :caption-suggestion-lost))]
    (if (seq findings)
      {:ok false :error/code :memory-caption-compression-invalid
       :findings findings}
      {:ok true :caption caption})))

(defn current-caption [memory-id]
  (let [current (read-edn (current-path memory-id))
        caption (when-let [id (:caption/id current)]
                  (read-edn (record-path :captions id)))
        review (when-let [id (:review/id current)]
                 (read-edn (record-path :reviews id)))]
    (when (and current caption review
               (exact-record? caption) (exact-record? review)
               (= :approve (get-in review [:body :verdict]))
               (= memory-id (:memory/id current))
               (= memory-id (get-in caption [:body :memory/id]))
               (= memory-id (get-in review [:body :memory/id]))
               (= (:caption/id current) (get-in review [:body :caption/id]))
               (= (:caption/version current)
                  (get-in caption [:body :caption/version]))
               (= (:caption/version current)
                  (get-in review [:body :caption/version])))
      {:memory/id memory-id :caption/id (:caption/id current)
       :caption/version (:caption/version current)
       :caption (:body caption) :review/id (:review/id current)})))

(defn caption-row? [row]
  (and (= :reflection (get-in row [:entry :evidence/type]))
       (= :memory-caption (get-in row [:entry :evidence/body :event]))
       (= caption-schema (get-in row [:entry :evidence/body :caption/schema]))))

(defn resolve-search-rows
  "Resolve FTS caption rows only when they name the currently reviewed version.
  The caller must still project :memory/id through normal memory authority."
  [rows]
  (->> rows
       (keep (fn [row]
               (let [body (get-in row [:entry :evidence/body])
                     current (current-caption (:memory/id body))]
                 (when (and current
                            (= (:caption/id body) (:caption/id current))
                            (= (:caption/version body) (:caption/version current)))
                   {:memory/id (:memory/id body)
                    :caption/id (:caption/id body)
                    :caption/version (:caption/version body)
                    :caption/text (:text body)
                    :caption/epistemic-status (:epistemic-status body)
                    :caption/review-id (:review/id current)
                    :fts-score (:score row)}))))
       vec))
