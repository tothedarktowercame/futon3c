(ns futon3c.apm.memory-snapshot
  "Atomic admission and verification of reviewed memories for one frame.

  This boundary does not perform attachment review. It admits only review
  results already visible in the substrate and supplied by an evidence reader."
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.string :as str]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.evidence.futon1b-backend :as f1b]
            [futon3c.evidence.store :as estore]
            [futon3c.substrate.client :as substrate])
  (:import [java.nio.file Files Path StandardCopyOption]
           [java.nio.file.attribute FileAttribute]))

(defn- nonblank? [x]
  (and (string? x) (not (str/blank? x))))

(defn- reviewed? [x]
  (= "reviewed" (some-> x name)))

(def ^:private visibility-read-bound-ms 5000)

(def ^:private lean-token
  #"[A-Za-z][A-Za-z0-9_'.]{5,}")

(defn- content-text [content]
  (let [content (if (and (map? content) (contains? content :evidence/body))
                  (:evidence/body content)
                  content)]
    (if (map? content)
      (str/join "\n" (map #(str (get content % "")) [:name :hook :body]))
      (str (or content "")))))

(defn- candidate-text [candidate]
  (content-text (select-keys candidate [:name :hook :body])))

(defn- candidate-has-text? [candidate]
  (some (fn [value]
          (or (nonblank? value)
              (and (coll? value) (seq value))))
        (map #(get candidate %) [:name :hook :body])))

(defn- fetched-content? [fetched]
  (or (nonblank? fetched)
      (and (map? fetched)
           (or (contains? fetched :evidence/body)
               (some #(contains? fetched %) [:name :hook :body])))))

(defn- identifiers [text]
  (set (re-seq lean-token (or text ""))))

(def ^:private typed-memory-kinds
  #{:substitutive :regulative})

(defn candidate-kind
  "Return CANDIDATE's explicit APM memory kind, or :unknown.

   Unknown includes absent and legacy values deliberately: this boundary does
   not infer utility from prose or silently reinterpret historical records."
  [candidate]
  (let [kind (:memory-use/kind candidate)]
    (if (contains? typed-memory-kinds kind) kind :unknown)))

(defn stratify-candidates
  "Deterministically group CANDIDATES without dropping or duplicating them.

   The result is an observable supply interface. `:ordered` puts explicit
   substitutive content first, then unknown legacy records, then regulative
   process memories. Callers must opt into this order; snapshots retain their
   historical relevance order unless `:kind-stratification` requests it."
  [candidates]
  (let [rank {:substitutive 0 :unknown 1 :regulative 2}
        indexed (map-indexed vector candidates)
        ordered (->> indexed
                     (sort-by (fn [[index candidate]]
                                [(get rank (candidate-kind candidate)) index]))
                     (mapv second))
        counts (->> candidates (map candidate-kind) frequencies
                    (into (sorted-map)))]
    {:ordered ordered
     :counts counts
     :buckets (into (sorted-map)
                    (for [kind [:substitutive :unknown :regulative]]
                      [kind (filterv #(= kind (candidate-kind %)) candidates)]))}))

(defn order-candidates
  "Order CANDIDATES by promotion provenance, overlap with BASE-TEXT, then id.
  TEXT-FN is called with a memory id only when the snapshot candidate carries
  none of :name/:hook/:body. Fetch failures score zero and remain observable in
  the returned ordering record."
  [candidates {:keys [problem-id base-text text-fn kind-stratification]}]
  (let [base-identifiers (identifiers base-text)
        measured
        (mapv (fn [candidate]
                (let [memory-id (:memory-id candidate)
                      promoted? (= problem-id
                                   (get-in candidate [:provenance :problem-id]))]
                  (if (candidate-has-text? candidate)
                    {:candidate candidate
                     :memory-id memory-id
                     :promoted? promoted?
                     :score (count (set/intersection
                                    base-identifiers
                                    (identifiers (candidate-text candidate))))}
                    (try
                      (let [fetched (when (fn? text-fn) (text-fn memory-id))]
                        (if (fetched-content? fetched)
                          {:candidate candidate
                           :memory-id memory-id
                           :promoted? promoted?
                           :score (count (set/intersection
                                          base-identifiers
                                          (identifiers (content-text fetched))))
                           :fetched? true}
                          {:candidate candidate :memory-id memory-id
                           :promoted? promoted? :score 0 :fetch-failed? true}))
                      (catch Throwable _
                        {:candidate candidate :memory-id memory-id
                         :promoted? promoted? :score 0 :fetch-failed? true})))))
              candidates)
        relevance-ordered (sort-by (juxt (complement :promoted?)
                                         (comp - :score)
                                         :memory-id)
                                   measured)
        relevance-candidates (mapv :candidate relevance-ordered)
        supply (stratify-candidates relevance-candidates)
        stratified? (= :substitutive-first kind-stratification)]
    {:ordered (if stratified? (:ordered supply) relevance-candidates)
     :ordering
     {:signal (cond-> []
                stratified? (conj :memory-kind)
                true (into [:promoted-this-frame :identifier-overlap
                            :memory-id]))
      :kind-stratification (if stratified? :substitutive-first :observed-only)
      :kind-counts (:counts supply)
      ;; A missing base text silently zeroes every overlap score; say so.
      :base-text-present? (boolean (seq base-identifiers))
      :scores (into (sorted-map) (map (juxt :memory-id :score)) measured)
      :promoted-this-frame (count (filter :promoted? measured))
      :textless-fetched (count (filter :fetched? measured))
      :fetch-failed (->> measured (filter :fetch-failed?)
                         (mapv :memory-id))}}))

(defn evidence-text-fn
  "Return a production text reader for snapshot candidates absent inline text."
  []
  (let [backend (f1b/make-futon1b-backend (substrate/configured-url))]
    #(estore/get-entry* backend %)))

(defn validate-candidate
  [{:keys [memory-id depositor reviewer review-evidence-id
           attachment-status pattern-ids]}]
  (cond
    (not (every? nonblank? [memory-id depositor reviewer review-evidence-id]))
    {:ok false :finding :snapshot-candidate-identity-missing}
    (= depositor reviewer)
    {:ok false :finding :snapshot-reviewer-is-depositor}
    (not (reviewed? attachment-status))
    {:ok false :finding :snapshot-attachment-not-reviewed}
    (not (and (vector? pattern-ids) (seq pattern-ids)
              (every? nonblank? pattern-ids)))
    {:ok false :finding :snapshot-patterns-missing}
    :else {:ok true}))

(defn snapshot-body
  [frame-id problem-id candidates ordering-options]
  (let [{:keys [ordered ordering]}
        (order-candidates candidates
                          (assoc ordering-options :problem-id problem-id))
        ordering (assoc ordering :base-file-blob
                        (:base-file-blob ordering-options))
        provenance-summary
        (when (some :provenance ordered)
          (->> ordered
               (keep #(get-in % [:provenance :frame-id]))
               frequencies
               (into (sorted-map))))]
    (cond-> {:snapshot/version 2
             :snapshot/frame-id frame-id
             :snapshot/problem-id problem-id
             :snapshot/review-policy :persisted-independent-review
             :snapshot/ordering ordering
             :snapshot/memories ordered}
      (seq (:lineage ordering-options))
      (assoc :snapshot/lineage (vec (:lineage ordering-options)))
      provenance-summary
      (assoc :snapshot/provenance-summary provenance-summary))))

(defn candidate-visible?
  "Freshly verify that CANDIDATE describes the current reviewed attachment and
  its independently authored persisted review evidence."
  ([candidate]
   (let [backend (f1b/make-futon1b-backend (substrate/configured-url))]
     (candidate-visible? candidate
                         #(substrate/hyperedges-by-end
                           % {:limit 10 :timeout-ms 5000 :request-budget 2})
                         #(estore/get-entry* backend %))))
  ([{:keys [memory-id depositor reviewer review-evidence-id
            attachment-status pattern-ids]}
    fetch-hyperedges fetch-entry]
   (let [edge (->> (fetch-hyperedges memory-id)
                   (filter #(= :memory/assert (:hx/type %)))
                   (filter #(= :current (get-in % [:hx/props :state])))
                   first)
         memory (fetch-entry memory-id)
         review (fetch-entry review-evidence-id)
         review-body (:evidence/body review)]
     (and edge memory review
          (reviewed? attachment-status)
          (reviewed? (get-in edge [:hx/props :attachment-status]))
          (= (set pattern-ids)
             (set (get-in edge [:hx/props :roles :patterns])))
          (= review-evidence-id
             (get-in edge [:hx/props :review :evidence-id]))
          (= depositor (:evidence/author memory))
          (= reviewer (:evidence/author review))
          (not= depositor reviewer)
          (nonblank? (:review/reason review-body))
          (nonblank? (:review/residual review-body))
          (= memory-id (get-in review [:evidence/subject :ref/id]))))))

(defn publish!
  "Validate CANDIDATES, publish one immutable EDN snapshot atomically, and
  verify it by a fresh read. Existing identical content is an idempotent replay;
  existing different content fails closed."
  [{:keys [frame-id problem-id candidates path evidence-visible?
           base-text base-file-blob text-fn lineage kind-stratification]}]
  (let [validations (mapv validate-candidate candidates)
        ;; Every visibility check is an independent, bounded substrate read.
        ;; Realize them concurrently so the publication latency is bounded by
        ;; the slowest candidate rather than by the size of an inherited
        ;; snapshot. Preserve fail-closed membership and deterministic order.
        visibility (when (fn? evidence-visible?)
                     (doall (pmap evidence-visible? candidates)))
        visibility-failure
        (some (fn [[candidate observation]]
                (when (and (map? observation)
                           (= :not-obtained (:transport/evidence observation)))
                  (assoc observation :memory-id (:memory-id candidate))))
              (map vector candidates visibility))
        invisible (when visibility
                    (->> (map vector candidates visibility)
                         (keep (fn [[candidate observation]]
                                 (let [visible? (if (map? observation)
                                                  (:visible? observation)
                                                  observation)]
                                   (when (false? visible?)
                                     (:memory-id candidate)))))
                         vec))
        body (snapshot-body frame-id problem-id candidates
                            {:base-text base-text
                             :base-file-blob base-file-blob
                             :lineage lineage
                             :kind-stratification kind-stratification
                             :text-fn (or text-fn (evidence-text-fn))})
        digest (machine/ledger-digest [body])
        snapshot (assoc body :snapshot/id digest :snapshot/digest digest)
        target (Path/of (str path) (make-array String 0))]
    (cond
      (or (not (nonblank? frame-id)) (not (nonblank? problem-id))
          (not (vector? candidates)))
      {:ok false :error/code :memory-snapshot-input-invalid}
      (some (complement :ok) validations)
      {:ok false :error/code :memory-snapshot-candidate-invalid
       :findings (mapv :finding (remove :ok validations))}
      visibility-failure
      (merge {:ok false :error/code :memory-snapshot-visibility-not-obtained
              :error/component :transport
              :visibility/candidate-count (count candidates)
              :visibility/execution :parallel
              :visibility/per-read-bound-ms visibility-read-bound-ms
              :visibility/aggregate-bound-ms
              (* (count candidates) visibility-read-bound-ms)}
             visibility-failure)
      (seq invisible)
      {:ok false :error/code :memory-snapshot-review-not-visible
       :memory-ids invisible}
      (Files/exists target (make-array java.nio.file.LinkOption 0))
      (let [existing (edn/read-string (slurp (.toFile target)))]
        (if (= snapshot existing)
          {:ok true :snapshot snapshot :path (str target) :idempotent? true}
          {:ok false :error/code :memory-snapshot-existing-mismatch}))
      :else
      (let [parent (or (.getParent target)
                       (Path/of "." (make-array String 0)))
            _ (Files/createDirectories parent (make-array FileAttribute 0))
            tmp (Files/createTempFile parent ".memory-snapshot-"
                                      ".edn" (make-array FileAttribute 0))]
        (try
          (spit (.toFile tmp) (str (pr-str snapshot) "\n"))
          (Files/move tmp target
                      (into-array StandardCopyOption
                                  [StandardCopyOption/ATOMIC_MOVE]))
          (let [observed (edn/read-string (slurp (.toFile target)))]
            (if (= snapshot observed)
              {:ok true :snapshot snapshot :path (str target)
               :idempotent? false}
              {:ok false :error/code :memory-snapshot-postcondition-failed}))
          (finally
            (Files/deleteIfExists tmp)))))))

(defn publish-cumulative!
  "Publish OWN-CANDIDATES on top of the campaign-ordered PRIOR-CANDIDATES.
  Prior candidates are deduplicated first (earliest carrier wins), then stale prior
  reviews are dropped with an explicit account. Own candidates remain subject
  to publish!'s fail-closed validation and visibility boundary."
  [{:keys [prior-candidates own-candidates evidence-visible?] :as args}]
  (let [evidence-visible? (when (fn? evidence-visible?)
                            (memoize
                             (fn [candidate]
                               (try
                                 {:visible? (boolean (evidence-visible? candidate))
                                  :transport/acquired-outcome :success
                                  :transport/classified-outcome :success
                                  :transport/evidence :obtained}
                                 (catch Throwable t
                                   (let [message (or (.getMessage t) "")
                                         outcome (or (:transport/acquired-outcome
                                                      (ex-data t))
                                                     (if (re-find #"(?i)timeout"
                                                                  message)
                                                       :timeout :unavailable))]
                                     {:visible? nil
                                      :transport/acquired-outcome outcome
                                      :transport/classified-outcome outcome
                                      :transport/evidence :not-obtained
                                      :error/message message}))))))
        args (assoc args :evidence-visible? evidence-visible?)
        origin-valid?
        (fn [candidate]
          (let [provenance (:provenance candidate)
                depositor-frame
                (some->> (:depositor candidate)
                         (re-matches #"^(f[0-9]+)-.+$") second)]
            (and (map? provenance)
                 (every? nonblank?
                         ((juxt :campaign-id :frame-id :problem-id)
                          provenance))
                 (= depositor-frame (:frame-id provenance)))))
        prior (reduce (fn [ordered candidate]
                        (if (some #(= (:memory-id candidate) (:memory-id %))
                                  ordered)
                          ordered
                          (conj ordered candidate)))
                      [] prior-candidates)
        ;; Visibility is independent per immutable candidate. Realize these
        ;; bounded reads concurrently, then let both the prior filter and the
        ;; final fail-closed publication consume the same memoized answers.
        _ (when evidence-visible?
            (dorun (pmap evidence-visible?
                         (reduce (fn [ordered candidate]
                                   (if (some #(= (:memory-id candidate)
                                                (:memory-id %))
                                             ordered)
                                     ordered
                                     (conj ordered candidate)))
                                 prior own-candidates))))
        inspected
        (mapv (fn [candidate]
                (let [shape (validate-candidate candidate)
                      observation (when (and (:ok shape)
                                             (fn? evidence-visible?))
                                    (evidence-visible? candidate))
                      visible? (and (:ok shape)
                                    (or (nil? observation)
                                        (true? (:visible? observation))))]
                  (cond
                    (not (:ok shape))
                    {:candidate candidate :finding (:finding shape)}
                    (not visible?)
                    {:candidate candidate :finding :snapshot-review-not-visible}
                    :else {:candidate candidate})))
              prior)
        retained (mapv :candidate (remove :finding inspected))
        dropped (mapv (fn [{:keys [candidate finding]}]
                        {:memory-id (:memory-id candidate)
                         :provenance (:provenance candidate)
                         :finding finding})
                      (filter :finding inspected))
        merged (reduce (fn [ordered candidate]
                         (if (some #(= (:memory-id candidate) (:memory-id %))
                                   ordered)
                           ordered
                           (conj ordered candidate)))
                       retained own-candidates)
        invalid-origins (mapv :memory-id (remove origin-valid? merged))
        visibility-candidates
        (reduce (fn [ordered candidate]
                  (if (some #(= (:memory-id candidate) (:memory-id %)) ordered)
                    ordered (conj ordered candidate)))
                prior own-candidates)
        visibility-failure
        (some (fn [candidate]
                (let [observation (when (fn? evidence-visible?)
                                    (evidence-visible? candidate))]
                  (when (= :not-obtained (:transport/evidence observation))
                    (assoc observation :memory-id (:memory-id candidate)))))
              visibility-candidates)
        published (when (and (empty? invalid-origins)
                             (nil? visibility-failure))
                    (publish! (assoc args :candidates merged)))]
    (if (seq invalid-origins)
      {:ok false :error/code :memory-snapshot-provenance-invalid
       :memory-ids invalid-origins}
      (if visibility-failure
        (merge {:ok false :error/code :memory-snapshot-visibility-not-obtained
                :error/component :transport
                :transport/operation :post-publication-verification
                :visibility/candidate-count (count visibility-candidates)
                :visibility/execution :parallel
                :visibility/per-read-bound-ms visibility-read-bound-ms
                :visibility/aggregate-bound-ms
                (* (count visibility-candidates) visibility-read-bound-ms)}
               visibility-failure)
        (cond-> published
          (:ok published) (assoc :prior-dropped dropped
                                 :visibility/candidate-count (count merged)
                                 :visibility/execution :parallel
                                 :visibility/per-read-bound-ms
                                 visibility-read-bound-ms
                                 :visibility/aggregate-bound-ms
                                 (* (count merged)
                                    visibility-read-bound-ms)))))))

(defn verify-student-access
  [{:keys [path expected frame-id problem-id accessible-memory-ids]}]
  (try
    (let [observed (edn/read-string (slurp (str path)))
          body (dissoc observed :snapshot/id :snapshot/digest)
          digest (machine/ledger-digest [body])
          expected-ids (set (map :memory-id (:snapshot/memories observed)))
          findings (cond-> []
                     (not= frame-id (:snapshot/frame-id observed))
                     (conj :snapshot-frame-mismatch)
                     (not= problem-id (:snapshot/problem-id observed))
                     (conj :snapshot-problem-mismatch)
                     (not= digest (:snapshot/digest observed))
                     (conj :snapshot-content-mismatch)
                     (not= expected (:snapshot/digest observed))
                     (conj :snapshot-expected-digest-mismatch)
                     (not= expected-ids (set accessible-memory-ids))
                     (conj :student-access-set-mismatch))]
      (if (seq findings)
        {:ok false :error/code :student-memory-access-invalid :findings findings}
        {:ok true :snapshot observed :accessible-memory-ids expected-ids}))
    (catch Throwable t
      {:ok false :error/code :student-memory-snapshot-unreadable
       :finding {:message (.getMessage t)}})))
