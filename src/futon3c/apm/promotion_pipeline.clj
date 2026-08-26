(ns futon3c.apm.promotion-pipeline
  "Pure gates for Scribe deposit -> independent Proctor review -> snapshot."
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn candidate-key [candidate]
  [(:content-digest candidate) (vec (sort (:pattern-ids candidate)))])

(defn dedupe-candidates
  "Deduplicate repeated discoveries while retaining every source attempt."
  [candidates]
  (->> candidates
       (group-by candidate-key)
       (mapv (fn [[_ xs]]
               (assoc (first xs) :source-attempts
                      (vec (sort (distinct (mapcat :source-attempts xs)))))))
       (sort-by candidate-key)
       vec))

(def required-lanes #{:solve :arc :trajectory :challenge})
(def lane-statuses #{:ran :ran-empty :not-run})
(def review-verdicts #{:approve :reassign :reject :cannot-judge})
(def mechanical-reviewer "promotion-mechanical-guard")

(defn- occurrence-count [s needle]
  (loop [from 0 n 0]
    (let [at (.indexOf ^String s ^String needle from)]
      (if (neg? at) n (recur (+ at (count needle)) (inc n))))))

(defn proof-text?
  "Classify proof text from controller-observed content, never from an agent's
  :kind assertion.  The certified source is optional; when present, copying a
  whole declaration from it is proof text in addition to the byte/block
  limits."
  [candidate solver-certified-source]
  (let [body (str (or (:body candidate) ""))
        declaration-names
        (when (string? solver-certified-source)
          (set (map second
                    (re-seq #"(?m)^\s*(?:lemma|theorem|def)\s+([A-Za-z0-9_'.]+)"
                            solver-certified-source))))
        copies-declaration?
        (some #(re-find (re-pattern
                         (str "(?m)^\\s*(?:lemma|theorem|def)\\s+"
                              (java.util.regex.Pattern/quote %) "\\b"))
                        body)
              declaration-names)]
    (boolean
     (or (> (occurrence-count body ":= by") 3)
         (> (alength (.getBytes body java.nio.charset.StandardCharsets/UTF_8))
            4096)
         copies-declaration?))))

(defn mechanical-candidate-findings
  "Cheap deterministic guards before independent LLM review. Provenance may
  name the problem; reusable hook/body text may not."
  [candidate {:keys [problem-id solver-certified-source]}]
  (let [hook (str (or (:hook candidate) ""))
        body (str (or (:body candidate) ""))
        text (str hook "\n" body)
        pid (some-> problem-id str/lower-case)
        lower (str/lower-case text)
        proof-text? (proof-text? candidate solver-certified-source)]
    (cond-> []
      proof-text?
      (conj :proof-text-not-memory)
      (or (and pid (not (str/blank? pid)) (str/includes? lower pid))
          (and pid (str/includes? lower (str "apm_" pid "_")))
          (re-find #"(?i)Main\.lean:\d+" text))
      (conj :problem-identifier-in-body)
      (not (seq (:pattern-ids candidate)))
      (conj :no-parent-pattern))))

(defn- apply-mechanical-reviews [candidates context]
  (let [deduped (dedupe-candidates candidates)
        rejected (keep (fn [candidate]
                         (when-let [codes (seq
                                           (mechanical-candidate-findings
                                            candidate context))]
                           {:memory-id (:memory-id candidate)
                            :reviewer mechanical-reviewer
                            :verdict :reject
                            :reason (str "mechanical rejection: "
                                         (str/join ", " (map name codes)))
                            :residual "revise into a reusable, witnessed pattern memory"
                            :finding-codes (vec codes)}))
                       deduped)
        rejected-ids (set (map :memory-id rejected))]
    {:candidates (vec (remove #(contains? rejected-ids (:memory-id %)) deduped))
     :mechanical-reviews (vec rejected)}))

(defn- valid-lane? [{:keys [lane status reason]}]
  (and (contains? required-lanes lane)
       (contains? lane-statuses status)
       (or (= :ran status)
           (and (string? reason) (not (str/blank? reason))))))

(defn validate-deposit
  ([deposit] (validate-deposit deposit {}))
  ([{:keys [depositor candidates lanes]} context]
  (let [findings (cond-> []
                   (not (string? depositor)) (conj :depositor-missing)
                   (not (and (vector? candidates) (seq candidates)))
                   (conj :candidates-missing)
                   (some #(not (and (string? (:memory-id %))
                                    (string? (:content-digest %))
                                    (vector? (:pattern-ids %)))) candidates)
                   (conj :candidate-shape-invalid)
                   (not (and (vector? lanes)
                             (= required-lanes (set (map :lane lanes)))
                             (every? valid-lane? lanes)))
                   (conj :lane-report-invalid))]
    (if (seq findings) {:ok false :findings findings}
        (assoc (apply-mechanical-reviews candidates context) :ok true)))))

(defn validate-guide-deposit
  "Gate a Guide's store-mode candidates. The Guide is not a mining seat, so
  there is no lane report; every candidate must still name a bound pattern."
  ([deposit] (validate-guide-deposit deposit {}))
  ([{:keys [depositor candidates]} context]
  (let [findings (cond-> []
                   (not (string? depositor)) (conj :depositor-missing)
                   (not (and (vector? candidates) (seq candidates)))
                   (conj :candidates-missing)
                   (some #(not (and (string? (:memory-id %))
                                    (string? (:content-digest %))
                                    (vector? (:pattern-ids %))
                                    (vector? (:source-attempts %)))) candidates)
                   (conj :candidate-shape-invalid))]
    (if (seq findings) {:ok false :findings findings}
        (assoc (apply-mechanical-reviews candidates context) :ok true)))))

(declare validate-review*)

(defn publishing-review?
  "A merit verdict publishes only when its attachment projection succeeded."
  [review]
  (and (contains? #{:approve :reassign} (:verdict review))
       (not= false (:projection/valid? review))))

(defn- materialized? [artifact]
  (and (map? artifact)
       (every? #(and (string? %) (not (str/blank? %)))
               ((juxt :artifact-id :content-digest
                      :persisted-content-digest :read-back-content-digest
                      :persistence-receipt-id) artifact))
       (= (:content-digest artifact) (:persisted-content-digest artifact)
          (:read-back-content-digest artifact))))

(defn- exact-patterns? [expected actual]
  (and (= (count expected) (count actual))
       (= (set expected) (set actual))))

(defn- disposition-finding [candidate review]
  (let [projection-invalid? (= false (:projection/valid? review))
        verdict (:verdict review)
        status (:attachment-status review)]
    (cond
      (not (materialized? (:materialization candidate)))
      :candidate-not-materialized
      (not (materialized? (:review-materialization review)))
      :review-evidence-not-materialized
      (not (seq (:pattern-ids review)))
      :review-patterns-missing
      projection-invalid?
      :promotion-review-projection-failed
      (= :cannot-judge verdict) :promotion-pass-unresolved
      (= :approve verdict)
      (cond
        (not= :reviewed status) :approved-attachment-not-reviewed
        (not (exact-patterns? (:pattern-ids candidate)
                              (:pattern-ids review)))
        :approved-patterns-mismatch
        :else nil)
      (= :reassign verdict)
      (when-not (= :reviewed status) :reassigned-attachment-not-reviewed)
      (= :reject verdict)
      (when-not (= :proposed status) :rejected-attachment-not-proposed)
      :else :promotion-disposition-verdict-invalid)))

(defn validate-complete-dispositions
  "Require one materialized, persisted disposition for every dispatched
  candidate before snapshot publication. Projection failure and cannot-judge
  are apparatus holds, never candidate dispositions."
  [candidates reviews]
  (let [candidate-ids (mapv :memory-id candidates)
        review-ids (mapv :memory-id reviews)
        by-id (into {} (map (juxt :memory-id identity)) reviews)
        accounting? (and (= (count candidate-ids) (count (distinct candidate-ids)))
                         (= (count review-ids) (count (distinct review-ids)))
                         (= (set candidate-ids) (set review-ids)))
        findings
        (cond-> []
          (not accounting?)
          (conj {:finding :promotion-disposition-accounting-invalid
                 :candidate-ids candidate-ids :review-ids review-ids})
          accounting?
          (into (keep (fn [candidate]
                        (let [review (by-id (:memory-id candidate))]
                          (when-let [finding
                                     (disposition-finding candidate review)]
                            {:finding finding
                             :memory-id (:memory-id candidate)})))
                      candidates)))]
    (if (seq findings)
      {:ok false :error/code :promotion-pass-incomplete
       :findings findings}
      {:ok true
       :dispositions
       (mapv (fn [candidate]
               (let [review (by-id (:memory-id candidate))]
                 {:memory-id (:memory-id candidate)
                  :verdict (:verdict review)
                  :candidate-materialization (:materialization candidate)
                  :review-materialization (:review-materialization review)
                  :attachment-status (:attachment-status review)
                  :pattern-ids (:pattern-ids review)
                  :publishing? (publishing-review? review)}))
             candidates)})))

(defn validate-certified-promotion-pass
  "Bind a completed review pass to the freshly read-back snapshot before the
  phase receipt can certify it.  PRIOR snapshot members are excluded by the
  caller; PUBLISHED-CANDIDATES is exactly this pass's contribution."
  [dispositions snapshot snapshot-path published-candidates]
  (let [expected-ids (->> dispositions
                          (filter :publishing?)
                          (mapv :memory-id))
        published-ids (mapv :memory-id published-candidates)
        snapshot-id (:snapshot/id snapshot)
        snapshot-digest (:snapshot/digest snapshot)
        snapshot-materialization
        {:artifact-id snapshot-id
         :content-digest snapshot-digest
         :persisted-content-digest snapshot-digest
         :read-back-content-digest snapshot-digest
         :persistence-receipt-id snapshot-path}
        findings
        (cond-> []
          (not (materialized? snapshot-materialization))
          (conj :promotion-snapshot-not-materialized)
          (not= expected-ids published-ids)
          (conj :promotion-published-candidates-not-exact)
          (not= (count published-ids) (count (distinct published-ids)))
          (conj :promotion-published-candidate-duplicate))]
    (if (seq findings)
      {:ok false :error/code :certified-promotion-pass-invalid
       :findings findings :expected-memory-ids expected-ids
       :published-memory-ids published-ids}
      {:ok true
       :witness {:snapshot-materialization snapshot-materialization
                 :published-memory-ids published-ids
                 :dispositions dispositions}})))

(defn validate-review [deposit reviewer reviews]
  (validate-review* (:candidates (validate-deposit deposit))
                    (:depositor deposit) reviewer reviews))

(defn validate-review*
  "Validate an independent review against already-gated CANDIDATES."
  [candidates depositor reviewer reviews]
  (let [by-id (into {} (map (juxt :memory-id identity)) candidates)
        approved (filterv publishing-review? reviews)
        findings (cond-> []
                   (not (string? reviewer)) (conj :reviewer-missing)
                   (= depositor reviewer) (conj :reviewer-is-depositor)
                   (not= (set (keys by-id)) (set (map :memory-id reviews)))
                   (conj :review-set-mismatch)
                   (some #(not= reviewer (:reviewer %)) reviews)
                   (conj :review-attribution-mismatch)
                   (some #(not (contains? review-verdicts (:verdict %))) reviews)
                   (conj :review-verdict-invalid)
                   (some #(not (and (string? (:reason %))
                                    (not (str/blank? (:reason %)))
                                    (string? (:residual %))
                                    (not (str/blank? (:residual %)))))
                         reviews)
                   (conj :review-reasoning-missing)
                   (some #(and (publishing-review? %)
                               (not (and (string? (:review-evidence-id %))
                                         (= :reviewed (:attachment-status %))
                                         (seq (:pattern-ids %))))) reviews)
                   (conj :approved-review-evidence-invalid))]
    (if (seq findings) {:ok false :findings findings}
        {:ok true
         :candidates
         (mapv (fn [review]
                 (merge (by-id (:memory-id review))
                        {:depositor depositor}
                        (select-keys review [:reviewer :review-evidence-id
                                             :attachment-status :pattern-ids])))
               approved)})))

(defn validate-publication-accounting
  "Require an exact accounting between approved reviews and attached snapshot
  memories. Rejected candidates remain explicit in REVIEWS; approvals may not
  be silently filtered at any later boundary."
  [reviews snapshot-candidates]
  (let [approved-ids (->> reviews
                          (filter publishing-review?)
                          (map :memory-id)
                          set)
        attached-ids (set (map :memory-id snapshot-candidates))]
    (if (= approved-ids attached-ids)
      {:ok true :approved-memory-ids approved-ids}
      {:ok false :error/code :promotion-publication-accounting-invalid
       :approved-memory-ids approved-ids
       :attached-memory-ids attached-ids
       :missing-approved-memory-ids (set/difference approved-ids attached-ids)
       :unapproved-attached-memory-ids
       (set/difference attached-ids approved-ids)})))

(defn validate-extension-publication-accounting
  "Account for a Guide publication that preserves a prior reviewed snapshot
  and adds only candidates approved by the current independent review."
  [reviews prior-candidates snapshot-candidates]
  (let [approved-ids (->> reviews
                          (filter publishing-review?)
                          (map :memory-id)
                          set)
        prior-ids (set (map :memory-id prior-candidates))
        attached-ids (set (map :memory-id snapshot-candidates))
        expected-ids (set/union prior-ids approved-ids)]
    (if (= expected-ids attached-ids)
      {:ok true
       :prior-memory-ids prior-ids
       :approved-memory-ids approved-ids
       :attached-memory-ids attached-ids}
      {:ok false
       :error/code :promotion-extension-publication-accounting-invalid
       :prior-memory-ids prior-ids
       :approved-memory-ids approved-ids
       :attached-memory-ids attached-ids
       :missing-prior-memory-ids (set/difference prior-ids attached-ids)
       :missing-approved-memory-ids (set/difference approved-ids attached-ids)
       :unapproved-new-memory-ids (set/difference attached-ids expected-ids)})))
