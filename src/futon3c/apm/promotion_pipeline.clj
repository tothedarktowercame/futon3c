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
(def review-verdicts #{:approve :reassign :reject})
(def mechanical-reviewer "promotion-mechanical-guard")

(defn- occurrence-count [s needle]
  (loop [from 0 n 0]
    (let [at (.indexOf ^String s ^String needle from)]
      (if (neg? at) n (recur (+ at (count needle)) (inc n))))))

(defn mechanical-candidate-findings
  "Cheap deterministic guards before independent LLM review. Provenance may
  name the problem; reusable hook/body text may not."
  [candidate {:keys [problem-id solver-certified-source]}]
  (let [hook (str (or (:hook candidate) ""))
        body (str (or (:body candidate) ""))
        text (str hook "\n" body)
        pid (some-> problem-id str/lower-case)
        lower (str/lower-case text)
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
    (cond-> []
      (or (> (occurrence-count body ":= by") 3)
          (> (alength (.getBytes body java.nio.charset.StandardCharsets/UTF_8))
             4096)
          copies-declaration?)
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

(defn validate-review [deposit reviewer reviews]
  (validate-review* (:candidates (validate-deposit deposit))
                    (:depositor deposit) reviewer reviews))

(defn validate-review*
  "Validate an independent review against already-gated CANDIDATES."
  [candidates depositor reviewer reviews]
  (let [by-id (into {} (map (juxt :memory-id identity)) candidates)
        approved (filterv #(contains? #{:approve :reassign} (:verdict %)) reviews)
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
                   (some #(and (contains? #{:approve :reassign} (:verdict %))
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
                          (filter #(contains? #{:approve :reassign}
                                              (:verdict %)))
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
                          (filter #(contains? #{:approve :reassign}
                                              (:verdict %)))
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
