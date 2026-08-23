(ns futon3c.apm.promotion-pipeline
  "Pure gates for Scribe deposit -> independent Proctor review -> snapshot."
  (:require [clojure.string :as str]))

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

(defn- valid-lane? [{:keys [lane status reason]}]
  (and (contains? required-lanes lane)
       (contains? lane-statuses status)
       (or (= :ran status)
           (and (string? reason) (not (str/blank? reason))))))

(defn validate-deposit [{:keys [depositor candidates lanes]}]
  (let [findings (cond-> []
                   (not (string? depositor)) (conj :depositor-missing)
                   (not (and (vector? candidates) (seq candidates)))
                   (conj :candidates-missing)
                   (some #(not (and (string? (:memory-id %))
                                    (string? (:content-digest %))
                                    (vector? (:pattern-ids %)))) candidates)
                   (conj :candidate-shape-invalid)
                   ;; A candidate with no bound pattern cannot be reviewed for
                   ;; coherent fit and is rejected downstream regardless of
                   ;; content (f27: 3 of 3 rejected pattern-attachment-missing).
                   ;; Gate it here, where the depositor can still repair it.
                   (some #(and (vector? (:pattern-ids %))
                               (not (seq (:pattern-ids %)))) candidates)
                   (conj :candidate-patterns-missing)
                   (not (and (vector? lanes)
                             (= required-lanes (set (map :lane lanes)))
                             (every? valid-lane? lanes)))
                   (conj :lane-report-invalid))]
    (if (seq findings) {:ok false :findings findings}
        {:ok true :candidates (dedupe-candidates candidates)})))

(defn validate-review [deposit reviewer reviews]
  (let [depositor (:depositor deposit)
        candidates (:candidates (validate-deposit deposit))
        by-id (into {} (map (juxt :memory-id identity)) candidates)
        approved (filterv #(contains? #{:approve :reassign} (:verdict %)) reviews)
        findings (cond-> []
                   (not (string? reviewer)) (conj :reviewer-missing)
                   (= depositor reviewer) (conj :reviewer-is-depositor)
                   (not= (set (keys by-id)) (set (map :memory-id reviews)))
                   (conj :review-set-mismatch)
                   (some #(not= reviewer (:reviewer %)) reviews)
                   (conj :review-attribution-mismatch)
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
