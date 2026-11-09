(ns futon3c.arxana.workplan-backlinks
  "Small exact-form backlink index. Search is a build input, never a page read."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.arxana.records :as records])
  (:import [java.security MessageDigest]
           [java.math BigInteger]
           [java.util.regex Pattern]))

(def default-rules "resources/arxana/workplan/entity-backlink-rules.edn")
(def default-index "data/arxana/workplan/backlinks/entity-backlinks.edn")

(defn read-rules ([] (read-rules default-rules)) ([p] (edn/read-string (slurp p))))
(defn- sha256 [s]
  (format "%064x" (BigInteger. 1 (.digest (doto (MessageDigest/getInstance "SHA-256")
                                            (.update (.getBytes (str s) "UTF-8")))))))
(defn rule-records [rules]
  (mapv (fn [{:keys [entity/slug forms]}]
          (let [entity (records/make-clusion {:type-revision [:lexicon/entity 1] :endpoints []
                                               :asserted-content {:entity/slug slug}})
                rule (records/make-clusion {:type-revision [:workplan/entity-match-rule 1]
                                             :endpoints [{:role :entity :kind :clusion :target {:clusion/id (:id entity)}}]
                                             :asserted-content {:rule/id (str "entity-forms/" slug)}})
                att (records/make-attestation {:subject-clusion-id (:id rule) :producer "workplan/backlink-rules"
                                               :basis :human :method {:id "workplan/entity-forms" :version 1}
                                               :standing :verified :coverage :complete
                                               :content {:entity/slug slug :forms forms :rule/revision 1}})]
            {:entity entity :rule rule :attestation att})) (:rules rules)))

(defn materialise-rules! [dispatch! rules]
  (mapv (fn [{:keys [entity rule attestation]}]
          ;; The entity may be named by no workplan item (kangaroo-pouch is
          ;; not), so the rule's endpoint target is asserted here too;
          ;; assert is idempotent for entities the workplan load wrote.
          [(dispatch! {:protocol/version "arxana.v1" :message/id (str "backlink-entity:" (:id entity))
                       :operation :clusion/assert :principal/id "workplan/backlink-builder" :body {:record entity}})
           (dispatch! {:protocol/version "arxana.v1" :message/id (str "backlink-rule:" (:id rule))
                       :operation :clusion/assert :principal/id "workplan/backlink-builder" :body {:record rule}})
           (dispatch! {:protocol/version "arxana.v1" :message/id (str "backlink-rule-att:" (:id attestation))
                       :operation :evidence/append :principal/id "workplan/backlink-builder" :body {:record attestation}})])
        (rule-records rules)))

(defn- whole-word-pattern [form]
  (re-pattern (str "(?<![\\p{L}\\p{N}_])" (Pattern/quote form) "(?![\\p{L}\\p{N}_])")))
(defn- cp [^String s n] (.codePointCount s 0 n))
(defn- cp-subs [^String s a b]
  (let [start (.offsetByCodePoints s 0 a) end (.offsetByCodePoints s 0 b)]
    (.substring s start end)))
(defn- occurrences [^String text form]
  (let [m (re-matcher (whole-word-pattern form) text)]
    (loop [out []]
      (if (.find m) (recur (conj out [(cp text (.start m)) (cp text (.end m))])) out))))

(defn build-index
  "Pure builder over recorded text-search responses (maps by exact form)."
  [rules responses]
  (let [enabled (for [{:keys [entity/slug forms]} (:rules rules)
                      {:keys [form/text enabled?]} forms :when enabled?]
                  [slug text])
        rows (for [[slug form] enabled
                   :let [response (get responses form)]
                   result (:results response)
                   :let [entry (:entry result) body (:evidence/body entry)
                         text (:text body) ranges (occurrences (or text "") form)]
                   :when (seq ranges)
                   range ranges]
               {:entity/slug slug :form form :evidence/id (:evidence/id entry)
                :role (:role body) :session/id (or (:session-id body) (:evidence/session-id entry))
                :time (or (:at body) (:evidence/at entry)) :codepoint-range range
                :snippet (let [[a b] range n (.codePointCount ^String text 0 (.length ^String text))
                               lo (max 0 (- a 45)) hi (min n (+ b 45))]
                           (cp-subs text lo hi))})
        stats (into {} (for [[slug form] enabled :let [rs (:results (get responses form))
                                                       kept (filter #(seq (occurrences (get-in % [:entry :evidence/body :text] "") form)) rs)]]
                         [form {:entity/slug slug :hits (count rs) :kept (count kept)
                                :dropped/metadata-only (- (count rs) (count kept))
                                :occurrences (reduce + (map #(count (occurrences (get-in % [:entry :evidence/body :text] "") form)) kept))}]))
        as-of (into (sorted-set) (keep #(get-in responses [% :index-as-of]) (map second enabled)))]
    {:format/version 1 :rules/hash (sha256 (pr-str rules)) :index/as-of (vec as-of)
     :stats stats :backlinks (vec (sort-by (juxt :entity/slug :time :evidence/id :codepoint-range) rows))}))

(defn write-index! [path index]
  (.mkdirs (.getParentFile (io/file path))) (spit path (str (pr-str index) "\n")) index)
(defn recorded-responses [dir]
  (into {} (for [form ["Agency" "Giraffe" "kangaroo" "Kangaroo"]]
             [form (edn/read-string (slurp (str dir "/" form ".json")))])))

(defn- ok! [r] (or (:ok r) (throw (ex-info "public read failed" {:response r}))))
(defn- entity! [read! id] (:record (ok! (read! :entity/get {:entity/id id}))))
(defn entity-view [read! index-path {:keys [entity/slug query/limit]}]
  (let [rules (read-rules) rr (rule-records rules)
        available (mapv #(get-in % [:attestation :content :entity/slug]) rr)]
    (if-not slug
      {:result/status :complete :entities available}
      (let [{:keys [entity rule attestation]} (first (filter #(= slug (get-in % [:attestation :content :entity/slug])) rr))
            ;; Public reads prove the rule and entity are the stored records shown.
            entity (entity! read! (:id entity)) rule (entity! read! (:id rule))
            stored-att (entity! read! (:id attestation))
            index (edn/read-string (slurp index-path)) all (filterv #(= slug (:entity/slug %)) (:backlinks index))
            all (vec (sort-by :time #(compare %2 %1) all)) total (count all) limit (min 200 (max 1 (long (or limit 100))))
            shown (subvec all 0 (min limit total)) truncated? (< limit total)]
        (cond-> {:result/status (if truncated? :truncated :complete) :result/total total
                 :entity entity :rule rule :rule/attestation stored-att
                 :backlinks shown :dropped/metadata-only (reduce + (for [[_ s] (:stats index) :when (= slug (:entity/slug s))] (:dropped/metadata-only s)))
                 :index/as-of (:index/as-of index) :record/ids [(:id entity) (:id rule) (:id attestation)]}
          truncated? (assoc :result/truncation :limit))))))

(defn -main [& [responses-dir output]]
  (let [rules (read-rules) index (build-index rules (recorded-responses (or responses-dir "test/resources/arxana/backlinks")))]
    (write-index! (or output default-index) index)
    (println (pr-str (select-keys index [:rules/hash :index/as-of :stats])))))
