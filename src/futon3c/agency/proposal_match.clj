(ns futon3c.agency.proposal-match
  "Conservative structural comparison of pattern-level flexiarg texts.

   Parsing delegates section grammar to futon.flexiarg.projection, the canonical
   flexiarg parser.  Exact normalised equality is a safety floor: it can prove
   identical structure, but it does not interpret paraphrases as merges."
  (:require [clojure.string :as str]
            [futon.flexiarg.projection :as flexiarg])
  (:import [java.text Normalizer Normalizer$Form]
           [java.util Locale]))

;; :conclusion is the pattern's claim. Without it, the 256 generated
;; iiching/exotype-* records (one template's clauses, differing only in their
;; conclusion and encoding) matched each other as 16,932 exact "merges".
(def ^:private structural-fields [:conclusion :if :however :then :because :scope])

(defn- components-by-name [text]
  (let [components (flexiarg/parse-components text)
        conclusion (some (fn [{:keys [name-key text]}]
                           (when (and (contains? flexiarg/conclusion-aliases name-key)
                                      (not (str/blank? text)))
                             text))
                         components)]
    (cond-> (into {}
                  (keep (fn [{:keys [name-key text]}]
                          (when-not (str/blank? text)
                            [(keyword name-key) text])))
                  components)
      conclusion (assoc :conclusion conclusion))))

(defn flexiarg->clauses
  "Parse one pattern-level flexiarg TEXT into its id and structural clauses.
   Missing clauses are absent, rather than represented by empty strings.  The
   section syntax is supplied by futon.flexiarg.projection/parse-components."
  [text]
  (let [components (components-by-name text)
        id (or (flexiarg/extract-meta text "flexiarg")
               (flexiarg/extract-meta text "arg")
               (flexiarg/extract-meta text "multiarg"))]
    (cond-> {}
      (not (str/blank? id)) (assoc :id id)
      true (into (keep (fn [field]
                         (when-let [value (get components field)]
                           [field value])))
                 structural-fields))))

(defn normalise
  "NFKC-normalise S, case-fold with Locale/ROOT, collapse whitespace, and trim
   terminal punctuation.  Words and internal punctuation are never removed."
  [s]
  (when (string? s)
    (-> (Normalizer/normalize s Normalizer$Form/NFKC)
        (.toLowerCase Locale/ROOT)
        (str/replace #"\s+" " ")
        str/trim
        (str/replace #"[\p{P}\s]+$" "")
        str/trim)))

(defn- field-result [field a b]
  (let [a? (contains? a field)
        b? (contains? b field)]
    (cond
      (and (= :scope field) (not a?) (not b?)) :same
      (or (not a?) (not b?)) :missing
      (= (normalise (get a field)) (normalise (get b field))) :same
      :else :different)))

(defn match
  "Compare two pattern-level clause maps.  Id is deliberately ignored.

   All six fields (conclusion, IF, HOWEVER, THEN, BECAUSE, scope) equal =>
   :merge.  Any difference => :adjacent.  Otherwise a
   missing field => :uncomparable.  Scope absent on both sides is equal."
  [a b]
  (let [fields (into (array-map)
                     (map (fn [field] [field (field-result field a b)]))
                     structural-fields)
        statuses (set (vals fields))
        verdict (cond
                  (contains? statuses :different) :adjacent
                  (contains? statuses :missing) :uncomparable
                  :else :merge)]
    {:verdict verdict :fields fields}))

(defn corpus-report
  "Compare every unordered pair in CLAUSE-MAPS.  Retain all same-id pairs and
   every cross-id merge, plus aggregate counts for a bounded corpus report."
  [clause-maps]
  (let [items (vec clause-maps)
        n (count items)]
    (loop [i 0
           report {:patterns n :pairs-compared 0
                   :same-id [] :cross-id-merges []}]
      (if (>= i n)
        (assoc report
               :merge-count (count (:cross-id-merges report))
               :same-id-adjacent-count
               (count (filter #(= :adjacent (get-in % [:match :verdict]))
                              (:same-id report))))
        (recur
         (inc i)
         (loop [j (inc i) acc report]
           (if (>= j n)
             acc
             (let [a (nth items i)
                   b (nth items j)
                   result (match a b)
                   pair {:left-index i :right-index j
                         :left-id (:id a) :right-id (:id b)
                         :match result}
                   same-id? (and (some? (:id a)) (= (:id a) (:id b)))
                   cross-merge? (and (not= (:id a) (:id b))
                                     (= :merge (:verdict result)))]
               (recur (inc j)
                      (cond-> (update acc :pairs-compared inc)
                        same-id? (update :same-id conj pair)
                        cross-merge? (update :cross-id-merges conj pair)))))))))))
