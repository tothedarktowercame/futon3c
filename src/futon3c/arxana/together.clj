(ns futon3c.arxana.together
  "Pure codepoint text integration. Tombstones and insertion attributes persist.
  Character order compares Lamport counter first, then editor (String.compareTo).
  Callers deliver operations causally; inserts are not retry/idempotency messages.")

(def empty-doc "An empty document, including its tombstones." [])

(defn compare-ids
  "Compare [editor counter] ids by counter, then editor."
  [[a n] [b m]]
  (let [c (compare n m)] (if (zero? c) (compare a b) c)))

(defn- id-shape? [id]
  (and (vector? id) (= 2 (count id))
       (string? (first id)) (not (empty? (first id)))))

(defn- valid-id? [id]
  (and (id-shape? id) (integer? (second id)) (pos? (second id))))

(defn- codepoints [^String s]
  (vec (.toArray (.codePoints s))))

(defn check-op
  "Return nil or a refusal keyword, without changing DOC.
  Identity authenticates an insert's editor. Malformed shapes are refused before
  semantic checks. All generated ids, including tombstoned ones, must be unused."
  [doc op identity]
  (let [known (set (map :id doc))]
    (if-not (map? op)
      :invalid-op
      (case (:op op)
        :insert
        (let [{:keys [id after text attrs]} op]
          (cond
            (not (id-shape? id)) :invalid-id
            (not (and (integer? (second id)) (pos? (second id)))) :invalid-counter
            (not= (first id) identity) :wrong-author
            (not (string? text)) :invalid-text
            (empty? text) :empty-text
            (not (map? attrs)) :invalid-attrs
            (and (some? after) (not (contains? known after))) :unknown-after
            (and after (<= (second id) (second after))) :counter-not-after
            (some #(contains? known [(first id) (+' (second id) %)])
                  (range (count (codepoints text)))) :id-in-use
            :else nil))
        :delete
        (cond
          (not (vector? (:ids op))) :invalid-ids
          (not-every? valid-id? (:ids op)) :invalid-id
          (some #(not (contains? known %)) (:ids op)) :unknown-id
          :else nil)
        :invalid-op))))

(defn- insert-character [doc after character]
  (let [start (if (nil? after) 0
               (inc (first (keep-indexed #(when (= (:id %2) after) %1) doc))))
        at (loop [i start]
             (if (and (< i (count doc))
                      (pos? (compare-ids (:id (nth doc i)) (:id character))))
               (recur (inc i)) i))]
    (into (conj (subvec doc 0 at) character) (subvec doc at))))

(defn apply-op
  "Integrate OP into DOC, or throw ex-info with :together/error.
  This replay/client operation checks structure, not connection identity; submit
  performs that authentication. Deletion is idempotent, insertion is not."
  [doc op]
  (if-let [error (check-op doc op (when (id-shape? (:id op)) (first (:id op))))]
    (throw (ex-info "Invalid Together operation" {:together/error error :op op}))
    (case (:op op)
      :insert
      (let [{:keys [id after text attrs]} op]
        (first
         (reduce (fn [[d left] [k cp]]
                   (let [next-id [(first id) (+' (second id) k)]]
                     [(insert-character d left {:id next-id :cp cp :attrs attrs :deleted? false})
                      next-id]))
                 [doc after] (map-indexed vector (codepoints text)))))
      :delete
      (let [deleted (set (:ids op))]
        (mapv #(if (contains? deleted (:id %)) (assoc % :deleted? true) %) doc)))))

(defn text
  "Visible Unicode text; counts and insertion ids use codepoints, not UTF-16."
  [doc]
  (apply str (map #(String. (Character/toChars (int (:cp %))))
                  (remove :deleted? doc))))

(defn ids "Visible character ids in document order." [doc]
  (mapv :id (remove :deleted? doc)))

(defn next-counter
  "One above the largest counter, including tombstones; 1 for an empty doc."
  [doc]
  (inc' (reduce max 0 (map (comp second :id) doc))))

(defn submit
  "Pure hub submission; refuses invalid operations or a non-existent parent rev.
  AT is caller-supplied event metadata; this engine neither reads a clock nor
  interprets timestamps. Refusal returns only {:error kw}, leaving HUB untouched."
  [hub op identity parents-rev at]
  (if-let [error (or (check-op (:doc hub) op identity)
                     (when-not (and (integer? parents-rev)
                                    (<= 0 parents-rev (count (:log hub))))
                       :invalid-parents))]
    {:error error}
    (let [entry {:rev (inc (count (:log hub))) :op op :author identity
                 :parents {:rev parents-rev} :at at}]
      {:hub {:doc (apply-op (:doc hub) op) :log (conj (:log hub) entry)}
       :entry entry})))

(defn rebuild
  "Rebuild a hub document from its entries, independent of their input order."
  [log]
  (reduce (fn [doc entry] (apply-op doc (:op entry))) empty-doc (sort-by :rev log)))

(defn runs
  "Compress the whole ordered document, tombstones included. A run extends only
  across equal attrs/deletion flags and consecutive ids of the same editor."
  [doc]
  (first
   (reduce
    (fn [[out previous] {:keys [id cp attrs deleted?] :as character}]
      (let [piece (String. (Character/toChars (int cp)))
            contiguous? (and previous (= (first id) (first (:id previous)))
                             (= (second id) (inc' (second (:id previous))))
                             (= attrs (:attrs previous)) (= deleted? (:deleted? previous)))]
        [(if contiguous?
           (update-in out [(dec (count out)) :text] str piece)
           (conj out {:id id :text piece :attrs attrs :deleted deleted?})) character]))
    [[] nil] doc)))

(defn from-runs
  "Expand snapshot runs in their supplied order; never integrate them as edits.
  Reject malformed runs or reused ids with :together/error."
  [rs]
  (when-not (vector? rs)
    (throw (ex-info "Invalid runs" {:together/error :invalid-runs})))
  (reduce
   (fn [doc {:keys [id text attrs deleted] :as run}]
     (let [op {:op :insert :id id :after nil :text text :attrs attrs}
           error (or (when-not (boolean? deleted) :invalid-run)
                     (check-op doc op (when (id-shape? id) (first id))))]
       (when error (throw (ex-info "Invalid run" {:together/error error :run run})))
       (into doc (map-indexed (fn [k cp] {:id [(first id) (+' (second id) k)]
                                         :cp cp :attrs attrs :deleted? deleted})
                             (codepoints text)))))
   [] rs))
