(ns futon3c.util.edn-comment-preserving
  "Comment-preserving EDN rewrite helpers.

   This namespace performs targeted source edits against existing EDN text so
   comment lines and surrounding formatting remain intact. The current v0
   implementation supports the War Machine anchors substrate shape:

   - top-level map
   - keyed :anchors vector of maps identified by :id
   - keyed :coherence-evidence vector of maps identified by :id

   The writer verifies that the rewritten file parses back to the requested
   DATA, so semantic correctness remains the hard invariant while comment and
   layout preservation are maintained structurally."
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [rewrite-clj.zip :as z])
  (:import [java.nio.file AtomicMoveNotSupportedException CopyOption Files Paths StandardCopyOption]))

(defn- line-start-offsets
  [text]
  (vec (cons 0
             (map inc
                  (keep-indexed (fn [idx ch]
                                  (when (= ch \newline) idx))
                                text)))))

(defn- row-col->offset
  [line-starts row col]
  (+ (nth line-starts (dec row))
     (dec col)))

(defn- node-span
  [line-starts zloc]
  (let [{:keys [row col end-row end-col]} (meta (z/node zloc))]
    [(row-col->offset line-starts row col)
     (row-col->offset line-starts end-row end-col)]))

(defn- replace-span
  [text [start end] replacement]
  (str (subs text 0 start) replacement (subs text end)))

(defn- apply-edits
  [text edits]
  (reduce (fn [acc {:keys [span replacement]}]
            (replace-span acc span replacement))
          text
          (sort-by (comp first :span) > edits)))

(defn- ascend-map
  [zloc]
  (loop [loc zloc]
    (when loc
      (if (= :map (z/tag loc))
        loc
        (recur (z/up loc))))))

(defn- top-level-value-loc
  [root-loc k]
  (some-> (z/find-value root-loc z/next k) z/right))

(defn- map-value-loc
  [map-loc k]
  (some-> (z/find-value map-loc z/next k) z/right))

(defn- find-map-by-id
  [root-loc vector-key entry-id]
  (let [vector-loc (top-level-value-loc root-loc vector-key)
        id-loc (z/find-value vector-loc z/next entry-id)
        map-loc (ascend-map id-loc)]
    (when (= entry-id (:id (z/sexpr map-loc)))
      map-loc)))

(defn- render-edn
  [value]
  (str/replace (with-out-str (pp/pprint value))
               #"\n\z"
               ""))

(defn- indent-block
  [rendered indent]
  (str/replace rendered "\n" (str "\n" indent)))

(defn- spaces
  [n]
  (apply str (repeat n \space)))

(defn- map-entry-indent
  [map-loc]
  (or (some->> (z/string map-loc)
               (re-find #"\n([ \t]+):")
               second)
      "  "))

(defn- value-indent
  [value-loc]
  (let [{:keys [col]} (meta (z/node value-loc))]
    (spaces (dec col))))

(defn- map-entry-render
  [map-loc k value]
  (let [key-indent (map-entry-indent map-loc)
        key-str (pr-str k)
        continuation-indent (str key-indent
                                 (spaces (inc (count key-str))))
        rendered (render-edn value)]
    (str "\n"
         key-indent
         key-str
         " "
         (indent-block rendered continuation-indent))))

(defn- vector-item-indent
  [vector-loc]
  (or (some->> (z/string vector-loc)
               (re-find #"\n([ \t]+)\S")
               second)
      (let [{:keys [col]} (meta (z/node vector-loc))]
        (spaces col))))

(defn- vector-item-render
  [vector-loc value]
  (let [item-indent (vector-item-indent vector-loc)
        rendered (render-edn value)]
    (str "\n"
         item-indent
         (indent-block rendered item-indent))))

(defn- diff-map-edits
  [line-starts root-loc vector-key entry-id old-map new-map]
  (let [map-loc (find-map-by-id root-loc vector-key entry-id)]
    (when-not map-loc
      (throw (ex-info (str "entry " entry-id " not found under " vector-key)
                      {:vector-key vector-key
                       :entry-id entry-id})))
    (let [removed-keys (seq (remove #(contains? new-map %) (keys old-map)))]
      (when removed-keys
        (throw (ex-info "comment-preserving writer does not support removing keys"
                        {:entry-id entry-id
                         :removed-keys (vec removed-keys)}))))
    (let [changed-existing
          (keep (fn [[k old-value]]
                  (when (and (contains? new-map k)
                             (not= old-value (get new-map k)))
                    (let [value-loc (map-value-loc map-loc k)]
                      (when-not value-loc
                        (throw (ex-info "map value loc missing for existing key"
                                        {:entry-id entry-id :key k})))
                      {:span (node-span line-starts value-loc)
                       :replacement (indent-block (render-edn (get new-map k))
                                                  (value-indent value-loc))})))
                old-map)
          missing-keys
          (->> (keys new-map)
               (remove #(contains? old-map %)))
          append-edit
          (when (seq missing-keys)
            (let [[_ end] (node-span line-starts map-loc)]
              {:span [(- end 1) (- end 1)]
               :replacement (apply str (map #(map-entry-render map-loc % (get new-map %))
                                            missing-keys))}))]
      (cond-> (vec changed-existing)
        append-edit (conj append-edit)))))

(defn- vector-append-edits
  [line-starts root-loc vector-key old-items new-items]
  (when-not (and (<= (count old-items) (count new-items))
                 (= old-items (subvec (vec new-items) 0 (count old-items))))
    (throw (ex-info "comment-preserving writer currently supports append-only vector growth"
                    {:vector-key vector-key
                     :old-count (count old-items)
                     :new-count (count new-items)})))
  (let [appended (subvec (vec new-items) (count old-items))
        vector-loc (top-level-value-loc root-loc vector-key)]
    (if (seq appended)
      (let [[_ end] (node-span line-starts vector-loc)]
        [{:span [(- end 1) (- end 1)]
          :replacement (apply str (map #(vector-item-render vector-loc %) appended))}])
      [])))

(defn- write-text-atomically!
  [path text]
  (let [target (Paths/get path (make-array String 0))
        parent (or (.getParent target)
                   (Paths/get "." (make-array String 0)))
        tmp (Files/createTempFile parent "edn-comment-preserving-" ".tmp"
                                  (make-array java.nio.file.attribute.FileAttribute 0))]
    (spit (.toFile tmp) text)
    (try
      (Files/move tmp target
                  (into-array CopyOption
                              [StandardCopyOption/REPLACE_EXISTING
                               StandardCopyOption/ATOMIC_MOVE]))
      (catch AtomicMoveNotSupportedException _
        (Files/move tmp target
                    (into-array CopyOption
                                [StandardCopyOption/REPLACE_EXISTING]))))))

(defn append-items-to-top-level-vector-preserving-comments
  "Append ITEMS to the top-level vector at VECTOR-KEY in PATH while preserving
   surrounding comments and layout.

   Constraints:
   - VECTOR-KEY must already exist and point to a vector
   - append-only growth only; existing items are untouched
   - resulting file must parse back to the requested data

   This is the generic append path used by append-only audit substrates such as
   VSATARCS `:bilateral-evidence`."
  [path vector-key items]
  (let [original-text (slurp path)
        original-data (edn/read-string original-text)
        original-items (vec (get original-data vector-key))
        _ (when-not (vector? (get original-data vector-key))
            (throw (ex-info "top-level key is not an existing vector"
                            {:path path :vector-key vector-key})))
        requested-data (update original-data vector-key
                               (fn [xs] (vec (concat xs items))))
        requested-items (vec (get requested-data vector-key))
        line-starts (line-start-offsets original-text)
        root-loc (z/of-string original-text)
        edits (vector-append-edits line-starts root-loc vector-key
                                   original-items requested-items)
        rewritten-text (apply-edits original-text edits)
        rewritten-data (edn/read-string rewritten-text)]
    (when-not (= rewritten-data requested-data)
      (throw (ex-info "comment-preserving vector append did not reach requested data"
                      {:path path
                       :vector-key vector-key
                       :item-count (count items)})))
    (write-text-atomically! path rewritten-text)
    {:ok true
     :path path
     :vector-key vector-key
     :appended-count (count items)}))

(defn write-edn-preserving-comments
  "Rewrite PATH so it semantically becomes DATA while preserving comments and
   surrounding layout for the War Machine anchors substrate."
  [path data]
  (let [original-text (slurp path)
        original-data (edn/read-string original-text)
        line-starts (line-start-offsets original-text)
        root-loc (z/of-string original-text)
        old-anchors (vec (:anchors original-data))
        new-anchors (vec (:anchors data))
        old-anchor-by-id (into {} (map (juxt :id identity)) old-anchors)
        new-anchor-by-id (into {} (map (juxt :id identity)) new-anchors)
        changed-anchor-ids (->> new-anchors
                                (map :id)
                                (filter #(not= (get old-anchor-by-id %)
                                               (get new-anchor-by-id %))))
        anchor-edits
        (mapcat (fn [anchor-id]
                  (let [old-anchor (get old-anchor-by-id anchor-id)
                        new-anchor (get new-anchor-by-id anchor-id)]
                    (when-not old-anchor
                      (throw (ex-info "comment-preserving writer does not support inserting anchors"
                                      {:anchor-id anchor-id})))
                    (diff-map-edits line-starts root-loc :anchors anchor-id old-anchor new-anchor)))
                changed-anchor-ids)
        coherence-edits
        (vector-append-edits line-starts
                             root-loc
                             :coherence-evidence
                             (vec (:coherence-evidence original-data))
                             (vec (:coherence-evidence data)))
        rewritten-text (apply-edits original-text (concat anchor-edits coherence-edits))
        rewritten-data (edn/read-string rewritten-text)]
    (when-not (= rewritten-data data)
      (throw (ex-info "comment-preserving rewrite did not reach requested data"
                      {:path path})))
    (write-text-atomically! path rewritten-text)
    {:ok true
     :path path
     :changed-anchor-ids (vec changed-anchor-ids)
     :coherence-rows-appended (- (count (:coherence-evidence data))
                                 (count (:coherence-evidence original-data)))}))
