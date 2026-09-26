(ns futon3c.diagramprover.wiring
  "Ingest mission wiring specs as typed open hypergraphs and report structural
  wiring findings."
  (:require [clojure.string :as str]
            [futon3c.diagramprover.graph :as graph]))

(defn vertex-key
  "The wire vertex an entry of a box's :reads/:writes names. A keyword is its
  own vertex, as it always was. `[field {:record r}]` scopes the field to the
  record r and is the vertex `[field r]`: the same key on two different records
  is two wires, and the one-writer rule holds per record. Anything else is
  its own vertex, unchanged."
  [entry]
  (if (and (vector? entry) (= 2 (count entry))
           (keyword? (first entry)) (map? (second entry)) (:record (second entry)))
    [(first entry) (:record (second entry))]
    entry))

(defn vertex-field
  "The field key of a vertex: the keyword itself, or the first of `[field r]`."
  [v]
  (if (vector? v) (first v) v))

(defn vertex-record
  "The record a vertex is scoped to, or nil for an unscoped field."
  [v]
  (when (vector? v) (second v)))

(defn- entries [box role] (map vertex-key (or (get box role) [])))

(defn- fields-in [boxes]
  (->> boxes
       (mapcat #(concat (entries % :reads) (entries % :writes)))
       distinct
       (sort-by str)))

(defn- scopes-of
  "field key -> set of the records some vertex of the map scopes it to."
  [vertices]
  (reduce (fn [m v] (if-let [r (vertex-record v)] (update m (vertex-field v) (fnil conj #{}) r) m))
          {} vertices))

(defn ingest
  "Turn an EDN wiring spec into an open hypergraph.

  Fields are wire vertices. Boxes are hyperedges from their read fields to
  their written fields. Missing read/write collections are empty."
  [{:keys [spec/id boxes]}]
  (let [[initial field->vertex]
        (reduce (fn [[g index] field]
                  (let [[g' vertex] (graph/add-vertex
                                     g (cond-> {:vtype :wiring/field :field field}
                                         (vertex-record field) (assoc :record (vertex-record field))))]
                    [g' (assoc index field vertex)]))
                [(graph/make-graph) {}]
                (fields-in boxes))]
    (reduce (fn [g box]
              (first
               (graph/add-edge
                g
                (mapv field->vertex (entries box :reads))
                (mapv field->vertex (entries box :writes))
                {:box/id (:box/id box)})))
            (assoc initial :spec/id id)
            (or boxes []))))

(defn written-never-read
  "Return findings for fields written by at least one box and read by none."
  [wiring-graph]
  (->> (graph/vertices wiring-graph)
       (keep (fn [vertex]
               (let [writers (graph/in-edges wiring-graph vertex)]
                 (when (and (seq writers)
                            (empty? (graph/out-edges wiring-graph vertex)))
                   {:finding :written-never-read
                    :field (:field (graph/vertex-data wiring-graph vertex))
                    :writers (->> writers
                                  (map #(-> (graph/edge-data wiring-graph %)
                                            :box/id))
                                  (sort-by str)
                                  vec)}))))
       (sort-by (comp str :field))
       vec))

(defn read-never-written
  "Return findings for fields read by at least one box and written by none."
  [wiring-graph]
  (->> (graph/vertices wiring-graph)
       (keep (fn [vertex]
               (let [readers (graph/out-edges wiring-graph vertex)]
                 (when (and (seq readers)
                            (empty? (graph/in-edges wiring-graph vertex)))
                   {:finding :read-never-written
                    :field (:field (graph/vertex-data wiring-graph vertex))
                    :readers (->> readers
                                  (map #(-> (graph/edge-data wiring-graph %)
                                            :box/id))
                                  (sort-by str)
                                  vec)}))))
       (sort-by (comp str :field))
       vec))

(defn multiply-written
  "Return findings for fields written by two or more boxes."
  [wiring-graph]
  (->> (graph/vertices wiring-graph)
       (keep (fn [vertex]
               (let [writers (graph/in-edges wiring-graph vertex)]
                 (when (<= 2 (count writers))
                   {:finding :multiply-written
                    :field (:field (graph/vertex-data wiring-graph vertex))
                    :writers (->> writers
                                  (map #(-> (graph/edge-data wiring-graph %)
                                            :box/id))
                                  (sort-by str)
                                  vec)}))))
       (sort-by (comp str :field))
       vec))

(defn- site-file
  "SITE's file, relative to its repo root."
  [site]
  (cond
    (:file site) (:file site)
    (:ns site) (str "src/"
                    (-> (:ns site)
                        (str/replace "." "/")
                        (str/replace "-" "_"))
                    ".clj")
    :else (throw (ex-info "Wiring site must name :file or :ns" {:site site}))))

(defn- site-path [repo-root site]
  (str (java.io.File. (str repo-root) (str (site-file site)))))

;; ---------------------------------------------------------------------------
;; A paren-aware scan of Clojure source text into a form tree. Not a reader:
;; it never evaluates, resolves or rejects anything, so `::alias/k`, reader
;; conditionals and tagged literals cost nothing. A node is
;; {:kind k :start s :end e :children [...]} with k one of :list :vector :map
;; :set :fn (#(...)) or :token; strings, regexes and comments are recorded
;; separately (they are text, not code). `^meta` is kept on the following
;; node as :meta and is not a child; `#_` discards the next form.

(def ^:private token-delimiters
  #{\( \) \[ \] \{ \} \" \; \,})

(defn- ws? [c] (or (Character/isWhitespace (char c)) (= c \,)))

(defn parse-forms
  "TEXT -> {:forms [top-level nodes] :text-spans [[start end kind] ...]}.
  :text-spans are the strings, regexes, comments and #_-discarded forms."
  [^String text]
  (let [n (count text)
        spans (volatile! [])
        at (fn [i] (when (< i n) (.charAt text i)))]
    (letfn [(skip-ws [i]
              (loop [i i]
                (let [c (at i)]
                  (cond (nil? c) i
                        (ws? c) (recur (inc i))
                        (= c \;) (let [e (let [j (.indexOf text "\n" (int i))] (if (neg? j) n j))]
                                   (vswap! spans conj [i e :comment])
                                   (recur e))
                        :else i))))
            (string-end [i]           ; i at the opening quote
              (loop [j (inc i)]
                (let [c (at j)]
                  (cond (nil? c) n
                        (= c \\) (recur (+ j 2))
                        (= c \") (inc j)
                        :else (recur (inc j))))))
            (token-end [i]
              (loop [j i]
                (let [c (at j)]
                  (if (or (nil? c) (ws? c) (token-delimiters c)) j (recur (inc j))))))
            (coll [i kind closer open-len]
              (loop [j (+ i open-len) kids []]
                (let [j (skip-ws j)
                      c (at j)]
                  (cond (nil? c) [{:kind kind :start i :end n :children kids} n]
                        (= c closer) [{:kind kind :start i :end (inc j) :children kids} (inc j)]
                        (#{\) \] \}} c) (recur (inc j) kids) ; stray closer: skip
                        :else (let [[node j'] (form j)]
                                (recur j' (if node (conj kids node) kids)))))))
            (form [i]                 ; -> [node-or-nil next-index]
              (let [i (skip-ws i)
                    c (at i)
                    c2 (at (inc i))]
                (cond
                  (nil? c) [nil n]
                  (= c \() (coll i :list \) 1)
                  (= c \[) (coll i :vector \] 1)
                  (= c \{) (coll i :map \} 1)
                  (= c \") (let [e (string-end i)] (vswap! spans conj [i e :string]) [nil e])
                  (= c \\) (let [e (token-end (+ i 2))]
                              [{:kind :token :start i :end e :text (subs text i e)} e])
                  (= c \^) (let [[_ j] (form (inc i))
                                 [node j'] (form j)]
                             [(when node (assoc node :meta true)) j'])
                  (#{\' \`} c) (let [[node j] (form (inc i))]
                                  [(when node (assoc node :quoted? true)) j])
                  (= c \@) (form (inc i))
                  (= c \~) (form (if (= c2 \@) (+ i 2) (inc i)))
                  (= c \#)
                  (cond (= c2 \{) (coll i :set \} 2)
                        (= c2 \() (coll i :fn \) 2)
                        (= c2 \") (let [e (string-end (inc i))] (vswap! spans conj [i e :regex]) [nil e])
                        (= c2 \_) (let [[node j] (form (+ i 2))]
                                    (when node (vswap! spans conj [(:start node) (:end node) :discard]))
                                    [nil j])
                        (= c2 \') (form (+ i 2))
                        (= c2 \?) (let [k (if (= (at (+ i 2)) \@) (+ i 3) (+ i 2))]
                                    (if (= (at k) \() (coll k :list \) 1) (form k)))
                        :else (let [e (token-end (inc i))]
                                [{:kind :token :start i :end e :text (subs text i e)} e]))
                  :else (let [e (token-end i)]
                          [{:kind :token :start i :end e :text (subs text i e)} (max e (inc i))]))))]
      (let [forms (loop [i 0 acc []]
                    (let [i (skip-ws i)]
                      (if (>= i n)
                        acc
                        (let [c (at i)]
                          (if (#{\) \] \}} c)
                            (recur (inc i) acc)
                            (let [[node j] (form i)]
                              (recur j (if node (conj acc node) acc))))))))]
        {:forms forms :text-spans @spans}))))

(defn var-form
  "The top-level form in TEXT whose second element is the symbol VAR-NAME
  (`(defn name …)`, `(def ^:private name …)`, …), as {:start :end :text},
  or nil. Found by `parse-forms`, a paren-aware text scan; no code is read
  or evaluated."
  [^String text var-name]
  (some (fn [{:keys [kind children start end quoted?]}]
          (let [named (second children)]
            (when (and (not quoted?) (= :list kind) (= :token (:kind named))
                       (= (str var-name) (:text named)))
              {:start start :end end :text (subs text start end)})))
        (:forms (parse-forms text))))

(defn- field-occurs? [text field]
  (boolean
   (re-find (re-pattern
             (str (java.util.regex.Pattern/quote (str field)) "(?![\\w-])"))
            text)))

;; ---------------------------------------------------------------------------
;; A textual read/write distinction. HEURISTIC: it classifies where a keyword
;; token sits in the form tree, not what the code does at runtime. A keyword
;; passed through a variable, built with `keyword`, or read by a helper that
;; takes it as data is :unclassified, and so is every occurrence in a string,
;; regex, comment or #_ form.

(def ^:private thread-heads #{"->" "->>" "some->" "some->>"})

(defn- head-text [node]
  (let [h (first (:children node))]
    (when (= :token (:kind h)) (:text h))))

(defn- thread-first-step?
  "NODE (a list) is a step of a thread-first form: position 2 onward of ->
  or some->, or a step position (3, 5, …; the tests are not threaded) of
  cond->. The threaded value is the step's missing first
  argument, so every argument sits one position earlier than in a plain
  call. ->> and cond->> thread last and are not covered; as-> names its
  value explicitly, so its steps are plain calls the ordinary rules read."
  [form idx]
  (when (and form idx (#{:list :fn} (:kind form)))
    (let [h (head-text form)]
      (or (and (#{"->" "some->"} h) (<= 2 idx))
          (and (= "cond->" h) (<= 3 idx) (odd? idx))))))

(defn- classify-keyword
  "WRITE: map-literal key, key argument of assoc/update, a key in the path of
  assoc-in/update-in. READ: argument of get, a key in the path of get-in, a
  keyword in function position (also as a step of ->, ->>, some->, some->>).
  Binding reads are classified separately by `binding-reads`. Inside a thread-first step
  (`thread-first-step?`: ->, some->, cond->) the same calls with the
  threaded argument omitted: (assoc :k v …) and (update :k f) write :k,
  (assoc-in [:k …] v) and (update-in [:k …] f) write the path's keys,
  (get :k) and (get-in [:k …]) read them. An entry of the key vector of
  (select-keys m [:k …]), or of the step (select-keys [:k …]), is a READ.
  Still :unclassified (a stated limit): the key of (dissoc m :k),
  (contains? m :k) and the keys of (rename-keys m {…}). Else :unclassified."
  [parent idx grand gidx great ggidx _field]
  (let [h (head-text parent)
        step? (thread-first-step? grand gidx)]
    (case (:kind parent)
      (:list :fn) (cond (zero? idx) :reads
                        (and (= "get" h) (= 2 idx)) :reads
                        (and (thread-heads h) (<= 2 idx)) :reads
                        (and (= "assoc" h) (<= 2 idx) (even? idx)) :writes
                        (and (= "update" h) (= 2 idx)) :writes
                        (and step? (= "get" h) (= 1 idx)) :reads
                        (and step? (= "assoc" h) (odd? idx)) :writes
                        (and step? (= "update" h) (= 1 idx)) :writes
                        :else :unclassified)
      :vector (let [gh (when (#{:list :fn} (:kind grand)) (head-text grand))
                    gstep? (thread-first-step? great ggidx)]
                (cond (and (= 2 gidx) (#{"get-in" "select-keys"} gh)) :reads
                      (and (= 2 gidx) (#{"assoc-in" "update-in"} gh)) :writes
                      (and gstep? (= 1 gidx) (#{"get-in" "select-keys"} gh)) :reads
                      (and gstep? (= 1 gidx) (#{"assoc-in" "update-in"} gh)) :writes
                      :else :unclassified))
      :map (if (even? idx) :writes :unclassified)
      :unclassified)))

(def ^:private path-fns #{"get-in" "assoc-in" "update-in"})
(def ^:private key-fns #{"get" "assoc" "update"})

(defn- token-text [node] (when (= :token (:kind node)) (:text node)))

;; ---------------------------------------------------------------------------
;; Return position. A box that declares :returns-record r says its site's
;; function returns a map literal of record r; the keys of a map literal in
;; return position are then writes of r. Heuristic, like `classify-keyword`:
;; the return positions of a defn are the last form of each body (each arity),
;; and, recursively, the branches of a trailing if/if-not/if-let/if-some, the
;; results of cond and case, and the last form of a trailing when/when-not/
;; when-let/when-some/do/let/loop (a `recur` is not a return). A map literal
;; passed as an argument, or in a non-final body form, is not in return position,
;; except as an argument of a returned (merge|into|conj ...), any position, or as the
;; key-value pairs of a returned (assoc m :k v ...): the returned value contains it.
;;
;; A let that binds a name to a map literal makes that literal reachable: a
;; return position that is the name, or (assoc|merge|update name ...), is the
;; literal (`literal-of`). The threaded first argument of a -> / cond-> in
;; return position is itself a return position, recursively: a literal, a
;; let-bound name, or a cond/if/case whose branches are the literals. The keys
;; the threaded steps add (assoc :k v) are not attributed. ->> and cond->> thread
;; last, so their first argument is not the value and they are NOT covered.
;; Return positions are [literal env] pairs, env being the let-bound literals
;; in scope.
;;
;; Nested: inside an attributed literal, a value at key :r that is a map literal
;; (or resolves to a let-bound one by `literal-of`) is attributed to record r
;; when some box at the site scopes a field to r (:scoped in `site-cfg`), and
;; only to r; a nested literal under a key no box scopes is not attributed.

(declare return-maps)

(defn- literal-of
  "The map literal NODE stands for under ENV: itself, the let-bound literal it
  names, or the one named as the first argument of assoc/merge/update/->/cond->."
  [node env]
  (when node
    (case (:kind node)
      :map node
      :token (get env (:text node))
      :list (when (#{"assoc" "merge" "update" "->" "cond->"} (head-text node))
              (literal-of (nth (:children node) 1 nil) env))
      nil)))

(defn- bind-literals
  "ENV extended by the name/init pairs of a let binding vector: a name whose
  init is (or names) a map literal is bound to it; any other rebinding of a
  name removes it."
  [env bindings]
  (if (= :vector (:kind bindings))
    (reduce (fn [env [sym init]]
              (if (and sym (= :token (:kind sym)))
                (if-let [l (literal-of init env)] (assoc env (:text sym) l) (dissoc env (:text sym)))
                env))
            env (partition 2 (:children bindings)))
    env))

(defn- return-maps
  "The [literal env] pairs in return position of NODE."
  [node env]
  (when node
    (case (:kind node)
      (:map :token) (when-let [l (literal-of node env)] [[l env]])
      (:list :fn)
      (let [kids (:children node) h (head-text node) n (count kids)
            branch #(mapcat (fn [x] (return-maps x env)) (keep (fn [i] (nth kids i nil)) %))]
        (case h
          ("if" "if-not" "if-let" "if-some") (branch [2 3])
          ("when" "when-not" "when-let" "when-some" "do") (when (< 1 n) (return-maps (peek kids) env))
          ("let" "let*" "loop") (when (< 2 n) (return-maps (peek kids) (bind-literals env (nth kids 1))))
          "cond" (mapcat #(return-maps % env) (map second (partition 2 (rest kids))))
          "case" (let [args (drop 2 kids)]
                   (concat (mapcat #(return-maps % env) (map second (partition 2 args)))
                           (when (odd? (count args)) (return-maps (last args) env))))
          ;; the threaded first argument of -> / cond-> is itself a return position
          ("->" "cond->") (when (< 1 n) (return-maps (nth kids 1) env))
          "update" (when-let [l (literal-of node env)] [[l env]])
          ;; (assoc m :k v ...): the first argument if it is (or names) a literal, and the
          ;; key-value pairs themselves, which `pairs-of` reads as a literal's entries
          "assoc" (concat (when-let [l (literal-of node env)] [[l env]])
                          [[node env]])
          ;; (merge|into|conj a b ...): every argument that is a map literal, or names a
          ;; let-bound one, in any position; other arguments (a param, a call) stay
          ;; unattributed, as does a literal nested in a call inside an argument
          ("merge" "into" "conj")
          (let [lits (keep (fn [a] (case (:kind a)
                                     :map a
                                     :token (get env (:text a))
                                     nil))
                           (rest kids))
                first-arg (literal-of node env)]
            (map (fn [l] [l env]) (distinct (concat (when first-arg [first-arg]) lits))))
          nil))
      nil)))

(defn- defn-return-maps
  "The [literal env] pairs in return position of the top-level defn/defn- FORM."
  [form]
  (when (and (#{:list} (:kind form)) (#{"defn" "defn-"} (head-text form)))
    (let [kids (drop 2 (:children form))
          arities (filter #(and (= :list (:kind %)) (= :vector (:kind (first (:children %))))) kids)]
      (if (seq arities)
        (mapcat #(return-maps (peek (:children %)) {}) arities)
        (let [after-params (rest (drop-while #(not= :vector (:kind %)) kids))]
          (when (seq after-params) (return-maps (last after-params) {})))))))

(defn- key->record [k]
  (when (and (= :token (:kind k)) (str/starts-with? (str (:text k)) ":"))
    (keyword (subs (:text k) 1))))

(defn- pairs-of
  "The [key value] entries of a literal: a map's, or the pairs of a returned (assoc m k v ...)."
  [literal]
  (partition 2 (if (= :map (:kind literal))
                 (:children literal)
                 (drop 2 (:children literal)))))

(defn- expand-owners
  "ACC (start -> #{records}) with LITERAL owned by OWNERS, and, recursively, the
  literals under its keys that name a scoped record (each owned by that record
  alone)."
  [acc literal env owners scoped]
  (let [acc (update acc (:start literal) (fnil into #{}) owners)]
    (reduce (fn [acc [k v]]
              (let [r (key->record k)
                    l (when (and r (scoped r)) (literal-of v env))]
                (if (and l (not (contains? (get acc (:start l) #{}) r)))
                  (expand-owners acc l env #{r} scoped)
                  acc)))
            acc (pairs-of literal))))

(defn- owner-map
  "start offset -> #{records} for every map literal whose keys count as writes
  of a record: the literals in return position (owned by the site's
  :returns-record records) and the scoped literals nested under them."
  [forms {:keys [returns scoped]}]
  (reduce (fn [acc form]
            (reduce (fn [acc [l env]] (expand-owners acc l env returns (or scoped #{})))
                    acc (defn-return-maps form)))
          {} forms))

(defn- site-cfg
  "What a site's boxes say about naming a record in its text: :aliases
  {record #{receiver-symbol ...}} from :record-aliases {:flight [\"f\" \"fl\"]},
  :returns #{record ...} from :returns-record, and :scoped #{record ...} (the
  records some box at the site scopes a field to). Read from every box that
  shares the site, because the aliases are names in that site's text."
  [boxes]
  {:aliases (reduce (fn [m b]
                      (reduce (fn [m [r names]] (update m r (fnil into #{}) names))
                              m (:record-aliases b)))
                    {} boxes)
   :returns (set (keep :returns-record boxes))
   :scoped (set (keep vertex-record (mapcat #(concat (entries % :reads) (entries % :writes)) boxes)))})

(defn- attributed-records
  "Of CANDIDATES (record keywords), those the occurrence of a field key names.
  Four forms name a record, and only these:
   * the key sits in the path of get-in/assoc-in/update-in whose FIRST element
     is the record's key: (get-in st [:sources :wants t]);
   * the receiver symbol is the record's name: (get-in sources [:wants t]),
     (get sources :wants), (assoc sources :wants v), (update sources :wants f);
   * a keyword call on such a receiver: (:target flight), where the receiver is
     the record's name or one of the site's declared :record-aliases;
   * the key of a map literal in return position of a site whose box declares
     :returns-record r (see `return-maps`): also a literal argument of a returned
     merge/into/conj and the key-value pairs of a returned assoc; or a scoped
     literal nested under it or under a let-bound literal it returns (see `owner-map`).
  Textual and heuristic like `classify-keyword`; a receiver threaded through
  -> is not seen (the path form still is). PARENT/IDX/GRAND/GIDX are the
  occurrence's enclosing node, its index there, and that node's parent; CFG is
  {:aliases :returns :scoped :owners} (see `site-cfg`, `owner-map`)."
  [parent idx grand gidx candidates cfg]
  (let [gh (when (and grand (#{:list :fn} (:kind grand))) (head-text grand))
        ph (when (and parent (#{:list :fn} (:kind parent))) (head-text parent))]
    (set
     (for [r candidates
           :let [rkey (str r) rname (name r)]
           :when (or (and (= :vector (:kind parent)) (path-fns gh)
                          (or (and (pos? idx) (= rkey (token-text (first (:children parent)))))
                              (and (= 2 gidx) (= rname (token-text (nth (:children grand) 1 nil))))))
                     (and (key-fns ph) (= 2 idx) (= rname (token-text (nth (:children parent) 1 nil))))
                     (and ph (zero? idx)
                          (contains? (conj (get (:aliases cfg) r #{}) rname)
                                     (token-text (nth (:children parent) 1 nil))))
                     (and (or (= :map (:kind parent))
                              (and (= "assoc" ph) (<= 2 idx)))
                          (even? idx)
                          (contains? (get (:owners cfg) (:start parent) #{}) r)))]
       r))))

(defn- quoted-form? [node]
  (or (:quoted? node)
      (and (= :list (:kind node))
           (#{"quote" "clojure.core/quote"} (head-text node)))))

(defn- binding-reads
  "Positions reading FIELD in actual binding patterns, with their record
  attribution. Data maps with a :keys entry are not bindings. Supports
  defn/defn-/fn/defmethod parameters and let/loop/conditional/comprehension
  bindings, including nested patterns. Unknown macro binding forms remain
  unclassified. A map's :as or its direct binding initializer may name a
  declared record alias; a nested pattern does not inherit its parent's
  record. Bare destructured parameters need positional evidence for scope.
  Quotation (including syntax quotation) is conservatively not evaluated."
  [forms field candidates cfg]
  (let [found (volatile! {})
        fstr (str field)
        records (fn [names]
                  (set (filter (fn [r]
                                 (some (conj (get (:aliases cfg) r #{}) (name r)) names))
                               candidates)))]
    (letfn [(pattern [node init]
              (when-not (quoted-form? node)
                (case (:kind node)
                  :vector (doseq [child (:children node)] (pattern child nil))
                  :map
                  (let [pairs (partition 2 (:children node))
                        as-name (some (fn [[k v]] (when (= ":as" (token-text k)) (token-text v))) pairs)
                        attr (records (remove nil? [as-name (when-not (quoted-form? init)
                                                             (token-text init))]))]
                    (doseq [[binding selector] pairs
                            :let [k (token-text binding)]]
                      (cond
                        (and k (or (= ":keys" k)
                                   (re-matches #":[^:/]+/keys" k)))
                        (doseq [sy (:children selector)
                                :let [s (token-text sy)]
                                :when (and s (not (quoted-form? sy)))
                                :let [s (str/replace s #"^:" "")
                                      key-text (if (= ":keys" k)
                                                 (str ":" s)
                                                 (str (subs k 0 (- (count k) 4))
                                                      (last (str/split s #"/"))))]
                                :when (= fstr key-text)]
                          (vswap! found assoc (:start sy) attr))
                        (and k (str/starts-with? k ":")) nil
                        :else
                        (do (when (= fstr (token-text selector))
                              (vswap! found assoc (:start selector) attr))
                            (pattern binding nil)))))
                  nil)))
            (bindings [v comprehension?]
              (when (= :vector (:kind v))
                (doseq [[pat init] (partition 2 (:children v))]
                  (cond
                    (and comprehension? (= ":let" (token-text pat))) (bindings init false)
                    (and comprehension? (#{":when" ":while"} (token-text pat))) nil
                    :else (pattern pat init)))))
            (parameters [args]
              (let [args (if (= :map (:kind (first args))) (rest args) args)]
                (if (= :vector (:kind (first args)))
                  (pattern (first args) nil)
                  (doseq [arity args :when (and (= :list (:kind arity))
                                               (not (quoted-form? arity))
                                               (= :vector (:kind (first (:children arity)))))]
                    (pattern (first (:children arity)) nil)))))
            (visit [node]
              (when-not (quoted-form? node)
                (let [kids (:children node)
                      h (when (= :list (:kind node)) (head-text node))]
                  (cond
                    (#{"defn" "defn-" "clojure.core/defn" "clojure.core/defn-"} h)
                    (parameters (drop 2 kids))
                    (#{"fn" "fn*" "clojure.core/fn"} h)
                    (parameters (if (= :token (:kind (second kids))) (drop 2 kids) (rest kids)))
                    (= "defmethod" h) (parameters (drop 3 kids))
                    (#{"let" "let*" "loop" "loop*" "if-let" "when-let" "if-some" "when-some"} h)
                    (bindings (second kids) false)
                    (#{"for" "doseq"} h) (bindings (second kids) true))
                  (doseq [child kids] (visit child)))))]
      (doseq [form forms] (visit form)))
    @found))

(defn- field-usage*
  "`field-usage`, optionally restricted by :only-record r (count only the
  occurrences that name record r) or :not-records rs (count only those that
  name none of rs). With neither, exactly `field-usage`. :cfg is the site's
  `site-cfg`."
  [^String text field {:keys [only-record not-records cfg]}]
  (let [fstr (str field)
        text-matches (count (re-seq (re-pattern
                                     (str (java.util.regex.Pattern/quote fstr) "(?![\\w-])"))
                                    text))
        uses (volatile! [])
        token-hits (volatile! 0)
        forms (:forms (parse-forms text))
        cfg (assoc cfg :owners (when (seq (:returns cfg)) (owner-map forms cfg)))
        cands (fn [] (cond only-record #{only-record} not-records not-records :else #{}))
        binding-uses (binding-reads forms field (cands) cfg)
        counts? (fn [attr] (cond only-record (contains? attr only-record)
                                 not-records (empty? attr)
                                 :else true))]
    (letfn [(visit [node parent idx grand gidx great ggidx]
              (cond
                (quoted-form? node) nil
                (= :token (:kind node))
                (cond
                  (contains? binding-uses (:start node))
                  (do (when (= fstr (:text node)) (vswap! token-hits inc))
                      (when (counts? (get binding-uses (:start node)))
                        (vswap! uses conj :reads)))
                  (= fstr (:text node))
                  (do (vswap! token-hits inc)
                      (when (counts? (when parent (attributed-records parent idx grand gidx (cands) cfg)))
                        (vswap! uses conj (if parent
                                            (classify-keyword parent idx grand gidx great ggidx field)
                                            :unclassified)))))
                :else
                (doseq [[i child] (map-indexed vector (:children node))]
                  (visit child node i parent idx grand gidx))))]
      (doseq [form forms]
        (visit form nil nil nil nil nil nil)))
    (let [f (frequencies @uses)]
      {:reads (get f :reads 0)
       :writes (get f :writes 0)
       ;; an occurrence in a string, regex or comment cannot be attributed to a
       ;; record: it belongs to the unscoped field, and to no scope
       :unclassified (+ (get f :unclassified 0)
                        (if only-record 0 (max 0 (- text-matches @token-hits))))})))

(defn field-usage
  "Occurrences of FIELD in TEXT as {:reads n :writes m :unclassified k}.
  Textual and heuristic (see `classify-keyword`): every occurrence the
  boundary-aware text match finds is counted exactly once, and one the form
  tree cannot place in a read or write position is :unclassified. A
  destructuring `{:keys [field-name]}` entry is a read (it is a symbol, not
  the keyword, in the text)."
  [^String text field]
  (field-usage* text field {}))

(defn- vertex-usage
  "Usage counts for VERTEX at TEXT. A scoped vertex `[field r]` counts only the
  occurrences that name r (`attributed-records`). An unscoped field that some
  vertex of the map scopes to records SCOPES counts only the occurrences that
  name none of them, so a field whose scoped writer lives at another site
  keeps its own occurrences. Any other field: `field-usage`, unchanged. CFG
  is the site's `site-cfg`."
  [text vertex scopes cfg]
  (let [f (vertex-field vertex)]
    (cond (vertex-record vertex) (field-usage* text f {:only-record (vertex-record vertex) :cfg cfg})
          (seq (scopes f)) (field-usage* text f {:not-records (scopes f) :cfg cfg})
          :else (field-usage text f))))

(defn- vertex-occurs? [text vertex scopes cfg]
  (if (or (vertex-record vertex) (seq (scopes (vertex-field vertex))))
    (pos? (reduce + (vals (vertex-usage text vertex scopes cfg))))
    (or (field-occurs? text vertex)
        (pos? (:reads (field-usage text vertex))))))

(defn- site-scope
  "{:text …} for SITE: the whole file, or with :var only that top-level form;
  {:error …} when the file cannot be read; {:var-not-found true} when :var
  names no top-level form (a finding, never a silent pass)."
  [repo-root site]
  (try
    (let [text (slurp (site-path repo-root site))]
      (if-let [v (:var site)]
        (if-let [f (var-form text v)]
          {:text (:text f)}
          {:var-not-found true})
        {:text text}))
    (catch Exception e
      {:error (str (.getMessage e))})))

;; ---------------------------------------------------------------------------
;; :passes -- a value handed positionally, checked at both ends.
;;
;; A box may carry :passes, a vector of
;;   {:value V                      ; a field entry: :observation or [:f {:record r}]
;;    :from  F                      ; where the argument comes from, one of
;;                                  ;   {:keyed-read [:f :record]}   (:f rec), (get rec :f),
;;                                  ;      (get-in rec [.. :f]), (get-in x [:record .. :f]), or a
;;                                  ;      local bound to one (also {:keys [f]} of rec)
;;                                  ;   {:returns-of "ns/fn"}         a call to fn, or a local bound to one
;;                                  ;   {:literal-arg-key :k}         a map literal (under assoc/merge/
;;                                  ;      update/->/cond->, or let-bound) that has key :k
;;                                  ;   {:element-of F'}              (get local expr) where local is F'
;;    :to    {:call "ns/fn" :arg n :callee-box id}}   ; n is 1-based
;; and it is EVIDENCE OF OCCURRENCE, for the declaring box and the callee box, only
;; when all three hold:
;;   (a) the call exists: in the caller's site text (its :var scope) a form whose head
;;       is :call as written, or (partial call ...) with the argument inside the leading
;;       ones; the head may not be computed;
;;   (b) the nth argument's provenance is F, followed through let, loop and
;;       {:keys [..]} destructuring, and through the return positions of if/cond/case/
;;       let/do. A loop local with several sources (its init and each recur) needs each
;;       source accepted, at least one of them real; a source that is the return of the
;;       SAME passing call is typed :self-recurrent and does not count as real;
;;   (c) the callee box's site var is that call's fn, and in the arity matching the call's
;;       argument count its nth parameter is a plain symbol used in the body.
;; Where a condition fails the declared entries get no evidence, so they are
;; :declaration-without-occurrence, with :passes-failed naming the first failure.
;; LIMITS (the next ones): a function value passed as an argument and called under
;; the parameter's name (:via-param); a map literal that is an element of a returned
;; sequence ((cons step ...)); a call threaded through -> / ->>; shadowing in the
;; callee body is not modelled; the receiver of a keyed read must be a symbol.

(defn- walk-nodes
  "Every [node ancestors] in NODE's subtree (NODE included), ancestors being the
  chain from BASE-CHAIN down to the node's parent. Quoted data is not a call
  or a use of a parameter."
  [node base-chain]
  (when-not (quoted-form? node)
    (cons [node base-chain]
          (mapcat #(walk-nodes % (conj base-chain node)) (:children node)))))

(defn- contains-node? [outer inner]
  (and (<= (:start outer) (:start inner)) (<= (:end inner) (:end outer))))

(defn- record-names [r aliases] (conj (get aliases r #{}) (name r)))

(defn- return-leaves
  "The forms in return position of NODE, any kind (a call, a symbol, a literal);
  a `recur` is no return."
  [node]
  (if (or (quoted-form? node) (not (#{:list :fn} (:kind node))))
    [node]
    (let [kids (:children node) h (head-text node) n (count kids)
          at (fn [is] (mapcat return-leaves (keep #(nth kids % nil) is)))]
      (case h
        ("if" "if-not" "if-let" "if-some") (at [2 3])
        ("when" "when-not" "when-let" "when-some" "do" "let" "let*" "loop")
        (if (< 1 n) (return-leaves (peek kids)) [node])
        "cond" (mapcat return-leaves (map second (partition 2 (rest kids))))
        "case" (let [args (drop 2 kids)]
                 (concat (mapcat return-leaves (map second (partition 2 args)))
                         (when (odd? (count args)) (return-leaves (last args)))))
        "recur" []
        [node]))))

(defn- pattern-entries
  "The names a binding pattern binds, from INIT: a symbol, or a {:keys [a b] :as m} map."
  [pat init]
  (cond
    (= :token (:kind pat)) [{:sym (:text pat) :init init}]
    (= :map (:kind pat))
    (let [pairs (partition 2 (:children pat))]
      (concat
       (for [[k v] pairs :when (and (= ":keys" (token-text k)) (= :vector (:kind v)))
             sy (:children v) :when (= :token (:kind sy))]
         {:sym (:text sy) :init init :key (str ":" (:text sy))})
       (for [[k v] pairs :when (and (= ":as" (token-text k)) (= :token (:kind v)))]
         {:sym (:text v) :init init})))
    :else []))

(defn- binding-pairs [form]
  (let [v (nth (:children form) 1 nil)]
    (if (= :vector (:kind v))
      (vec (map-indexed (fn [i [p init]] {:i i :pat p :init init}) (partition 2 (:children v))))
      [])))

(defn- sym-sources
  "The bindings of symbol SYM visible at NODE, whose ancestors are CHAIN: nil when
  none (a parameter or a global), else {:entries [...] :chain chain-of-the-binding-form
  :loop? bool :pair-index i :form form}. Innermost let/let*/loop first; inside a
  binding vector only the earlier pairs are visible."
  [sym node chain]
  (loop [i (dec (count chain)) child node]
    (when-not (neg? i)
      (let [anc (nth chain i)
            h (when (#{:list} (:kind anc)) (head-text anc))]
        (if-not (#{"let" "let*" "loop"} h)
          (recur (dec i) anc)
          (let [pairs (binding-pairs anc)
                in-vector? (= :vector (:kind child))
                visible (if in-vector?
                          (let [j (some (fn [{:keys [i init]}] (when (contains-node? init node) i)) pairs)]
                            (if j (filter #(< (:i %) j) pairs) pairs))
                          pairs)
                hit (last (filter (fn [pr] (some #(= sym (:sym %)) (pattern-entries (:pat pr) (:init pr)))) visible))]
            (if hit
              {:entries (filter #(= sym (:sym %)) (pattern-entries (:pat hit) (:init hit)))
               :chain (subvec chain 0 i) :form anc :loop? (= "loop" h) :pair-index (:i hit)
               :vector (nth (:children anc) 1)}
              (recur (dec i) anc))))))))

(declare prov)

(def ^:private no-prov {:ok? true :real? false :self? false})

(defn- combine [rs]
  (if-let [bad (first (remove :ok? rs))]
    bad
    {:ok? true :real? (boolean (some :real? rs)) :self? (boolean (some :self? rs))}))

(defn- keyed-form-ok?
  "NODE is a keyed read of field F on a receiver naming record R."
  [node f r aliases]
  (let [kids (:children node) h (head-text node) names (record-names r aliases)
        fk (str f) rn (fn [i] (token-text (nth kids i nil)))]
    (boolean
     (and (#{:list :fn} (:kind node))
          (or (and (= fk (token-text (first kids))) (contains? names (rn 1)))
              (and (= "get" h) (contains? names (rn 1)) (= fk (rn 2)))
              (and (= "get-in" h)
                   (let [path (nth kids 2 nil)
                         path-toks (when (= :vector (:kind path)) (map token-text (:children path)))]
                     (and (some #{fk} path-toks)
                          (or (contains? names (rn 1)) (= (str r) (first path-toks)))))))))))

(defn- literal-node
  "The map literal NODE stands for: itself, a let-bound one it names, or the first
  argument of assoc/merge/update/->/cond->."
  [node chain]
  (case (:kind node)
    :map node
    :token (when-let [src (sym-sources (:text node) node chain)]
             (some (fn [e] (literal-node (:init e) (conj (:chain src) (:form src) (:vector src)))) (:entries src)))
    :list (when (#{"assoc" "merge" "update" "->" "cond->"} (head-text node))
            (some-> (nth (:children node) 1 nil) (literal-node (conj chain node))))
    nil))

(defn- prov-leaf
  [spec node chain ctx depth]
  (let [{:keys [aliases call visited]} ctx
        fail (fn [why] {:ok? false :real? false :self? false :why why})]
    (cond
      (quoted-form? node) (fail :quoted-data)
      (< 8 depth) (fail :provenance-too-deep)
      (and (#{:list} (:kind node)) (= call (head-text node)) (not (:returns-of spec)))
      {:ok? true :real? false :self? true}
      (:element-of spec)
      (if (and (= "get" (head-text node)) (< 2 (count (:children node))))
        (prov (:element-of spec) (nth (:children node) 1) (conj chain node) ctx (inc depth))
        (fail :not-an-element))
      (= :token (:kind node))
      (let [t (:text node)]
        (if (or (str/starts-with? t ":") (re-matches #"[-+]?\d.*" t))
          (fail :not-a-symbol)
          (if-let [src (sym-sources t node chain)]
            (let [k [t (:start (:vector src)) (:pair-index src)]]
              (if (contains? visited k)
                no-prov
                (let [ctx (update ctx :visited conj k)
                      inits (for [e (:entries src)]
                                  (if (:key e)
                                    (if (and (:keyed-read spec)
                                             (= (:key e) (str (first (:keyed-read spec))))
                                             (contains? (record-names (second (:keyed-read spec)) aliases)
                                                        (token-text (:init e))))
                                      {:ok? true :real? true :self? false}
                                      (fail :destructured-key-not-declared))
                                    (prov spec (:init e) (conj (:chain src) (:form src) (:vector src)) ctx (inc depth))))
                          recurs (when (:loop? src)
                                   (for [[n cch] (walk-nodes (:form src) (:chain src))
                                         :when (and (#{:list} (:kind n)) (= "recur" (head-text n)))
                                         :let [a (nth (:children n) (inc (:pair-index src)) nil)]
                                         :when a]
                                     (prov spec a (conj cch n) ctx (inc depth))))]
                  (combine (concat inits recurs)))))
            (fail :unbound-symbol))))
      (:keyed-read spec)
      (let [[f r] (:keyed-read spec)]
        (if (keyed-form-ok? node f r aliases) {:ok? true :real? true :self? false} (fail :not-the-keyed-read)))
      (:returns-of spec)
      (if (and (#{:list} (:kind node)) (= (:returns-of spec) (head-text node)))
        {:ok? true :real? true :self? false}
        (fail :not-a-call-to-the-source))
      (:literal-arg-key spec)
      (if-let [lit (literal-node node chain)]
        (if (some #(= (str (:literal-arg-key spec)) (token-text %)) (take-nth 2 (:children lit)))
          {:ok? true :real? true :self? false}
          (fail :literal-lacks-the-key))
        (fail :not-a-literal))
      :else (fail :unknown-from))))

(defn- prov [spec node chain ctx depth]
  (combine (map #(prov-leaf spec % chain ctx depth) (return-leaves node))))

(defn- call-sites
  "[call-node ancestors arg-index-shift] for every call of CALL in FORMS: a form headed
  by CALL (shift 0) or (partial CALL ...) (shift 1)."
  [forms call]
  (for [form forms
        [n chain] (walk-nodes form [])
        :when (#{:list} (:kind n))
        :let [h (head-text n)
              shift (cond (= call h) 0
                          (and (= "partial" h) (= call (token-text (nth (:children n) 1 nil)))) 1)]
        :when shift]
    [n chain shift]))

(defn- callee-check
  "Condition (c): the callee's nth parameter, in the arity matching ARGC (or the first
  with enough parameters when the call is a partial), is a plain symbol used in the body."
  [callee-text call var n argc partial?]
  (let [form (first (:forms (parse-forms callee-text)))
        kids (drop 2 (:children form))
        arities (filter #(and (= :list (:kind %)) (= :vector (:kind (first (:children %))))) kids)
        candidates (if (seq arities)
                     (map (fn [a] [(first (:children a)) (rest (:children a))]) arities)
                     (let [pv (first (filter #(= :vector (:kind %)) kids))]
                       (when pv [[pv (rest (drop-while #(not (identical? pv %)) kids))]])))
        pick (first (filter (fn [[pv _]] (let [c (count (:children pv))]
                                           (if partial? (<= n c) (= argc c))))
                            candidates))]
    (cond
      (not (and form (#{"defn" "defn-"} (head-text form)))) {:ok? false :why :callee-not-a-defn}
      (not= var (token-text (nth (:children form) 1 nil))) {:ok? false :why :callee-var-mismatch}
      (not= var (last (str/split call #"/"))) {:ok? false :why :callee-var-mismatch}
      (nil? pick) {:ok? false :why :passes-arity-mismatch}
      :else
      (let [[pv body] pick
            p (nth (:children pv) (dec n) nil)
            sym (token-text p)]
        (cond
          (or (nil? sym) (str/starts-with? sym "&") (str/starts-with? sym ":"))
          {:ok? false :why :param-not-a-plain-symbol}
          (not (some (fn [b] (some (fn [[nn _]] (= sym (token-text nn))) (walk-nodes b []))) body))
          {:ok? false :why :param-unused}
          :else {:ok? true})))))

(defn- check-pass
  "Conditions (a)-(c) for PASS carried by BOX. Returns {:ok? bool :why kw :self-recurrent? bool}."
  [repo-root by-id aliases box pass]
  (let [{:keys [from to value]} pass
        {:keys [call arg callee-box]} to
        callee (get by-id callee-box)
        caller-text (:text (site-scope repo-root (:site box)))
        callee-scope (when callee (site-scope repo-root (:site callee)))]
    (cond
      (not (and value from call (pos-int? arg) callee-box)) {:ok? false :why :malformed-passes}
      (not callee) {:ok? false :why :callee-box-unknown}
      (nil? caller-text) {:ok? false :why :caller-site-unreadable}
      (nil? (:text callee-scope)) {:ok? false :why :callee-site-unreadable}
      :else
      (let [forms (:forms (parse-forms caller-text))
            sites (call-sites forms call)]
        (if (empty? sites)
          {:ok? false :why :call-not-found}
          (let [results (for [[n chain shift] sites
                              :let [a (nth (:children n) (+ arg shift) nil)]]
                          (assoc (if-not a
                                   {:ok? false :why :call-has-too-few-arguments}
                                   (prov from a (conj chain n) {:aliases aliases :call call :visited #{}} 0))
                                 :argc (dec (count (:children n))) :partial? (pos? shift)))
                good (first (filter #(and (:ok? %) (:real? %)) results))]
            (if-not good
              {:ok? false :why (or (:why (first (remove :ok? results))) :argument-has-no-real-source)}
              (let [c (callee-check (:text callee-scope) call (:var (:site callee)) arg
                                    (:argc good) (:partial? good))]
                (if (:ok? c)
                  {:ok? true :self-recurrent? (boolean (:self? good))}
                  c)))))))))

(defn- passes-evidence
  "{:evidence {[box-id vertex role] 1} :failed {[box-id vertex] why}} over every :passes of
  BOXES. A verified pass is evidence for the declaring box in each role it declares the
  value in, and for the callee box's :reads of it."
  [repo-root boxes cfg-of]
  (let [by-id (into {} (map (juxt :box/id identity)) boxes)]
    (reduce
     (fn [acc box]
       (reduce
        (fn [acc pass]
          (let [v (vertex-key (:value pass))
                cb (get-in pass [:to :callee-box])
                res (check-pass repo-root by-id (:aliases (cfg-of (:site box))) box pass)]
            (if (:ok? res)
              (-> acc
                  (update :evidence
                          (fn [ev]
                            (cond-> ev
                              (some #{v} (entries box :reads)) (assoc [(:box/id box) v :reads] 1)
                              (some #{v} (entries box :writes)) (assoc [(:box/id box) v :writes] 1)
                              (some #{v} (entries (get by-id cb) :reads)) (assoc [cb v :reads] 1))))
                  (update :self-recurrent (fnil into #{}) (when (:self-recurrent? res) [[(:box/id box) v]])))
              (-> acc
                  (update :failed assoc [(:box/id box) v] (:why res))
                  (update :failed assoc [cb v] (:why res))))))
        acc (:passes box)))
     {:evidence {} :failed {}} boxes)))

(defn conformance
  "Compare declared box reads/writes with occurrences in their named sites.

  Occurrence scanning is deliberately textual and counts comments. A commented
  reader is useful drift to surface, and this cheap check intentionally avoids
  pretending to provide parser-level precision. A site is {:file …} or {:ns …},
  scanned whole, or either with :var \"name\", scanned over that one
  top-level form only (found by `var-form`'s paren-aware text scan); a :var
  naming no form is the finding :var-not-found. Boxes without a site are
  exempt from conformance findings.

  With {:heuristic? true}, a declared field that occurs at its site but has
  no occurrence in the declared role's position is the finding
  :declared-write-not-found or :declared-read-not-found, carrying the
  `field-usage` counts and :heuristic true. Off by default, so a map's report
  without the option is the report it had before the heuristic existed.

  Direction is checked only with :heuristic? true: a default run checks that
  each declared field occurs at its site, never whether it is read or
  written there. An :unclassified occurrence is the heuristic's stated
  limit, not a defect. Example: the worked War Machine map's live-wiring
  test reads the three A/B/D hashes as members of a set literal
  (`(= #{:observation-model-hash ...} ...)`), a read no position rule sees,
  so each is :declared-read-not-found with {:unclassified 1}."
  ([repo-root spec] (conformance repo-root spec {}))
  ([repo-root {:keys [boxes]} {:keys [heuristic?]}]
   (let [boxes (or boxes [])
         field-universe (set (fields-in boxes))
         scopes (scopes-of field-universe)
         boxes-by-site (group-by :site (filter :site boxes))
         cfg-of (into {} (map (fn [[site bs]] [site (site-cfg bs)])) boxes-by-site)
         {:keys [evidence failed]} (if (some :passes boxes)
                                     (passes-evidence repo-root boxes cfg-of)
                                     {:evidence {} :failed {}})
         evidence-of (fn [box field role] (get evidence [(:box/id box) field role] 0))
         evidenced? (fn [box field role] (pos? (evidence-of box field role)))
         ;; An unreadable or malformed site is a FINDING, not an exception: the
         ;; checker's own boundary must not escape unstructured (the ToolBackend
         ;; lesson from the peripheral session, applied to the verifier itself).
         site-text (into {}
                         (map (fn [[site _]]
                                [site (site-scope repo-root site)]))
                         boxes-by-site)
         unreadable-sites
         (for [[site {:keys [error var-not-found]}] site-text
               :when (or error var-not-found)]
           (if error
             {:finding :site-unreadable :site site :error error}
             {:finding :var-not-found :site site :var (:var site)}))
         missing-declarations
         (for [box boxes
               :let [site (:site box)
                     text (get-in site-text [site :text])]
               :when (and site text)
               [role fields] [[:reads (entries box :reads)]
                              [:writes (entries box :writes)]]
               field fields
               :when (not (or (vertex-occurs? text field scopes (cfg-of site))
                              (evidenced? box field role)))]
           (cond-> {:finding :declaration-without-occurrence
                    :box/id (:box/id box)
                    :field field
                    :role role
                    :site site}
             (contains? failed [(:box/id box) field])
             (assoc :passes-failed (get failed [(:box/id box) field]))))
         undeclared-occurrences
         (for [[site site-boxes] boxes-by-site
               :let [text (get-in site-text [site :text])
                     declared-here (set (fields-in site-boxes))]
               :when text
               field field-universe
               :when (and (not (contains? declared-here field))
                          (vertex-occurs? text field scopes (cfg-of site)))]
           {:finding :occurrence-without-declaration
            :field field
            :site site
            :declared-by []})
         role-mismatches
         (when heuristic?
           (for [box boxes
                 :let [site (:site box)
                       text (get-in site-text [site :text])]
                 :when (and site text)
                 [role fields] [[:reads (entries box :reads)]
                                [:writes (entries box :writes)]]
                 field fields
                 :when (or (vertex-occurs? text field scopes (cfg-of site))
                           (evidenced? box field role))
                 :let [u (update (vertex-usage text field scopes (cfg-of site)) role
                                 + (evidence-of box field role))]
                 :when (zero? (get u role))]
             {:finding (if (= :writes role) :declared-write-not-found :declared-read-not-found)
              :box/id (:box/id box)
              :field field
              :role role
              :site site
              :usage u
              :heuristic true}))]
     (->> (concat unreadable-sites missing-declarations undeclared-occurrences role-mismatches)
          (sort-by (juxt (comp str :finding) (comp str :field)))
          vec))))

(defn usage
  "Per declared (box, role, field) at a readable site, the `field-usage`
  counts {:reads n :writes m :unclassified k}, marked :heuristic true. A verified
  :passes adds its evidence (one occurrence in the role) to the box's entry."
  [repo-root {:keys [boxes]}]
  (let [boxes (or boxes [])
        scopes (scopes-of (fields-in boxes))
        cfg-of (into {} (map (fn [[site bs]] [site (site-cfg bs)]))
                     (group-by :site (filter :site boxes)))
        evidence (when (some :passes boxes) (:evidence (passes-evidence repo-root boxes cfg-of)))]
    (vec
     (for [box boxes
           :let [site (:site box)
                 text (when site (:text (site-scope repo-root site)))]
           :when text
           [role fields] [[:reads (entries box :reads)]
                          [:writes (entries box :writes)]]
           field fields]
       {:box/id (:box/id box) :site site :role role :field field
        :usage (update (vertex-usage text field scopes (cfg-of site)) role
                       + (get evidence [(:box/id box) field role] 0))
        :heuristic true}))))

(defn- normal-path [root path]
  (let [f (java.io.File. (str path))
        f (if (.isAbsolute f) f (java.io.File. (str root) (str path)))]
    (str (.normalize (.toAbsolutePath (.toPath f))))))

(defn load-closure-findings
  "Every site's file must be in LOAD-CLOSURE, else the finding
  :site-not-in-load-closure. LOAD-CLOSURE is the test registry's run-record
  field `:load-closure` (futon3c.test-registry, `closure-from-entries`: a
  vector of {:ns :path :sha256}, :path repo-relative inside the registered
  repo and absolute outside it) or a plain collection of paths. Site paths
  resolve against REPO-ROOT, relative closure paths against :closure-root
  (default REPO-ROOT); both are normalised, not canonicalised, as the
  registry records them. Pure apart from path arithmetic: nothing is read."
  ([repo-root spec load-closure] (load-closure-findings repo-root spec load-closure {}))
  ([repo-root {:keys [boxes]} load-closure {:keys [closure-root]}]
   (let [closure (set (map #(normal-path (or closure-root repo-root)
                                         (if (map? %) (:path %) %))
                           load-closure))]
     (->> (for [site (distinct (keep :site boxes))
                :let [path (normal-path repo-root (site-file site))]
                :when (not (contains? closure path))]
            {:finding :site-not-in-load-closure :site site :path path})
          (sort-by (comp str :path))
          vec))))

(defn sites-resolve?
  "True iff every site of SPEC is in LOAD-CLOSURE (see `load-closure-findings`)."
  [repo-root spec load-closure & [opts]]
  (empty? (load-closure-findings repo-root spec load-closure (or opts {}))))

(defn phase-chain-findings
  "Report structural defects in a declared cycle phase chain.

  The cycle convention documented beside `phase-order` in
  `futon3c.peripheral.problem` is that the engine clears the cycle when an
  advance returns the last phase. The last phase is therefore a transition,
  never an enterable state, and tools declared there are unreachable."
  [{:keys [phases]}]
  (if-not phases
    []
    (let [order (vec (or (:order phases) []))
          tools (or (:tools phases) {})
          order-set (set order)
          terminal (peek order)
          terminal-tools (get tools terminal #{})
          terminal-finding
          (when (and terminal (seq terminal-tools))
            {:finding :terminal-phase-with-tools
             :phase terminal
             :tools (vec (sort-by str terminal-tools))})
          orphan-tool-phases
          (for [phase (keys tools)
                :when (not (contains? order-set phase))]
            {:finding :phase-tools-without-phase :phase phase})
          duplicate-phases
          (for [[phase n] (frequencies order)
                :when (< 1 n)]
            {:finding :duplicate-phase :phase phase})]
      (->> (concat (when terminal-finding [terminal-finding])
                   orphan-tool-phases
                   duplicate-phases)
           (sort-by (juxt (comp str :finding) (comp str :phase)))
           vec))))
