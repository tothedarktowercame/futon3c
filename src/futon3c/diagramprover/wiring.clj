(ns futon3c.diagramprover.wiring
  "Ingest mission wiring specs as typed open hypergraphs and report structural
  wiring findings."
  (:require [clojure.string :as str]
            [futon3c.diagramprover.graph :as graph]))

(defn- fields-in [boxes]
  (->> boxes
       (mapcat #(concat (or (:reads %) []) (or (:writes %) [])))
       distinct
       (sort-by str)))

(defn ingest
  "Turn an EDN wiring spec into an open hypergraph.

  Fields are wire vertices. Boxes are hyperedges from their read fields to
  their written fields. Missing read/write collections are empty."
  [{:keys [spec/id boxes]}]
  (let [[initial field->vertex]
        (reduce (fn [[g index] field]
                  (let [[g' vertex] (graph/add-vertex
                                     g {:vtype :wiring/field :field field})]
                    [g' (assoc index field vertex)]))
                [(graph/make-graph) {}]
                (fields-in boxes))]
    (reduce (fn [g box]
              (first
               (graph/add-edge
                g
                (mapv field->vertex (or (:reads box) []))
                (mapv field->vertex (or (:writes box) []))
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
                  (#{\' \` \@} c) (form (inc i))
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
  (some (fn [{:keys [kind children start end]}]
          (let [named (second children)]
            (when (and (= :list kind) (= :token (:kind named))
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

(defn- keys-vector?
  "PARENT (a vector) is the value of :keys (or :ns/keys for a field in ns) in
  GRAND, a destructuring map."
  [grand gidx field]
  (and (= :map (:kind grand)) (odd? gidx)
       (let [k (:text (nth (:children grand) (dec gidx)))]
         (= k (if-let [ns (namespace field)] (str ":" ns "/keys") ":keys")))))

(defn- classify-keyword
  "WRITE: map-literal key, key argument of assoc/update, a key in the path of
  assoc-in/update-in. READ: argument of get, a key in the path of get-in, a
  keyword in function position (also as a step of ->, ->>, some->, some->>),
  an entry of a destructuring :keys vector. Else :unclassified."
  [parent idx grand gidx field]
  (let [h (head-text parent)]
    (case (:kind parent)
      (:list :fn) (cond (zero? idx) :reads
                        (and (= "get" h) (= 2 idx)) :reads
                        (and (thread-heads h) (<= 2 idx)) :reads
                        (and (= "assoc" h) (<= 2 idx) (even? idx)) :writes
                        (and (= "update" h) (= 2 idx)) :writes
                        :else :unclassified)
      :vector (let [gh (when (#{:list :fn} (:kind grand)) (head-text grand))]
                (cond (and (= 2 gidx) (= "get-in" gh)) :reads
                      (and (= 2 gidx) (#{"assoc-in" "update-in"} gh)) :writes
                      (keys-vector? grand gidx field) :reads
                      :else :unclassified))
      :map (if (even? idx) :writes :unclassified)
      :unclassified)))

(defn field-usage
  "Occurrences of FIELD in TEXT as {:reads n :writes m :unclassified k}.
  Textual and heuristic (see `classify-keyword`): every occurrence the
  boundary-aware text match finds is counted exactly once, and one the form
  tree cannot place in a read or write position is :unclassified. A
  destructuring `{:keys [field-name]}` entry is a read (it is a symbol, not
  the keyword, in the text)."
  [^String text field]
  (let [fstr (str field)
        fname (name field)
        text-matches (count (re-seq (re-pattern
                                     (str (java.util.regex.Pattern/quote fstr) "(?![\\w-])"))
                                    text))
        uses (volatile! [])
        token-hits (volatile! 0)]
    (letfn [(visit [node parent idx grand gidx]
              (if (= :token (:kind node))
                (cond
                  (= fstr (:text node))
                  (do (vswap! token-hits inc)
                      (vswap! uses conj (if parent
                                          (classify-keyword parent idx grand gidx field)
                                          :unclassified)))
                  (and (= fname (:text node)) parent (= :vector (:kind parent))
                       (keys-vector? grand gidx field))
                  (vswap! uses conj :reads))
                (doseq [[i child] (map-indexed vector (:children node))]
                  (visit child node i parent idx))))]
      (doseq [form (:forms (parse-forms text))]
        (visit form nil nil nil nil)))
    (let [f (frequencies @uses)]
      {:reads (get f :reads 0)
       :writes (get f :writes 0)
       :unclassified (+ (get f :unclassified 0) (max 0 (- text-matches @token-hits)))})))

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
  without the option is the report it had before the heuristic existed."
  ([repo-root spec] (conformance repo-root spec {}))
  ([repo-root {:keys [boxes]} {:keys [heuristic?]}]
   (let [boxes (or boxes [])
         field-universe (set (fields-in boxes))
         boxes-by-site (group-by :site (filter :site boxes))
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
               [role fields] [[:reads (or (:reads box) [])]
                              [:writes (or (:writes box) [])]]
               field fields
               :when (not (field-occurs? text field))]
           {:finding :declaration-without-occurrence
            :box/id (:box/id box)
            :field field
            :role role
            :site site})
         undeclared-occurrences
         (for [[site site-boxes] boxes-by-site
               :let [text (get-in site-text [site :text])
                     declared-here (set (fields-in site-boxes))]
               :when text
               field field-universe
               :when (and (not (contains? declared-here field))
                          (field-occurs? text field))]
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
                 [role fields] [[:reads (or (:reads box) [])]
                                [:writes (or (:writes box) [])]]
                 field fields
                 :when (field-occurs? text field)
                 :let [u (field-usage text field)]
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
  counts {:reads n :writes m :unclassified k}, marked :heuristic true."
  [repo-root {:keys [boxes]}]
  (vec
   (for [box (or boxes [])
         :let [site (:site box)
               text (when site (:text (site-scope repo-root site)))]
         :when text
         [role fields] [[:reads (or (:reads box) [])]
                        [:writes (or (:writes box) [])]]
         field fields]
     {:box/id (:box/id box) :site site :role role :field field
      :usage (field-usage text field) :heuristic true})))

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
